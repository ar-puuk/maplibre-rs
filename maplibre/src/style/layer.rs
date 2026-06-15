//! Vector tile layer drawing utilities.

use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
};

use cint::{Alpha, EncodedSrgb};
use csscolorparser::Color;
use serde::{Deserialize, Deserializer, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum StyleProperty<T> {
    Constant(T),
    Expression(serde_json::Value),
}

impl<T: std::str::FromStr + Clone> StyleProperty<T> {
    pub fn evaluate(&self, feature_properties: &HashMap<String, String>) -> Option<T> {
        match self {
            StyleProperty::Constant(value) => Some(value.clone()),
            StyleProperty::Expression(expr) => {
                Self::eval_expr(expr, feature_properties)
            }
        }
    }

    fn eval_expr(expr: &serde_json::Value, feature_properties: &HashMap<String, String>) -> Option<T> {
        let arr = expr.as_array()?;
        let op = arr.first().and_then(|v| v.as_str())?;

        match op {
            "match" if arr.len() > 3 => {
                // ["match", ["get", "prop"], key_or_keys, value, ..., fallback]
                // Keys can be: a string, a number, or an array of strings/numbers.
                let get_arr = arr.get(1).and_then(|v| v.as_array())?;
                if get_arr.first().and_then(|v| v.as_str()) != Some("get") {
                    return None;
                }
                let prop_name = get_arr.get(1).and_then(|v| v.as_str())?;

                let feature_val = match feature_properties.get(prop_name) {
                    None => return arr.last().and_then(|v| v.as_str()).and_then(|s| s.parse::<T>().ok()),
                    Some(v) => v,
                };

                let mut i = 2;
                while i < arr.len() - 1 {
                    let key_val = match arr.get(i) {
                        Some(v) => v,
                        None => break,
                    };
                    let matches = if let Some(match_keys) = key_val.as_array() {
                        match_keys.iter().any(|k| {
                            k.as_str().map(|s| s == feature_val.as_str()).unwrap_or(false)
                                || k.as_i64().map(|n| n.to_string() == *feature_val).unwrap_or(false)
                                || k.as_f64().map(|n| n.to_string() == *feature_val).unwrap_or(false)
                        })
                    } else if let Some(key_str) = key_val.as_str() {
                        key_str == feature_val.as_str()
                    } else if let Some(key_num) = key_val.as_i64() {
                        key_num.to_string() == *feature_val
                    } else {
                        false
                    };

                    if matches {
                        return arr.get(i + 1).and_then(|v| v.as_str()).and_then(|s| s.parse::<T>().ok());
                    }
                    i += 2;
                }
                // Fallback
                if i == arr.len() - 1 {
                    arr.get(i).and_then(|v| v.as_str()).and_then(|s| s.parse::<T>().ok())
                } else {
                    None
                }
            }
            "case" => {
                // ["case", cond1, val1, ..., fallback] — return fallback (last element)
                arr.last().and_then(|v| v.as_str()).and_then(|s| s.parse::<T>().ok())
            }
            "step" => {
                // ["step", input, default, stop1, val1, ...] — return default before zoom-eval
                arr.get(2).and_then(|v| v.as_str()).and_then(|s| s.parse::<T>().ok())
            }
            "interpolate" => {
                // ["interpolate", type, input, z0, val0, ...] — return last parseable value
                arr.iter().rev()
                    .find_map(|v| v.as_str().and_then(|s| s.parse::<T>().ok()))
            }
            "literal" => {
                arr.get(1).and_then(|v| v.as_str()).and_then(|s| s.parse::<T>().ok())
            }
            "to-color" => {
                arr.get(1).and_then(|v| v.as_str()).and_then(|s| s.parse::<T>().ok())
            }
            _ => None,
        }
    }

    pub fn deserialize_color_or_none<'de, D>(
        deserializer: D,
    ) -> Result<Option<StyleProperty<T>>, D::Error>
    where
        D: Deserializer<'de>,
    {
        // For Color types, allow either a raw color string, or an expression value.
        let v = serde_json::Value::deserialize(deserializer).map_err(serde::de::Error::custom)?;
        if let Some(s) = v.as_str() {
            if let Ok(color) = s.parse::<T>() {
                return Ok(Some(StyleProperty::Constant(color)));
            }
        }
        // If it's a structural generic expression like match arrays
        if v.is_array() {
            return Ok(Some(StyleProperty::Expression(v)));
        }
        Ok(None)
    }
}

impl StyleProperty<f32> {
    pub fn deserialize_f32_or_none<'de, D>(
        deserializer: D,
    ) -> Result<Option<StyleProperty<f32>>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let v = serde_json::Value::deserialize(deserializer).map_err(serde::de::Error::custom)?;
        if let Some(f) = v.as_f64() {
            return Ok(Some(StyleProperty::Constant(f as f32)));
        }
        if v.is_array() {
            return Ok(Some(StyleProperty::Expression(v)));
        }
        // Handle {"stops": [[zoom, value], ...]} format
        if v.is_object() {
            return Ok(Some(StyleProperty::Expression(v)));
        }
        Ok(None)
    }

    /// Evaluate a zoom-dependent f32 property at the given zoom level.
    /// Supports constants and `{"stops": [[z0, v0], [z1, v1], ...]}`.
    pub fn evaluate_at_zoom(&self, zoom: f32) -> f32 {
        match self {
            StyleProperty::Constant(v) => *v,
            StyleProperty::Expression(expr) => {
                let stops = expr
                    .get("stops")
                    .and_then(|s| s.as_array())
                    .or_else(|| expr.as_array());
                let Some(stops) = stops else {
                    return 1.0;
                };
                // Parse stops as [(zoom, value), ...]
                let parsed: Vec<(f32, f32)> = stops
                    .iter()
                    .filter_map(|stop| {
                        let arr = stop.as_array()?;
                        let z = arr.first()?.as_f64()? as f32;
                        let v = arr.get(1)?.as_f64()? as f32;
                        Some((z, v))
                    })
                    .collect();

                if parsed.is_empty() {
                    return 1.0;
                }
                if zoom <= parsed[0].0 {
                    return parsed[0].1;
                }
                if zoom >= parsed[parsed.len() - 1].0 {
                    return parsed[parsed.len() - 1].1;
                }
                // Linear interpolation between stops
                for window in parsed.windows(2) {
                    let (z0, v0) = window[0];
                    let (z1, v1) = window[1];
                    if zoom >= z0 && zoom <= z1 {
                        let t = (zoom - z0) / (z1 - z0);
                        return v0 + t * (v1 - v0);
                    }
                }
                parsed[parsed.len() - 1].1
            }
        }
    }
}

impl StyleProperty<csscolorparser::Color> {
    /// Evaluate a color expression with a known zoom level.
    /// Handles zoom-dependent `step` and `interpolate` expressions accurately;
    /// falls back to the generic `evaluate()` for all other expression types.
    pub fn evaluate_color_at_zoom(
        &self,
        feature_properties: &HashMap<String, String>,
        zoom: f32,
    ) -> Option<csscolorparser::Color> {
        match self {
            StyleProperty::Constant(c) => return Some(c.clone()),
            StyleProperty::Expression(expr) => {
                let arr = match expr.as_array() {
                    Some(a) => a,
                    None => return self.evaluate(feature_properties),
                };
                let op = match arr.first().and_then(|v| v.as_str()) {
                    Some(s) => s,
                    None => return self.evaluate(feature_properties),
                };

                match op {
                    "step" => {
                        let is_zoom_input = arr.get(1)
                            .and_then(|v| v.as_array())
                            .and_then(|a| a.first())
                            .and_then(|v| v.as_str()) == Some("zoom");

                        if !is_zoom_input {
                            return self.evaluate(feature_properties);
                        }
                        // ["step", ["zoom"], default, stop1, val1, ...]
                        let default_color = arr.get(2)
                            .and_then(|v| v.as_str())
                            .and_then(|s| s.parse::<csscolorparser::Color>().ok());
                        let mut result = default_color;
                        let mut i = 3;
                        while i + 1 < arr.len() {
                            let stop_z = arr.get(i)
                                .and_then(|v| v.as_f64())
                                .unwrap_or(f64::MAX);
                            if zoom as f64 >= stop_z {
                                if let Some(s) = arr.get(i + 1).and_then(|v| v.as_str()) {
                                    result = s.parse().ok();
                                }
                            }
                            i += 2;
                        }
                        result
                    }
                    "interpolate" => {
                        let is_zoom_input = arr.get(2)
                            .and_then(|v| v.as_array())
                            .and_then(|a| a.first())
                            .and_then(|v| v.as_str()) == Some("zoom");

                        if !is_zoom_input {
                            return self.evaluate(feature_properties);
                        }
                        // ["interpolate", type, ["zoom"], z0, col0, z1, col1, ...]
                        let mut stops: Vec<(f32, csscolorparser::Color)> = Vec::new();
                        let mut i = 3;
                        while i + 1 < arr.len() {
                            let stop_z = arr.get(i)
                                .and_then(|v| v.as_f64())
                                .unwrap_or(0.0) as f32;
                            if let Some(s) = arr.get(i + 1).and_then(|v| v.as_str()) {
                                if let Ok(c) = s.parse::<csscolorparser::Color>() {
                                    stops.push((stop_z, c));
                                }
                            }
                            i += 2;
                        }
                        if stops.is_empty() {
                            return None;
                        }
                        if zoom <= stops[0].0 {
                            return Some(stops[0].1.clone());
                        }
                        let last = stops.len() - 1;
                        if zoom >= stops[last].0 {
                            return Some(stops[last].1.clone());
                        }
                        for w in stops.windows(2) {
                            let (z0, ref c0) = w[0];
                            let (z1, ref c1) = w[1];
                            if zoom >= z0 && zoom <= z1 {
                                let t = ((zoom - z0) / (z1 - z0)) as f64;
                                return Some(csscolorparser::Color {
                                    r: c0.r + (c1.r - c0.r) * t,
                                    g: c0.g + (c1.g - c0.g) * t,
                                    b: c0.b + (c1.b - c0.b) * t,
                                    a: c0.a + (c1.a - c0.a) * t,
                                });
                            }
                        }
                        Some(stops[last].1.clone())
                    }
                    _ => self.evaluate(feature_properties),
                }
            }
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BackgroundPaint {
    #[serde(rename = "background-color")]
    #[serde(
        default,
        deserialize_with = "StyleProperty::<Color>::deserialize_color_or_none"
    )]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub background_color: Option<StyleProperty<Color>>,
    // TODO a lot
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct FillPaint {
    #[serde(rename = "fill-color")]
    #[serde(
        default,
        deserialize_with = "StyleProperty::<Color>::deserialize_color_or_none"
    )]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fill_color: Option<StyleProperty<Color>>,
    // TODO a lot
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct LinePaint {
    #[serde(rename = "line-color")]
    #[serde(
        default,
        deserialize_with = "StyleProperty::<Color>::deserialize_color_or_none"
    )]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line_color: Option<StyleProperty<Color>>,

    #[serde(rename = "line-width")]
    #[serde(
        default,
        deserialize_with = "StyleProperty::<f32>::deserialize_f32_or_none"
    )]
    pub line_width: Option<StyleProperty<f32>>,
    // TODO a lot
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum RasterResampling {
    #[serde(rename = "linear")]
    Linear,
    #[serde(rename = "nearest")]
    Nearest,
}

/// Raster tile layer description
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct RasterPaint {
    #[serde(rename = "raster-brightness-max")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub raster_brightness_max: Option<f32>,
    #[serde(rename = "raster-brightness-min")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub raster_brightness_min: Option<f32>,
    #[serde(rename = "raster-contrast")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub raster_contrast: Option<f32>,
    #[serde(rename = "raster-fade-duration")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub raster_fade_duration: Option<u32>,
    #[serde(rename = "raster-hue-rotate")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub raster_hue_rotate: Option<f32>,
    #[serde(rename = "raster-opacity")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub raster_opacity: Option<f32>,
    #[serde(rename = "raster-resampling")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub raster_resampling: Option<RasterResampling>,
    #[serde(rename = "raster-saturation")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub raster_saturation: Option<f32>,
}

impl Default for RasterPaint {
    fn default() -> Self {
        RasterPaint {
            raster_brightness_max: Some(1.0),
            raster_brightness_min: Some(0.0),
            raster_contrast: Some(0.0),
            raster_fade_duration: Some(0),
            raster_hue_rotate: Some(0.0),
            raster_opacity: Some(1.0),
            raster_resampling: Some(RasterResampling::Linear),
            raster_saturation: Some(0.0),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SymbolPaint {
    #[serde(rename = "text-field")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text_field: Option<String>,

    #[serde(rename = "text-size")]
    #[serde(
        default,
        deserialize_with = "StyleProperty::<f32>::deserialize_f32_or_none"
    )]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text_size: Option<StyleProperty<f32>>,
    // TODO a lot
}

/// Extract the property name from a text-field template string like "{NAME}" → "NAME".
/// If no braces, returns the string as-is.
fn extract_text_field_property(template: &str) -> String {
    let trimmed = template.trim();
    if trimmed.starts_with('{') && trimmed.ends_with('}') {
        trimmed[1..trimmed.len() - 1].to_string()
    } else {
        trimmed.to_string()
    }
}

/// Extract a text-field property name from a layout JSON value.
/// Handles both:
///   - `"text-field": "{NAME}"` (constant string)
///   - `"text-field": {"stops": [[2, "{ABBREV}"], [4, "{NAME}"]]}` (zoom-dependent)
fn parse_text_field_from_layout(layout: &serde_json::Value) -> Option<String> {
    let tf = layout.get("text-field")?;
    if let Some(s) = tf.as_str() {
        return Some(extract_text_field_property(s));
    }
    // Zoom-dependent: use the last stop's value (highest zoom = most detailed)
    if let Some(stops) = tf.get("stops").and_then(|v| v.as_array()) {
        if let Some(last_stop) = stops.last() {
            if let Some(s) = last_stop.get(1).and_then(|v| v.as_str()) {
                return Some(extract_text_field_property(s));
            }
        }
    }
    None
}

/// Extract text-size from a layout JSON value.
/// Handles constant numbers and zoom-dependent `{"stops": [[z, size], ...]}`.
fn parse_text_size_from_layout(layout: &serde_json::Value) -> Option<StyleProperty<f32>> {
    let ts = layout.get("text-size")?;
    if let Some(f) = ts.as_f64() {
        return Some(StyleProperty::Constant(f as f32));
    }
    // Object with stops or array
    if ts.is_object() || ts.is_array() {
        return Some(StyleProperty::Expression(ts.clone()));
    }
    None
}

/// The different types of paints.
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(tag = "type", content = "paint")]
pub enum LayerPaint {
    #[serde(rename = "background")]
    Background(BackgroundPaint),
    #[serde(rename = "line")]
    Line(LinePaint),
    #[serde(rename = "fill")]
    Fill(FillPaint),
    #[serde(rename = "raster")]
    Raster(RasterPaint),
    #[serde(rename = "symbol")]
    Symbol(SymbolPaint),
}

impl LayerPaint {
    pub fn get_color(&self) -> Option<Alpha<EncodedSrgb<f32>>> {
        match self {
            LayerPaint::Background(paint) => paint.background_color.as_ref().and_then(|property| {
                if let StyleProperty::Constant(color) = property {
                    Some(color.clone().into())
                } else {
                    None // Expression types have no single static color
                }
            }),
            LayerPaint::Line(paint) => paint.line_color.as_ref().and_then(|property| {
                if let StyleProperty::Constant(color) = property {
                    Some(color.clone().into())
                } else {
                    None
                }
            }),
            LayerPaint::Fill(paint) => paint.fill_color.as_ref().and_then(|property| {
                if let StyleProperty::Constant(color) = property {
                    Some(color.clone().into())
                } else {
                    None
                }
            }),
            LayerPaint::Raster(_) => None,
            LayerPaint::Symbol(_) => None,
        }
    }
}

/// Stores all the styles for a specific layer.
#[derive(Debug, Clone)]
pub struct StyleLayer {
    pub index: u32,
    pub id: String,
    pub type_: String,
    pub filter: Option<serde_json::Value>,
    pub maxzoom: Option<u8>,
    pub minzoom: Option<u8>,
    pub metadata: Option<HashMap<String, String>>,
    pub paint: Option<LayerPaint>,
    pub source: Option<String>,
    pub source_layer: Option<String>,
}

impl Serialize for StyleLayer {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;
        // Count non-None optional fields
        let mut count = 2; // id + type are always present
        if self.filter.is_some() {
            count += 1;
        }
        if self.maxzoom.is_some() {
            count += 1;
        }
        if self.minzoom.is_some() {
            count += 1;
        }
        if self.metadata.is_some() {
            count += 1;
        }
        if self.paint.is_some() {
            count += 1;
        }
        if self.source.is_some() {
            count += 1;
        }
        if self.source_layer.is_some() {
            count += 1;
        }
        let mut map = serializer.serialize_map(Some(count))?;
        map.serialize_entry("id", &self.id)?;
        map.serialize_entry("type", &self.type_)?;
        if let Some(ref filter) = self.filter {
            map.serialize_entry("filter", filter)?;
        }
        if let Some(ref maxzoom) = self.maxzoom {
            map.serialize_entry("maxzoom", maxzoom)?;
        }
        if let Some(ref minzoom) = self.minzoom {
            map.serialize_entry("minzoom", minzoom)?;
        }
        if let Some(ref metadata) = self.metadata {
            map.serialize_entry("metadata", metadata)?;
        }
        if let Some(ref paint) = self.paint {
            // Serialize just the inner paint data (without the LayerPaint tag)
            match paint {
                LayerPaint::Background(p) => map.serialize_entry("paint", p)?,
                LayerPaint::Line(p) => map.serialize_entry("paint", p)?,
                LayerPaint::Fill(p) => map.serialize_entry("paint", p)?,
                LayerPaint::Raster(p) => map.serialize_entry("paint", p)?,
                LayerPaint::Symbol(p) => map.serialize_entry("paint", p)?,
            }
        }
        if let Some(ref source) = self.source {
            map.serialize_entry("source", source)?;
        }
        if let Some(ref source_layer) = self.source_layer {
            map.serialize_entry("source-layer", source_layer)?;
        }
        map.end()
    }
}

#[derive(Deserialize)]
struct StyleLayerDef {
    id: String,
    #[serde(rename = "type")]
    type_: String,
    filter: Option<serde_json::Value>,
    maxzoom: Option<u8>,
    minzoom: Option<u8>,
    metadata: Option<HashMap<String, String>>,
    source: Option<String>,
    #[serde(rename = "source-layer")]
    source_layer: Option<String>,
    paint: Option<serde_json::Value>,
    layout: Option<serde_json::Value>,
}

impl<'de> serde::Deserialize<'de> for StyleLayer {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let def = StyleLayerDef::deserialize(deserializer)?;

        let paint = if let Some(p) = def.paint {
            match def.type_.as_str() {
                "background" => serde_json::from_value(p.clone())
                    .map(LayerPaint::Background)
                    .ok(),
                "line" => serde_json::from_value(p.clone())
                    .map(LayerPaint::Line)
                    .map_err(|e| log::error!("line paint failed {}: {:?}", def.id, e))
                    .ok(),
                "fill" => serde_json::from_value(p.clone())
                    .map(LayerPaint::Fill)
                    .map_err(|e| log::error!("fill paint failed {}: {:?}", def.id, e))
                    .ok(),
                "raster" => serde_json::from_value(p.clone())
                    .map(LayerPaint::Raster)
                    .ok(),
                "symbol" => {
                    let mut paint: Option<SymbolPaint> = serde_json::from_value(p.clone())
                        .map_err(|e| log::error!("symbol paint failed {}: {:?}", def.id, e))
                        .ok();
                    // text-field and text-size live in layout, not paint — merge them in
                    if let (Some(sp), Some(layout)) = (paint.as_mut(), def.layout.as_ref()) {
                        if sp.text_field.is_none() {
                            sp.text_field = parse_text_field_from_layout(layout);
                        }
                        if sp.text_size.is_none() {
                            sp.text_size = parse_text_size_from_layout(layout);
                        }
                    }
                    paint.map(LayerPaint::Symbol)
                }
                _ => None,
            }
        } else if def.type_ == "symbol" {
            // Symbol layers may have no paint but still have layout with text-field/text-size
            let text_field = def.layout.as_ref().and_then(parse_text_field_from_layout);
            let text_size = def.layout.as_ref().and_then(parse_text_size_from_layout);
            Some(LayerPaint::Symbol(SymbolPaint {
                text_field,
                text_size,
            }))
        } else {
            None
        };

        Ok(StyleLayer {
            index: 0,
            id: def.id,
            type_: def.type_,
            filter: def.filter,
            maxzoom: def.maxzoom,
            minzoom: def.minzoom,
            metadata: def.metadata,
            paint,
            source: def.source,
            source_layer: def.source_layer,
        })
    }
}

impl Eq for StyleLayer {}
impl PartialEq for StyleLayer {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl Hash for StyleLayer {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl Default for StyleLayer {
    fn default() -> Self {
        Self {
            index: 0,
            id: "id".to_string(),
            type_: "background".to_string(),
            filter: None,
            maxzoom: None,
            minzoom: None,
            metadata: None,
            paint: None,
            source: None,
            source_layer: Some("does not exist".to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_evaluate_case_returns_fallback() {
        let json = r##"["case", ["==", ["get", "class"], "water"], "#C2DAEA", "#F8F4F0"]"##;
        let expr: serde_json::Value = serde_json::from_str(json).unwrap();
        let prop: StyleProperty<csscolorparser::Color> = StyleProperty::Expression(expr);
        // case: returns last element (fallback)
        let color = prop.evaluate(&HashMap::new()).unwrap();
        assert_eq!(color.to_rgba8()[0..3], [248, 244, 240]);
    }

    #[test]
    fn test_evaluate_step_returns_default() {
        let json = r##"["step", ["zoom"], "#ffffff", 8, "#f0eeeb", 14, "#e8e0d8"]"##;
        let expr: serde_json::Value = serde_json::from_str(json).unwrap();
        let prop: StyleProperty<csscolorparser::Color> = StyleProperty::Expression(expr);
        // step without zoom: returns default (index 2)
        let color = prop.evaluate(&HashMap::new()).unwrap();
        assert_eq!(color.to_rgba8()[0..3], [255, 255, 255]);
    }

    #[test]
    fn test_evaluate_color_at_zoom_step() {
        let json = r##"["step", ["zoom"], "#ffffff", 8, "#f0eeeb", 14, "#e8e0d8"]"##;
        let expr: serde_json::Value = serde_json::from_str(json).unwrap();
        let prop: StyleProperty<csscolorparser::Color> = StyleProperty::Expression(expr);
        let props = HashMap::new();
        // zoom 5 → default (#ffffff)
        assert_eq!(prop.evaluate_color_at_zoom(&props, 5.0).unwrap().to_rgba8()[0..3], [255, 255, 255]);
        // zoom 10 → #f0eeeb
        assert_eq!(prop.evaluate_color_at_zoom(&props, 10.0).unwrap().to_rgba8()[0..3], [240, 238, 235]);
        // zoom 15 → #e8e0d8
        assert_eq!(prop.evaluate_color_at_zoom(&props, 15.0).unwrap().to_rgba8()[0..3], [232, 224, 216]);
    }

    #[test]
    fn test_evaluate_color_at_zoom_interpolate() {
        let json = r##"["interpolate", ["linear"], ["zoom"], 0, "#ffffff", 10, "#000000"]"##;
        let expr: serde_json::Value = serde_json::from_str(json).unwrap();
        let prop: StyleProperty<csscolorparser::Color> = StyleProperty::Expression(expr);
        let props = HashMap::new();
        // zoom 0 → white
        let c0 = prop.evaluate_color_at_zoom(&props, 0.0).unwrap();
        assert_eq!(c0.to_rgba8()[0..3], [255, 255, 255]);
        // zoom 10 → black
        let c10 = prop.evaluate_color_at_zoom(&props, 10.0).unwrap();
        assert_eq!(c10.to_rgba8()[0..3], [0, 0, 0]);
        // zoom 5 → midpoint grey ~[127, 127, 127]
        let c5 = prop.evaluate_color_at_zoom(&props, 5.0).unwrap();
        let r = c5.to_rgba8()[0];
        assert!(r >= 125 && r <= 130, "expected ~128, got {r}");
    }

    #[test]
    fn test_evaluate_match_missing_property_returns_fallback() {
        let json = r#"
        [
            "match",
            ["get", "ADM0_A3"],
            ["ARM", "ATG"],
            "rgba(1, 2, 3, 1)",
            "rgba(9, 9, 9, 1)"
        ]
        "#;
        let expr: serde_json::Value = serde_json::from_str(json).unwrap();
        let prop: StyleProperty<csscolorparser::Color> = StyleProperty::Expression(expr);

        // Feature that does NOT have the property → should return the JSON fallback color
        let empty_props = HashMap::new();
        let color = prop.evaluate(&empty_props).unwrap();
        assert_eq!(color.to_rgba8(), [9, 9, 9, 255]);
    }

    #[test]
    fn test_evaluate_match() {
        let json = r#"
        [
            "match",
            ["get", "ADM0_A3"],
            ["ARM", "ATG"],
            "rgba(1, 2, 3, 1)",
            "rgba(0, 0, 0, 1)"
        ]
        "#;
        let expr: serde_json::Value = serde_json::from_str(json).unwrap();
        let prop: StyleProperty<csscolorparser::Color> = StyleProperty::Expression(expr);

        let mut feature_properties = HashMap::new();
        feature_properties.insert("ADM0_A3".to_string(), "ARM".to_string());

        let color = prop.evaluate(&feature_properties).unwrap();
        assert_eq!(color.to_rgba8(), [1, 2, 3, 255]);
    }

    #[test]
    fn test_symbol_text_field_from_layout() {
        let json = r#"{
            "id": "countries-label",
            "type": "symbol",
            "paint": {
                "text-color": "rgba(8, 37, 77, 1)"
            },
            "layout": {
                "text-field": "{NAME}",
                "text-font": ["Open Sans Semibold"]
            },
            "source": "maplibre",
            "source-layer": "centroids"
        }"#;
        let layer: StyleLayer = serde_json::from_str(json).unwrap();
        assert_eq!(layer.type_, "symbol");
        match &layer.paint {
            Some(LayerPaint::Symbol(sp)) => {
                assert_eq!(sp.text_field.as_deref(), Some("NAME"));
            }
            other => panic!("expected Symbol paint, got {:?}", other),
        }
    }

    #[test]
    fn test_symbol_text_field_zoom_dependent() {
        let json = r#"{
            "id": "test-label",
            "type": "symbol",
            "paint": {},
            "layout": {
                "text-field": {"stops": [[2, "{ABBREV}"], [4, "{NAME}"]]}
            },
            "source": "maplibre",
            "source-layer": "centroids"
        }"#;
        let layer: StyleLayer = serde_json::from_str(json).unwrap();
        match &layer.paint {
            Some(LayerPaint::Symbol(sp)) => {
                // Should pick the last stop (highest zoom) → NAME
                assert_eq!(sp.text_field.as_deref(), Some("NAME"));
            }
            other => panic!("expected Symbol paint, got {:?}", other),
        }
    }

    #[test]
    fn test_demotiles_symbol_layers_have_text_field() {
        let style: crate::style::Style = Default::default();
        for layer in &style.layers {
            if layer.type_ == "symbol" {
                match &layer.paint {
                    Some(LayerPaint::Symbol(sp)) => {
                        assert!(
                            sp.text_field.is_some(),
                            "symbol layer '{}' should have text_field parsed from layout",
                            layer.id
                        );
                    }
                    _ => panic!("symbol layer '{}' has no Symbol paint", layer.id),
                }
            }
        }
    }
}
