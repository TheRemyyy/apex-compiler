//! JSON module for Apex
//!
//! Provides JSON parsing and serialization:
//! - Value: JSON value type
//! - parse(): Parse JSON string
//! - stringify(): Serialize to JSON string
//! - from_json(): Deserialize to struct
//! - to_json(): Serialize from struct

use std::collections::HashMap;

/// JSON value type
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
}

impl Value {
    /// Create a null value
    pub fn null() -> Self {
        Value::Null
    }

    /// Create a boolean value
    pub fn bool(v: bool) -> Self {
        Value::Bool(v)
    }

    /// Create a number value
    pub fn number(v: f64) -> Self {
        Value::Number(v)
    }

    /// Create a string value
    pub fn string(v: impl Into<String>) -> Self {
        Value::String(v.into())
    }

    /// Create an array value
    pub fn array(v: Vec<Value>) -> Self {
        Value::Array(v)
    }

    /// Create an object value
    pub fn object(v: HashMap<String, Value>) -> Self {
        Value::Object(v)
    }

    /// Check if null
    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }

    /// Check if boolean
    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    /// Check if number
    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    /// Check if string
    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    /// Check if array
    pub fn is_array(&self) -> bool {
        matches!(self, Value::Array(_))
    }

    /// Check if object
    pub fn is_object(&self) -> bool {
        matches!(self, Value::Object(_))
    }

    /// Get as boolean
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(v) => Some(*v),
            _ => None,
        }
    }

    /// Get as number
    pub fn as_number(&self) -> Option<f64> {
        match self {
            Value::Number(v) => Some(*v),
            _ => None,
        }
    }

    /// Get as integer
    pub fn as_i64(&self) -> Option<i64> {
        self.as_number().map(|n| n as i64)
    }

    /// Get as string
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Value::String(v) => Some(v),
            _ => None,
        }
    }

    /// Get as array
    pub fn as_array(&self) -> Option<&Vec<Value>> {
        match self {
            Value::Array(v) => Some(v),
            _ => None,
        }
    }

    /// Get as mutable array
    pub fn as_array_mut(&mut self) -> Option<&mut Vec<Value>> {
        match self {
            Value::Array(v) => Some(v),
            _ => None,
        }
    }

    /// Get as object
    pub fn as_object(&self) -> Option<&HashMap<String, Value>> {
        match self {
            Value::Object(v) => Some(v),
            _ => None,
        }
    }

    /// Get as mutable object
    pub fn as_object_mut(&mut self) -> Option<&mut HashMap<String, Value>> {
        match self {
            Value::Object(v) => Some(v),
            _ => None,
        }
    }

    /// Get field from object
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.as_object()?.get(key)
    }

    /// Get mutable field from object
    pub fn get_mut(&mut self, key: &str) -> Option<&mut Value> {
        self.as_object_mut()?.get_mut(key)
    }

    /// Get element from array
    pub fn get_index(&self, index: usize) -> Option<&Value> {
        self.as_array()?.get(index)
    }

    /// Set field in object
    pub fn set(&mut self, key: impl Into<String>, value: Value) -> Result<(), String> {
        match self {
            Value::Object(obj) => {
                obj.insert(key.into(), value);
                Ok(())
            }
            _ => Err("Not an object".to_string()),
        }
    }

    /// Push to array
    pub fn push(&mut self, value: Value) -> Result<(), String> {
        match self {
            Value::Array(arr) => {
                arr.push(value);
                Ok(())
            }
            _ => Err("Not an array".to_string()),
        }
    }

    /// Serialize to JSON string
    pub fn stringify(&self) -> String {
        match self {
            Value::Null => "null".to_string(),
            Value::Bool(v) => v.to_string(),
            Value::Number(v) => {
                if v.is_nan() {
                    "null".to_string()
                } else if v.is_infinite() {
                    if v.is_sign_positive() {
                        "null".to_string()
                    } else {
                        "null".to_string()
                    }
                } else {
                    // Handle integers without decimal
                    if v.fract() == 0.0 && *v >= i64::MIN as f64 && *v <= i64::MAX as f64 {
                        format!("{:.0}", v)
                    } else {
                        v.to_string()
                    }
                }
            }
            Value::String(v) => escape_string(v),
            Value::Array(arr) => {
                let items: Vec<String> = arr.iter().map(|v| v.stringify()).collect();
                format!("[{}]", items.join(","))
            }
            Value::Object(obj) => {
                let items: Vec<String> = obj
                    .iter()
                    .map(|(k, v)| format!("{}:{}", escape_string(k), v.stringify()))
                    .collect();
                format!("{{{}}}", items.join(","))
            }
        }
    }

    /// Pretty print JSON
    pub fn pretty_print(&self) -> String {
        self.pretty_print_indent(0)
    }

    fn pretty_print_indent(&self, indent: usize) -> String {
        let spaces = "  ".repeat(indent);
        
        match self {
            Value::Null => "null".to_string(),
            Value::Bool(v) => v.to_string(),
            Value::Number(v) => {
                if v.fract() == 0.0 && *v >= i64::MIN as f64 && *v <= i64::MAX as f64 {
                    format!("{:.0}", v)
                } else {
                    v.to_string()
                }
            }
            Value::String(v) => escape_string(v),
            Value::Array(arr) => {
                if arr.is_empty() {
                    return "[]".to_string();
                }
                let items: Vec<String> = arr
                    .iter()
                    .map(|v| format!("{}  {}", spaces, v.pretty_print_indent(indent + 1)))
                    .collect();
                format!("[\n{}\n{}]", items.join(",\n"), spaces)
            }
            Value::Object(obj) => {
                if obj.is_empty() {
                    return "{}".to_string();
                }
                let items: Vec<String> = obj
                    .iter()
                    .map(|(k, v)| {
                        format!(
                            "{}  {}: {}",
                            spaces,
                            escape_string(k),
                            v.pretty_print_indent(indent + 1)
                        )
                    })
                    .collect();
                format!("{{\n{}\n{}}}", items.join(",\n"), spaces)
            }
        }
    }
}

/// Escape a string for JSON
fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len() + 2);
    result.push('"');
    
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\b' => result.push_str("\\b"),
            '\f' => result.push_str("\\f"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            c if c.is_control() => {
                result.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => result.push(c),
        }
    }
    
    result.push('"');
    result
}

/// Parse JSON string
pub fn parse(input: &str) -> Result<Value, String> {
    let mut parser = Parser::new(input);
    parser.parse()
}

/// Serialize value to JSON string
pub fn stringify(value: &Value) -> String {
    value.stringify()
}

/// Pretty print JSON
pub fn pretty_print(value: &Value) -> String {
    value.pretty_print()
}

/// Create a JSON object from key-value pairs
pub fn object(pairs: Vec<(&str, Value)>) -> Value {
    let mut map = HashMap::new();
    for (k, v) in pairs {
        map.insert(k.to_string(), v);
    }
    Value::Object(map)
}

/// Create a JSON array
pub fn array(items: Vec<Value>) -> Value {
    Value::Array(items)
}

/// Simple JSON parser
struct Parser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn parse(&mut self) -> Result<Value, String> {
        self.skip_whitespace();
        let value = self.parse_value()?;
        self.skip_whitespace();
        
        if self.pos < self.input.len() {
            return Err(format!("Unexpected character at position {}", self.pos));
        }
        
        Ok(value)
    }

    fn parse_value(&mut self) -> Result<Value, String> {
        self.skip_whitespace();
        
        if self.pos >= self.input.len() {
            return Err("Unexpected end of input".to_string());
        }

        match self.peek() {
            'n' => self.parse_null(),
            't' => self.parse_true(),
            'f' => self.parse_false(),
            '"' => self.parse_string(),
            '[' => self.parse_array(),
            '{' => self.parse_object(),
            c if c.is_ascii_digit() || c == '-' => self.parse_number(),
            c => Err(format!("Unexpected character '{}' at position {}", c, self.pos)),
        }
    }

    fn parse_null(&mut self) -> Result<Value, String> {
        self.expect("null")?;
        Ok(Value::Null)
    }

    fn parse_true(&mut self) -> Result<Value, String> {
        self.expect("true")?;
        Ok(Value::Bool(true))
    }

    fn parse_false(&mut self) -> Result<Value, String> {
        self.expect("false")?;
        Ok(Value::Bool(false))
    }

    fn parse_string(&mut self) -> Result<Value, String> {
        self.expect_char('"')?;
        let mut result = String::new();
        
        while self.pos < self.input.len() {
            let c = self.peek();
            
            if c == '"' {
                self.advance();
                return Ok(Value::String(result));
            }
            
            if c == '\\' {
                self.advance();
                if self.pos >= self.input.len() {
                    return Err("Unexpected end of string".to_string());
                }
                
                match self.peek() {
                    '"' => result.push('"'),
                    '\\' => result.push('\\'),
                    '/' => result.push('/'),
                    'b' => result.push('\x08'),
                    'f' => result.push('\x0c'),
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    't' => result.push('\t'),
                    'u' => {
                        self.advance();
                        let hex = self.take(4);
                        if hex.len() != 4 {
                            return Err("Invalid unicode escape".to_string());
                        }
                        let code = u32::from_str_radix(hex, 16)
                            .map_err(|_| "Invalid unicode escape")?;
                        if let Some(c) = char::from_u32(code) {
                            result.push(c);
                        } else {
                            return Err("Invalid unicode codepoint".to_string());
                        }
                        continue;
                    }
                    c => return Err(format!("Invalid escape sequence: \\{}" , c)),
                }
            } else {
                result.push(c);
            }
            
            self.advance();
        }
        
        Err("Unterminated string".to_string())
    }

    fn parse_number(&mut self) -> Result<Value, String> {
        let start = self.pos;
        
        // Optional minus
        if self.peek() == '-' {
            self.advance();
        }
        
        // Integer part
        if self.peek() == '0' {
            self.advance();
        } else {
            if !self.peek().is_ascii_digit() {
                return Err("Expected digit".to_string());
            }
            while self.pos < self.input.len() && self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        
        // Fractional part
        if self.pos < self.input.len() && self.peek() == '.' {
            self.advance();
            if !self.peek().is_ascii_digit() {
                return Err("Expected digit after decimal point".to_string());
            }
            while self.pos < self.input.len() && self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        
        // Exponent part
        if self.pos < self.input.len() && (self.peek() == 'e' || self.peek() == 'E') {
            self.advance();
            if self.pos < self.input.len() && (self.peek() == '+' || self.peek() == '-') {
                self.advance();
            }
            if !self.peek().is_ascii_digit() {
                return Err("Expected digit in exponent".to_string());
            }
            while self.pos < self.input.len() && self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        
        let num_str = &self.input[start..self.pos];
        let num = num_str
            .parse::<f64>()
            .map_err(|_| format!("Invalid number: {}", num_str))?;
        
        Ok(Value::Number(num))
    }

    fn parse_array(&mut self) -> Result<Value, String> {
        self.expect_char('[')?;
        self.skip_whitespace();
        
        let mut items = Vec::new();
        
        if self.pos < self.input.len() && self.peek() == ']' {
            self.advance();
            return Ok(Value::Array(items));
        }
        
        loop {
            self.skip_whitespace();
            let value = self.parse_value()?;
            items.push(value);
            
            self.skip_whitespace();
            
            if self.pos >= self.input.len() {
                return Err("Unterminated array".to_string());
            }
            
            match self.peek() {
                ',' => {
                    self.advance();
                    continue;
                }
                ']' => {
                    self.advance();
                    return Ok(Value::Array(items));
                }
                c => return Err(format!("Expected ',' or ']', found '{}'", c)),
            }
        }
    }

    fn parse_object(&mut self) -> Result<Value, String> {
        self.expect_char('{')?;
        self.skip_whitespace();
        
        let mut obj = HashMap::new();
        
        if self.pos < self.input.len() && self.peek() == '}' {
            self.advance();
            return Ok(Value::Object(obj));
        }
        
        loop {
            self.skip_whitespace();
            
            // Parse key
            let key = match self.parse_value()? {
                Value::String(s) => s,
                _ => return Err("Object key must be a string".to_string()),
            };
            
            self.skip_whitespace();
            self.expect_char(':')?;
            self.skip_whitespace();
            
            // Parse value
            let value = self.parse_value()?;
            obj.insert(key, value);
            
            self.skip_whitespace();
            
            if self.pos >= self.input.len() {
                return Err("Unterminated object".to_string());
            }
            
            match self.peek() {
                ',' => {
                    self.advance();
                    continue;
                }
                '}' => {
                    self.advance();
                    return Ok(Value::Object(obj));
                }
                c => return Err(format!("Expected ',' or '}}', found '{}'", c)),
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len() {
            match self.peek() {
                ' ' | '\t' | '\n' | '\r' => self.advance(),
                _ => break,
            }
        }
    }

    fn peek(&self) -> char {
        self.input.chars().nth(self.pos).unwrap_or('\0')
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn take(&mut self, n: usize) -> &str {
        let start = self.pos;
        for _ in 0..n {
            if self.pos < self.input.len() {
                self.advance();
            }
        }
        &self.input[start..self.pos]
    }

    fn expect(&mut self, s: &str) -> Result<(), String> {
        if self.input[self.pos..].starts_with(s) {
            self.pos += s.len();
            Ok(())
        } else {
            Err(format!("Expected '{}' at position {}", s, self.pos))
        }
    }

    fn expect_char(&mut self, c: char) -> Result<(), String> {
        if self.peek() == c {
            self.advance();
            Ok(())
        } else {
            Err(format!("Expected '{}' at position {}", c, self.pos))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_null() {
        assert_eq!(parse("null").unwrap(), Value::Null);
    }

    #[test]
    fn test_parse_bool() {
        assert_eq!(parse("true").unwrap(), Value::Bool(true));
        assert_eq!(parse("false").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_parse_number() {
        assert_eq!(parse("42").unwrap(), Value::Number(42.0));
        assert_eq!(parse("-3.14").unwrap(), Value::Number(-3.14));
        assert_eq!(parse("1.5e10").unwrap(), Value::Number(1.5e10));
    }

    #[test]
    fn test_parse_string() {
        assert_eq!(
            parse("\"hello\"").unwrap(),
            Value::String("hello".to_string())
        );
        assert_eq!(
            parse("\"hello\\nworld\"").unwrap(),
            Value::String("hello\nworld".to_string())
        );
    }

    #[test]
    fn test_parse_array() {
        let arr = parse("[1, 2, 3]").unwrap();
        assert_eq!(arr.as_array().unwrap().len(), 3);
    }

    #[test]
    fn test_parse_object() {
        let obj = parse(r#"{"name": "test", "value": 42}"#).unwrap();
        assert_eq!(obj.get("name").unwrap().as_string().unwrap(), "test");
        assert_eq!(obj.get("value").unwrap().as_number().unwrap(), 42.0);
    }

    #[test]
    fn test_stringify() {
        let value = object(vec![
            ("name", Value::string("test")),
            ("value", Value::Number(42.0)),
            ("active", Value::Bool(true)),
        ]);
        
        let json = stringify(&value);
        assert!(json.contains("\"name\":\"test\""));
        assert!(json.contains("\"value\":42"));
        assert!(json.contains("\"active\":true"));
    }

    #[test]
    fn test_roundtrip() {
        let original = r#"{"name":"test","items":[1,2,3],"nested":{"a":1}}"#;
        let parsed = parse(original).unwrap();
        let stringified = stringify(&parsed);
        let reparsed = parse(&stringified).unwrap();
        assert_eq!(parsed, reparsed);
    }
}
