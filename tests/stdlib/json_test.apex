// Standard Library Tests: JSON
// Tests for JSON parsing and serialization

import std.io.*;

#[test]
function test_json_parse_null(): Boolean {
    value: JsonValue = json::parse("null");
    return value.is_null();
}

#[test]
function test_json_parse_bool(): Boolean {
    t: JsonValue = json::parse("true");
    f: JsonValue = json::parse("false");
    return t.as_bool() == Some(true) && f.as_bool() == Some(false);
}

#[test]
function test_json_parse_number(): Boolean {
    int_val: JsonValue = json::parse("42");
    float_val: JsonValue = json::parse("3.14");
    negative: JsonValue = json::parse("-10");
    
    return int_val.as_number() == Some(42.0) &&
           float_val.as_number() == Some(3.14) &&
           negative.as_number() == Some(-10.0);
}

#[test]
function test_json_parse_string(): Boolean {
    str_val: JsonValue = json::parse('"hello world"');
    return str_val.as_string() == Some("hello world");
}

#[test]
function test_json_parse_array(): Boolean {
    arr: JsonValue = json::parse("[1, 2, 3]");
    return arr.is_array() && arr.as_array().len() == 3;
}

#[test]
function test_json_parse_object(): Boolean {
    obj: JsonValue = json::parse('{"name": "test", "value": 42}');
    return obj.is_object() && 
           obj.get("name").as_string() == Some("test") &&
           obj.get("value").as_number() == Some(42.0);
}

#[test]
function test_json_stringify(): Boolean {
    obj: JsonValue = json::object([
        ("name", json::string("Apex")),
        ("version", json::number(1.0))
    ]);
    json_str: String = json::stringify(obj);
    return json_str.contains('"name":"Apex"');
}

#[test]
function test_json_roundtrip(): Boolean {
    original: String = '{"items":[1,2,3],"count":3}';
    parsed: JsonValue = json::parse(original);
    stringified: String = json::stringify(parsed);
    reparsed: JsonValue = json::parse(stringified);
    
    return reparsed.get("count").as_number() == Some(3.0);
}

// Main
function main(): None {
    println("Run with: apex test");
    return None;
}
