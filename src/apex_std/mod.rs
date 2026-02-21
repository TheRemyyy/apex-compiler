//! Apex Standard Library
//!
//! Core modules for the Apex programming language

//! Apex Standard Library

pub mod collections;
pub mod json;
pub mod http;

// Re-export common types
pub use collections::{HashMap, HashSet, Vec};
pub use json::Value as JsonValue;
pub use json::{parse as parse_json, stringify as stringify_json};
