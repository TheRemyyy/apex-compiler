// Standard Library Tests: Collections
// Tests for Vec, HashMap, HashSet

import std.io.*;

// ========== Vec Tests ==========

#[test]
function test_vec_new(): Boolean {
    vec: Vec<Integer> = Vec::new();
    return vec.is_empty() && vec.len() == 0;
}

#[test]
function test_vec_push_pop(): Boolean {
    vec: Vec<Integer> = Vec::new();
    vec.push(1);
    vec.push(2);
    vec.push(3);
    
    if vec.len() != 3 {
        return false;
    }
    
    val: Option<Integer> = vec.pop();
    return val == Some(3) && vec.len() == 2;
}

#[test]
function test_vec_get(): Boolean {
    vec: Vec<String> = Vec::new();
    vec.push("first");
    vec.push("second");
    
    return vec.get(0) == Some("first") && vec.get(1) == Some("second");
}

#[test]
function test_vec_contains(): Boolean {
    vec: Vec<Integer> = Vec::new();
    vec.push(10);
    vec.push(20);
    vec.push(30);
    
    return vec.contains(20) && !vec.contains(40);
}

#[test]
function test_vec_reverse(): Boolean {
    vec: Vec<Integer> = Vec::new();
    vec.push(1);
    vec.push(2);
    vec.push(3);
    vec.reverse();
    
    return vec.get(0) == Some(3) && vec.get(2) == Some(1);
}

// ========== HashMap Tests ==========

#[test]
function test_hashmap_new(): Boolean {
    map: HashMap<String, Integer> = HashMap::new();
    return map.is_empty();
}

#[test]
function test_hashmap_insert_get(): Boolean {
    map: HashMap<String, Integer> = HashMap::new();
    map.insert("key1", 100);
    map.insert("key2", 200);
    
    return map.get("key1") == Some(100) && map.get("key2") == Some(200);
}

#[test]
function test_hashmap_contains_key(): Boolean {
    map: HashMap<String, String> = HashMap::new();
    map.insert("name", "Apex");
    
    return map.contains_key("name") && !map.contains_key("missing");
}

#[test]
function test_hashmap_remove(): Boolean {
    map: HashMap<String, Integer> = HashMap::new();
    map.insert("temp", 42);
    removed: Option<Integer> = map.remove("temp");
    
    return removed == Some(42) && !map.contains_key("temp");
}

#[test]
function test_hashmap_update(): Boolean {
    map: HashMap<String, Integer> = HashMap::new();
    map.insert("counter", 1);
    map.insert("counter", 2); // Update
    
    return map.get("counter") == Some(2);
}

// ========== HashSet Tests ==========

#[test]
function test_hashset_new(): Boolean {
    set: HashSet<Integer> = HashSet::new();
    return set.is_empty();
}

#[test]
function test_hashset_insert_contains(): Boolean {
    set: HashSet<String> = HashSet::new();
    set.insert("apple");
    set.insert("banana");
    
    return set.contains("apple") && set.contains("banana") && !set.contains("cherry");
}

#[test]
function test_hashset_remove(): Boolean {
    set: HashSet<Integer> = HashSet::new();
    set.insert(1);
    set.insert(2);
    set.remove(1);
    
    return !set.contains(1) && set.contains(2);
}

#[test]
function test_hashset_no_duplicates(): Boolean {
    set: HashSet<Integer> = HashSet::new();
    set.insert(42);
    set.insert(42); // Duplicate
    set.insert(42); // Another duplicate
    
    return set.len() == 1;
}

// Main
function main(): None {
    println("Run with: apex test");
    return None;
}
