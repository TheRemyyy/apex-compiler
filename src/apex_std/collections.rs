//! Collections module for Apex
//!
//! Provides efficient data structures:
//! - Vec<T>: Dynamic array
//! - HashMap<K, V>: Hash map
//! - HashSet<T>: Hash set
//! - LinkedList<T>: Doubly-linked list
//! - VecDeque<T>: Double-ended queue
//! - BinaryHeap<T>: Priority queue

use std::collections::{BinaryHeap, HashMap as StdHashMap, HashSet as StdHashSet, LinkedList, VecDeque};

/// Dynamic array (vector)
#[derive(Debug, Clone)]
pub struct Vec<T> {
    inner: Vec<T>,
}

impl<T> Vec<T> {
    /// Create a new empty vector
    pub fn new() -> Self {
        Self { inner: Vec::new() }
    }

    /// Create a vector with specified capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: Vec::with_capacity(capacity),
        }
    }

    /// Add an element to the end
    pub fn push(&mut self, value: T) {
        self.inner.push(value);
    }

    /// Remove and return the last element
    pub fn pop(&mut self) -> Option<T> {
        self.inner.pop()
    }

    /// Get element at index
    pub fn get(&self, index: usize) -> Option<&T> {
        self.inner.get(index)
    }

    /// Get mutable reference at index
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.inner.get_mut(index)
    }

    /// Set element at index
    pub fn set(&mut self, index: usize, value: T) -> Result<(), String> {
        if index < self.inner.len() {
            self.inner[index] = value;
            Ok(())
        } else {
            Err(format!("Index {} out of bounds", index))
        }
    }

    /// Remove element at index
    pub fn remove(&mut self, index: usize) -> Option<T> {
        if index < self.inner.len() {
            Some(self.inner.remove(index))
        } else {
            None
        }
    }

    /// Insert element at index
    pub fn insert(&mut self, index: usize, value: T) -> Result<(), String> {
        if index <= self.inner.len() {
            self.inner.insert(index, value);
            Ok(())
        } else {
            Err(format!("Index {} out of bounds", index))
        }
    }

    /// Get the number of elements
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Clear all elements
    pub fn clear(&mut self) {
        self.inner.clear();
    }

    /// Get capacity
    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }

    /// Reserve capacity
    pub fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional);
    }

    /// Shrink to fit
    pub fn shrink_to_fit(&mut self) {
        self.inner.shrink_to_fit();
    }

    /// Get first element
    pub fn first(&self) -> Option<&T> {
        self.inner.first()
    }

    /// Get last element
    pub fn last(&self) -> Option<&T> {
        self.inner.last()
    }

    /// Check if contains element (requires T: PartialEq)
    pub fn contains(&self, value: &T) -> bool
    where
        T: PartialEq,
    {
        self.inner.contains(value)
    }

    /// Find index of element
    pub fn position(&self, value: &T) -> Option<usize>
    where
        T: PartialEq,
    {
        self.inner.iter().position(|x| x == value)
    }

    /// Reverse the vector
    pub fn reverse(&mut self) {
        self.inner.reverse();
    }

    /// Sort the vector (requires T: Ord)
    pub fn sort(&mut self)
    where
        T: Ord,
    {
        self.inner.sort();
    }

    /// Sort with custom comparator
    pub fn sort_by<F>(&mut self, compare: F)
    where
        F: FnMut(&T, &T) -> std::cmp::Ordering,
    {
        self.inner.sort_by(compare);
    }

    /// Create iterator
    pub fn iter(&self) -> std::slice::Iter<T> {
        self.inner.iter()
    }

    /// Create mutable iterator
    pub fn iter_mut(&mut self) -> std::slice::IterMut<T> {
        self.inner.iter_mut()
    }

    /// Convert to Vec (consumes self)
    pub fn into_vec(self) -> Vec<T> {
        self.inner
    }
}

impl<T> Default for Vec<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> From<Vec<T>> for Vec<T> {
    fn from(vec: Vec<T>) -> Self {
        Self { inner: vec }
    }
}

/// Hash map
#[derive(Debug, Clone)]
pub struct HashMap<K, V> {
    inner: StdHashMap<K, V>,
}

impl<K, V> HashMap<K, V>
where
    K: std::hash::Hash + Eq,
{
    /// Create a new empty hash map
    pub fn new() -> Self {
        Self {
            inner: StdHashMap::new(),
        }
    }

    /// Create with capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: StdHashMap::with_capacity(capacity),
        }
    }

    /// Insert a key-value pair
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.inner.insert(key, value)
    }

    /// Get value by key
    pub fn get(&self, key: &K) -> Option<&V> {
        self.inner.get(key)
    }

    /// Get mutable value by key
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.inner.get_mut(key)
    }

    /// Remove key-value pair
    pub fn remove(&mut self, key: &K) -> Option<V> {
        self.inner.remove(key)
    }

    /// Check if contains key
    pub fn contains_key(&self, key: &K) -> bool {
        self.inner.contains_key(key)
    }

    /// Get number of elements
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Clear all elements
    pub fn clear(&mut self) {
        self.inner.clear();
    }

    /// Get keys
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.inner.keys()
    }

    /// Get values
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.inner.values()
    }

    /// Get mutable values
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.inner.values_mut()
    }

    /// Get or insert
    pub fn entry(&mut self, key: K) -> std::collections::hash_map::Entry<K, V> {
        self.inner.entry(key)
    }

    /// Get value or default
    pub fn get_or_insert(&mut self, key: K, default: V) -> &V {
        self.inner.entry(key).or_insert(default)
    }
}

impl<K, V> Default for HashMap<K, V>
where
    K: std::hash::Hash + Eq,
{
    fn default() -> Self {
        Self::new()
    }
}

/// Hash set
#[derive(Debug, Clone)]
pub struct HashSet<T> {
    inner: StdHashSet<T>,
}

impl<T> HashSet<T>
where
    T: std::hash::Hash + Eq,
{
    /// Create a new empty hash set
    pub fn new() -> Self {
        Self {
            inner: StdHashSet::new(),
        }
    }

    /// Insert an element
    pub fn insert(&mut self, value: T) -> bool {
        self.inner.insert(value)
    }

    /// Remove an element
    pub fn remove(&mut self, value: &T) -> bool {
        self.inner.remove(value)
    }

    /// Check if contains element
    pub fn contains(&self, value: &T) -> bool {
        self.inner.contains(value)
    }

    /// Get number of elements
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Clear all elements
    pub fn clear(&mut self) {
        self.inner.clear();
    }

    /// Union with another set
    pub fn union(&self, other: &Self) -> impl Iterator<Item = &T> {
        self.inner.union(&other.inner)
    }

    /// Intersection with another set
    pub fn intersection(&self, other: &Self) -> impl Iterator<Item = &T> {
        self.inner.intersection(&other.inner)
    }

    /// Difference with another set
    pub fn difference(&self, other: &Self) -> impl Iterator<Item = &T> {
        self.inner.difference(&other.inner)
    }

    /// Symmetric difference
    pub fn symmetric_difference(&self, other: &Self) -> impl Iterator<Item = &T> {
        self.inner.symmetric_difference(&other.inner)
    }

    /// Check if subset
    pub fn is_subset(&self, other: &Self) -> bool {
        self.inner.is_subset(&other.inner)
    }

    /// Check if superset
    pub fn is_superset(&self, other: &Self) -> bool {
        self.inner.is_superset(&other.inner)
    }

    /// Check if disjoint
    pub fn is_disjoint(&self, other: &Self) -> bool {
        self.inner.is_disjoint(&other.inner)
    }
}

impl<T> Default for HashSet<T>
where
    T: std::hash::Hash + Eq,
{
    fn default() -> Self {
        Self::new()
    }
}

/// Double-ended queue
#[derive(Debug, Clone)]
pub struct VecDeque<T> {
    inner: VecDeque<T>,
}

impl<T> VecDeque<T> {
    /// Create a new empty deque
    pub fn new() -> Self {
        Self {
            inner: VecDeque::new(),
        }
    }

    /// Push to front
    pub fn push_front(&mut self, value: T) {
        self.inner.push_front(value);
    }

    /// Push to back
    pub fn push_back(&mut self, value: T) {
        self.inner.push_back(value);
    }

    /// Pop from front
    pub fn pop_front(&mut self) -> Option<T> {
        self.inner.pop_front()
    }

    /// Pop from back
    pub fn pop_back(&mut self) -> Option<T> {
        self.inner.pop_back()
    }

    /// Get front
    pub fn front(&self) -> Option<&T> {
        self.inner.front()
    }

    /// Get back
    pub fn back(&self) -> Option<&T> {
        self.inner.back()
    }

    /// Get number of elements
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Clear all elements
    pub fn clear(&mut self) {
        self.inner.clear();
    }
}

impl<T> Default for VecDeque<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Priority queue (binary heap)
#[derive(Debug, Clone)]
pub struct BinaryHeap<T> {
    inner: BinaryHeap<T>,
}

impl<T> BinaryHeap<T>
where
    T: Ord,
{
    /// Create a new empty heap
    pub fn new() -> Self {
        Self {
            inner: BinaryHeap::new(),
        }
    }

    /// Push an element
    pub fn push(&mut self, value: T) {
        self.inner.push(value);
    }

    /// Pop the largest element
    pub fn pop(&mut self) -> Option<T> {
        self.inner.pop()
    }

    /// Peek at the largest element
    pub fn peek(&self) -> Option<&T> {
        self.inner.peek()
    }

    /// Get number of elements
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Clear all elements
    pub fn clear(&mut self) {
        self.inner.clear();
    }
}

impl<T> Default for BinaryHeap<T>
where
    T: Ord,
{
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec_basic() {
        let mut vec = Vec::new();
        assert!(vec.is_empty());
        
        vec.push(1);
        vec.push(2);
        vec.push(3);
        
        assert_eq!(vec.len(), 3);
        assert_eq!(vec.get(0), Some(&1));
        assert_eq!(vec.get(2), Some(&3));
        assert_eq!(vec.pop(), Some(3));
        assert_eq!(vec.len(), 2);
    }

    #[test]
    fn test_hashmap_basic() {
        let mut map = HashMap::new();
        map.insert("key1", "value1");
        map.insert("key2", "value2");
        
        assert_eq!(map.get(&"key1"), Some(&"value1"));
        assert_eq!(map.get(&"nonexistent"), None);
        assert!(map.contains_key(&"key2"));
    }

    #[test]
    fn test_hashset_basic() {
        let mut set = HashSet::new();
        set.insert(1);
        set.insert(2);
        set.insert(3);
        
        assert!(set.contains(&1));
        assert!(set.contains(&2));
        assert!(!set.contains(&4));
        assert_eq!(set.len(), 3);
    }

    #[test]
    fn test_vecdeque_basic() {
        let mut deque = VecDeque::new();
        deque.push_back(1);
        deque.push_back(2);
        deque.push_front(0);
        
        assert_eq!(deque.front(), Some(&0));
        assert_eq!(deque.back(), Some(&2));
        assert_eq!(deque.pop_front(), Some(0));
        assert_eq!(deque.len(), 2);
    }
}
