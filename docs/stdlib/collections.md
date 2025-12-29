# Collections

Apex provides built-in collection types for managing groups of data. These are implemented as efficient intrinsic types.

## List`<T>`

`List<T>` is a dynamic array that grows automatically.

> **Note**: `Set<T>` and `Map<K, V>` are currently defined in the type system but standard library (methods) support is still in development. The following documentation for Maps refers to the intended API.

### List Methods

#### `push(element: T): None`

Adds an element to the end of the list.

```apex
list: List<Integer> = List<Integer>();
list.push(42);
```

#### `pop(): T`

Removes and returns the last element from the list.

```apex
last: Integer = list.pop();
```

#### `get(index: Integer): T`

Returns the element at the specified index. Panics if index is out of bounds.

```apex
val: Integer = list.get(0);
```

#### `set(index: Integer, value: T): None`

Updates the element at the specified index. Panics if index is out of bounds.

```apex
list.set(0, 100);
```

#### `length(): Integer`

Returns the number of elements in the list.

```apex
size: Integer = list.length();
```

## Map`<K, V>`

`Map<K, V>` is a key-value store. Currently implemented as an association list (O(n) lookup), with hash map optimization planned.

### Map Methods

#### `insert(key: K, value: V): None`

Inserts a key-value pair into the map. If the key already exists, the value is updated.

```apex
scores: Map<String, Integer> = Map<String, Integer>();
scores.insert("Alice", 100);
```

#### `get(key: K): V`

Retrieves the value associated with the key. Panics if the key is not found (use `contains` check first).

```apex
score: Integer = scores.get("Alice");
```

#### `contains(key: K): Boolean`

Returns `true` if the map contains the specified key.

```apex
if (scores.contains("Alice")) {
    println("Alice found");
}
```

#### `length(): Integer`

Returns the number of key-value pairs in the map.

```apex
count: Integer = scores.length();
```
