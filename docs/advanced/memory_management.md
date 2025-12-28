# Memory Management

Apex is a systems language that compiles to native code. it uses LLVM as a backend.

## Stack vs Heap

- **Stack**: Used for primitive types (`Integer`, `Float`, `Boolean`) and small structs. Fast allocation/deallocation.
- **Heap**: Used for dynamic data like `String`, `List`, and class instances. Managed automatically via RAII.

## RAII (Resource Acquisition Is Initialization)

Apex follows the RAII pattern.

- Memory is allocated when an object is created.
- Memory is freed when the object goes out of scope (droppped).

There is no Garbage Collector (GC), ensuring predictable performance and low latency.

## Smart Pointers

(Reference)
Apex supports smart pointers to manage heap memory:

- `Box<T>`: Unique ownership on the heap.
- `Rc<T>`: Reference counting (shared ownership, single-threaded).
- `Arc<T>`: Atomic reference counting (shared ownership, thread-safe).
