# Async / Await

Apex has first-class support for asynchronous programming.

## Async Functions

Define an async function using `async`. It returns a `Task<T>`.

```apex
async function fetchData(): Task<String> {
    // Simulate network delay
    return "Data";
}
```

## Await

Use `await` to wait for an async function to complete.

```apex
async function main(): Task<None> {
    data: String = await fetchData();
    println("Received: {data}");
    return None;
}
```

## Tasks

`Task<T>` represents a future value. The runtime schedules these tasks efficiently.
