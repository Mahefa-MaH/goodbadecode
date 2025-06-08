**Title:** Zig vs. C: Memory Management Contrasts

**Summary:**  Zig's compile-time memory management and built-in error handling offer improved safety and performance compared to C's manual memory management, which is prone to errors like memory leaks and dangling pointers.  Zig's type system also enforces stricter rules leading to fewer runtime surprises.

**Good Code (Zig):**

```zig
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const num_elements = 10;
    var arr = try allocator.alloc(i32, num_elements);
    defer allocator.free(arr);

    for (0..num_elements) |i| {
        arr[i] = i * 2;
    }

    for (0..num_elements) |i| {
        std.debug.print("arr[{d}] = {d}\n", .{ i, arr[i] });
    }
}
```

**Bad Code (C):**

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    int num_elements = 10;
    int *arr = (int *)malloc(num_elements * sizeof(int)); 

    for (int i = 0; i < num_elements; i++) {
        arr[i] = i * 2;
    }

    for (int i = 0; i < num_elements; i++) {
        printf("arr[%d] = %d\n", i, arr[i]);
    }
    // Missing free(arr);  <-- Memory Leak!
    return 0;
}
```

**Key Takeaways:**

* **Error Handling:** Zig's `try` keyword handles allocation errors gracefully, preventing crashes.  The C code silently ignores potential `malloc` failures.
* **Memory Management:** Zig's `defer allocator.free(arr);` ensures memory is always freed, preventing memory leaks. The C code lacks explicit memory deallocation, leading to a memory leak.
* **Allocator Management:** Zig explicitly manages allocators, giving more control and enabling features like custom allocators. C relies on the global heap, offering less control and potential for fragmentation.
* **Type Safety:** Zig's stricter type system helps catch errors at compile time, while C's looser typing can lead to runtime errors.
* **Readability:**  Zig's `defer` statement enhances code clarity regarding resource management. C requires manual memory freeing, making the code less readable and more prone to errors.
* **Compile-Time Safety:** Zig's compile-time checks catch many errors before runtime unlike C where memory errors are only caught at runtime, often leading to crashes.


