const std = @import("std");

// Good Code:  Using built-in error handling
pub fn goodCode(comptime len: usize) ![]u8 {
    var buf = try std.ArrayList(u8).initCapacity(std.heap.page_allocator, len);
    defer buf.deinit();
    for (0..len) |i| {
        try buf.append( @intCast(u8, i));
    }
    return buf.toOwnedSlice();
}

// Bad Code: Ignoring errors, potential for undefined behavior
pub fn badCode(comptime len: usize) []u8 {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit(); //This is a defer statement but it does not handle any error
    for (0..len) |i| {
        _ = buf.appendAssumeCapacity(@intCast(u8, i)); // Ignoring potential errors
    }
    return buf.toOwnedSlice();
}

test {
    const good = try goodCode(10);
    const bad = badCode(10);
    std.debug.print("Good code: {any}\n", .{good});
    std.debug.print("Bad code: {any}\n", .{bad});
}
