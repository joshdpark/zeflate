const std = @import("std");

pub fn parseGzip(stream: anytype) !void {
    // std.debug.print("gzip header bytes: \n", .{});
    const filetype = try stream.readInt(u16, .little);
    _ = filetype;
    // std.debug.print("filetype: {x}\n", .{filetype});
    const isdeflate = try stream.readByte();
    if (isdeflate != 8)
        @panic("not deflate compression");
    const flags = try stream.readByte();
    const name = (flags >> 3) & 1;
    // std.debug.print("flags: {b:0>8}\n", .{flags});
    _ = try stream.readInt(i32, .little);
    // std.debug.print("mtime: {d}\n", .{mtime});
    _ = try stream.readByte();
    // std.debug.print("xfl: {d}\n", .{xfl});
    _ = try stream.readByte();
    // std.debug.print("os: {d}\n", .{os});
    if (name > 0) {
        // std.debug.print("\nfilename: ", .{});
        while (stream.readByte()) |byte| {
            if (byte == 0)
                break;
            // std.debug.print("{c}", .{byte});
        } else |_| {}
        // std.debug.print("\n", .{});
    }
}
