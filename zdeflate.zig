const std = @import("std");
const io = std.io;
const fs = std.fs;
const testing = std.testing;
const mem = std.mem;
const deflate = std.compress.deflate;
const out = std.io.getStdOut();

pub fn main() !void {
    var sfa = std.heap.stackFallback(1024, std.heap.page_allocator);
    const argsallocator = sfa.get();
    const args = try std.process.argsAlloc(argsallocator);
    defer std.process.argsFree(argsallocator, args);
    const filename = args[1];
    const fd = try std.fs.cwd().openFile(filename, .{});
    defer fd.close();

    var buf_reader = std.io.bufferedReader(fd.reader());
    const stream = buf_reader.reader();

    {
        // std.debug.print("gzip header bytes: \n", .{});
        const filetype = try stream.readInt(u16, .little);
        // std.debug.print("filetype: {x}\n", .{filetype});
        if (filetype != 0x8b1f)
            @panic("not a gzip");
        const isdeflate = try stream.readByte();
        if (isdeflate != 8)
            @panic("not deflate compression");
        const flags = try stream.readByte();
        const name = (flags >> 3) & 1;
        // std.debug.print("flags: {b:0>8}", .{flags});
        const mtime = try stream.readInt(i32, .little);
        _ = mtime;
        const xfl = try stream.readByte();
        _ = xfl;
        const os = try stream.readByte();
        _ = os;
        if (name > 0) {
            // std.debug.print("\nfilename: ", .{});
            while (stream.readByte()) |byte| {
                if (byte == 0)
                    break;
                // std.debug.print("{c}", .{byte});
            } else |_| {}
        }
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var inflater = try deflate.decompressor(allocator, stream, null);
    defer inflater.deinit();

    const output = try inflater.reader().readAllAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(output);

    var bout = std.io.bufferedWriter(out.writer());
    var w = bout.writer();
    _ = try w.write(output);
}
