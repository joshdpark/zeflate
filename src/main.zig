const std = @import("std");
const decompressor = @import("decompress.zig").decompressor;
const parseGzip = @import("handle_gzip.zig").parseGzip;
const stdout = std.io.getStdOut();

pub fn main() !void {
    var sfa = std.heap.stackFallback(1024, std.heap.page_allocator);
    const argsallocator = sfa.get();
    const args = try std.process.argsAlloc(argsallocator);
    defer std.process.argsFree(argsallocator, args);
    const filename = args[1];
    const fd = try std.fs.cwd().openFile(filename, .{});
    defer fd.close();

    var breader = std.io.bufferedReader(fd.reader());
    const stream = breader.reader();
    const bstdout = std.io.bufferedWriter(stdout.writer());

    try parseGzip(stream);
    // all of this is to just handle the gzip specification

    var deflate_decoder = decompressor(stream, bstdout);
    try deflate_decoder.decompress();
}
