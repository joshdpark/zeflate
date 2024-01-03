/// This is to work through a decompressor example from an example text that
/// will be compressed.

// GOAL: read a gzip file (disregard the gzip headers) and output the header for
// the first deflate block
const std = @import("std");
const stdout = std.io.getStdOut().writer();
const BitReader = @import("bitreader.zig").BitReader;

const BlockHead = struct {
    bfinal: u1,
    btype: u2,
    hlit: u5,
    hdist: u5,
    hclen: u4,
};

// TODO: this isn't very elegant but it works
fn read_header(hd: *BlockHead, r: *BitReader) void {
    std.debug.print("bitstream: {b:0>17}\n", .{r.bitbuf & (1 << 17) - 1});
    hd.bfinal = @truncate(r.getbits(1));
    hd.btype = @truncate(r.getbits(2));
    hd.hlit = @truncate(r.getbits(5));
    hd.hdist = @truncate(r.getbits(5));
    hd.hclen = @truncate(r.getbits(4));
}

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
    try stdout.print("gzip header bytes: \n", .{});
    for (0..10) |_| {
        try stdout.print("{x} ", .{try stream.readByte()});
    }
    {
        try stdout.print("\nfilename: \n", .{});
        while (true) {
            const byte = try stream.readByte();
            if (byte == 0)
                break;
            try stdout.print("{c}", .{byte});
        }
    }
    // {
    //     try stdout.print("\nskipping crc32\n", .{});
    //     var crc_bytes: u32 = 0;
    //     while (true) {
    //         const byte = try stream.readByte();
    //         crc_bytes += 1;
    //         if (byte == 0)
    //             break;
    //     }
    //     try stdout.print("crc32 had {d} bytes\n", .{crc_bytes});
    // }
    // set up bitreader
    var bitreader = BitReader.init();
    bitreader.refill(&stream);
    var block_head: BlockHead = undefined;
    read_header(&block_head, &bitreader);
    std.debug.print("{any}\n", .{block_head});
}
