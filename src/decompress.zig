/// Definitions:
/// code word: the bits that make up a huffman code
/// code length: the number of bits that make up a code word
/// symbol: the encoded byte that a huffman code word maps to
const std = @import("std");
const BitReader = @import("bitreader.zig").BitReader;
const bitReader = @import("bitreader.zig").bitReader;
const mem = std.mem;
const io = std.io;
const testing = std.testing;
const assert = std.debug.assert;

const htree = @import("htree.zig");
const Entry = htree.Entry;

const maxlitcodes = 286; // (257 - 286)
const maxdistcodes = 30; // (1-32)
const maxcodes = maxlitcodes + maxdistcodes;
// const maxcodelen = 15; // maximum number of bits in a huffman code;

// hard coded tables in the rfc
// rfc 3.2.7 under (HCLEN + 4) in block format
const precode_lookup = [_]u5{ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };

const BlockHeader = struct {
    bfinal: u1,
    btype: u2,
    hlit: usize,
    hdist: usize,
    hclen: usize,

    fn init(r: anytype) BlockHeader {
        return .{
            .bfinal = @truncate(r.getbits(1)),
            .btype = @truncate(r.getbits(2)),
            .hlit = r.getbits(5),
            .hdist = r.getbits(5),
            .hclen = r.getbits(4),
        };
    }

    fn show(self: *const BlockHeader) void {
        std.debug.print(
            \\{d} (bfinal)
            \\{d} (btype) code block compression type
            \\{d} (hlit) literal codes (257-286 literals)
            \\{d} (hdist) distance codes (1-32)
            \\{d} (hclen) code lengths (4-19)
            \\
        , .{
            self.bfinal,
            self.btype,
            self.hlit + 257,
            self.hdist + 1,
            self.hclen + 4,
        });
    }
};

/// the deflate rfc states that the lookbehind buffer for LZ77 decoding
/// has a maximum length of 32,768 (32k elements) and so we keep a back
/// buffer to make sure that we can read back to that max length. Once
/// our cur pointer has advanced far enough, we can replace the back
/// buffer with the forward buffer.
/// +-------------------------------------------------------------------+
/// |                         buf[65,536 + 258]                         |
/// +------------------+------------------+-----------------------------+
/// | 32k back buffer  | 32k front buffer | 258 extra bytes for padding |
/// +------------------+------------------+-----------------------------+
fn RotateWriter(comptime WriterType: type) type {
    return struct {
        writer: WriterType,
        buf: [buflen + 258]u8 = undefined,
        cur: usize = halflen, // default to being at halfpoint

        const Self = @This();
        const buflen: usize = 1 << 16; // 65,536 bytes
        const halflen: usize = 1 << 15; // 32,768 bytes

        fn appendByte(self: *Self, byte: u8) void {
            assert(self.cur < self.buf.len);
            self.buf[self.cur] = byte;
            self.cur += 1;
        }

        fn appendSequence(self: *Self, distance: usize, length: usize) void {
            const dst = self.buf[self.cur..][0..length];
            const src = self.buf[self.cur - distance ..][0..length];
            mem.copyForwards(u8, dst, src);
            self.cur += length;
        }

        /// replace back buffer with front buffer with the bytes of padding and
        /// then write back buffer to the out writer.
        /// TODO: the final write is being handled manually, instead handle it
        /// in a less adhoc way.
        fn rotate(self: *Self) !void {
            assert(self.cur >= buflen);
            const back = self.buf[0..halflen];
            const front = self.buf[halflen..buflen];
            const extra = self.cur - buflen;
            @memcpy(back, front);
            @memcpy(self.buf[halflen..][0..extra], self.buf[buflen..][0..extra]);

            // reset pointer
            self.cur = halflen + extra;
        }

        fn writeout(self: *Self) !void {
            _ = try self.writer.write(self.buf[0..halflen]);
        }

        fn flush(self: *Self) !void {
            _ = try self.writer.write(self.buf[halflen..self.cur]);
        }
    };
}

fn rotateWriter(writer: anytype) RotateWriter(@TypeOf(writer)) {
    return .{ .writer = writer };
}

fn inflatePrecode(r: anytype, precode: *htree.PrecodeTable, lengths: []u4) void {
    var i: usize = 0;
    while (i < lengths.len) {
        r.refill();
        const entry = precode.decode(r.peek_lsb(7));
        r.consume(entry.length);
        var rep: usize = 1;
        if (entry.symbol < 16) {
            lengths[i] = @as(u4, @intCast(entry.symbol));
        } else if (entry.symbol == 16) {
            rep = r.getbits(2) + 3;
            @memset(lengths[i..][0..rep], lengths[i - 1]);
        } else if (entry.symbol == 17) {
            rep = r.getbits(3) + 3;
            @memset(lengths[i..][0..rep], 0);
        } else if (entry.symbol == 18) {
            rep = r.getbits(7) + 11;
            @memset(lengths[i..][0..rep], 0);
        } else unreachable;
        i += rep;
    }
}

fn inflate(reader: anytype, ring: anytype, literals: *htree.LiteralsTable, distances: *htree.DistancesTable) !void {
    // base lengths/distances and how many extra bits to consume and how
    // rfc 3.2.5
    const lenconsume = [29]u3{ 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0 };
    const lenbase = [29]u16{ 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258 };
    const distconsume = [30]u4{ 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13 };
    const distbase = [30]u16{ 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577 };

    while (true) {
        reader.refill();
        var entry: Entry = literals.decode(reader.peek_lsb(15));
        reader.consume(entry.length);
        switch (entry.kind) {
            .link => unreachable,
            .literal => ring.appendByte(@intCast(entry.symbol)),
            .match => {
                const len_extra: u9 = @truncate(reader.getbits(lenconsume[entry.symbol])); // up to 5 extra bits
                const len = lenbase[entry.symbol] + len_extra;

                entry = distances.decode(reader.peek_lsb(15));
                reader.consume(entry.length);
                const dist_extra: usize = reader.getbits(distconsume[entry.symbol]); // up to 13 extra bits for 29 bits
                const distance = distbase[entry.symbol] + dist_extra;
                ring.appendSequence(distance, len);
            },
            .end => break,
        }
        // swap out the front of the buffer into the back
        if (ring.cur >= ring.buf.len - 258) {
            try ring.rotate();
            try ring.writeout();
        }
    }
}

fn Decompressor(comptime ReaderType: type, comptime WriterType: type) type {
    return struct {
        bit_reader: ReaderType,
        ring_writer: WriterType,
        lengths: [maxcodes]u4 = [_]u4{0} ** maxcodes,
        literals: htree.LiteralsTable = undefined,
        distances: htree.DistancesTable = undefined,

        const Self = @This();

        fn dynamicBlock(self: *Self, header: *const BlockHeader) !void {
            // setup hlit and hdist lengths
            const hlit = header.hlit + 257;
            const hdist = header.hdist + 1;
            const hclen = header.hclen + 4;

            // populate codelen codelength table
            self.bit_reader.refill();
            for (0..hclen) |i| {
                self.lengths[precode_lookup[i]] = @truncate(self.bit_reader.getbits(3));
            }
            // no need to keep precode htable
            var precode = htree.PrecodeTable{};
            precode.build(self.lengths[0..precode_lookup.len]);
            // resize data slice to hold literal/length codes
            const codelen_count = hlit + hdist;
            inflatePrecode(&self.bit_reader, &precode, self.lengths[0..codelen_count]);

            // build literals/length table
            self.literals.build(self.lengths[0..hlit]);
            self.distances.build(self.lengths[hlit..][0..hdist]);
            try inflate(&self.bit_reader, &self.ring_writer, &self.literals, &self.distances);
        }

        pub fn decompress(self: *Self) !void {
            try self.bit_reader.initialize();
            while (true) {
                const header = BlockHeader.init(&self.bit_reader);
                switch (header.btype) {
                    0 => unreachable,
                    1 => unreachable,
                    2 => try dynamicBlock(self, &header),
                    3 => unreachable,
                }
                if (header.bfinal == 1)
                    break;
            }
            // write out the leftovers in the buffer TODO: this is a hack, formalize this
            try self.ring_writer.flush();
        }
    };
}

pub fn decompressor(reader: anytype, writer: anytype) Decompressor(BitReader(@TypeOf(reader)), RotateWriter(@TypeOf(writer))) {
    return .{
        .bit_reader = bitReader(reader),
        .ring_writer = rotateWriter(writer),
    };
}

test Decompressor {
    const parseGzip = @import("handle_gzip.zig").parseGzip;
    const input: []const u8 = @embedFile("testdata/gunzip.c.gz");
    const correct: []const u8 = @embedFile("testdata/gunzip.c");
    var input_io = io.fixedBufferStream(input);
    const stream = input_io.reader();

    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    const output_io = list.writer();

    try parseGzip(stream);
    var deflate_decoder = decompressor(stream, output_io);
    try deflate_decoder.decompress();
    try testing.expectEqualSlices(u8, list.items, correct);
}
