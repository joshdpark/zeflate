/// GOAL: read a gzip file (disregard the gzip headers) and output the header for
/// the first deflate block
///
/// Definitions:
/// code word: the bits that make up a huffman code
/// code length: the number of bits that make up a code word
/// symbol: the encoded byte that a huffman code word maps to
const std = @import("std");
const BitReader = @import("bitreader.zig").BitReader;
const bitReader = @import("bitreader.zig").bitReader;
const mem = std.mem;
const io = std.io;
const assert = std.debug.assert;
const stdout = std.io.getStdOut();

const MAXLITCODES = 286; // (257 - 286)
const MAXDISTCODES = 30; // (1-32)
const MAXCODES = MAXLITCODES + MAXDISTCODES;
const MAXCODELEN = 16; // maximum number of bits in a huffman code;
const PREFIX_SIZE = 3; // prefix length for code lookup TODO: write doc on this
const PREFIX_LEN = 1 << PREFIX_SIZE;

// const table_bits = 8;
// const TABLEBITSLEN = 1 << table_bits;

// hard coded tables in the rfc
// rfc 3.2.7 under (HCLEN + 4) in block format
const CodeLengthLookup = [_]u5{ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };
/// a data structure for packing in a code length and a symbol index
const Entry = union(enum) {
    entry: packed struct { // package contain symbol and length (sl)
        symbol: u12 = 0,
        length: u4 = 0,
    },
    sub: packed struct {
        base: u12 = 0,
        varbits: u4 = 0,
    },
};

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

    fn show(self: *BlockHeader) void {
        std.debug.print(
            \\{d} (hlit) literal codes (257-286 literals)
            \\{d} (hdist) distance codes (1-32)
            \\{d} (hclen) code lengths (4-19)
            \\
        , .{ @as(usize, self.hlit) + 257, self.hdist + 1, @as(u5, self.hclen) + 4 });
    }
};

/// A huffman table used for entropy decoding variable sequences of bits
const HTable = struct {
    subtable: []Entry = undefined,
    decode_table: []Entry = undefined,
    table_bits: u4 = undefined,

    const Self = @This();
    const prefix_size = PREFIX_SIZE;

    fn decode(self: *Self, reader: anytype) !Entry {
        const window = reader.peek_lsb(self.table_bits);
        // std.debug.print("window: {b:0>7}/{d}, ", .{ window, window });
        const ret: Entry = self.decode_table[window];
        return ret;
    }

    fn buildHuffTable(h: *HTable, lengths: []u4, decode_table: []Entry, comptime table_bits: u4) void {
        h.table_bits = table_bits;
        h.decode_table = decode_table;

        // declare arrays for holding intermediate tables
        var freq = [_]u16{0} ** MAXCODELEN;
        var offsets = [_]u16{0} ** MAXCODELEN;
        var sorted_symbols = [_]u16{0} ** MAXCODES;

        // counts of codeworld length
        std.debug.print("lengths: {d}\n", .{lengths});
        for (lengths) |len| {
            freq[len] += 1;
        }

        var maxlen: u4 = MAXCODELEN - 1; // the maximum code word length (1-15)
        while (freq[maxlen] == 0)
            maxlen -= 1;

        offsets[0] = 0;
        offsets[1] = freq[0];
        std.debug.print("freq: {d}\n", .{freq});
        for (1..maxlen) |len| {
            offsets[len + 1] = offsets[len] + freq[len];
        }
        // std.debug.print("offsets: {d}\n", .{offsets});

        // symbols in lexigraphic order, first by byte value and then by codeword length
        var symbol: u16 = 0;
        while (symbol < lengths.len) : (symbol += 1) {
            sorted_symbols[offsets[lengths[symbol]]] = symbol;
            offsets[lengths[symbol]] += 1;
        }
        // std.debug.print("sorted_symbols: {d}\n", .{sorted_symbols});

        // for each symbol: length combination, write the table entry into the
        // decode table.
        var len: u4 = 1; // start at minimum codeworld length
        while (freq[len] == 0)
            len += 1;

        var count: u16 = undefined;
        var codeword: u16 = 0; // first codeword always starts at 0
        var i: usize = offsets[0]; // sorted_symbol index, skip unusued symbols
        // iterate through each codeword length for the main table
        // std.debug.print("table_bits: {d}\n", .{table_bits});
        while (len <= table_bits) : (len += 1) {
            count = freq[len];
            // for each count of codeword length, write an entry into the decode table
            while (count > 0) : (count -= 1) {
                // std.debug.print("codeword: {d}, symbol: {d}, length: {d}\n", .{ codeword, sorted_symbols[i], len });
                var reverse: u16 = @bitReverse(codeword) >> (@bitSizeOf(@TypeOf(codeword)) - 1) - len + 1;
                const stride: u16 = @as(u16, 1) << len;
                const entry = Entry{
                    .entry = .{
                        .symbol = @intCast(sorted_symbols[i]),
                        .length = len,
                    },
                };
                while (true) {
                    decode_table[reverse] = entry;
                    reverse += stride;
                    if (reverse >= (1 << table_bits))
                        break;
                }
                codeword += 1;
                i += 1;
            }
            codeword <<= 1;
        }

        // iterate through codewords that can't fit in the main table
        var subtable_start: u16 = @as(u16, 1) << table_bits;
        const main_table: []Entry = decode_table[0..subtable_start];
        var subtable: []Entry = decode_table[subtable_start..];
        const mask: u16 = subtable_start - 1; // mask for subtable low bits
        var suffix: u16 = 0;
        var prefix: u16 = @bitCast(@as(i16, -1));
        // get first code of subtable, including prefix and suffix bits
        assert(len == table_bits + 1);
        while (len <= maxlen) : ({
            len += 1;
            codeword <<= 1;
        }) {
            count = freq[len];
            assert(freq[len] != 0);
            while (count > 0) : ({
                count -= 1;
                codeword += 1;
                i += 1;
            }) {
                assert(len < MAXCODELEN);
                // std.debug.print("codeword: {d}, symbol: {d}, length: {d}\n", .{ codeword, sorted_symbols[i], len });
                const reverse: u16 = @bitReverse(codeword) >> @bitSizeOf(@TypeOf(codeword)) - 1 - len + 1;
                // allot a subtable at the end of our previous table (main or another subtable)
                if (reverse & mask != prefix) {
                    prefix = reverse & mask;
                    var slots_needed: u16 = count;
                    var bit_space: u4 = len - table_bits;
                    while (slots_needed < @as(u16, 1) << bit_space) {
                        bit_space += 1;
                        slots_needed = (slots_needed << 1) + freq[bit_space + table_bits];
                    }
                    const subtable_end = subtable_start + (@as(u16, 1) << bit_space);
                    subtable = decode_table[subtable_start..subtable_end];
                    // std.debug.print("starting new subtable at i: {d} for prefix: {d} of length: {d}\n", .{ subtable_start, prefix, @as(u16, 1) << bit_space });
                    main_table[prefix] = .{ .sub = .{
                        .base = @intCast(subtable_start),
                        .varbits = bit_space,
                    } };
                    subtable_start = subtable_end; // next subtable start index
                }
                suffix = reverse >> table_bits;
                const stride = @as(u16, 1) << (len - table_bits);
                const entry: Entry = .{
                    .entry = .{
                        .symbol = @intCast(sorted_symbols[i]),
                        .length = len,
                    },
                };
                while (true) {
                    subtable[suffix] = entry;
                    suffix += stride;
                    if (suffix >= subtable.len)
                        break;
                }
            }
        }
        // for (0.., h.decode_table[0..subtable_start]) |cw, e| {
        //     std.debug.print("codeword: {b:0>16}, symbol: {d}, length: {d}\n", .{ cw, e.entry.symbol, e.entry.length });
        // }
    }

    /// build the literals/length code table
    fn buildLitLen(self: *HTable, lengths: []u4, br: anytype) void {
        var i: usize = 0;
        while (i < lengths.len) {
            br.refill();
            const codeword: Entry = try self.decode(br);
            assert(codeword != .sub); // should only be main table decode entries
            br.consume(codeword.entry.length);
            const codelen: u12 = codeword.entry.symbol;
            var rep: usize = 1;
            if (codelen < 16)
                lengths[i] = @intCast(codelen);
            if (codelen == 16) {
                rep = br.getbits(2) + 3;
                @memset(lengths[i..][0..rep], lengths[i - 1]);
            }
            if (codelen == 17) {
                rep = br.getbits(3) + 3;
                @memset(lengths[i..][0..rep], 0);
            }
            if (codelen == 18) {
                rep = br.getbits(7) + 11;
                @memset(lengths[i..][0..rep], 0);
            }
            i += rep;
        }
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
    };
}

fn rotateWriter(writer: anytype) RotateWriter(@TypeOf(writer)) {
    return .{ .writer = writer };
}

fn inflate(reader: anytype, ring: anytype, literals: *HTable, distances: *HTable) !void {
    // base lengths/distances and how many extra bits to consume and how
    // rfc 3.2.5
    const lenconsume = [29]u3{ 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0 };
    const lenbase = [29]u16{ 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258 };
    const distconsume = [30]u4{ 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13 };
    const distbase = [30]u16{ 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577 };

    while (true) {
        reader.refill();
        var entry: Entry = try literals.decode(reader);
        var sym: u12 = undefined;
        while (true) {
            switch (entry) {
                .sub => |s| {
                    var offset = reader.peek_lsb(s.varbits + literals.table_bits);
                    offset >>= literals.table_bits;
                    entry = literals.decode_table[s.base + offset];
                    assert(entry != .sub);
                    continue;
                },
                .entry => |e| {
                    assert(e.length != 0);
                    reader.consume(e.length);
                    sym = e.symbol;
                    break;
                },
            }
        }
        assert(sym <= 257 + 29 and sym != 0); // should never be above this number
        if (sym < 256) {
            // std.debug.print("literal: {c}\n", .{@as(u8, @truncate(sym))});
            ring.appendByte(@as(u8, @truncate(sym)));
        } else if (sym == 256) { // end of block
            break;
        } else {
            const lenid: usize = sym - 257;
            const len_extra: u9 = @truncate(reader.getbits(lenconsume[lenid])); // up to 5 extra bits
            const len = lenbase[lenid] + len_extra;

            entry = try distances.decode(reader);
            outer: while (true) {
                switch (entry) {
                    .sub => |s| {
                        var offset = reader.peek_lsb(s.varbits + literals.table_bits);
                        offset >>= literals.table_bits;
                        entry = distances.decode_table[s.base + offset];
                        assert(entry != .sub); // it should not be another subtable
                        assert(entry.entry.length != 0);
                        continue :outer;
                    },
                    .entry => |e| {
                        reader.consume(e.length);
                        sym = e.symbol;
                        assert(sym <= distconsume.len);
                        break :outer;
                    },
                }
            }
            const dist_extra: usize = reader.getbits(distconsume[sym]); // up to 13 extra bits for 29 bits
            const distance = distbase[sym] + dist_extra;
            ring.appendSequence(distance, len);
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
        lengths: [MAXCODES]u4 = [_]u4{0} ** MAXCODES,
        decode_table: [3000]Entry = [_]Entry{.{ .entry = .{ .symbol = 0, .length = 0 } }} ** 3000,

        const Self = @This();

        fn dynamicDecodeBlock(self: *Self, header: *const BlockHeader) !void {
            // setup hlit and hdist lengths
            const hlit = header.hlit + 257;
            const hdist = header.hdist + 1;
            const hclen = header.hclen + 4;

            // populate codelen codelength table
            self.bit_reader.refill();
            for (0..hclen) |i| {
                self.lengths[CodeLengthLookup[i]] = @truncate(self.bit_reader.getbits(3));
            }
            // no need to keep precode htable
            var precode = HTable{};
            // std.debug.print("Build precode\n", .{});
            precode.buildHuffTable(self.lengths[0..CodeLengthLookup.len], self.decode_table[0..128], 7);
            // resize data slice to hold literal/length codes
            const codelen_count = hlit + hdist;
            precode.buildLitLen(self.lengths[0..codelen_count], &self.bit_reader);

            // build literals/length table
            var litlen = HTable{};
            // std.debug.print("Build literals/length\n", .{});
            litlen.buildHuffTable(
                self.lengths[0..hlit],
                self.decode_table[0..2348],
                9,
            );

            // build distances tables
            var dist = HTable{};
            // std.debug.print("Build Distance\n", .{});
            dist.buildHuffTable(
                self.lengths[hlit..][0..hdist],
                self.decode_table[2348..][0..402],
                8,
            );
            // if (true)
            //     unreachable;
            try inflate(&self.bit_reader, &self.ring_writer, &litlen, &dist);
        }

        fn decompress(self: *Self) !void {
            try self.bit_reader.initialize();
            while (true) {
                const header = BlockHeader.init(&self.bit_reader);
                switch (header.btype) {
                    0 => unreachable,
                    1 => unreachable,
                    2 => try dynamicDecodeBlock(self, &header),
                    3 => unreachable,
                }
                if (header.bfinal == 1)
                    break;
            }
            // write out the leftovers in the buffer TODO: this is a hack, formalize this
            _ = try self.ring_writer.writer.writer().write(self.ring_writer.buf[1 << 15 .. self.ring_writer.cur]);
        }
    };
}

fn decompressor(reader: anytype, writer: anytype) Decompressor(BitReader(@TypeOf(reader)), RotateWriter(@TypeOf(writer))) {
    return .{
        .bit_reader = bitReader(reader),
        .ring_writer = rotateWriter(writer),
    };
}

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

    // all of this is to just handle the gzip specification
    {
        std.debug.print("gzip header bytes: \n", .{});
        const filetype = try stream.readInt(u16, .little);
        std.debug.print("filetype: {x}\n", .{filetype});
        if (filetype != 0x8b1f)
            @panic("not a gzip");
        const isdeflate = try stream.readByte();
        if (isdeflate != 8)
            @panic("not deflate compression");
        const flags = try stream.readByte();
        const name = (flags >> 3) & 1;
        std.debug.print("flags: {b:0>8}\n", .{flags});
        const mtime = try stream.readInt(i32, .little);
        std.debug.print("mtime: {d}\n", .{mtime});
        const xfl = try stream.readByte();
        std.debug.print("xfl: {d}\n", .{xfl});
        const os = try stream.readByte();
        std.debug.print("os: {d}\n", .{os});
        if (name > 0) {
            std.debug.print("\nfilename: ", .{});
            while (stream.readByte()) |byte| {
                if (byte == 0)
                    break;
                std.debug.print("{c}", .{byte});
            } else |_| {}
            std.debug.print("\n", .{});
        }
    }

    var deflate_decoder = decompressor(stream, bstdout);
    try deflate_decoder.decompress();
}
