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

const MAXLOOKUP = 8;
const MAXLOOKUPSIZE = 1 << MAXLOOKUP;

// hard coded tables in the rfc
// rfc 3.2.7 under (HCLEN + 4) in block format
const CodeLengthLookup = [_]u5{ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };
/// a data structure for packing in a code length and a symbol index
const Symlen = packed struct {
    symbol: u12 = 0,
    length: u4 = 0,
};

const BlockHeader = struct {
    bfinal: u1,
    btype: u2,
    hlit: u5,
    hdist: u5,
    hclen: u4,

    fn init(r: anytype) BlockHeader {
        return .{
            .bfinal = @truncate(r.getbits(1)),
            .btype = @truncate(r.getbits(2)),
            .hlit = @truncate(r.getbits(5)),
            .hdist = @truncate(r.getbits(5)),
            .hclen = @truncate(r.getbits(4)),
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

const LenMap = packed struct {
    offset: u16,
    lj_base: u16,
};

/// A huffman table used for entropy decoding variable sequences of bits
const HTable = struct {
    data: []Symlen,
    maxlen: u4 = MAXCODELEN - 1, // the maximum code word length (1-15)
    // the left-justified (lj) base table is basically all the code words for each symbol
    // but left-shifted so that they all have a bit-length of the maximum number of
    // code words.
    lenmap: [MAXCODELEN]LenMap = undefined,
    prefix_start: [PREFIX_LEN]u16 = [_]u16{0} ** PREFIX_LEN,
    decode_table: [MAXLOOKUPSIZE]Symlen = undefined,

    const Self = @This();
    const prefix_size = PREFIX_SIZE;

    /// NOTES:
    /// A 'symbol' is the byte that is being encoded in a huffman code
    /// A 'code' is a variable number of bits that decodes to a symbol
    /// A 'code length' aka 'hclen' (huffman code length) is the number of bits that makes up the code
    /// The deflate format specifies that the run-length encoded symbols 0-18 are encoded by
    /// the number of code lengths from an array of ranges, build a huffman tree
    fn buildHuffTable(h: *HTable) void {
        // TODO:find a way to build the canonical codes so that they are in lsb order, not
        // in the msb order; this is to improve performance so that I don't need peek_msb
        // codelength freq table
        var freq = [_]u12{0} ** (MAXCODELEN + 1);
        for (h.data) |item| {
            const len = item.length;
            freq[len] += 1;
        }

        while (h.maxlen > 0 and freq[h.maxlen] == 0)
            h.maxlen -= 1;

        var next_code: [MAXCODELEN]u12 = [_]u12{0} ** MAXCODELEN;
        {
            var code: u12 = 0;
            freq[0] = 0;
            var i: u4 = 1;
            while (i <= h.maxlen) : (i += 1) {
                code = (code + freq[i - 1]) << 1;
                next_code[i] = code;
            }
        }

        std.debug.print("next_code: {d}\n", .{next_code});

        // TODO: how do I manage a subtable
        {
            for (h.data, 0..) |sl, i| {
                const nbits = sl.length;
                if (nbits > 0) {
                    const code: u12 = next_code[nbits];
                    next_code[nbits] += 1;
                    {
                        var n = nbits;
                        var rev = @bitReverse(code) >> (12 - nbits); // symbol size of 12
                        // fill top bits of reversed code
                        while (rev < MAXLOOKUPSIZE) : (rev += @as(u12, 1) << n) {
                            const symbol: u12 = @truncate(i); // usize -> u12
                            h.decode_table[rev] = Symlen{ .symbol = symbol, .length = nbits };
                        }
                    }
                }
            }
            // for (0.., decode_table) |i, sl| {
            //     std.debug.print("index: {d}, symbol: {d}, length: {d}\n", .{ i, sl.symbol, sl.length });
            // }
        }
    }

    /// build the literals/length code table
    fn buildLitLen(self: *HTable, br: anytype) void {
        var i: usize = 0;
        while (i < self.data.len) {
            br.refill();
            const code: u16 = try self.decode(br);
            var rep: usize = 1;
            self.data[i].length = @truncate(code);
            if (code == 16) {
                rep = br.getbits(2) + 3;
                const prev = self.data[i - 1].length;
                for (self.data[i..][0..rep]) |*val| {
                    val.*.length = prev;
                }
            }
            if (code == 17) {
                rep = br.getbits(3) + 3;
                for (self.data[i..][0..rep]) |*val| {
                    val.*.length = 0;
                }
            }
            if (code == 18) {
                rep = br.getbits(7) + 11;
                for (self.data[i..][0..rep]) |*val| {
                    val.*.length = 0;
                }
            }
            i += rep;
        }
        {
            for (self.data) |d| {
                std.debug.print("length:{d} \n", .{d.length});
            }
            if (true)
                unreachable;
        }
    }

    fn decode(self: *Self, reader: anytype) !u16 {
        const window = reader.peek_lsb(MAXLOOKUP);
        const ret: Symlen = self.decode_table[window];
        reader.consume(ret.length);
        return ret.symbol;
    }

    /// One additional advantage is that I don't have to store the minimum code length.
    /// That is already being precalculated in the prefix_start table.
    fn lookup_decode(self: *Self, reader: anytype) !u16 {
        const width: u4 = self.maxlen;
        const window = reader.peek_msb(width);
        const prefix = window >> (width - prefix_size);
        var length: u6 = @truncate(self.prefix_start[prefix]);
        if (length > prefix_size) {
            while (window >= self.lenmap[length + 1].lj_base)
                length += 1;
        }
        reader.consume(length);
        const rshift = width - length;
        const lm = self.lenmap[length];
        const symbol_id = lm.offset + (window - lm.lj_base >> rshift);
        return self.data[symbol_id].symbol;
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
    const lenconsume = [_]u3{ 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0 };
    const lenbase = [_]u16{ 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258 };
    const distconsume = [_]u4{ 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13 };
    const distbase = [_]u16{ 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577 };

    var sym: u16 = undefined;
    while (true) {
        reader.refill();
        sym = try literals.lookup_decode(reader); // literal, up to 15 bits
        if (sym < 256) {
            ring.appendByte(@as(u8, @truncate(sym)));
        } else if (sym == 256) { // end of block
            break;
        } else {
            const lenid: usize = sym - 257;
            const len_extra: u9 = @truncate(reader.getbits(lenconsume[lenid])); // up to 5 extra bits
            const len = lenbase[lenid] + len_extra;

            const distcode = try distances.lookup_decode(reader); // distance, up to 16 bits
            const dist_extra: usize = reader.getbits(distconsume[distcode]); // up to 13 extra bits for 29 bits
            const distance = distbase[distcode] + dist_extra;

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
        data: [MAXCODES]Symlen = [_]Symlen{Symlen{ .symbol = 0, .length = 0 }} ** MAXCODES,

        const Self = @This();

        fn dynamicDecodeBlock(self: *Self, header: *const BlockHeader) !void {
            // populate codelen codelength table
            self.bit_reader.refill();
            for (0..@as(usize, header.hclen) + 4) |i| {
                self.data[CodeLengthLookup[i]].length = @truncate(self.bit_reader.getbits(3));
            }
            var litlen = HTable{ .data = self.data[0..CodeLengthLookup.len] };
            litlen.buildHuffTable();
            // resize data slice to hold literal/length codes
            const codelen_count = @as(usize, header.hlit) + @as(usize, header.hdist) + 258;
            litlen.data = self.data[0..codelen_count];
            litlen.buildLitLen(&self.bit_reader);

            // build literals/length table
            litlen = .{ .data = self.data[0 .. @as(u16, header.hlit) + 257] };
            litlen.buildHuffTable();

            // build distances tables
            var dist = HTable{ .data = self.data[@as(usize, header.hlit) + 257 ..][0 .. @as(usize, header.hdist) + 1] };
            dist.buildHuffTable();
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

    var deflate_decoder = decompressor(stream, bstdout);
    try deflate_decoder.decompress();
}
