/// GOAL: read a gzip file (disregard the gzip headers) and output the header for
/// the first deflate block
///
/// Definitions:
/// code word: the bits that make up a huffman code
/// code length: the number of bits that make up a code word
/// symbol: the encoded byte that a huffman code word maps to
const std = @import("std");
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

// hard coded tables in the rfc
// rfc 3.2.7 under (HCLEN + 4) in block format
const CodeLengthLookup = [_]u5{ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };

fn BitReader(comptime ReaderType: type) type {
    return struct {
        reader: ReaderType,
        buf: [4096]u8 = undefined,
        bitptr: usize = 0, // ptr to the stream
        bitbuf: u64 = 0, // bit buffer
        bitcount: u6 = 0, // number of bits in bitbuf

        const Self = @This();

        // initialize the buf and the bitbuf
        fn initialize(self: *Self) !void {
            _ = try self.reader.read(self.buf[0..]);
            self.refill();
        }

        pub fn refill(self: *Self) void {
            // grab the next word and insert them right above the current top
            self.bitbuf |= self.read64() << self.bitcount;

            // advance the ptr for next iteration
            self.bitptr += (63 - self.bitcount) >> 3;

            // update the bitcount
            self.bitcount |= 56; // 0b111000; bitcount is in [56,63)
        }

        fn read64(self: *Self) u64 {
            if (self.bitptr + 8 > self.buf.len) {
                const left = self.lookahead();
                if (left < 8) {
                    var end: u64 = 0;
                    var shift: u6 = 0;
                    for (self.buf[self.bitptr..]) |byte| {
                        end <<= shift;
                        end |= byte;
                        shift += 8;
                    }
                    return end;
                }
            }
            return @bitCast(self.buf[self.bitptr..][0..8].*);
        }

        // read from stream into the buf
        fn lookahead(self: *Self) usize {
            const remaining = self.buf.len - self.bitptr;
            @memcpy(self.buf[0..remaining], self.buf[self.bitptr..][0..remaining]);
            // read into the top buffer
            const left = self.reader.read(self.buf[remaining..]) catch @panic("read error");
            self.bitptr = 0; // reset bitptr
            return left;
        }

        fn peek_msb(self: *Self, n: u6) u64 {
            assert(n > 0);
            return @bitReverse(self.bitbuf << (63 - n + 1));
        }

        fn peek_lsb(self: *Self, count: u6) u64 {
            assert(count >= 0 and count <= 56);
            assert(count <= self.bitcount);

            const mask: u64 = (@as(u64, 1) << count) - 1;

            return self.bitbuf & mask;
        }

        fn consume(self: *Self, count: u6) void {
            // TODO: a single code read can use up to 15 bits for a literal, 15 + 5 bits for
            // lengths, and x + 13 bits for distances.
            if (count <= self.bitcount)
                self.refill();

            self.bitbuf >>= count;
            self.bitcount -= count;
        }

        fn getbits(self: *Self, count: u6) u64 {
            const bits = self.peek_lsb(count);
            self.consume(count);
            return bits;
        }
    };
}

fn bitReader(reader: anytype) BitReader(@TypeOf(reader)) {
    return .{ .reader = reader };
}

/// a data structure for packing in a code length and a symbol index
const Symlen = packed struct {
    symbol: u12,
    length: u4,
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

const HTable = struct {
    data: []Symlen,
    minlen: u4 = MAXCODELEN - 1, // the minimum code word length
    maxlen: u4 = 0, // the maximum code word length (1-15)
    // the left-justified (lj) base table is basically all the code words for each symbol
    // but left-shifted so that they all have a bit-length of the maximum number of
    // code words.
    lj_base: [MAXCODELEN]u16 = [_]u16{0} ** MAXCODELEN,
    prefix_start: [PREFIX_LEN]u16 = [_]u16{0} ** PREFIX_LEN,
    offset: [MAXCODELEN]u16 = [_]u16{0} ** MAXCODELEN,

    const Self = @This();
    const prefix_size = PREFIX_SIZE;

    /// NOTES:
    /// A 'symbol' is the byte that is being encoded in a huffman code
    /// A 'code' is a variable number of bits that decodes to a symbol
    /// A 'code length' aka 'hclen' (huffman code length) is the number of bits that makes up the code
    /// The deflate format specifies that the run-length encoded symbols 0-18 are encoded by
    /// the number of code lengths from an array of ranges, build a huffman tree
    fn buildHuffTable(h: *HTable) void {
        // codelength count table
        var freq = [_]u16{0} ** MAXCODELEN;
        for (h.data) |item| {
            const len = item.length;
            freq[len] += 1;
            h.maxlen = @max(len, h.maxlen);
            h.minlen = if (len != 0) @min(len, h.minlen) else h.minlen;
        }

        // lj_base table
        {
            var code: u16 = 0;
            freq[0] = 0;
            var i: u4 = h.minlen;
            while (i <= h.maxlen) : (i += 1) {
                code = (code + freq[i - 1]) << 1;
                h.lj_base[i] = code << (h.maxlen - i);
            }
            // add sentinel value for absolute maximum code value
            h.lj_base[h.maxlen + 1] = code << 1;
        }

        // prefix_start table
        {
            const w = prefix_size; // width of prefix
            const rshift: u4 = h.maxlen - w; // amt to shift to get prefix width
            var i: u4 = h.maxlen;
            // Look at the prefix for each left-justified code, since we need to record
            // the shortest length that matches that prefix.
            while (i >= h.minlen) : (i -= 1) {
                const base = h.lj_base[i] >> (h.maxlen - i); // get the base value of each length
                const range = base + freq[i];
                for (base..range) |b| {
                    const lj = b << h.maxlen - i; // left justify the code
                    const prefix = lj >> rshift; // keep the prefix
                    h.prefix_start[prefix] = i;
                }
            }
            // second pass to fill in the gaps for prefix values that weren't populated
            var j: usize = 1;
            while (j < h.prefix_start.len) : (j += 1) {
                if (h.prefix_start[j - 1] != 0 and h.prefix_start[j] == 0)
                    h.prefix_start[j] = h.prefix_start[j - 1];
            }
        }

        // offset table
        h.offset[1] = 0;
        for (1..h.maxlen) |len| {
            h.offset[len + 1] = h.offset[len] + freq[len];
        }

        // populate the symbol index section of data
        var offset_copy: [MAXCODELEN]u16 = undefined;
        @memcpy(offset_copy[0..], h.offset[0..]);
        {
            var symbol: u9 = 0;
            while (symbol < h.data.len) : (symbol += 1) {
                const len = h.data[symbol].length;
                if (len != 0) {
                    h.data[offset_copy[len]].symbol = symbol;
                    offset_copy[len] += 1;
                }
            }
        }
    }

    /// build the literals/length code table
    fn buildLitLen(decodeTable: *HTable, br: anytype) void {
        var i: usize = 0;
        var lens = decodeTable.data;
        while (i < lens.len) {
            const code: u16 = try decodeTable.lookup_decode(br);
            var rep: usize = 1;
            lens[i].length = @truncate(code);
            if (code == 16) {
                rep = br.getbits(2) + 3;
                const prev = lens[i - 1].length;
                for (lens[i..][0..rep]) |*val| {
                    val.*.length = prev;
                }
            }
            if (code == 17) {
                rep = br.getbits(3) + 3;
                for (lens[i..][0..rep]) |*val| {
                    val.*.length = 0;
                }
            }
            if (code == 18) {
                rep = br.getbits(7) + 11;
                for (lens[i..][0..rep]) |*val| {
                    val.*.length = 0;
                }
            }
            i += rep;
        }
    }

    /// One additional advantage is that I don't have to store the minimum code length.
    /// That is already being precalculated in the prefix_start table.
    fn lookup_decode(self: *Self, reader: anytype) !u16 {
        const width: u4 = self.maxlen;
        const window = reader.peek_msb(width);
        const prefix = window >> (width - prefix_size);
        var length: u6 = @truncate(self.prefix_start[prefix]);
        if (length > prefix_size) {
            while (window >= self.lj_base[length + 1])
                length += 1;
        }
        reader.consume(length);
        const rshift = width - length;
        const symbol_id = self.offset[length] + ((window - self.lj_base[length]) >> rshift);
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
/// | 32 k back buffer | 32k front buffer | 258 extra bytes for padding |
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

            // write out back buffer and flush
            var w = self.writer.writer();
            _ = try w.write(back);
            try self.writer.flush();

            // reset pointer
            self.cur = halflen + extra;
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
        sym = try literals.lookup_decode(reader);
        if (sym < 256) {
            ring.appendByte(@as(u8, @truncate(sym)));
        } else if (sym == 256) { // end of block
            break;
        } else {
            const lenid: usize = sym - 257;
            const len_extra: u9 = @truncate(reader.getbits(lenconsume[lenid]));
            const len = lenbase[lenid] + len_extra;

            const distcode = try distances.lookup_decode(reader);
            const dist_extra: usize = reader.getbits(distconsume[distcode]);
            const distance = distbase[distcode] + dist_extra;

            ring.appendSequence(distance, len);
        }
        // swap out the front of the buffer into the back
        if (ring.cur >= ring.buf.len - 258)
            try ring.rotate();
    }
}

fn Decompressor(comptime ReaderType: type, comptime WriterType: type) type {
    return struct {
        bit_reader: ReaderType,
        ring_writer: WriterType,

        header: BlockHeader,
        litlen_tbl: HTable, // codelen htable, reuse for literal/length htable
        dist_tbl: HTable, // distance htable
        data: [MAXCODES]Symlen = [_]Symlen{Symlen{ .symbol = 0, .length = 0 }} ** MAXCODES,

        const Self = @This();

        fn decompress(self: *Self) !void {
            try self.bit_reader.initialize();
            while (true) {
                self.header = BlockHeader.init(&self.bit_reader);
                self.bit_reader.refill();
                // populate codelen codelength table
                for (0..@as(usize, self.header.hclen) + 4) |i| {
                    self.data[CodeLengthLookup[i]].length = @truncate(self.bit_reader.getbits(3));
                }
                self.litlen_tbl = HTable{ .data = self.data[0..CodeLengthLookup.len] };
                self.litlen_tbl.buildHuffTable();
                // resize data slice to hold literal/length codes
                const codelen_count = @as(u9, self.header.hlit) + @as(u9, self.header.hdist) + 258;
                self.litlen_tbl.data = self.data[0..codelen_count];
                self.litlen_tbl.buildLitLen(&self.bit_reader);

                // build literals/lenght table
                self.litlen_tbl = .{
                    .data = self.data[0 .. @as(u16, self.header.hlit) + 257],
                };
                self.litlen_tbl.buildHuffTable();

                // build distances tables
                self.dist_tbl = HTable{
                    .data = self.data[@as(u16, self.header.hlit) + 257 ..][0 .. @as(u16, self.header.hdist) + 1],
                };
                self.dist_tbl.buildHuffTable();
                try inflate(&self.bit_reader, &self.ring_writer, &self.litlen_tbl, &self.dist_tbl);
                if (self.header.bfinal == 1)
                    break;
            }
            // write out the leftovers in the buffer
            // TODO: this is a hack, formalize this
            if (self.ring_writer.cur > 1 << 15) {
                _ = try self.ring_writer.writer.writer().write(self.ring_writer.buf[1 << 15 .. self.ring_writer.cur]);
            }
        }
    };
}

fn decompressor(reader: anytype, writer: anytype) Decompressor(BitReader(@TypeOf(reader)), RotateWriter(@TypeOf(writer))) {
    return .{
        .bit_reader = bitReader(reader),
        .ring_writer = rotateWriter(writer),
        .header = undefined,
        .litlen_tbl = undefined,
        .dist_tbl = undefined,
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
