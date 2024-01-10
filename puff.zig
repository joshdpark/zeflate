// GOAL: read a gzip file (disregard the gzip headers) and output the header for
// the first deflate block
const std = @import("std");
const io = std.io;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const FixedBufferAllocator = std.heap.FixedBufferAllocator;
const stdout = std.io.getStdOut().writer();
const MAXLITCODES = 286; // (257 - 286)
const MAXDISTCODES = 30; // (1-32)
const MAXCODES = MAXLITCODES + MAXDISTCODES;
const MAXCODELEN = 16; // maximum number of bits in a huffman code;

fn BitReader(comptime ReaderType: type) type {
    return struct {
        buf: u64 = 0,
        bitcount: u6 = 0,
        reader: ReaderType,

        const Self = @This();

        inline fn refill(self: *Self) void {
            while (self.bitcount < 56) {
                const byte: u64 = self.reader.readByte() catch {
                    //TODO: handle end of stream error
                    return;
                };
                self.buf |= byte << self.bitcount;
                self.bitcount += 8;
            }
        }

        // get n lowest bits in the bitbuf in lsb order.
        inline fn peek_lsb(self: *Self, n: u6) u64 {
            const mask = (@as(u64, 1) << n) - 1;
            return self.buf & mask;
        }

        // get n lowest bits in the bitbuf in msb order.
        inline fn peek_msb(self: *Self, n: u6) u64 {
            var code = 0;
            for (0..n) |_| {
                code <<= 1;
                code |= self.buf & 1;
                self.buf >>= 1;
            }
            return code;
        }

        inline fn consume(self: *Self, n: u6) void {
            assert(n <= self.bitcount);
            self.buf >>= n;
            self.bitcount -= n;
        }

        fn getbits(self: *Self, n: u6) u64 {
            if (n == 0)
                return 0;
            if (self.bitcount < n)
                self.refill();
            const value: u64 = self.peek_lsb(n);
            self.consume(n);
            return value;
        }

        fn getcode(self: *Self, n: u6) u64 {
            if (self.bitcount < n)
                self.refill();
            var code: u64 = 0;
            for (0..n) |_| {
                code <<= 1;
                code |= self.buf & 1;
                self.buf >>= 1;
            }
            self.bitcount -= n;
            return code;
        }
    };
}

fn bitReader(reader: anytype) BitReader(@TypeOf(reader)) {
    return .{ .reader = reader };
}

const CodeLengthLookup = [_]u5{ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };

const BlockHeader = struct {
    bfinal: u1,
    btype: u2,
    hlit: u5,
    hdist: u5,
    hclen: u4,

    fn show(self: *@This()) void {
        std.debug.print("{any}\n", .{self});
        std.debug.print(
            \\There are:
            \\{d} (hlit) literal codes (257-286 literals)
            \\{d} (hdist) distance codes (1-32) - 1
            \\{d} (hclen) code lengths (4-19) - 4
            \\
        , .{ @as(usize, self.hlit) + 257, self.hdist + 1, @as(u5, self.hclen) + 4 });
    }
};
const HuffTable = struct {
    freq: []u16, // frequency table of code lengths
    symbol: []u16, // up to 286 literal/length codes
};

// There are a number of different tables for doing decoding of canonical prefix codes
// 1. a weight/frequency table for each symbol -> symbol : weight
// 2. a code length table -> symbol : number of bits for code word
// 3. a code word table -> symbol : bits
// 4. a base table -> code length : starting number of first codeword
// 5. a offset table -> code length : offset into the code word table
// A decode table based on Moffat/Turpin
fn DecodeTable(ReaderType: type) type {
    return struct {
        bit_reader: ReaderType,
        base: []u16, // the base table can be left justified;
        offset: []u16,
        symbol: []u16,

        fn canonical_decode(self: *@This()) u16 {
            var code = self.bit_reader.getbits(1);
            var length = 1;
            while (code < self.base[length]) : (length += 1) {
                code <<= 1;
                code |= self.bit_reader.getbits(1);
            }
            return self.symbol[self.offset[length] + (code - self.base[length])];
        }

        fn fast_decode_one_shift(self: @This()) u16 {
            // V is a w-bit window (w >= length of longest codeword) into the bitstream
            const w = 8; // setting w manually;
            var v = self.bit_reader.peek_msb(w);
            var length = 1;
            while (self.base[length] > v)
                length += 1;
            const id = self.offset[length] + ((v - self.base[length]) << w - length);
            // TODO begin: does v need to be in the struct?
            v <<= length;
            self.bit_reader.consume(w);
            v |= self.bit_reader.peek_msb(length);
            // TODO end;
            return self.symbol[id];
        }
    };
}

fn decodeTable(reader: type) DecodeTable(@TypeOf(reader)) {
    return .{ .bit_reader = bitReader(reader) };
}

/// NOTES:
/// A 'symbol' is the byte that is being encoded in a huffman code
/// A 'code' is a variable number of bits that decodes to a symbol
/// A 'code length' aka 'hclen' (huffman code length) is the number of bits that makes up the code
/// The deflate format specifies that the run-length encoded symbols 0-18 are encoded by
/// the number of code lengths
/// from an array of ranges, build a huffman tree
fn buildHuffTable(h: *HuffTable, code_lengths: []const u16, comptime hclen: u9) void {
    std.debug.print(
        \\code_lengths
        \\This is the most important table. This is the table in which all of
        \\the other tables are derived.  This function essentially takes this
        \\table and populates an array of canonically ordered symbols
        \\index: symbol
        \\value: code length
        \\{d}
        \\
        \\
    , .{code_lengths});

    // preallocate a bunch of tables that can hold the code frequency table, the code
    // range table (for each code length, what integer does it start at?), and the code
    // table (the actual variable bits aka the codes for each symbol in input)

    // preallocate a bitlen freq table (i: codelen, array[i]: count of symbols with codelen)
    // var bl_counts = [_]u8{0} ** (MAXCODELEN + 1); // 8 code lengths (0b0-0b111)
    var next_code = [_]u16{0} ** (MAXCODELEN + 1);
    var codes = [_]u16{0} ** hclen; // 19 symbols
    var max: u16 = 0; // the max code_length in the set of codes
    for (code_lengths) |x| {
        h.freq[x] += 1;
        max = @max(x, max);
    }
    std.debug.print(
        \\huffman table counts (h.freq)
        \\The frequency table of each code length
        \\index: code length
        \\value: frequency
        \\{d}
        \\
        \\
    , .{h.freq});
    var code: u16 = 0;
    h.freq[0] = 0;
    for (1..max + 1) |i| {
        code = (code + h.freq[i - 1]) << 1;
        next_code[i] = code;
    }
    std.debug.print(
        \\next_code
        \\index: code length
        \\value: the starting value (in binary) of code
        \\Each subsequent value in the same code length is
        \\{d}
        \\
        \\
    , .{next_code});
    for (code_lengths, 0..) |cl, i| {
        if (next_code[cl] != 0) {
            codes[i] = next_code[cl];
            next_code[cl] += 1;
        }
    }
    std.debug.print("codes:\n{b}\n", .{codes});
    // generate offsets into symbol table for each length
    // index: code length; value: offset for symbol table
    var offs = [_]u16{0} ** (MAXCODELEN + 1);
    offs[1] = 0;
    for (1..max + 1) |len| {
        offs[len + 1] = offs[len] + h.freq[len];
    }
    std.debug.print(
        \\offsets
        \\index: code length
        \\value: offset into the symbol
        \\{d}
        \\
        \\
    , .{offs});
    // create a symbols table
    // each symbol is sorted in by their code length. You can retrieve their
    // code lengths by indexing in the code_lengths table
    // var symbols = [_]u4{0} ** (MAXCODELEN + 1); // 16 symbols (0-15)
    for (code_lengths, 0..) |len, symbol| {
        if (len != 0) {
            h.symbol[offs[len]] = @truncate(symbol);
            offs[len] += 1;
        }
    }
    std.debug.print(
        \\symbols (h.symbol)
        \\The symbols in their canonical order. This means that the table will
        \\be ordered first by the code length of the symbol, and then by the
        \\code length values
        \\{d}
        \\
    , .{h.symbol});
}

fn decode(br: anytype, h: *HuffTable) !u16 {
    var len: usize = 1; // length of bits in code
    var code: u16 = 0;
    var first: u16 = 0;
    var freq: u16 = undefined;
    var index: u16 = 0;
    // TODO; use a limit table (previously calculated as next_code)
    while (len <= MAXCODELEN) : (len += 1) {
        code |= @truncate(br.getbits(1));
        freq = h.freq[len];
        if (@as(i32, @intCast(code)) - @as(i32, @intCast(freq)) < @as(i32, @intCast(first)))
            return h.symbol[index + (code - first)];
        index += freq;
        first = (first + freq) << 1;
        code <<= 1;
    }
    return error.OutOfCodes;
}

fn decodeDynamic(br: anytype, bh: *BlockHeader, len_tbl: *HuffTable, literals: *HuffTable, distances: *HuffTable) !void {
    const rle_length: u9 = @as(u9, bh.hlit) + 258 + @as(u9, bh.hdist);
    var codelen_table: [MAXCODES]u16 = undefined;
    var prev: u16 = undefined;
    var i: usize = 0;
    var minlen: u8 = 15;
    while (i < rle_length) {
        const code = try decode(br, len_tbl);
        switch (code) {
            0...15 => {
                codelen_table[i] = code;
                // std.debug.print("{d}, ", .{code});
                prev = code;
                minlen = if (code == 0) minlen else @min(code, minlen);
                i += 1;
            },
            // NOTE: fun thing, the huffman codes are read in MSB but when
            // reading the additional bits for codes 16-18, those are read LSB;
            16 => {
                const repeat = br.getbits(2) + 3;
                for (0..repeat) |_| {
                    // std.debug.print("{d}, ", .{prev});
                    codelen_table[i] = prev;
                    i += 1;
                }
            },
            17 => {
                const repeat = br.getbits(3) + 3;
                // std.debug.print("code: 17, repeat 0 {d} times\n", .{repeat});
                for (0..repeat) |_| {
                    codelen_table[i] = 0;
                    i += 1;
                }
            },
            18 => {
                const repeat = br.getbits(7) + 11;
                // std.debug.print("code 18, repeat 0 {d} times\n", .{repeat});
                for (0..repeat) |_| {
                    codelen_table[i] = 0;
                    i += 1;
                }
            },
            else => {},
        }
    }
    const litcount: u16 = 257 + @as(u16, bh.hlit);
    const distcount: u16 = bh.hdist + 1;
    const lit_table = codelen_table[0..litcount];
    const dist_table = codelen_table[litcount..][0..distcount];
    std.debug.print("literal codes: {d}\n", .{lit_table});
    std.debug.print("distance codes: {d}\n", .{dist_table});
    std.debug.print("minlen: {d}\n", .{minlen});

    // build a freq/symbol table for literals
    buildHuffTable(literals, lit_table[0..], MAXLITCODES);
    // build a freq/symbol table for distances
    buildHuffTable(distances, dist_table[0..], MAXDISTCODES);
}

fn inflate(reader: anytype, literals: *HuffTable, distances: *HuffTable) !void {
    // base lengths/distances and how many extra bits to consume and how
    const lenconsume = [_]u3{ 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0 };
    const lenbase = [_]u16{ 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258 };
    const distconsume = [_]u4{ 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13 };
    const distbase = [_]u16{ 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577 };

    var buffer: [1 << 15]u8 = undefined;
    var fbs = io.fixedBufferStream(buffer[0..]);
    var writer = fbs.writer();
    // const seeker = fbs.seeker();

    var sym: u16 = undefined;
    while (true) {
        // for (0..30) |_| {
        sym = try decode(reader, literals);
        if (sym < 256) {
            try writer.writeByte(@as(u8, @truncate(sym)));
            std.debug.print("{c}", .{@as(u8, @truncate(sym))});
        } else if (sym == 256) {
            break;
        } else {
            const lenid: usize = sym - 257;
            const len_extra: u9 = @truncate(reader.getbits(lenconsume[lenid]));
            const len = lenbase[lenid] + len_extra;

            const distcode = try decode(reader, distances);
            const dist_extra: usize = reader.getbits(distconsume[distcode]);
            const distance = distbase[distcode] + dist_extra;

            // seeker.seekBy(distance);
            for (0..len) |_| {
                try writer.writeByte(fbs.buffer[fbs.pos - distance]);
            }
            std.debug.print("<{d}, {d}>\n", .{ len, distance });
        }
    }
    try stdout.print("{s}", .{fbs.buffer});
}

// TODO: this isn't very elegant but it works
fn readHeader(hd: *BlockHeader, r: anytype) void {
    std.debug.print("\nbitstream: {b:0>17}\n", .{r.peek_lsb(17)});
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
    // try stdout.print("gzip header bytes: \n", .{});
    for (0..10) |_| {
        std.debug.print("{x} ", .{try stream.readByte()});
    }
    {
        std.debug.print("\nfilename: ", .{});
        while (stream.readByte()) |byte| {
            if (byte == 0)
                break;
            std.debug.print("{c}", .{byte});
        } else |_| {}
    }
    // set up bitreader
    var br = bitReader(stream);
    br.refill();
    var block_header: BlockHeader = undefined;
    readHeader(&block_header, &br);
    block_header.show(); // show the header values
    // decode lengths and literals
    var hclen = [_]u16{0} ** 19; // 19 different symbols (0-18) for hclen
    br.refill();
    for (0..@as(usize, block_header.hclen) + 4) |i| {
        hclen[CodeLengthLookup[i]] = @truncate(br.getbits(3));
    }
    var lenfreq = [_]u16{0} ** CodeLengthLookup.len;
    var lensym = [_]u16{0} ** CodeLengthLookup.len;
    var len_tbl: HuffTable = .{
        .freq = lenfreq[0..],
        .symbol = lensym[0..],
    };
    buildHuffTable(&len_tbl, hclen[0..], hclen.len);

    // decode codelength codes to literal/length code tables
    var litfreq = [_]u16{0} ** MAXCODELEN;
    var litsym = [_]u16{0} ** MAXLITCODES;
    var lit: HuffTable = .{
        .freq = litfreq[0..],
        .symbol = litsym[0..],
    };
    // build a freq/symbol table for distances
    var distfreq = [_]u16{0} ** MAXCODELEN;
    var distsym = [_]u16{0} ** MAXDISTCODES;
    var dist: HuffTable = .{
        .freq = distfreq[0..],
        .symbol = distsym[0..],
    };
    try decodeDynamic(&br, &block_header, &len_tbl, &lit, &dist);
    try inflate(&br, &lit, &dist);
}
