/// GOAL: read a gzip file (disregard the gzip headers) and output the header for
/// the first deflate block
///
/// Definitions:
/// code word: the bits that make up a huffman code
/// code length: the number of bits that make up a code word
/// symbol: the encoded byte that a huffman code word maps to
const std = @import("std");
const io = std.io;
const assert = std.debug.assert;
const stdout = std.io.getStdOut().writer();
const bitReader = @import("bitreader_naive.zig").bitReader;

const MAXLITCODES = 286; // (257 - 286)
const MAXDISTCODES = 30; // (1-32)
const MAXCODES = MAXLITCODES + MAXDISTCODES;
const MAXCODELEN = 15; // maximum number of bits in a huffman code;

// hard coded tables in the rfc
// rfc 3.2.7 under (HCLEN + 4) in block format
const CodeLengthLookup = [_]u5{ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };
// rfc 3.2.5
// const lenconsume = [_]u3{ 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0 };
// const lenbase = [_]u16{ 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258 };
// const distconsume = [_]u4{ 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13 };
// const distbase = [_]u16{ 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577 };

const BlockState = struct {
    bfinal: u1,
    btype: u2,
    hlit: u5,
    hdist: u5,
    hclen: u4,

    fn init(r: anytype) BlockState {
        return .{
            .bfinal = @truncate(r.getbits(1)),
            .btype = @truncate(r.getbits(2)),
            .hlit = @truncate(r.getbits(5)),
            .hdist = @truncate(r.getbits(5)),
            .hclen = @truncate(r.getbits(4)),
        };
    }

    fn show(self: *BlockState) void {
        std.debug.print(
            \\{d} (hlit) literal codes (257-286 literals)
            \\{d} (hdist) distance codes (1-32)
            \\{d} (hclen) code lengths (4-19)
            \\
        , .{ @as(usize, self.hlit) + 257, self.hdist + 1, @as(u5, self.hclen) + 4 });
    }
};

///TODO: this is a lot of book-keeping right now. Most of this state doesn't need to be
///stored in the struct. A lot of it can be calculated on the fly, which can reduce the
///amount of registers that are needed.
const HuffTable = struct {
    freq: []u16, // frequency table of code lengths
    minlen: u4, // the minimum code word length
    maxlen: u4, // the maximum code word length (1-15)
    // deflate specification canonical codes are smallest->largest in order of lengths and
    // then in order of symbols
    base: []u16,
    // the left-justified (lj) base table is basically all the code words for each symbol
    // but left-shifted so that they all have a bit-length of the maximum number of
    // code words.
    lj_base: []u16,
    prefix_size: u4, // TODO: make this compile time known
    prefix_start: []u16,
    offset: []u16,
    symbol: []u16,

    const Self = @This();

    fn canonical_decode(self: *Self, reader: anytype) !u64 {
        var length = self.minlen;
        var code = reader.getcode(length);
        while (code >= self.base[length] + self.freq[length]) : (length += 1) {
            code <<= 1;
            code |= reader.getbits(1);
        }
        std.debug.print("code length: {d}, code: {d} ", .{ length, code });
        const symbol_id = self.offset[length] + (code - self.base[length]);
        std.debug.print("symbol_id: {d} ", .{symbol_id});
        std.debug.print("symbol: {d}\n", .{self.symbol[symbol_id]});
        // write the symbol to the writer
        return self.symbol[symbol_id];
    }

    fn one_shift_decode(self: *Self, reader: anytype) !u64 {
        const max: u6 = @truncate(self.maxlen);
        const window = reader.peek_msb(max);
        var length = self.minlen;
        while (window >= self.lj_base[length + 1]) {
            length += 1;
        }
        reader.consume(length);
        const code = (window >> (@as(u3, @truncate(max)) - @as(u3, @truncate(length))));
        const symbol_id = self.offset[length] + (code - self.base[length]);
        return self.symbol[symbol_id];
    }

    /// One additional advantage is that I don't have to store the minimum code length.
    /// That is already being precalculated in the prefix_start table.
    fn table_lookup_decode(self: *Self, reader: anytype) !u16 {
        const max: u4 = self.maxlen;
        const prefix_size = self.prefix_size;
        const window = reader.peek_msb(max);
        // std.debug.print("window: {b:0>16} ", .{window});
        const prefix = window >> (max - prefix_size);
        // std.debug.print("prefix: {b:0>16} ", .{prefix});
        var length: u6 = @truncate(self.prefix_start[prefix]);
        if (length > self.prefix_size) {
            while (window >= self.lj_base[length + 1])
                length += 1;
        }
        // std.debug.print("length: {d} ", .{length});
        reader.consume(length);
        const code = (window >> (@as(u3, @truncate(max)) - @as(u3, @truncate(length))));
        // std.debug.print("code: {d} ", .{code});
        const symbol_id = self.offset[length] + (code - self.base[length]);
        // std.debug.print("symbol: {d}\n", .{self.symbol[symbol_id]});
        return self.symbol[symbol_id];
    }
};

/// NOTES:
/// A 'symbol' is the byte that is being encoded in a huffman code
/// A 'code' is a variable number of bits that decodes to a symbol
/// A 'code length' aka 'hclen' (huffman code length) is the number of bits that makes up the code
/// The deflate format specifies that the run-length encoded symbols 0-18 are encoded by
/// the number of code lengths
/// from an array of ranges, build a huffman tree
fn buildHuffTable(h: *HuffTable, code_lengths: []const u4, comptime prefix_size: u4) void {
    std.debug.print("codelengths\n{d}\n", .{code_lengths});

    // TODO: this isn't currently used yet
    // for a max code word length of 15, this can be captured in a u4 (16 states);
    const prefix_start: [1 << prefix_size]u4 = undefined;
    std.debug.print("prefix_start len: {d}\n", .{prefix_start.len});

    // codelength count table
    for (code_lengths) |x| {
        h.freq[x] += 1;
        h.maxlen = @max(x, h.maxlen);
        h.minlen = if (x != 0) @min(x, h.minlen) else h.minlen;
    }
    std.debug.print("codelength counts\n{d}\n", .{h.freq[0..h.maxlen]});

    // base table
    {
        var code: u16 = 0;
        h.freq[0] = 0;
        var i: u4 = h.minlen;
        while (i <= h.maxlen) : (i += 1) {
            code = (code + h.freq[i - 1]) << 1;
            h.base[i] = code;
            h.lj_base[i] = code << (h.maxlen - i);
        }
        // add sentinel value for absolute maximum code value
        h.base[h.maxlen + 1] = code << 1;
        h.lj_base[h.maxlen + 1] = code << 1;
    }
    std.debug.print("base table\n{d}\n", .{h.base});
    std.debug.print("lj_base table\n{d}\n", .{h.lj_base});

    // prefix_start table
    {
        const w = h.prefix_size; // width of prefix
        const rshift: u4 = h.maxlen - w; // amt to shift to get prefix width
        var i = h.maxlen;
        // Look at the prefix for each left-justified code, since we need to record
        // the shortest length that matches that prefix.
        while (i >= h.minlen) : (i -= 1) {
            const base = h.base[i];
            const range = base + h.freq[i];
            for (base..range) |b| {
                const lj = b << h.maxlen - i; // left justify the code
                const prefix = lj >> rshift; // keep the prefix
                h.prefix_start[prefix] = i;
                // std.debug.print("lj_base[{d}]: {b:0>7}; 3-bit prefix: {b:0}\n", .{ i, b, prefix });
            }
        }
        // second pass to fill in the gaps for prefix values that weren't populated
        i = 1;
        while (i < h.prefix_start.len) : (i += 1) {
            if (h.prefix_start[i - 1] != 0 and h.prefix_start[i] == 0)
                h.prefix_start[i] = h.prefix_start[i - 1];
        }
    }
    std.debug.print("prefix_start:\n{d}\n", .{h.prefix_start});

    // offset table
    h.offset[1] = 0;
    for (1..h.maxlen) |len| {
        h.offset[len + 1] = h.offset[len] + h.freq[len];
    }
    std.debug.print("offset table\n{d}\n", .{h.offset});

    // symbol table
    // TODO: should I just reuse the code_lengths slice? At this point, I don't need it
    // anymore and I could then just use that as the symbol table.
    // TODO: using a copy, but this won't work for the larger tables
    var copy: [MAXCODELEN]u16 = undefined;
    @memcpy(copy[0..], h.offset);
    for (code_lengths, 0..) |len, symbol| {
        if (len != 0) {
            h.symbol[copy[len]] = @truncate(symbol);
            copy[len] += 1;
        }
    }
    std.debug.print("symbol table\n{d}\n", .{h.symbol[0..code_lengths.len]});
}

/// build the literals/length code table
fn buildLitLen(br: anytype, decodeTable: *HuffTable, lens: []u4) void {
    var i: usize = 0;
    while (i < lens.len) {
        const code: u16 = try decodeTable.table_lookup_decode(br);
        var rep: usize = 1;
        lens[i] = @truncate(code);
        // try stdout.print("decoded symbol: {d}\n", .{code});
        if (code == 16) {
            rep = br.getbits(2) + 3;
            const prev = lens[i - 1];
            for (lens[i..][0..rep]) |*val| {
                val.* = prev;
            }
            // std.debug.print("repeat previous {d} times\n", .{rep});
        }
        if (code == 17) {
            rep = br.getbits(3) + 3;
            for (lens[i..][0..rep]) |*val| {
                val.* = 0;
            }
            // std.debug.print("repeat 0 {d} times\n", .{rep});
        }
        if (code == 18) {
            rep = br.getbits(7) + 11;
            for (lens[i..][0..rep]) |*val| {
                val.* = 0;
            }
            // std.debug.print("repeat 0 {d} times\n", .{rep});
        }
        i += rep;
    }
    std.debug.print("literal/length code length table:\n{d}\n", .{lens[0..]});
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
    {
        // std.debug.print("gzip header bytes: \n", .{});
        for (0..10) |_| {
            _ = try stream.readByte();
            // std.debug.print("{x} ", .{try stream.readByte()});
        }
    }
    {
        // std.debug.print("\nfilename: ", .{});
        while (stream.readByte()) |byte| {
            if (byte == 0)
                break;
            // std.debug.print("{c}", .{byte});
        } else |_| {}
    }
    // set up bitreader
    var br = bitReader(stream);
    br.refill();
    var state: BlockState = BlockState.init(&br);
    state.show(); // show the header values

    // TODO: you can change this so that all the codelengths can be represented as a
    // packed struct
    // symlen = packed struct {
    //    .sym = u9,
    //    .len = u4,
    // }
    // Now the codelen array can be renamed 'data' and be used for storing both the
    // code length table as well as the symbol table;
    // represents all possible code lengths, both for huffman-codelength and literal/length
    // codelengths
    var codelens = [_]u4{0} ** MAXCODES;
    br.refill();
    for (0..@as(usize, state.hclen) + 4) |i| {
        codelens[CodeLengthLookup[i]] = @truncate(br.getbits(3));
    }
    var lenfreq = [_]u16{0} ** MAXCODELEN;
    var lenbase = [_]u16{0} ** MAXCODELEN;
    var lenljbase = [_]u16{0} ** MAXCODELEN;
    var lenprefixstart = [_]u16{0} ** 8;
    var lenoffset = [_]u16{0} ** MAXCODELEN;
    var lensym = [_]u16{0} ** MAXCODES;
    var len_tbl: HuffTable = .{
        .freq = lenfreq[0..],
        .maxlen = 0,
        .minlen = MAXCODELEN,
        .base = lenbase[0..],
        .lj_base = lenljbase[0..],
        .prefix_size = 3,
        .prefix_start = lenprefixstart[0..],
        .offset = lenoffset[0..],
        .symbol = lensym[0..],
    };
    buildHuffTable(&len_tbl, codelens[0..CodeLengthLookup.len], 3);
    const number_of_codelens: u9 = @as(u9, state.hlit) + @as(u9, state.hdist) + 258;
    buildLitLen(&br, &len_tbl, codelens[0..number_of_codelens]);
}
