// GOAL: read a gzip file (disregard the gzip headers) and output the header for
// the first deflate block
const std = @import("std");
const io = std.io;
const assert = std.debug.assert;
const FixedBufferAllocator = std.heap.FixedBufferAllocator;
const stdout = std.io.getStdOut().writer();
const bitReader = @import("bitreader_naive.zig").bitReader;

const MAXLITCODES = 286; // (257 - 286)
const MAXDISTCODES = 30; // (1-32)
const MAXCODES = MAXLITCODES + MAXDISTCODES;
const MAXCODELEN = 16; // maximum number of bits in a huffman code;

const CodeLengthLookup = [_]u5{ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };

const BlockState = struct {
    bfinal: u1,
    btype: u2,
    hlit: u5,
    hdist: u5,
    hclen: u4,

    fn populate(self: *BlockState, r: anytype) void {
        std.debug.print("\nbitstream: {b:0>17}\n", .{r.peek_lsb(17)});
        self.bfinal = @truncate(r.getbits(1));
        self.btype = @truncate(r.getbits(2));
        self.hlit = @truncate(r.getbits(5));
        self.hdist = @truncate(r.getbits(5));
        self.hclen = @truncate(r.getbits(4));
    }

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
    var state: BlockState = undefined;
    BlockState.populate(&state, &br);
    state.show(); // show the header values

    // decode code lengths
    var hclen = [_]u16{0} ** 19; // 19 different symbols (0-18) for hclen
    br.refill();
    for (0..@as(usize, state.hclen) + 4) |i| {
        hclen[CodeLengthLookup[i]] = @truncate(br.getbits(3));
    }
    var lenfreq = [_]u16{0} ** CodeLengthLookup.len;
    var lensym = [_]u16{0} ** CodeLengthLookup.len;
    var len_tbl: HuffTable = .{
        .freq = lenfreq[0..],
        .symbol = lensym[0..],
    };
    buildHuffTable(&len_tbl, hclen[0..], hclen.len);
}
