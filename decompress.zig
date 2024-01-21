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
// const bitReader = @import("bitreader_naive.zig").bitReader;

const MAXLITCODES = 286; // (257 - 286)
const MAXDISTCODES = 30; // (1-32)
const MAXCODES = MAXLITCODES + MAXDISTCODES;
const MAXCODELEN = 16; // maximum number of bits in a huffman code;

// hard coded tables in the rfc
// rfc 3.2.7 under (HCLEN + 4) in block format
const CodeLengthLookup = [_]u5{ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };

const Variant4 = struct {
    bitptr: usize = 0, // ptr to the stream
    bitbuf: u64 = 0, // bit buffer
    bitcount: u6 = 0, // number of bits in bitbuf
    buf: []u8,

    const Self = @This();

    pub fn refill(self: *Self) void {
        // grab the next word and insert them right above the current top
        self.bitbuf |= self.read64() << self.bitcount;

        // advance the ptr for next iteration
        self.bitptr += (63 - self.bitcount) >> 3;

        // update the bitcount
        self.bitcount |= 56; // 0b111000; bitcount is in [56,63)
    }

    fn read64(self: *Self) u64 {
        if (self.bitptr + 8 <= self.buf.len) {
            return @bitCast(self.buf[self.bitptr..][0..8].*);
        } else {
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

    fn peek_msb(self: *Self, n: u6) u64 {
        var code: u64 = 0;
        var buf = self.bitbuf;
        for (0..n) |_| {
            code <<= 1;
            code |= buf & 1;
            buf >>= 1;
        }
        return code;
    }

    fn peek_lsb(self: *Self, count: u6) u64 {
        assert(count >= 0 and count <= 56);
        assert(count <= self.bitcount);

        const mask: u64 = (@as(u64, 1) << count) - 1;

        return self.bitbuf & mask;
    }

    fn consume(self: *Self, count: u6) void {
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

/// a data structure for packing in a code length and a symbol index
const Symlen = packed struct {
    symbol: u9,
    length: u4,
};

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
    data: []Symlen,
    minlen: u4 = MAXCODELEN - 1, // the minimum code word length
    maxlen: u4 = 0, // the maximum code word length (1-15)
    // the left-justified (lj) base table is basically all the code words for each symbol
    // but left-shifted so that they all have a bit-length of the maximum number of
    // code words.
    lj_base: []u16,
    prefix_size: u4, // TODO: make this compile time known
    prefix_start: []u16,
    offset: []u16,

    const Self = @This();

    // fn canonical_decode(self: *Self, reader: anytype) !u64 {
    //     var length = self.minlen;
    //     var code = reader.getcode(length);
    //     while (code >= self.base[length] + self.freq[length]) : (length += 1) {
    //         code <<= 1;
    //         code |= reader.getbits(1);
    //     }
    //     const symbol_id = self.offset[length] + (code - self.base[length]);
    //     return self.data[symbol_id].symbol;
    // }

    fn one_shift_decode(self: *Self, reader: anytype) !u64 {
        const width: u6 = @truncate(self.maxlen);
        const window = reader.peek_msb(width);
        var length = self.minlen;
        while (window >= self.lj_base[length + 1]) {
            length += 1;
        }
        reader.consume(length);
        const rshift = width - length;
        const symbol_id = self.offset[length] + ((window - self.lj_base[length]) >> rshift);
        return self.data[symbol_id].symbol;
    }

    /// One additional advantage is that I don't have to store the minimum code length.
    /// That is already being precalculated in the prefix_start table.
    fn table_lookup_decode(self: *Self, reader: anytype) !u16 {
        const width: u4 = self.maxlen;
        const prefix_size = self.prefix_size;
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

/// NOTES:
/// A 'symbol' is the byte that is being encoded in a huffman code
/// A 'code' is a variable number of bits that decodes to a symbol
/// A 'code length' aka 'hclen' (huffman code length) is the number of bits that makes up the code
/// The deflate format specifies that the run-length encoded symbols 0-18 are encoded by
/// the number of code lengths
/// from an array of ranges, build a huffman tree
/// TODO: create an accelerated table with sym;len that is based on a prefix (max 8) that
/// will automatically get the symbol and code length. If code length > the max that the
/// prefix will show, then try out the slower method; Do this after finishing the encoder.
fn buildHuffTable(h: *HuffTable) void {
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
        const w = h.prefix_size; // width of prefix
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
    @memcpy(offset_copy[0..], h.offset);
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
fn buildLitLen(br: anytype, decodeTable: *HuffTable) void {
    var i: usize = 0;
    var lens = decodeTable.data;
    while (i < lens.len) {
        const code: u16 = try decodeTable.table_lookup_decode(br);
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

fn inflate(reader: anytype, list: *std.ArrayList(u8), pos: *usize, literals: *HuffTable, distances: *HuffTable) !void {
    // base lengths/distances and how many extra bits to consume and how
    // rfc 3.2.5
    const lenconsume = [_]u3{ 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0 };
    const lenbase = [_]u16{ 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258 };
    const distconsume = [_]u4{ 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13 };
    const distbase = [_]u16{ 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577 };

    var sym: u16 = undefined;
    while (true) {
        // for (0..30) |_| {
        sym = try literals.table_lookup_decode(reader);
        if (sym < 256) {
            try list.append(@as(u8, @truncate(sym)));
            pos.* += 1;
        } else if (sym == 256) {
            break;
        } else {
            const lenid: usize = sym - 257;
            const len_extra: u9 = @truncate(reader.getbits(lenconsume[lenid]));
            const len = lenbase[lenid] + len_extra;

            const distcode = try distances.table_lookup_decode(reader);
            const dist_extra: usize = reader.getbits(distconsume[distcode]);
            const distance = distbase[distcode] + dist_extra;

            for (0..len) |_| {
                try list.append(list.items[pos.* - distance]);
                pos.* += 1;
            }
        }
    }
}
pub fn main() !void {
    var sfa = std.heap.stackFallback(1024, std.heap.page_allocator);
    const argsallocator = sfa.get();
    const args = try std.process.argsAlloc(argsallocator);
    defer std.process.argsFree(argsallocator, args);
    const filename = args[1];
    const fd = try std.fs.cwd().openFile(filename, .{});
    defer fd.close();

    const file_size = try fd.getEndPos();
    var buf_reader = std.io.bufferedReader(fd.reader());
    const stream = buf_reader.reader();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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
    const input = try allocator.alloc(u8, file_size);
    // var input = std.ArrayList(u8).init(allocator);
    defer allocator.free(input);

    var list = std.ArrayList(u8).init(allocator);
    defer list.deinit();

    var buf_stdout = std.io.bufferedWriter(stdout);
    const bstdout = buf_stdout.writer();
    _ = try stream.readAll(input);

    var br = Variant4{ .buf = input };
    br.refill();
    var pos: usize = 0;
    while (true) {
        const state: BlockState = BlockState.init(&br);
        var data = [_]Symlen{Symlen{ .symbol = 0, .length = 0 }} ** MAXCODES;
        br.refill();
        for (0..@as(usize, state.hclen) + 4) |i| {
            data[CodeLengthLookup[i]].length = @truncate(br.getbits(3));
        }
        var lenljbase = [_]u16{0} ** MAXCODELEN;
        var lenprefixstart = [_]u16{0} ** 8;
        var lenoffset = [_]u16{0} ** MAXCODELEN;
        var len_tbl: HuffTable = .{
            .data = data[0..CodeLengthLookup.len],
            .lj_base = lenljbase[0..],
            .prefix_size = 3,
            .prefix_start = lenprefixstart[0..],
            .offset = lenoffset[0..],
        };
        buildHuffTable(&len_tbl);
        // resize data slice to hold literal/length codes
        const codelen_count = @as(u9, state.hlit) + @as(u9, state.hdist) + 258;
        len_tbl.data = data[0..codelen_count];
        buildLitLen(&br, &len_tbl);

        // build literals tables
        const literals_prefix_size = 3;
        var litljbase = [_]u16{0} ** MAXCODELEN;
        var litprefixstart = [_]u16{0} ** (1 << literals_prefix_size);
        var litoffset = [_]u16{0} ** MAXCODELEN;
        var literals: HuffTable = .{
            .data = data[0 .. @as(u16, state.hlit) + 257],
            .lj_base = litljbase[0..],
            .prefix_size = literals_prefix_size,
            .prefix_start = litprefixstart[0..],
            .offset = litoffset[0..],
        };
        buildHuffTable(&literals);
        // build distances tables
        var distljbase = [_]u16{0} ** MAXCODELEN;
        var distprefixstart = [_]u16{0} ** (1 << 3); // 2^3 prefix
        var distoffset = [_]u16{0} ** MAXCODELEN;
        var distances: HuffTable = .{
            .data = data[@as(u16, state.hlit) + 257 ..][0 .. @as(u16, state.hdist) + 1],
            .lj_base = distljbase[0..],
            .prefix_size = 3,
            .prefix_start = distprefixstart[0..],
            .offset = distoffset[0..],
        };
        buildHuffTable(&distances);
        try inflate(&br, &list, &pos, &literals, &distances);
        if (state.bfinal == 1)
            break;
    }
    try bstdout.print("{s}", .{list.items});
}
