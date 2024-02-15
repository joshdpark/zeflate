const std = @import("std");
const assert = std.debug.assert;

const max_possible_codewords = 255 + 1 + 29 + 30; // 255 literals, 1 end of block, 29 lengths and 30 distances
const Entry = packed struct {
    kind: Kind = .end,
    length: u4 = 0,
    symbol: u8 = 0,
    codeword: u16 = 0,
    link: u16 = 0,

    const Kind = enum(u4) {
        literal,
        match,
        end,
        link,
    };

    fn show(s: *const @This()) void {
        std.debug.print("kind: {s: >7}, length: {d: >3}, symbol: {d: >3}, codeword: {d}, link: {d}\n", .{ @tagName(s.kind), s.length, s.symbol, s.codeword, s.link });
    }
};

fn HuffmanTable(
    comptime codes: usize,
    comptime lookup_bits: usize,
) type {
    return struct {
        entries: [codes]Entry = [_]Entry{.{}} ** codes,
        lookup: [lookup_length]Entry = [_]Entry{.{}} ** lookup_length,

        const Self = @This();
        const lookup_length = 1 << lookup_bits;
        const mask = (1 << lookup_bits) - 1;
        const max_possible_codelen = 15;

        /// build the sorted symbols table and the fast lookup table
        fn build(self: *Self, lengths: []const u4) void {

            // symbols can be byte values representing literals 0-255
            // end of block or a length for literals/length huffman trees or distances
            var freq: [max_possible_codelen + 1]u16 = [_]u16{0} ** (max_possible_codelen + 1); // count of each codeword length
            var offsets: [max_possible_codelen + 1]u16 = [_]u16{0} ** (max_possible_codelen + 1);
            const entries = self.entries[0..lengths.len];

            for (lengths) |len| {
                assert(len <= max_possible_codelen);
                freq[len] += 1;
            }
            var max: u4 = max_possible_codelen; // max codeword length;
            while (freq[max] == 0) max -= 1; // max codeword length
            offsets[0] = 0;
            offsets[1] = freq[0];
            for (1..max) |len|
                offsets[len + 1] = offsets[len] + freq[len];

            for (lengths, 0..) |len, symbol| {
                entries[offsets[lengths[symbol]]] = switch (symbol) {
                    0...255 => Entry{ .kind = .literal, .length = len, .symbol = @truncate(symbol) },
                    256 => Entry{ .kind = .end, .length = len, .symbol = 0 },
                    257...286 => Entry{ .kind = .match, .length = len, .symbol = @truncate(symbol - 257) },
                    else => unreachable,
                };
                offsets[len] += 1;
            }

            // create a codeword entry based on the sorted symbol for the main table
            var len: u4 = 1;
            while (freq[len] == 0) len += 1; // min codeworld length

            const nonzero: u16 = offsets[0]; // skip 0 length codes
            var codeword: u16 = 0;
            var count: u16 = freq[entries[nonzero].length];

            // populate the lookup table and links in symbol table
            for (entries[nonzero..], nonzero..) |*entry, i| {
                if (count == 0) {
                    codeword <<= 1;
                    count = freq[entry.length];
                }
                if (entry.length == 0) continue;
                assert(count != 0);
                assert(entry.length != 0);
                count -= 1;

                // reverse the codeword and add to lookup table
                var reversed: u16 = reverseCodeword(codeword, entry.length);
                entry.codeword = reversed;
                if (entry.length <= lookup_bits) {
                    while (true) {
                        self.lookup[reversed] = entry.*;
                        reversed += @as(u16, 1) << entry.length; // increment by stride
                        if (reversed >= lookup_length) break;
                    }
                } else {
                    const new: *Entry = &self.lookup[reversed & mask];
                    entry.link = new.link;
                    new.link = @as(u16, @intCast(i)); // usize->u16 index to symbol table
                }
                // advance the codeword
                codeword += 1;
            }
        }

        fn decode(self: *Self, codeword: u16) Entry {
            var sym = &self.lookup[codeword & mask];
            if (sym.length != 0) return sym.*;
            var ptr = sym.link; // index to symbol table
            while (true) {
                sym = &self.entries[ptr];
                assert(sym.length != 0);
                const code_mask = (@as(u16, 1) << sym.length) - 1;
                if ((sym.codeword ^ codeword) & code_mask == 0) return sym.*;
                ptr = sym.link;
            }
        }
    };
}

const PrecodeTable = HuffmanTable(19, 7);
const LiteralsTable = HuffmanTable(286, 8);

inline fn reverseCodeword(codeword: u16, len: u4) u16 {
    assert(len > 0);
    return @bitReverse(codeword) >> 16 - 1 - len + 1; // keep compiler happy
}

test PrecodeTable {
    var P: PrecodeTable = .{};
    const lengths = [_]u4{ 4, 3, 0, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 3, 2 };
    P.build(lengths[0..]);
    for (P.entries[12..]) |input| {
        const input_symbol = input.symbol;
        const output = P.decode(input.codeword);
        const output_symbol = output.symbol;
        try std.testing.expectEqual(input_symbol, output_symbol);
    }
}

test LiteralsTable {
    const lengths = [_]u4{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 9, 8, 10, 0, 9, 8, 9, 7, 8, 8, 8, 7, 7, 7, 9, 7, 6, 7, 7, 8, 7, 7, 8, 7, 8, 9, 7, 8, 7, 8, 10, 0, 9, 10, 9, 9, 8, 9, 0, 10, 9, 0, 0, 10, 9, 9, 9, 11, 0, 9, 9, 9, 9, 0, 0, 10, 0, 11, 9, 11, 9, 0, 7, 0, 6, 7, 6, 6, 5, 7, 8, 7, 6, 8, 9, 6, 7, 6, 6, 7, 0, 6, 6, 6, 7, 8, 8, 9, 8, 10, 10, 10, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 4, 4, 4, 5, 5, 6, 5, 5, 5, 6, 6, 7, 7, 7, 8, 8, 8, 10, 10, 0, 10, 8, 9, 9, 9, 9, 8, 7, 7, 5, 6, 4, 4, 4, 5, 4, 5, 4, 5, 4, 4, 4, 4, 4, 4, 4, 5, 5, 6 };
    var Lit: LiteralsTable = .{};
    Lit.build(lengths[0 .. 257 + 23]);
    std.debug.print("symbols:\n", .{});
    for (Lit.entries[0..]) |sym| {
        if (sym.length == 0) continue;
        const input_code = sym.codeword;
        const output_code = Lit.decode(input_code).codeword;
        try std.testing.expectEqual(input_code, output_code);
    }
}
