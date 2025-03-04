const std = @import("std");
const assert = std.debug.assert;

// 255 literals, 1 end of block, 29 lengths and 30 distances
const max_possible_codewords = 255 + 1 + 29 + 30;
pub const Entry = struct {
    kind: Kind = .end,
    length: u4 = 0,
    symbol: u16 = 0,

    const Kind = enum(u4) {
        literal,
        match,
        end,
        link,
    };

    pub fn show(s: *const @This()) void {
        std.debug.print(
            "kind: {s: >7}, length: {d: >3}, symbol: {d: >3}\n",
            .{ @tagName(s.kind), s.length, s.symbol },
        );
    }
};

pub fn HuffmanTable(
    comptime alphabet: usize,
    comptime enough: usize,
    comptime lookup_bits: u4,
) type {
    return struct {
        entries: [alphabet]Entry = [_]Entry{.{}} ** alphabet,
        codewords: [alphabet]u16 = [_]u16{0} ** alphabet, // store the codewords, useful for debugging
        lookup: [enough]Entry = [_]Entry{.{}} ** enough,

        const Self = @This();
        const lookup_length = 1 << lookup_bits;
        const mask = (1 << lookup_bits) - 1;
        const max_possible_codelen = 15;

        /// build the sorted symbols table and the fast lookup table
        pub fn build(self: *Self, lengths: []const u4) void {

            // symbols can be byte values representing literals 0-255
            // end of block or a length for literals/length huffman trees or distances
            var freq = [_]u16{0} ** (max_possible_codelen + 1); // count of each codeword length
            var offsets = [_]u16{0} ** (max_possible_codelen + 1);
            const entries = self.entries[0..lengths.len];
            const codewords = self.codewords[0..lengths.len];

            // DEFLATE 3.2.2
            // 1. count the number of codes for each codelength
            for (lengths) |len| {
                freq[len] += 1;
            }
            var max: u4 = max_possible_codelen;
            while (freq[max] == 0) max -= 1;

            // 2. find the numerical value of the smallest code for each codelength
            var codespace_used: u16 = 0;
            offsets[0] = 0;
            offsets[1] = freq[0];
            for (1..max) |len| {
                offsets[len + 1] = offsets[len] + freq[len];
                codespace_used = (codespace_used << 1) + freq[len];
            }
            codespace_used = (codespace_used << 1) + freq[max];

            // 3. assign numerical value to all codes
            for (lengths, 0..) |len, symbol| {
                entries[offsets[lengths[symbol]]] = switch (symbol) {
                    0...255 => Entry{ .kind = .literal, .length = len, .symbol = @intCast(symbol) },
                    256 => Entry{ .kind = .end, .length = len, .symbol = 0 },
                    257...286 => Entry{ .kind = .match, .length = len, .symbol = @intCast(symbol - 257) },
                    else => unreachable,
                };
                offsets[len] += 1;
            }

            // for keeping track of codeword
            const nonzero: u16 = offsets[0]; // skip 0 length codes
            var codeword: u16 = 0;
            var count: u16 = freq[entries[nonzero].length];
            // for keeping track of the subtable
            var prefix: u16 = std.math.maxInt(u16);
            var subtable_start: u16 = 1 << lookup_bits;
            var subtable: []Entry = undefined;

            // populate the lookup table and links in symbol table
            for (entries[nonzero..], codewords[nonzero..]) |*entry, *code| {
                assert(entry.length != 0);
                // advance the codeword
                if (count == 0) count = freq[entry.length];
                defer codeword += @as(u16, 1) << 15 - entry.length;
                defer count -= 1;

                // reverse the codeword and add to lookup table
                var reversed: u16 = @bitReverse(codeword) >> 1;
                code.* = reversed;
                if (entry.length <= lookup_bits) {
                    while (true) {
                        self.lookup[reversed] = entry.*;
                        reversed += @as(u16, 1) << entry.length; // increment by stride
                        if (reversed >= lookup_length) break;
                    }
                } else {
                    // check if we need to partition a new subtable for this prefix
                    // Every subtable is 2^n, where n is at least the number of bits
                    // above the main lookup table. However, n can be larger if the
                    // freq count of codewords at that bit codelength is small enough
                    // that the next codeword at a greater codeword length has the same
                    // codeword prefix
                    if (reversed & mask != prefix) {
                        prefix = reversed & mask;
                        var slots: u16 = count;
                        var space_bits: u4 = entry.length - lookup_bits;
                        while (slots < @as(u16, 1) << space_bits) {
                            assert(space_bits + lookup_bits <= max);
                            space_bits += 1;
                            slots = (slots << 1) + freq[space_bits + lookup_bits];
                        }
                        const space: u16 = @as(u16, 1) << space_bits;
                        subtable = self.lookup[subtable_start..][0..space];
                        // set Entry in main lookup region to hold a link
                        self.lookup[prefix] = Entry{
                            .kind = .link,
                            .length = space_bits, // extra bits to read from stream
                            .symbol = subtable_start, // index to subtable
                        };
                        subtable_start += space;
                    }
                    reversed >>= lookup_bits; // get high bits suffix
                    while (true) {
                        subtable[reversed] = entry.*;
                        reversed += @as(u16, 1) << entry.length - lookup_bits;
                        if (reversed >= subtable.len) break;
                    }
                }
            }
        }

        pub fn decode(self: *Self, codeword: u16) Entry {
            const entry = &self.lookup[codeword & mask];
            if (entry.kind != .link)
                return entry.*;
            const offset = (codeword >> lookup_bits) & (@as(u16, 1) << entry.length) - 1;
            return self.lookup[entry.symbol + offset];
        }
    };
}

// 255 literals, 1 end of block, 29 lengths and 30 distances
pub const PrecodeTable = HuffmanTable(19, 1 << 7, 7);
pub const LiteralsTable = HuffmanTable(286, 852, 9); // see enough in zlib for 852
pub const DistancesTable = HuffmanTable(30, 592, 6); // ^

inline fn reverseCodeword(codeword: u16, len: u4) u16 {
    assert(len > 0);
    return @bitReverse(codeword) >> 16 - 1 - len + 1; // keep compiler happy
}

test PrecodeTable {
    var P: PrecodeTable = .{};
    const lengths = [_]u4{ 4, 3, 0, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 3, 2 };
    P.build(lengths[0..]);
    for (P.entries[12..], P.codewords[12..]) |input, code| {
        const input_symbol = input.symbol;
        const output = P.decode(code);
        const output_symbol = output.symbol;
        try std.testing.expectEqual(input_symbol, output_symbol);
    }
}

test LiteralsTable {
    const lengths = [280]u4{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 9, 8, 10, 0, 9, 8, 9, 7, 8, 8, 8, 7, 7, 7, 9, 7, 6, 7, 7, 8, 7, 7, 8, 7, 8, 9, 7, 8, 7, 8, 10, 0, 9, 10, 9, 9, 8, 9, 0, 10, 9, 0, 0, 10, 9, 9, 9, 11, 0, 9, 9, 9, 9, 0, 0, 10, 0, 11, 9, 11, 9, 0, 7, 0, 6, 7, 6, 6, 5, 7, 8, 7, 6, 8, 9, 6, 7, 6, 6, 7, 0, 6, 6, 6, 7, 8, 8, 9, 8, 10, 10, 10, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 4, 4, 4, 5, 5, 5, 5, 6, 5, 5, 5, 6, 6, 7, 7, 7, 8, 8, 8, 10, 10, 0, 10 };
    var Lit: LiteralsTable = .{};
    Lit.build(lengths[0..]);
    for (Lit.entries[0..], Lit.codewords[0..]) |sym, code| {
        if (sym.length == 0) continue; // skip symbols that don't exist
        const output = Lit.decode(code);
        try std.testing.expectEqual(sym.symbol, output.symbol);
    }
}

test DistancesTable {
    const lengths = [28]u4{ 8, 9, 9, 9, 9, 8, 7, 7, 5, 6, 4, 4, 4, 5, 4, 5, 4, 5, 4, 4, 4, 4, 4, 4, 4, 5, 5, 6 };
    var Dist: DistancesTable = .{};
    Dist.build(lengths[0..]);
    for (Dist.entries[0..], Dist.codewords[0..]) |sym, code| {
        if (sym.length == 0) continue; // skip symbols that don't exist
        const output = Dist.decode(code);
        try std.testing.expectEqual(sym.symbol, output.symbol);
    }
}
