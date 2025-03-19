const std = @import("std");

fn htree(comptime alphabet_size: usize, comptime max_codelen: u4, comptime lookup_size: u4) type {
    return struct {
        freq: [max_codelen + 1]u12 = @splat(0),
        next_code: [max_codelen + 1]Code = undefined,
        symbol: [alphabet_size]Codeword = undefined,
        lookup: [1 << lookup_size]FastLookup = @splat(FastLookup{ .len = 0, .payload = .{ .next = .nil } }),

        const Code = u15;

        const CodewordIndex = enum(u12) {
            nil = std.math.maxInt(u12),
            _,
        };

        // can I make a comptime assertion that .nil > lookup.len?

        const FastLookup = packed struct {
            len: u4, // packed structs cannot have null, so I have to use 0
            payload: packed union {
                symbol: u12,
                next: CodewordIndex,
            },
        };

        const Codeword = packed struct {
            code: Code,
            len: u4,
            symbol: u12,
            next: CodewordIndex,
        };

        fn build(self: *@This(), codelens: []const u4) void {
            for (codelens) |len| self.freq[len] += 1;
            var max = self.freq.len - 1;
            while (self.freq[max] == 0) max -= 1;
            var code: Code = 0;
            self.next_code[0] = 0;
            for (self.freq[0..max], self.next_code[1..][0..max]) |freq, *next_code| {
                code = (code + freq) << 1;
                next_code.* = code;
            }
            var offset: [max_codelen + 1]u32 = undefined;
            offset[0] = 0;
            for (offset[0..max], offset[1..][0..max], self.freq[0..max]) |loff, *roff, freq|
                roff.* = loff + freq;
            // sort symbols canonically into symbols table
            for (0.., codelens) |i, len| {
                const symbol: u12 = @intCast(i);
                code = self.next_code[len];
                self.next_code[len] += 1;
                self.symbol[offset[len]] = .{ .code = code, .len = len, .symbol = symbol, .next = .nil };
                offset[len] += 1;
            }
            // create lookup table
            for (&self.symbol, 0..) |*codeword, symbol_idx| {
                var reverse = @bitReverse(codeword.code) >> (@bitSizeOf(Code) - codeword.len);
                // unable to do a fast lookup, use a linked list
                if (codeword.len > lookup_size) {
                    reverse &= (1 << lookup_size) - 1;
                    const link = self.lookup[reverse].payload.next;
                    self.lookup[reverse] = .{ .len = 0, .payload = .{ .next = @enumFromInt(symbol_idx) } };
                    codeword.next = link;
                    continue;
                }
                const stride: Code = @as(Code, 1) << codeword.len;
                while (reverse < self.lookup.len) : (reverse += stride)
                    self.lookup[reverse] = .{ .len = codeword.len, .payload = .{ .symbol = codeword.symbol } };
            }
        }
    };
}

test htree {
    const Htree = htree(8, 4, 3);
    // alphabet: A, B, C, D, E, F, G
    const codelengths = [_]u4{ 3, 3, 3, 3, 3, 2, 4, 4 };
    var decoder: Htree = .{};
    decoder.build(&codelengths);
    // try std.testing.expectEqualSlices(u12, &.{ 0, 0, 1, 5, 2 }, &decoder.freq);
    // try std.testing.expectEqualSlices(u12, &decoder.next_code, &.{ 0, 0, 0, 2, 14 });
    try std.testing.expectEqualSlices(Htree.Code, &decoder.next_code, &.{ 0, 0, 1, 7, 16 });
    // for (decoder.symbol) |codeword|
    //     std.debug.print("{any}\n", .{codeword});
    // symbol table tests
    try std.testing.expect(std.meta.eql(decoder.symbol[0], Htree.Codeword{ .len = 2, .code = 0, .symbol = 5, .next = .nil }));
    try std.testing.expect(std.meta.eql(decoder.symbol[1], Htree.Codeword{ .len = 3, .code = 2, .symbol = 0, .next = .nil }));
    try std.testing.expect(std.meta.eql(decoder.symbol[2], Htree.Codeword{ .len = 3, .code = 3, .symbol = 1, .next = .nil }));
    try std.testing.expect(std.meta.eql(decoder.symbol[3], Htree.Codeword{ .len = 3, .code = 4, .symbol = 2, .next = .nil }));
    try std.testing.expect(std.meta.eql(decoder.symbol[4], Htree.Codeword{ .len = 3, .code = 5, .symbol = 3, .next = .nil }));
    try std.testing.expect(std.meta.eql(decoder.symbol[5], Htree.Codeword{ .len = 3, .code = 6, .symbol = 4, .next = .nil }));
    try std.testing.expect(std.meta.eql(decoder.symbol[6], Htree.Codeword{ .len = 4, .code = 14, .symbol = 6, .next = .nil }));
    try std.testing.expect(std.meta.eql(decoder.symbol[7], Htree.Codeword{ .len = 4, .code = 15, .symbol = 7, .next = @enumFromInt(6) }));
    // lookup tests
    try std.testing.expect(std.meta.eql(decoder.lookup[0], Htree.FastLookup{ .len = 2, .payload = .{ .symbol = 5 } }));
    try std.testing.expect(std.meta.eql(decoder.lookup[1], Htree.FastLookup{ .len = 3, .payload = .{ .symbol = 2 } }));
    try std.testing.expect(std.meta.eql(decoder.lookup[2], Htree.FastLookup{ .len = 3, .payload = .{ .symbol = 0 } }));
    try std.testing.expect(std.meta.eql(decoder.lookup[3], Htree.FastLookup{ .len = 3, .payload = .{ .symbol = 4 } }));
    try std.testing.expect(std.meta.eql(decoder.lookup[4], Htree.FastLookup{ .len = 2, .payload = .{ .symbol = 5 } }));
    try std.testing.expect(std.meta.eql(decoder.lookup[5], Htree.FastLookup{ .len = 3, .payload = .{ .symbol = 3 } }));
    try std.testing.expect(std.meta.eql(decoder.lookup[6], Htree.FastLookup{ .len = 3, .payload = .{ .symbol = 1 } }));
    try std.testing.expect(std.meta.eql(decoder.lookup[7], Htree.FastLookup{ .len = 0, .payload = .{ .symbol = 7 } }));
    // for (decoder.lookup) |fast|
    //     std.debug.print("len: {d}, next: {d}\n", .{ fast.len, if (fast.len == 0) @intFromEnum(fast.payload.next) else fast.payload.symbol });
    try std.testing.expect(1 == 0);
}
