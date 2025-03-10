const std = @import("std");

fn htree(comptime alphabet_size: usize, comptime max_codelen: u4) type {
    return struct {
        freq: [max_codelen + 1]u12 = [_]u12{0} ** (max_codelen + 1),
        next_code: [max_codelen + 1]Code = undefined,
        symbol: [alphabet_size]Codeword = undefined,
        lookup: [1 << @bitSizeOf(@TypeOf(max_codelen))]Codeword = undefined,

        const Code = u15;

        const Codeword = packed struct {
            code: Code,
            len: u4,
            symbol: u12,
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
                const symbol: u8 = @intCast(i);
                code = self.next_code[len];
                self.next_code[len] += 1;
                self.symbol[offset[len]] = .{ .code = code, .len = len, .symbol = symbol };
                offset[len] += 1;
            }
            // create lookup table
            for (&self.symbol) |*codeword| {
                var reverse = @bitReverse(codeword.code) >> (@bitSizeOf(Code) - codeword.len);
                const stride: Code = @as(Code, 1) << codeword.len;
                std.debug.print("{any}, reversed code: {b} \n", .{ codeword.*, reverse });
                while (reverse < self.lookup.len) : (reverse += stride)
                    self.lookup[reverse] = codeword.*;
            }
        }
    };
}

test htree {
    const Htree = htree(8, 4);
    // alphabet: A, B, C, D, E, F, G
    const codelengths = [_]u4{ 3, 3, 3, 3, 3, 2, 4, 4 };
    var decoder: Htree = .{};
    decoder.build(&codelengths);
    // try std.testing.expectEqualSlices(u12, &.{ 0, 0, 1, 5, 2 }, &decoder.freq);
    // try std.testing.expectEqualSlices(u12, &decoder.next_code, &.{ 0, 0, 0, 2, 14 });
    try std.testing.expectEqualSlices(Htree.Code, &decoder.next_code, &.{ 0, 0, 1, 7, 16 });
    try std.testing.expect(std.meta.eql(decoder.symbol[0], Htree.Codeword{ .len = 2, .code = 0, .symbol = 5 }));
    try std.testing.expect(std.meta.eql(decoder.symbol[1], Htree.Codeword{ .len = 3, .code = 2, .symbol = 0 }));
    try std.testing.expect(std.meta.eql(decoder.symbol[2], Htree.Codeword{ .len = 3, .code = 3, .symbol = 1 }));
    try std.testing.expect(std.meta.eql(decoder.symbol[3], Htree.Codeword{ .len = 3, .code = 4, .symbol = 2 }));
    try std.testing.expect(std.meta.eql(decoder.symbol[4], Htree.Codeword{ .len = 3, .code = 5, .symbol = 3 }));
    try std.testing.expect(std.meta.eql(decoder.symbol[5], Htree.Codeword{ .len = 3, .code = 6, .symbol = 4 }));
    try std.testing.expect(std.meta.eql(decoder.symbol[6], Htree.Codeword{ .len = 4, .code = 14, .symbol = 6 }));
    try std.testing.expect(std.meta.eql(decoder.symbol[7], Htree.Codeword{ .len = 4, .code = 15, .symbol = 7 }));
}
