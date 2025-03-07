const std = @import("std");

fn htree(comptime alphabet_size: usize, comptime max_codelen: u4, comptime lookup_bits: u3) type {
    return struct {
        freq: [max_codelen + 1]u12 = [_]u12{0} ** (max_codelen + 1),
        next_code: [max_codelen + 1]u12 = undefined,
        symbol: [alphabet_size]Codeword = undefined,
        lookup: [(1 << lookup_bits) - 1]Codeword = undefined,

        const Codeword = packed struct {
            code: u12,
            len: u4,
            symbol: u8,
            next: u8,
        };

        fn build(self: *@This(), codelens: []const u8) void {
            for (codelens) |len| self.freq[len] += 1;
            var max: u4 = self.freq.len - 1;
            while (self.freq[max] == 0) max -= 1;
            self.next_code[0] = 0;
            var code: u12 = 0;
            var i: usize = 1;
            while (i <= max) : (i += 1) {
                code = (code + self.freq[i - 1]) << 1;
                self.next_code[i] = code;
            }

            var symbol: u8 = @as(u8, @intCast(codelens.len)) - 1;
            var links = [_]u8{0} ** (max_codelen + 1);
            while (true) : (symbol -= 1) {
                const len = codelens[symbol];
                const link: u8 = links[len];
                links[len] = symbol;
                code = self.next_code[len] + self.freq[len] - 1;
                self.freq[len] -= 1;
                self.symbol[symbol] = .{
                    .symbol = @intCast(symbol),
                    .code = code,
                    .len = @intCast(len),
                    .next = link,
                };
                if (symbol == 0) break;
            }

            for (self.symbol) |codeword| {
                if (codeword.len == 0) continue;
                code = codeword.code;
                var reverse: u12 = @bitReverse(code) >> (@bitSizeOf(@TypeOf(code)) - codeword.len);
                if (reverse > self.lookup.len) {
                    reverse &= @as(u12, @intCast(self.lookup.len)) - 1;
                    self.lookup[reverse] = .{ .code = 0, .symbol = 0, .len = codeword.len, .next = links[codeword.len] };
                    continue;
                }
                const stride: u12 = @as(u12, 1) << codeword.len;
                while (reverse < self.lookup.len) : (reverse += stride)
                    self.lookup[reverse] = codeword;
            }
        }
    };
}

test htree {
    const Htree = htree(8, 4, 3);
    // alphabet: A, B, C, D, E, F, G
    const codelengths = [_]u8{ 3, 3, 3, 3, 3, 2, 4, 4 };
    var decoder: Htree = .{};
    decoder.build(&codelengths);
    // try std.testing.expectEqualSlices(u12, &.{ 0, 0, 1, 5, 2 }, &decoder.freq);
    try std.testing.expectEqualSlices(u12, &decoder.next_code, &.{ 0, 0, 0, 2, 14 });
    // try std.testing.expectEqualSlices(u12, &decoder.next_code, &.{ 0, 0, 1, 7, 16 });
    for (decoder.symbol) |codeword| {
        std.debug.print("code: {d}, len: {d}, symbol: {d}, next: {d}\n", .{ codeword.code, codeword.len, codeword.symbol, codeword.next });
    }
    for (decoder.lookup) |codeword| {
        std.debug.print("code: {d}, len: {d}, symbol: {d}, next: {d}\n", .{ codeword.code, codeword.len, codeword.symbol, codeword.next });
    }
    // try std.testing.expect(std.meta.eql(decoder.symbol[0], Htree.Codeword{ .len = 3, .code = 2 }));
    // try std.testing.expect(std.meta.eql(decoder.symbol[1], Htree.Codeword{ .len = 3, .code = 3 }));
    // try std.testing.expect(std.meta.eql(decoder.symbol[2], Htree.Codeword{ .len = 3, .code = 4 }));
    // try std.testing.expect(std.meta.eql(decoder.symbol[3], Htree.Codeword{ .len = 3, .code = 5 }));
    // try std.testing.expect(std.meta.eql(decoder.symbol[4], Htree.Codeword{ .len = 3, .code = 6 }));
    // try std.testing.expect(std.meta.eql(decoder.symbol[5], Htree.Codeword{ .len = 2, .code = 0 }));
    // try std.testing.expect(std.meta.eql(decoder.symbol[6], Htree.Codeword{ .len = 4, .code = 14 }));
    // try std.testing.expect(std.meta.eql(decoder.symbol[7], Htree.Codeword{ .len = 4, .code = 15 }));
}
