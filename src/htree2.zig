const std = @import("std");

fn htree(comptime alphabet_size: usize, comptime max_codes: u4, comptime lookup_bits: usize) type {
    const lookup_size: usize = 1 << lookup_bits;
    return struct {
        freq: [max_codes]u12 = [_]u12{0} ** max_codes,
        next_code: [max_codes]u12 = [_]u12{0} ** max_codes,
        symbol: [alphabet_size]Codeword = undefined, // a table of symbols->Codewords
        lookup: [lookup_size]Entry = undefined,

        const Codeword = packed struct {
            code: u12,
            len: u4,
        };

        const Entry = packed struct {
            symbol: u8,
            len: u4,
            kind: enum(u1) {
                literal,
                link,
            },
        };

        fn buildtree(self: *@This(), codelengths: []const u8) void {
            for (codelengths) |length| self.freq[length] += 1;

            var max = max_codes - 1;
            while (self.freq[max] == 0) max -= 1;

            {
                self.freq[0] = 0;
                var code: u12 = 0;
                var bits: usize = 1;
                while (bits <= max) : (bits += 1) {
                    code = (code + self.freq[bits - 1]) << 1;
                    self.next_code[bits] = code;
                }
            }

            for (codelengths, 0..) |len, i| {
                if (self.freq[len] == 0) continue;
                const code = self.next_code[len];
                self.next_code[len] += 1;
                self.symbol[i] = .{ .code = code, .len = @intCast(len) };
            }

            // create lookup table
            {
                // var reverse: [self.symbol.len]Codeword = [_]Codeword{.{ .len = 0, .code = 0 }} ** self.symbol.len;
                for (&self.symbol, 0..) |*src, s| {
                    const sym: u8 = @intCast(s);
                    const reverse = @bitReverse(src.code) >> (@bitSizeOf(@TypeOf(src.code)) - src.len);
                    var i: u12 = reverse;
                    if (i > lookup_size) {
                        // TODO: add to linked list
                        self.lookup[i] = .{
                            .symbol = 0,
                            .len = src.len,
                            .kind = .link,
                        };
                        continue;
                    }
                    const stride: u12 = @as(u12, 1) << src.len;
                    while (i < lookup_size) : (i += stride) {
                        self.lookup[i] = .{
                            .symbol = sym,
                            .len = src.len,
                            .kind = .literal,
                        };
                    }
                }
            }
        }
    };
}

test htree {
    const Htree = htree(8, 5, 4);
    // alphabet: A, B, C, D, E, F, G
    const codelengths = [_]u8{ 3, 3, 3, 3, 3, 2, 4, 4 };
    var decoder: Htree = .{};
    decoder.buildtree(&codelengths);
    try std.testing.expectEqualSlices(u12, &decoder.freq, &.{ 0, 0, 1, 5, 2 });
    // try std.testing.expectEqualSlices(u12, &decoder.next_code, &.{ 0, 0, 0, 2, 14 });
    try std.testing.expectEqualSlices(u12, &decoder.next_code, &.{ 0, 0, 1, 7, 16 }); // how it should look
    try std.testing.expect(std.meta.eql(decoder.symbol[0], Htree.Codeword{ .len = 3, .code = 2 }));
    try std.testing.expect(std.meta.eql(decoder.symbol[1], Htree.Codeword{ .len = 3, .code = 3 }));
    try std.testing.expect(std.meta.eql(decoder.symbol[2], Htree.Codeword{ .len = 3, .code = 4 }));
    try std.testing.expect(std.meta.eql(decoder.symbol[3], Htree.Codeword{ .len = 3, .code = 5 }));
    try std.testing.expect(std.meta.eql(decoder.symbol[4], Htree.Codeword{ .len = 3, .code = 6 }));
    try std.testing.expect(std.meta.eql(decoder.symbol[5], Htree.Codeword{ .len = 2, .code = 0 }));
    try std.testing.expect(std.meta.eql(decoder.symbol[6], Htree.Codeword{ .len = 4, .code = 14 }));
    try std.testing.expect(std.meta.eql(decoder.symbol[7], Htree.Codeword{ .len = 4, .code = 15 }));
    for (decoder.lookup) |lookup| {
        std.debug.print("{any}\n", .{lookup});
    }
}
