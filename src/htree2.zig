const std = @import("std");

fn htree(comptime alphabet_size: usize, comptime max_codelen: u4, comptime lookup_bits: u3) type {
    return struct {
        freq: [max_codelen + 1]u12 = [_]u12{0} ** (max_codelen + 1),
        next_code: [max_codelen + 1]u16 = undefined,
        symbol: [alphabet_size]Codeword = undefined,
        lookup: [(1 << lookup_bits) - 1]Codeword = undefined,

        const Codeword = packed struct {
            code: u16,
            len: u4,
            symbol: u8,
            next: u8,
        };

        fn build(self: *@This(), codelens: []const u4) void {
            for (codelens) |len| self.freq[len] += 1;
            var max: u4 = self.freq.len - 1;
            while (self.freq[max] == 0) max -= 1;
            self.next_code[0] = 0;
            var code: u16 = 0;
            var i: usize = 1;
            while (i <= max) : (i += 1) {
                code = (code + self.freq[i - 1]) << 1;
                self.next_code[i] = code;
            }

            // I need to sort by symbol, that means I need to do this
            // differently.
            var links: [max_codelen + 1]u8 = @splat(0);
            // reuse freq for offset
            var offsets: [max_codelen + 1]u12 = undefined;
            offsets[0] = self.freq[0];
            for (offsets[1..], self.freq[0..self.freq.len - 1], self.freq[1..]) |*offset, *left, *right|
                offset.* = right.* + left.*;

            for (0.., offsets) |j, off|
                std.debug.print("offset[{d}] = {d}\n", .{j, off});
            i = codelens.len - 1;
            while (true) : (i -= 1) {
                const len = codelens[i];
                if (len == 0) continue;
                const symbol: u8  = @intCast(i);
                const order = offsets[len];
                offsets[len] -= 1;
                const next = links[len];
                links[len] = symbol;
                code = self.next_code[len] + self.freq[len];
                self.freq[len] -= 1;
                self.symbol[order] = .{.code = code, .len = len, .symbol = symbol, .next = next};
                if (i == 0) break;
            }
            for (0.., links) |j, next| {
                std.debug.print("link[{d}] = {d}\n", .{j, next});
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
    try std.testing.expectEqualSlices(u16, &decoder.next_code, &.{ 0, 0, 0, 2, 14 });
    // try std.testing.expectEqualSlices(u16, &decoder.next_code, &.{ 0, 0, 1, 7, 16 });
    for (decoder.symbol) |codeword| {
        std.debug.print("code: {d}, len: {d}, symbol: {d}, next: {d}\n", .{ codeword.code, codeword.len, codeword.symbol, codeword.next });
    }
    // try std.testing.expect(std.meta.eql(decoder.symbol[0], Htree.Codeword{ .len = 2, .code = 0 , .symbol = 5, .next = 5}));
    // try std.testing.expect(std.meta.eql(decoder.symbol[1], Htree.Codeword{ .len = 3, .code = 2 , .symbol = 0, .next = 0}));
    // try std.testing.expect(std.meta.eql(decoder.symbol[2], Htree.Codeword{ .len = 3, .code = 3 , .symbol = 1, .next = 0}));
    // try std.testing.expect(std.meta.eql(decoder.symbol[3], Htree.Codeword{ .len = 3, .code = 4 , .symbol = 2, .next = 1}));
    // try std.testing.expect(std.meta.eql(decoder.symbol[4], Htree.Codeword{ .len = 3, .code = 5 , .symbol = 3, .next = 2}));
    // try std.testing.expect(std.meta.eql(decoder.symbol[5], Htree.Codeword{ .len = 3, .code = 6 , .symbol = 4, .next = 3}));
    // try std.testing.expect(std.meta.eql(decoder.symbol[6], Htree.Codeword{ .len = 4, .code = 14 ,.symbol = 6,  .next = 6}));
    // try std.testing.expect(std.meta.eql(decoder.symbol[7], Htree.Codeword{ .len = 4, .code = 15 ,.symbol = 7,  .next = 6}));
}
