const std = @import("std");

const maxcodelen = 15; // deflate specifies that codelens cannot be longer than 15 bits

fn htree(comptime alphabet_size: usize) type {
    return struct {
        freq: [maxcodelen]u32 = [_]u32{0} ** maxcodelen,
        next_code: [maxcodelen]u32 = [_]u32{0} ** maxcodelen,
        code_table: [alphabet_size]LenSymbol = undefined,

        const LenSymbol = packed struct { length: u8, codeword: u24 };

        /// codelengths is a table that is symbol->codelength
        fn build(self: *@This(), codelengths: []const u32) void {
            // build freq table
            for (codelengths) |l|
                self.freq[l] += 1;

            // what's the maximum codelength?
            var maxlen: u32 = maxcodelen - 1;
            while (self.freq[maxlen] == 0)
                maxlen -= 1;

            // build next_code offset table
            var code: u32 = 0;
            self.freq[0] = 0;
            for (1..maxlen + 1) |len| {
                code = (code + self.freq[len - 1]) << 1;
                self.next_code[len] = code;
            }

            // build code table
            for (&self.code_table, codelengths) |*symbol, l| {
                if (l == 0)
                    continue;
                const codeword = self.next_code[l];
                self.next_code[l] += 1;
                const length = l;
                const pair: LenSymbol = .{ .length = @truncate(length), .codeword = @truncate(codeword) };
                symbol.* = pair;
            }
        }
    };
}

test htree {
    const Htree = htree(8);
    // alphabet: A, B, C, D, E, F, G
    const codelengths = [_]u32{ 3, 3, 3, 3, 3, 2, 4, 4 };
    var decoder: Htree = .{};
    decoder.build(&codelengths);
    try std.testing.expectEqualSlices(u32, &decoder.freq, &.{ 0, 0, 1, 5, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
    // try std.testing.expectEqualSlices(u32, &decoder.next_code, &.{ 0, 0, 0, 2, 14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
    for (decoder.code_table) |lensym| {
        std.debug.print("length: {d}, codeword: {d}\n", .{ lensym.length, lensym.codeword });
    }
    // try std.testing.expectEqualSlices(u32, &decoder.next_code, &.{});
}
