const std = @import("std");

fn htree(comptime alphabet_size: usize, comptime max_codelen: u4, comptime lookup_size: u4) type {
    return struct {
        freq: [max_codelen + 1]u12 = @splat(0),
        next_code: [max_codelen + 1]Code = @splat(0),
        symbol: [alphabet_size]Codeword = undefined,
        lookup: [1 << lookup_size]Entry = @splat(.{ .len = 0, .payload = .{ .next = .nil } }),

        const Code = u15;
        const Symbol = u12;

        const Index = enum(u12) {
            nil = std.math.maxInt(u12),
            _,
        };

        const Entry = packed struct {
            len: u4,
            payload: packed union {
                symbol: Symbol,
                next: Index,
            },
        };

        const Codeword = packed struct {
            code: Code, // 15 bits
            len: u4,
            symbol: Symbol,
            next: Index, // 12 bits
        };

        pub fn build(h: *@This(), codelengths: [alphabet_size]u4) void {
            for (codelengths) |len| h.freq[len] += 1;
            var max = h.freq.len - 1; // the max codelength in this corpus
            while (h.freq[max] == 0) max -= 1;

            h.next_code[0] = 0;
            var code: Code = 0;
            for (h.freq[0..max], h.next_code[1..][0..max]) |p, *next_code| {
                code = (code + p) << 1;
                next_code.* = code;
            }

            var offset: [max_codelen + 1]u12 = @splat(0);
            offset[0] = 0;
            for (offset[0..][0..max], offset[1..][0..max], h.freq[0..][0..max]) |loff, *roff, f|
                roff.* = loff + f;

            // create sorted symbol table
            for (codelengths, 0..) |len, i| {
                const symbol: Symbol = @intCast(i);
                code = h.next_code[len];
                h.next_code[len] += 1;
                h.symbol[offset[len]] = .{ .code = code, .len = len, .symbol = symbol, .next = .nil };
                offset[len] += 1;
            }

            // create lookup table
            {
                var i: Symbol = @intCast(h.lookup.len - 1);
                while (true) : (i -= 1) {
                    const codeword = &h.symbol[i];
                    var reverse = @bitReverse(codeword.code) >> @bitSizeOf(Code) - codeword.len;
                    if (codeword.len > lookup_size) {
                        reverse &= 1 << lookup_size - 1;
                        const tmp = h.lookup[reverse].payload.next;
                        h.lookup[reverse] = .{ .len = codeword.len, .payload = .{ .next = @enumFromInt(i) } };
                        h.symbol[i].next = tmp;
                    }
                    const stride = @as(u12, 1) << codeword.len;
                    while (reverse < h.lookup.len) : (reverse += stride) {
                        h.lookup[reverse] = .{ .len = codeword.len, .payload = .{ .symbol = i } };
                    }
                    if (i == 0) break;
                }
            }
        }
    };
}

test htree {
    const Htree = htree(8, 4, 3);
    // alphabet: A, B, C, D, E, F, G
    const codelengths = [_]u4{ 3, 3, 3, 3, 3, 2, 4, 4 };
    var decoder: Htree = .{};
    decoder.build(codelengths);
    try std.testing.expectEqualSlices(u12, &.{ 0, 0, 1, 5, 2 }, &decoder.freq);
    // try std.testing.expectEqualSlices(Htree.Code, &decoder.next_code, &.{ 0, 0, 0, 2, 14 });
    try std.testing.expectEqualSlices(Htree.Code, &decoder.next_code, &.{ 0, 0, 1, 7, 16 });
    // for (decoder.symbol) |codeword|
    //     std.debug.print("{any}\n", .{codeword});
    // symbol table tests
    try std.testing.expectEqualSlices(Htree.Codeword, &.{
        .{ .len = 2, .code = 0, .symbol = 5, .next = .nil },
        .{ .len = 3, .code = 2, .symbol = 0, .next = .nil },
        .{ .len = 3, .code = 3, .symbol = 1, .next = .nil },
        .{ .len = 3, .code = 4, .symbol = 2, .next = .nil },
        .{ .len = 3, .code = 5, .symbol = 3, .next = .nil },
        .{ .len = 3, .code = 6, .symbol = 4, .next = .nil },
        .{ .len = 4, .code = 14, .symbol = 6, .next = @enumFromInt(7) },
        .{ .len = 4, .code = 15, .symbol = 7, .next = .nil },
    }, &decoder.symbol);

    try std.testing.expectEqualSlices(Htree.Entry, &.{
        .{ .len = 2, .payload = .{ .symbol = 5 } },
        .{ .len = 3, .payload = .{ .symbol = 2 } },
        .{ .len = 3, .payload = .{ .symbol = 0 } },
        .{ .len = 3, .payload = .{ .symbol = 4 } },
        .{ .len = 2, .payload = .{ .symbol = 5 } },
        .{ .len = 3, .payload = .{ .symbol = 3 } },
        .{ .len = 3, .payload = .{ .symbol = 1 } },
        .{ .len = 0, .payload = .{ .symbol = 6 } },
    }, &decoder.lookup);

    // for (decoder.lookup) |fast|
    //     std.debug.print("len: {d}, next: {d}\n", .{ fast.len, if (fast.len == 0) @intFromEnum(fast.payload.next) else fast.payload.symbol });
    try std.testing.expect(1 == 0);
}
