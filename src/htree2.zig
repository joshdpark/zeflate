const std = @import("std");

fn htree(comptime alphabet_size: usize, comptime max_codelen: u4, comptime lookup_size: u4) type {
    return struct {
        bitbuffer: u64 = 0,
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
            var freq: [max_codelen + 1]u12 = @splat(0);
            var next_code: [max_codelen + 1]Code = undefined;
            for (codelengths) |len| freq[len] += 1;
            var max = freq.len - 1; // the max codelength in this corpus
            while (freq[max] == 0) max -= 1;

            next_code[0] = 0;
            var code: Code = 0;
            for (freq[0..max], next_code[1..][0..max]) |p, *n_code| {
                code = (code + p) << 1;
                n_code.* = code;
            }

            var offset: [max_codelen + 1]u12 = undefined;
            offset[0] = 0;
            for (offset[0..][0..max], offset[1..][0..max], freq[0..][0..max]) |loff, *roff, f|
                roff.* = loff + f;

            // create sorted symbol table
            for (codelengths, 0..) |len, i| {
                const symbol: Symbol = @intCast(i);
                code = next_code[len];
                next_code[len] += 1;
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
                        reverse &= (1 << lookup_size) - 1;
                        const tmp = h.lookup[reverse].payload.next;
                        h.lookup[reverse].payload.next = @enumFromInt(i);
                        h.symbol[i].next = tmp;
                        continue;
                    }
                    const stride = @as(u12, 1) << codeword.len;
                    while (reverse < h.lookup.len) : (reverse += stride) {
                        h.lookup[reverse] = .{ .len = codeword.len, .payload = .{ .symbol = codeword.symbol } };
                    }
                    if (i == 0) break;
                }
            }
        }

        fn decode(h: *@This()) Symbol {
            const prefix = h.bitbuffer & ((1 << lookup_size) - 1);
            const entry = h.lookup[prefix];
            if (entry.len > 0) {
                h.bitbuffer >>= entry.len; // consume code
                return entry.payload.symbol;
            }
            var codeword: Codeword = h.symbol[@intFromEnum(entry.payload.next)];
            while (true) {
                const mask = (@as(Symbol, 1) << codeword.len) - 1;
                if ((h.bitbuffer & mask) ^ codeword.code == 0) {
                    h.bitbuffer >>= codeword.len; // consume code
                    return codeword.symbol;
                }
                codeword = h.symbol[@intFromEnum(codeword.next)];
                // hitting nil should just lead to a buffer overflow and
                // therefore an error
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
        .{ .len = 0, .payload = .{ .next = @enumFromInt(6) } },
    }, &decoder.lookup);

    decoder.bitbuffer = 0b01110101111;
    try std.testing.expectEqual(7, decoder.decode());
    try std.testing.expectEqual(0b0111010, decoder.bitbuffer);
    try std.testing.expectEqual(2, decoder.decode());
    try std.testing.expectEqual(0b0111, decoder.bitbuffer);
    try std.testing.expectEqual(6, decoder.decode());
    try std.testing.expect(1 == 0);
}
