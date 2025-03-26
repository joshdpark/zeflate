fn htree(comptime alphabet_size: usize, comptime max_codelen: u4, comptime lookup_size: u4) type {
    return struct {
        bitbuffer: u64 = 0,
        codeword: [alphabet_size]Codeword = undefined,
        lookup: [1 << lookup_size]Entry = @splat(.{ .payload = .{ .next = .nil }, .len = 0 }),

        const Symbol = u16;
        const Code = u15;
        const CodewordIndex = enum(u16) { nil = std.math.maxInt(u16), _ };

        const Codeword = packed struct {
            symbol: Symbol,
            len: u4,
            code: Code,
            next: CodewordIndex,
        };

        const Entry = packed struct {
            payload: packed union {
                symbol: Symbol,
                next: CodewordIndex,
            },
            len: u4,
        };

        fn build(ht: *@This(), codelengths: []const u4) void {
            var freq: [max_codelen + 1]u32 = @splat(0);
            for (codelengths) |code| freq[code] += 1;
            var max = freq.len - 1;
            while (freq[max] == 0) max -= 1;
            // get offsets into the symbols table for sorting codewords
            var offset: [max_codelen + 1]u32 = undefined;
            offset[0] = 0;
            for (offset[1..][0..max], offset[0..max], freq[0..max]) |*off, prev, f|
                off.* = prev + f;
            // sort the symbols
            for (0.., codelengths) |i, len| {
                const symbol: Symbol = @intCast(i);
                const codeword: Codeword = .{
                    .symbol = symbol,
                    .len = len,
                    .code = undefined,
                    .next = .nil,
                };
                ht.codeword[offset[len]] = codeword;
                offset[len] += 1;
            }
            // create the code words, code lsb is on left.
            var code: Code = 0;
            for (ht.codeword[0 .. ht.codeword.len - 1]) |*codeword| {
                codeword.code = code;
                const bsize = @bitSizeOf(Code); // backing size of Code
                const inverse = ~code & ((@as(Code, 1) << codeword.len) - 1);
                const pos = bsize - 1 - @clz(inverse);
                const msz = @as(Code, 1) << pos;
                // flip the most significant 0 bit and clear all lsb 1 bits.
                code = (code | msz) & ((msz << 1) - 1);
            }
            ht.codeword[ht.codeword.len - 1].code = code;
            // populate lookup tables
            var i = ht.codeword.len;
            while (i > 0) {
                i -= 1;
                const codeword = ht.codeword[i];
                if (codeword.len == 0) {
                    continue;
                } else if (codeword.len <= lookup_size) {
                    const stride = @as(Code, 1) << codeword.len;
                    var lookup_idx = codeword.code;
                    while (lookup_idx < ht.lookup.len) : (lookup_idx += stride)
                        ht.lookup[lookup_idx] = .{ .payload = .{ .symbol = codeword.symbol }, .len = codeword.len };
                } else {
                    const prefix = codeword.code & ((@as(Code, 1) << lookup_size) - 1);
                    const tmp = ht.lookup[prefix].payload.next;
                    ht.lookup[prefix].payload.next = @enumFromInt(i);
                    ht.codeword[i].next = tmp;
                }
            }
        }

        fn query(ht: *@This()) Symbol {
            const mask = (1 << lookup_size) - 1;
            const entry = ht.lookup[ht.bitbuffer & mask];
            if (entry.len == 0) {
                var next: CodewordIndex = entry.payload.next;
                while (next != .nil) {
                    const codeword = ht.codeword[@intFromEnum(next)];
                    next = codeword.next;
                    const code = ht.bitbuffer & ((@as(Code, 1) << codeword.len) - 1);
                    if (code != codeword.code) continue;
                    ht.bitbuffer >>= codeword.len;
                    return codeword.symbol;
                }
                unreachable;
            }
            ht.bitbuffer >>= entry.len;
            return entry.payload.symbol;
        }
    };
}

const std = @import("std");
const Htree = htree(8, 4, 3);

test Htree {
    // alphabet: A, B, C, D, E, F, G
    const codelengths = [_]u4{ 3, 3, 3, 3, 3, 2, 4, 4 };
    var decoder: Htree = .{};
    decoder.build(&codelengths);
    // symbol table tests
    // the codes are in fact reversed
    try std.testing.expectEqualSlices(Htree.Codeword, &.{
        .{ .len = 2, .code = 0, .symbol = 5, .next = .nil }, // 00 -> 00
        .{ .len = 3, .code = 2, .symbol = 0, .next = .nil }, // 010 -> 010
        .{ .len = 3, .code = 6, .symbol = 1, .next = .nil }, // 011 -> 110
        .{ .len = 3, .code = 1, .symbol = 2, .next = .nil }, // 100 -> 001
        .{ .len = 3, .code = 5, .symbol = 3, .next = .nil }, // 101 -> 101
        .{ .len = 3, .code = 3, .symbol = 4, .next = .nil }, // 110 -> 011
        .{ .len = 4, .code = 7, .symbol = 6, .next = @enumFromInt(7) }, // 1110 -> 0111
        .{ .len = 4, .code = 15, .symbol = 7, .next = .nil }, // 1111 -> 1111
    }, &decoder.codeword);

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
    try std.testing.expectEqual(7, decoder.query());
    try std.testing.expectEqual(0b0111010, decoder.bitbuffer);
    try std.testing.expectEqual(0, decoder.query());
    try std.testing.expectEqual(0b0111, decoder.bitbuffer);
    try std.testing.expectEqual(6, decoder.query());

    try std.testing.expect(1 == 0);
}
