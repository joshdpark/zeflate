// create an algorithm using the lempel-ziv 77 algorithm specified in the
// DEFLATE rfc 1951. Here is the description:
//
// The compressor uses a chained hash table to find duplicated strings, using a
// hash function that operates on 3-byte sequences.  At any given point during
// compression, let XYZ be the next 3 input bytes to be examined (not necessarily
// all different, of course).
//
// First, the compressor examines the hash chain for
// XYZ.  If the chain is empty, the compressor simply writes out X as a literal
// byte and advances one byte in the input.  If the hash chain is not empty,
// indicating that the sequence XYZ (or, if we are unlucky, some other 3 bytes
// with the same hash function value) has occurred recently, the compressor
// compares all strings on the XYZ hash chain with the actual input data sequence
// starting at the current point, and selects the longest match.
//
// The compressor searches the hash chains starting with the most recent
// strings, to favor small distances and thus take advantage of the Huffman
// encoding.  The hash chains are singly linked. There are no deletions from the
// hash chains; the algorithm simply discards matches that are too old.  To avoid
// a worst-case situation, very long hash chains are arbitrarily truncated at a
// certain length, determined by a run-time parameter.

const std = @import("std");
const assert = std.debug.assert;

// key: XYZ bytes, value: linked list of absolute positions for where match was
// if there is a collision, then don't overwrite but prepend new value as a node
// to the list so that the position closer to the cursor is given priority.
//
// constraints:
// 1. the lookahead buffer + search buffer will be a page (4kb)
// 2. the sliding window is basically a pointer in this page,
//    the pointer and everything in front of it will be the lookahead buffer
//    while everything behind it to the beginning of the buffer will be the
//    search buffer

fn scanner(sz: usize) type {
    assert(std.math.isPowerOfTwo(sz));
    return struct {
        string: []const u8,
        hashtable: [sz]?i32,
        chain: [sz]i32, // needed for the negative values

        const chain_mask = sz - 1;

        const Token = union(enum) { literal: u8, pair: struct {
            dist: usize,
            len: usize,
        } };

        fn init(string: []const u8) @This() {
            return .{
                .string = string,
                .hashtable = @splat(null),
                .chain = @splat(-1),
            };
        }

        fn hash(substring: [3]u8) u16 {
            return @truncate(@as(u24, @bitCast(substring)) % 1009);
        }

        fn matchlen(self: *@This(), i: usize, j: usize) usize {
            const bound = @min(self.string.len - i, self.string.len - j);
            for (self.string[i..][0..bound], self.string[j..][0..bound], 0..) |*a, *b, l| {
                if (a.* != b.*) return l;
            }
            return bound;
        }

        fn search(self: *@This(), i: usize) Token {
            const substring: [3]u8 = self.string[i..][0..3].*;
            const h = hash(substring);
            var j: i32 = @intCast(i);
            // if next is empty, then update hashtable
            var next: i32 = self.hashtable[h] orelse {
                self.hashtable[h] = @intCast(i);
                return .{ .literal = substring[0] };
            };
            self.chain[@as(usize, @intCast(j)) & chain_mask] = next;
            // the key insight here is that any values in the chain table
            // must be greater than j -% sz which indicates the window area
            var match: Token = .{ .pair = .{ .dist = 0, .len = 0 } };
            while (next > i -| sz) {
                j = next;
                // cmp cursor[i..], cursor[j..] for matchlen
                const mlen = self.matchlen(i, @as(usize, @intCast(j)));
                if (mlen > match.pair.len)
                    match = .{ .pair = .{ .dist = i - @as(usize, @intCast(j)), .len = mlen } };
                // follow the list
                next = self.chain[@as(usize, @intCast(j)) & chain_mask];
            }
            return match;
        }

        fn scan(self: *@This()) void {
            for (0..self.string.len - 2) |i| {
                emit(self.search(i));
            }
        }

        fn emit(token: Token) void {
            switch (token) {
                .literal => |lit| std.debug.print("{c}", .{lit}),
                .pair => |p| std.debug.print("({d},{d})", .{ p.dist, p.len }),
            }
        }
    };
}

test scanner {
    const Scanner = scanner(1 << 16);
    const string = "aaabbbabba";
    var lz = Scanner.init(string);
    lz.scan();
}
