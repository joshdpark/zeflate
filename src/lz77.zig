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

fn scanner(size: u32) type {
    assert(size & (size - 1) == 0);
    return struct {
        const Idx = enum(u16) {
            nil = std.math.maxInt(u16),
            _,
        };

        const Hash_i = Idx;
        const Chain_i = Idx;

        const Token = union(enum) { char: u8, pair: struct {
            dist: usize,
            mlen: usize,
        } };

        input: []const u8,
        hash_table: [size]Chain_i,
        chain_table: [size]Chain_i,

        pub fn init(input: []const u8) @This() {
            return .{
                .input = input,
                .hash_table = @splat(.nil),
                .chain_table = @splat(.nil),
            };
        }

        fn hash(three: [3]u8) Hash_i {
            const h: u16 = @truncate(@as(u24, @bitCast(three)) % 1009);
            return @enumFromInt(h);
        }

        fn matchLength(cursor: []u8, back: []u8) u32 {
            const n = @min(cursor.len, back.len);
            for (cursor[0..][0..n], back[0..][0..n], 0..) |a, b, len|
                if (a != b) return len;
        }

        /// otherwise emit (dist,len) is match
        pub fn scan(self: *@This()) void {
            const N = self.input.len;
            for (0..N - 2) |i| {
                const string: [3]u8 = self.input[i..][0..3].*;
                const h = hash(string);
                // look for a previous match by taking the hash and
                // indexing into the hash_table,
                var prev: *Chain_i = &self.hash_table[@intFromEnum(h)];
                if (prev.* == .nil) {
                    prev.* = @enumFromInt(i);
                    emit(.{ .char = string[0] });
                } else {
                    // update chain table to point to last occurance of string
                    const tmp = prev.*;
                    prev.* = @enumFromInt(i);
                    self.chain_table[@intFromEnum(prev.*)] = tmp;
                    // iterate through the chain table linked list
                    // to find the longest string match
                    var mlen: usize = 0;
                    var dist: usize = @intFromEnum(prev.*);
                    // chain loop
                    while (prev.* != .nil) {
                        const a = self.input[@intFromEnum(prev.*)..];
                        const b = self.input[i..];
                        const len = @min(a.len, b.len);
                        // mlen loop
                        var test_mlen: usize = undefined;
                        for (a[0..len], b[0..len], 0..) |back, here, l| {
                            if (back != here) {
                                test_mlen = l;
                                break;
                            }
                        }
                        if (test_mlen > mlen) {
                            mlen = test_mlen;
                            dist = i;
                        }
                        // doing a full search down the chain
                        prev = &self.chain_table[@intFromEnum(prev.*)];
                    }
                    emit(.{ .pair = .{ .dist = dist, .mlen = mlen } });
                }
            }
        }

        fn emit(token: Token) void {
            _ = token;
        }
    };
}

test scanner {
    const Scanner = scanner(1 << 16);
    const string = "aaabbbabba";
    var lz = Scanner.init(string);
    lz.scan();
}
