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

fn lz_compressor(window_size: usize) type {
    // linked list for hash table should have 2^n entries
    assert(std.math.isPowerOfTwo(window_size));
    return struct {
        string: []const u8,
        hashtable: [window_size]i16,
        chain: [window_size]i16,

        const chain_mask = std.math.maxInt(u15);

        const Pair = struct {
            dist: u15 = 0, // a dist can never be more than 32k away
            len: u8 = 0, // a length must be in the range of 3..258
        };

        const Token = union(enum) {
            literal: u8,
            pair: Pair,
        };

        fn init(string: []const u8) @This() {
            return .{
                .string = string,
                .hashtable = @splat(-1),
                .chain = @splat(-1),
            };
        }

        fn hash(substring: u32) u32 {
            return substring % 1009;
        }

        /// i: compressor head
        /// j: backreference
        /// returns a matchlen
        fn matchlen(self: *@This(), i: usize, j: usize) i16 {
            assert(i > j);
            const string = self.string;
            // invariant: we cannot exceed the bounds of string[i..]
            const bound: u16 = @min(258, @as(u16, @intCast(string[i..].len)));
            var len: i16 = -3;
            for (string[j..][0..bound], string[i..][0..bound]) |ref, head| {
                if (ref != head) break;
                len += 1;
            }
            return len;
        }

        // i: compressor head
        fn search(self: *@This(), i: usize) Token {
            // the hashtable contains the head of the back reference linked list
            // update the list to the current location
            const h = hash(@bitCast(self.string[i..][0..4].*));
            var next: i16 = self.hashtable[h];
            self.hashtable[h] = @intCast(i);

            // no match, return a literal
            if (next < 0) return .{ .literal = self.string[i] };

            // update the linked list head
            self.chain[i & chain_mask] = next;

            var match: Pair = .{};
            // invariant: terminate on any next pointers that exceed the window
            // size boundary
            // next: [i -| window_size, i)
            while (next >= i -| window_size) {
                assert(next < i);
                const n: usize = @intCast(next);
                const mlen = self.matchlen(i, n);
                if (mlen > match.len) match = .{ .dist = @intCast(i - n), .len = @intCast(mlen) };
                next = self.hashtable[n & chain_mask];
            }
            return if (match.len > 0) .{ .pair = match } else .{ .literal = self.string[i] };
        }

        fn compress(self: *@This(), alloc: std.mem.Allocator, list: *std.ArrayListUnmanaged(Token)) void {
            // i: [0, stringlen - 4); to prevent overflow of hashing substring
            var i: usize = 0;
            while (i < self.string.len - 4) {
                i += emit(alloc, list, self.search(i));
            }
            // i: [stringlen - 4, stringlen)
            assert(i > self.string.len - 4);
            while (i < self.string.len) {
                i += emit(alloc, list, .{ .literal = self.string[i] });
            }
        }

        fn emit(alloc: std.mem.Allocator, list: *std.ArrayListUnmanaged(Token), token: Token) usize {
            list.append(alloc, token) catch unreachable;
            return switch (token) {
                .literal => 1,
                .pair => |p| p.len + 3,
            };
        }
    };
}

test lz_compressor {
    const compressor = lz_compressor(1 << 15);
    const string = "Peter Piper picked a peck of pickled peppers, a peck of pickled peppers Peter Piper picked. If Peter Piper picked a peck of pickled peppers, where's the peck of pickled peppers Peter Piper picked?";
    var lz = compressor.init(string);
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    var list: std.ArrayListUnmanaged(compressor.Token) = .empty;
    defer list.deinit(gpa.allocator());
    lz.compress(gpa.allocator(), &list);

    // debug print out how it looks
    for (list.items) |item| {
        switch (item) {
            .literal => |l| std.debug.print("{c}", .{l}),
            .pair => |p| std.debug.print("({d},{d})", .{ p.dist, p.len + 3 }),
        }
    }
}
