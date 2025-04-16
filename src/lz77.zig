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

const lz_compressor = struct {
    string: []const u8,
    hashtable: [window_size]?u15,
    chain: [window_size]u15,

    const window_size = 1 << 15;
    const max_window = std.math.maxInt(u15);

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
            .hashtable = @splat(null),
            .chain = undefined,
        };
    }

    fn hash(substring: u32) u15 {
        const fib: u64 = 11400714819323198486;
        return @intCast((substring *% fib) >> (64 - 15));
    }

    /// i: compressor head
    /// j: backreference
    /// returns a matchlen
    fn matchlen(self: *@This(), i: usize, j: usize) u8 {
        assert(i > j);
        const string = self.string;
        // invariant: we cannot exceed the bounds of string[i..]
        const bound: u16 = @intCast(@min(258, string[i..].len));
        var len: i16 = -3;
        for (string[j..][0..bound], string[i..][0..bound]) |ref, head| {
            if (ref != head) break;
            len += 1;
        }
        return if (len > 0) @intCast(len) else 0;
    }

    /// when calculating the distance between two points in the chain table, one
    /// issue that comes up is since this is a circular buffer, a next pointer
    /// might point to an index > the current index.
    fn distance(cur: u15, next: u15) usize {
        // return (max_window + a - b) & max_window;
        const result = if (cur > next) cur - next else max_window - next + cur;
        return @intCast(result);
    }

    // i: compressor head
    fn search(self: *@This(), i: usize) Token {
        const h = hash(@bitCast(self.string[i..][0..4].*));
        // index into chain table
        var k: u15 = @intCast(i & max_window);
        // distance from i to back reference
        var j: usize = 0;

        // next chain pointer; if there is no next pointer then we update the
        // hash table to point to k. This is so that the loop terminates
        // correctly because if (chain[k] == k), then the distance will be
        // outside the search window
        var next: u15 = self.hashtable[h] orelse k;
        // update the list head (stored in hashtable)
        // and next (to point to previous head)
        self.hashtable[h] = k;
        self.chain[k] = next;
        if (next == k) return .{ .literal = self.string[i] };

        // the loop invariant is that it terminates if the next pointer would
        // increase j (the distance from i) to or greater than the the window
        // size. This invariant should hold for initial values because when
        // chain[i] == i, then distance will be the window size
        var pair: Pair = .{};
        while (true) {
            j += distance(k, next);
            if (j > max_window) break;

            // update longest match if it exists
            const mlen = self.matchlen(i, i - j);
            if (mlen > pair.len) pair = .{ .dist = @intCast(j), .len = mlen };

            k = next;
            next = self.chain[next];
        }
        return if (pair.len > 4) .{ .pair = pair } else .{ .literal = self.string[i] };
    }

    fn compress(self: *@This(), alloc: std.mem.Allocator, list: *std.ArrayListUnmanaged(Token)) void {
        // i: [0, stringlen - 4); to prevent overflow of hashing substring
        var i: usize = 0;
        while (i < self.string.len - 4) {
            i += emit(alloc, list, self.search(i));
        }
        assert(i >= self.string.len - 5);
        while (i < self.string.len) {
            i += emit(alloc, list, .{ .literal = self.string[i] });
        }
    }

    fn emit(alloc: std.mem.Allocator, list: *std.ArrayListUnmanaged(Token), token: Token) usize {
        list.append(alloc, token) catch unreachable;
        return switch (token) {
            .literal => 1,
            .pair => |p| @as(usize, @intCast(p.len)) + 3,
        };
    }
};

test lz_compressor {
    // const string = "Peter Piper picked a peck of pickled peppers, a peck of pickled peppers Peter Piper picked. If Peter Piper picked a peck of pickled peppers, where's the peck of pickled peppers Peter Piper picked?";
    const string = @embedFile("test/lz77test");
    var lz: lz_compressor = .init(string);
    var dba: std.heap.DebugAllocator(.{}) = .init;
    defer _ = dba.deinit();
    var list: std.ArrayListUnmanaged(lz_compressor.Token) = .empty;
    defer list.deinit(dba.allocator());
    lz.compress(dba.allocator(), &list);

    var output: [string.len]u8 = undefined;
    var i: usize = 0;
    var literals: usize = 0;
    var pairs: usize = 0;
    for (list.items) |item| {
        switch (item) {
            .literal => |l| {
                output[i] = l;
                i += 1;
                literals += 1;
            },
            .pair => |p| {
                const bound = @as(usize, @intCast(p.len)) + 3;
                std.mem.copyForwards(u8, output[i..][0..bound], output[i - p.dist ..][0..bound]);
                i += bound;
                pairs += 1;
            },
        }
    }
    std.debug.print("from {d} characters to {d} literals and {d} pairs", .{ string.len, literals, pairs });
    try std.testing.expectEqualStrings(string, &output);
}
