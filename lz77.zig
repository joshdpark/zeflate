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
const mem = std.mem;
const expect = std.testing.expect;
const ArrayList = std.ArrayList;
const formatInt = std.fmt.formatInt;
// const window = std.mem.window;

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

const Pointer = struct {
    // 1 << 12 is the total size of 4kb
    offset: usize,
    len: u12,
};

/// this is an attempt at a first implementation of trying to make multiple
/// matches for a sequence of 3 bytes in a search buffer
const Window = struct {
    buf: []const u8,
    seq: [3]u8,

    const Iterator = struct {
        window: *const Window,
        pos: usize, // offset into search buffer

        /// get the next match of sequence
        fn next(self: *@This()) ?usize {
            const buf = self.window.buf;
            const seq = self.window.seq[0..];
            while (self.pos < buf.len - 3) : (self.pos += 1) {
                std.debug.print("{s}\n", .{buf[self.pos..][0..3]});
                // compare the sequence of 3 bytes against the search buffer
                if (std.mem.eql(u8, buf[self.pos..][0..3], seq)) {
                    const tmp = self.pos;
                    self.pos += 1;
                    return tmp;
                }
            }
            return null;
        }
    };

    fn iter(self: *const @This()) Iterator {
        return .{
            .window = self,
            .pos = 0,
        };
    }
};

test Window {
    const w = Window{
        .search_buffer = "aabbccddbbceeff",
        .sequence = [_]u8{ 'b', 'b', 'c' },
    };
    var it = w.iter();
    while (it.next()) |n| {
        std.debug.print("pos: {d}\n", .{n});
    } else {
        std.debug.print("pos: null\n", .{});
    }
}

/// NOTE: this is an linear search, since for each char in the search buffer,
/// it's comparing to the front of the lookahead buffer.
fn match(buf: []const u8, cursor: usize) ?Pointer {
    const search: []const u8 = buf[0..cursor];
    const lookahead: []const u8 = buf[cursor..];
    const here = buf[cursor];
    // TODO: I'm pretty sure I could use simd vectorization here to do an xor
    // comparison if there is a match, therefore removing branching.
    const matched: usize = blk: for (search, 0..) |behind, i| {
        if (behind == here) {
            break :blk i;
        }
    } else {
        return null;
    };
    const offset: usize = search[matched..].len;
    const len: u12 = blk: {
        const matchbuf = buf[matched..];
        var j: u12 = 0;
        while (j < lookahead.len and lookahead[j] == matchbuf[j]) {
            j += 1;
        }
        break :blk j; // go from index to len
    };
    return Pointer{ .offset = offset, .len = len };
}

test match {
    // std.debug.print("{any}\n", .{match("aabbccbb", 6).?});
    try expect(std.meta.eql(match("aabbccbb", 6).?, Pointer{ .offset = 4, .len = 2 }));
    try expect(match("aabbccdd", 4) == null);
    // match should also extend past the search buffer into the lookahead
    try expect(std.meta.eql(match("aabbaabbaabb", 4).?, Pointer{ .offset = 4, .len = 8 }));
}

// TODO: right now it matches on any length, which is inefficient because we
// really should only have a pointer if the match length > 2.
// TODO: this implementation always starts from the beginning of the search
// buffer and scans through from beginning to end. We want to make sure that we
// use the closest match first before going back through the search buffer.
// TODO: we are also not searching for the longest match, just the first match.
fn naive_lz77(input: []const u8, list: anytype) !void {
    var cursor: usize = 0;
    var matchlen: u12 = 0;
    while (cursor < input.len) {
        if (match(input, cursor)) |ptr| {
            matchlen += 1;
            _ = try list.writeByte('<');
            try formatInt(ptr.offset, 10, .lower, .{}, list);
            _ = try list.writeByte(',');
            try formatInt(ptr.len, 10, .lower, .{}, list);
            _ = try list.writeByte('>');
            cursor += ptr.len;
        } else {
            _ = try list.writeByte(input[cursor]);
            cursor += 1;
        }
    }
    // try list.flush();
}

test naive_lz77 {
    const str = "hello world hello world hello world foobar bar foo";
    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    try naive_lz77(str, list.writer());
    std.debug.print("{s}\n", .{list.items});
}

// TODO: implement a hashtable
fn hash() u8 {}
fn ChainedHash() type {}
// psuedocode
// while (window.next()) |xyz| {
// 	if xyz in hashchain,
// 		for each entry in chain
// 			compare the length of the match at current position
// 			select the longest match
// 	else
// 		add xyz to hashchain with position
// 		write x as a literal byte
// }

pub fn main() void {}
