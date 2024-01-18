/// create a hash chain data structure
const std = @import("std");
const mem = std.mem;
const stdout = std.io.getStdOut();

pub fn LZParser(comptime WriterType: type) type {
    return struct {
        buf: []const u8,
        writer: WriterType,
        curr: usize,
        hashtable: []i32, // points to first pos
        chain: []i32, // points to subsequent pos

        const Self = @This();
        const window_size = 1 << 15; // deflate specification states that search buffer is only 32,768 bytes

        pub fn init(writer: anytype, input: []const u8, buf: []i32) LZParser(@TypeOf(writer)) {
            const sz = buf.len;
            std.debug.assert(sz % 4 == 0);
            // by initializing the data to the negative window size, we get a nice property:
            // the while loop in match which makes sure that the link is within the window works
            @memset(buf[0..], -window_size);
            const partition = sz / 4;
            const ht = buf[0..partition];
            const chain = buf[partition..];

            return .{
                .buf = input,
                .writer = writer,
                .curr = 0,
                .hashtable = ht,
                .chain = chain,
            };
        }

        // TODO: replace this hash algorithm
        fn hash(string: [3]u8) usize {
            const k: u24 = @bitCast(string);
            const mask = 0xff;
            return @mod(k, 1009) & mask;
        }

        fn matchLength(curr: []const u8, prev: []const u8) u8 {
            var len: u8 = 0;
            const end = curr.len; // number of bytes until the end of the buffer
            while (len < end and curr[len] == prev[len]) {
                len += 1;
            }
            return len;
        }

        /// return the length of the match, store the match position in match_pos
        fn match(self: *Self, curr: usize, match_pos: *i32) u8 {
            const s = self.buf[curr..][0..3].*;
            const h = hash(s);
            var link: i32 = self.hashtable[h]; // link is a ptr to a match
            var longest_match: u8 = 0; // longest length is 258 (3..258)
            // how to deal with negative numbers?, link can't be negative
            while (link > @as(i32, @intCast(curr)) - window_size) {
                // first check if match is a collision
                if (std.mem.eql(u8, self.buf[curr..][0..3], self.buf[@intCast(link)..][0..3])) {
                    const matchlen = matchLength(self.buf[curr + 3 ..], self.buf[@intCast(link + 3)..]);
                    if (matchlen > longest_match) {
                        longest_match = matchlen;
                        match_pos.* = link; // Should I just store the distance from the current position?
                    }
                }
                link = self.chain[@intCast(link & (window_size - 1))];
            } else {
                self.chain[curr] = self.hashtable[h];
                self.hashtable[h] = @intCast(curr);
            }
            return longest_match;
        }

        fn encode(self: *Self) !void {
            var i: usize = 0;
            var offset: i32 = undefined;
            const text = self.buf;
            const text_end = self.buf.len - 3;
            while (i < text_end) {
                const len = self.match(i, &offset);
                if (len != 0) {
                    // std.debug.print("match: '{s}', len: {d}, pos: {d} text: {s}\n", .{ text[@as(usize, @intCast(i))..][0..3], b + 3, p, text[@as(usize, @intCast(i))..][0 .. b + 3] });
                    try self.writer.print("<{d}, {d}>", .{ len + 3, offset });
                } else {
                    // std.debug.print("no match, '{s}' inserted\n", .{text[@as(usize, @intCast(i))..][0..3]});
                    try self.writer.print("{c}", .{text[i]});
                }
                i += if (len == 0) 1 else len;
            }
        }
    };
}

test LZParser {
    const text =
        \\ she sells seashells by the seashore. peter piper picked a peck
        \\ of peckled peppers a peck of picked peppers peter piper picked. if
        \\ peter piper picked a peck of pickeled peppers, where is the peck of
        \\ picked peppers, peter piper picked.
    ;
    var data: [8192]i32 = undefined;
    const LZ = LZParser(@TypeOf(stdout.writer()));
    var parser = LZ.init(stdout.writer(), text[0..], data[0..]);
    try parser.encode();
}
