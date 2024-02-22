/// create a hash chain data structure
const std = @import("std");
const mem = std.mem;
const assert = std.debug.assert;
const stdout = std.io.getStdOut();

/// map every value from 1..256 to a distance code value [0, 16)
/// from 256..36,768, map upper bits to a distance code value [16..32)
const distance_map: [256]u16 = blk: {
    var result: [256]u16 = undefined;
    const extrabits = [_]u4{ 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13 };
    var i: usize = 0;
    var r: u16 = 0;
    for (extrabits[0..16]) |e| {
        const extra = 1 << e;
        for (0..extra) |_| {
            result[i] = r;
            i += 1;
        }
        r += 1;
    }
    break :blk result;
};

/// using algorithm from libdeflate, take advantage that codes [16, 30) are 128 (or 1 << 7)
/// times (code - 14)
inline fn dist_to_code(distance: u16) u16 {
    const n: u4 = if (distance <= 256) 0 else 7;
    // branchless version of above
    // const n = @as(u32, 256) -% distance >> 29;
    return distance_map[distance - 1 >> n] + (n << 1);
}

test dist_to_code {
    try std.testing.expect(dist_to_code(285) == 16);
    try std.testing.expect(dist_to_code(24590) == 29);
    try std.testing.expect(dist_to_code(4100) == 24);
    try std.testing.expect(dist_to_code(1) == 0);
    try std.testing.expect(dist_to_code(30) == 9);
}

// TODO: implement a length_to_code function;
// NOTE: is this necessary? 512 bytes aren't that much.. it should be fine

const Stat = struct {
    literals: [256]u16,
    lengths: [256]u16,
    distances: [30]u16,
};

/// This seems... like a hack? I don't know... I want to be able to pack information into
/// as few bytes as possible (4 bytes for each literal/pointer); Is there anyway to get
/// this as a tagged enum?
const Segment = packed struct {
    t: u8,
    d: packed union {
        literal: u24,
        distlen: dl,
    },

    const dl = packed struct {
        length: u8,
        distance: u16,
    };
};

const Tape = struct {
    buf: []Segment,
    cur: usize,

    pub fn init(buf: []Segment) Tape {
        return .{ .buf = buf, .cur = 0 };
    }

    fn append(self: *@This(), segment: Segment) void {
        self.buf[self.cur] = segment;
        self.cur += 1;
    }
};

const LZParser = struct {
    buf: []const u8,
    curr: usize,
    hashtable: []i32, // points to first pos
    chain: []i32, // points to subsequent pos

    const Self = @This();
    const window_size = 1 << 15; // deflate specification states that search buffer is only 32,768 bytes

    pub fn init(input: []const u8, buf: []i32) LZParser {
        const sz = buf.len;
        assert(sz % 4 == 0);
        // by initializing the data to the negative window size, we get a nice property:
        // the while loop in match which makes sure that the link is within the window works
        @memset(buf[0..], -window_size);
        const partition = sz / 4;
        const ht = buf[0..partition];
        const chain = buf[partition..];

        return .{
            .buf = input,
            .curr = 0,
            .hashtable = ht,
            .chain = chain,
        };
    }

    // TODO: replace this hash algorithm
    fn hash(string: [3]u8) usize {
        const k: u24 = @bitCast(string);
        const mask = 0xff;
        const prime = 1009;
        return @mod(k, prime) & mask;
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
        //TODO: match should be a u9, because of possibility of value > 255
        return longest_match;
    }

    fn tally(self: *Self, stat: *Stat, tape: *Tape) !void {
        var i: usize = 0;
        var offset: i32 = undefined;
        const text_end = self.buf.len - 3;
        var literals_count: u24 = 0;
        while (i < text_end) {
            const length = self.match(i, &offset);
            if (length != 0) {
                if (literals_count > 0) {
                    tape.append(.{ .t = 'l', .d = .{ .literal = literals_count } });
                    literals_count = 0;
                }
                const distance: u16 = @as(u16, @truncate(i)) - @as(u16, @intCast(offset));
                tape.append(.{ .t = 'd', .d = .{ .distlen = .{ .distance = distance, .length = length } } });
                stat.distances[dist_to_code(distance)] += 1;
                stat.lengths[length] += 1;
                i += length + 3;
            } else {
                stat.literals[self.buf[i]] += 1;
                literals_count += 1;
                i += 1;
            }
        }
    }
};

// TODO: create a huffman tree of the literals/length and the distances
// TODO: aggregate statistics on the frequency of literals & <distance, length> pointers
//   how do I do this?
test LZParser {
    const text =
        \\ she sells seashells by the seashore. peter piper picked a peck
        \\ of peckled peppers a peck of picked peppers peter piper picked. if
        \\ peter piper picked a peck of pickeled peppers, where is the peck of
        \\ picked peppers, peter piper picked.
    ;
    var data: [8192]i32 = undefined;
    var stat: Stat = undefined;
    @memset(&stat.literals, 0);
    @memset(&stat.lengths, 0);
    @memset(&stat.distances, 0);
    var segments: [1 << 12]Segment = undefined;
    std.debug.print("sizeof Segment: {d}\n", .{@sizeOf(Segment)});
    std.debug.print("sizeof u32: {d}\n", .{@sizeOf(u32)});
    var tape = Tape.init(segments[0..]);
    var parser = LZParser.init(text[0..], data[0..]);
    try parser.tally(&stat, &tape);
    for (0..20) |i| {
        const seg = tape.buf[i];
        if (seg.t == 'l') {
            std.debug.print("literal: {d}\n", .{seg.d.literal});
        } else {
            std.debug.print("distance: {d}, length: {d}\n", .{ seg.d.distlen.distance, seg.d.distlen.length });
        }
    }
    std.debug.print("freq: {any}\n", .{stat});
}
