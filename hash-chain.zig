/// create a hash chain data structure
const std = @import("std");
const mem = std.mem;

/// in the deflate specification, the maximum distance is 32,768 (§3.2.5)
const KeyNode = struct {
    key: [3]u8, // 3 bytes
    link: ?u24, // 3 bytes, can index into a total of (256**3 / 1 << 24) possible values
};

const HashChain = struct {
    table: []?KeyNode,
    empty: usize, // a ptr to an empty slot in the buffer

    const Self = @This();

    fn init(buf: []?KeyNode) HashChain {
        return .{
            .table = buf,
            .empty = buf.len - 1, // point to the last slot in the buffer
        };
    }

    fn hash(string: [3]u8) usize {
        const k: u24 = @bitCast(string);
        return @mod(k, 1009);
    }

    fn search(self: *Self, string: [3]u8) bool {
        // hash the item
        var i = @mod(hash(string), self.table.len);
        while (self.table[i]) |n| {
            if (mem.eql(u8, &n.key, &string))
                return true;
            if (n.link) |link| {
                i = link;
                continue;
            }
            while (self.table[self.empty]) |_|
                self.empty -= 1; // it should panic if there's not enough space
            i = self.empty;
        }
        self.table[i] = .{
            .key = string,
            .link = null,
        };
        return false;
    }
};

test HashChain {
    const text =
        \\ she sells seashells by the seashore. peter piper picked a peck
        \\ of peckled peppers a peck of picked peppers peter piper picked. if
        \\ peter piper picked a peck of pickeled peppers, where is the peck of
        \\ picked peppers, peter piper picked.
    ;
    var buf = [_]?KeyNode{null} ** 256;
    var hashchain = HashChain.init(buf[0..]);
    {
        var i: usize = 0;
        while (i < text.len - 4) : (i += 1) {
            const three = text[i..][0..3].*;
            const b = hashchain.search(three);
            if (b)
                std.debug.print("\"{s}\" already exists\n", .{three})
            else
                std.debug.print("\"{s}\" inserted\n", .{three});
        }
    }
    for (buf) |b| {
        if (b) |x|
            std.debug.print("{s}->{d}\n", .{ x.key, if (x.link) |l| l else 0 })
        else
            std.debug.print("null\n", .{});
    }
}
