/// Play with creating a huffman tree representation, first doing the naive
/// method of just using a basic tree with pointers, to building the canonical
/// huffman tree and then representing the tree in smaller and smaller ways that
/// can speed up the compression/decompression
const std = @import("std");
const assert = std.debug.assert;
const bitReader = @import("bitreader.zig").variant3;
const PriorityQueue = std.PriorityQueue;
const Allocator = std.mem.Allocator;
const Order = std.math.Order;

fn filo(comptime T: type) type {
    return struct {
        data: []T,
        available: u16,
        start: u16,
        end: u16,

        fn init(buf: []T) @This() {
            return .{ .data = buf, .available = @intCast(buf.len), .start = 0, .end = 0 };
        }

        fn empty(self: *@This()) bool {
            return self.data.len == self.available;
        }

        fn insert(self: *@This(), item: T) void {
            assert(self.available > 0);
            self.data[self.end] = item;
            if (self.end + 1 == self.data.len) {
                self.end = 0;
            } else {
                self.end += 1;
            }
            self.available -= 1;
        }

        fn remove(self: *@This()) T {
            assert(self.available < self.data.len);
            const item = self.data[self.start];
            if (self.start + 1 == self.data.len) {
                self.start = 0;
            } else {
                self.start += 1;
            }
            self.available += 1;
            return item;
        }
    };
}

test filo {
    var buf: [10]i32 = undefined;
    const itemq = filo(i32);
    var q = itemq.init(buf[0..]);
    q.insert(10);
    q.insert(-32);
    try std.testing.expect(10 == q.remove());
    std.debug.print("\n{any}\n", .{q.remove()});
}

const huffnode = struct {
    code: u8,
    freq: u32,
    // leaf nodes have null branches since huffman trees are always full
    branch: ?[2]*huffnode,
};

const HuffEntry = struct {
    sym: u8,
    bitlen: u8,
};

/// keeping track of both the bitlen (the depth of the tree) as well as the
/// doing a traversal of the tree. Should write results to an associated list of
/// bitlens and their codes.
fn traverse(root: *huffnode, bit_counts: []u8, codelist: []HuffEntry) u8 {
    const data = struct {
        node: *huffnode,
        bitlen: u8,
    };
    const dataq = filo(data);

    var max_bitlen: u8 = 0;
    var buf: [100]data = undefined;
    var queue = dataq.init(buf[0..100]);
    var i: usize = 0;
    queue.insert(data{ .node = root, .bitlen = 0 });
    while (!queue.empty()) {
        const d = queue.remove();
        if (d.node.branch) |branch| {
            for (branch) |b| {
                queue.insert(data{ .node = b, .bitlen = d.bitlen + 1 });
            }
        } else {
            // std.debug.print("sym: {c}, bitlen: {d}, \n", .{ d.node.code, d.bitlen });
            codelist[i] = .{ .sym = d.node.code, .bitlen = d.bitlen };
            bit_counts[d.bitlen] += 1;
            max_bitlen = @max(max_bitlen, d.bitlen);
            i += 1;
        }
    }
    return max_bitlen;
}

fn lessThan(context: void, a: *huffnode, b: *huffnode) Order {
    _ = context;
    return std.math.order(a.freq, b.freq);
}

/// build a frequecy table from bytes, pass in a static array of 256 u8 bytes
/// (initialize them all the 0) and just scan the content and increase the count
/// by 1;
fn freq_table(input: []const u8, buf: []u8) void {
    for (input) |c| {
        buf[c] += 1;
    }
}

fn build_tree(buf: []u8, allocator: Allocator) !*huffnode {
    const pq_t = PriorityQueue(*huffnode, void, lessThan);
    var pq = pq_t.init(allocator, {});
    defer pq.deinit();
    var i: u16 = 0; // this feels awful, I want to use u8, but get overflow otherwise

    // add all symbols as nodes
    while (i < buf.len) : (i += 1) {
        if (buf[i] == 0)
            continue;
        // std.debug.print("i: {d}\n", .{i});
        var n = try allocator.create(huffnode);
        n.* = .{ .code = @as(u8, @truncate(i)), .freq = buf[i], .branch = null };
        try pq.add(n);
    }
    var root: *huffnode = undefined;
    // pop off top two nodes in the queue and combine them and then add back
    // into the queue unless the queue is empty.
    while (true) {
        const fst = pq.remove();
        const snd = pq.remove();
        // std.debug.print("fst: {d}, snd {d}\n", .{ fst.freq, snd.freq });
        var n = try allocator.create(huffnode);
        n.* = .{ .code = 0, .freq = fst.freq + snd.freq, .branch = .{ fst, snd } };
        root = n;
        if (pq.peek() != null) {
            try pq.add(n);
        } else {
            break;
        }
    }
    return root;
}

/// TODO: instead of descending down the tree, you need to instead use a table
/// of huffman values that use the max_bitlen; Since these are prefix codes, you
/// can store all leaves in the tree as entries in a table that are indexed by
/// their bitcode
fn decode(reader: anytype, huff_table: []HuffEntry) !void {
    var bit_reader = bitReader(reader);
    try bit_reader.refill();
    std.debug.print("bitbuf: {b:0>64}\n", .{bit_reader.bitbuf});
    inline for (0..3) |_| {
        const bits = bit_reader.peek(4);
        // std.debug.print("bits: {b:0>4}\n", .{bits});
        const entry = huff_table[bits];
        std.debug.print("bits: {b}, sym: {c}\n", .{ bits, entry.sym });
        bit_reader.consume(entry.bitlen);
    }
}

/// generate a canonical prefix encoding
fn canonical(root: *huffnode, huff_table: []HuffEntry) void {
    var bl_counts = [_]u8{0} ** 16;
    var tree = [_]HuffEntry{.{ .sym = 0, .bitlen = 0 }} ** 16;
    // zero out canon table
    for (huff_table) |*c| {
        c.* = HuffEntry{ .sym = 0, .bitlen = 0 };
    }
    const max = traverse(root, bl_counts[0..], tree[0..]);
    // std.debug.print("bl_count: {any}\n", .{ bl_counts, tree });
    var next_code = [_]u16{0} ** 15;
    std.debug.print("bl_counts: {d}, max: {d}\n", .{ bl_counts, max });
    {
        var code: u16 = 0;
        bl_counts[0] = 0;
        var bits: u16 = 1;
        while (bits <= max) : (bits += 1) {
            code = (code + bl_counts[bits - 1]) << 1;
            std.debug.print("N: {d}, code[N]: {b}\n", .{ bits, code });
            next_code[bits] = code;
        }
    }
    for (tree) |t| {
        const len = t.bitlen;
        if (len != 0) {
            huff_table[next_code[len] << @as(u4, @truncate(max - len))] = t;
            next_code[len] += 1;
        }
    }
    {
        var i: usize = 1;
        while (i < huff_table.len) : (i += 1) {
            const prev: HuffEntry = huff_table[i - 1];
            if (huff_table[i].bitlen == 0)
                huff_table[i] = prev;
        }
    }
    for (0.., huff_table) |i, t| {
        // if (t.bitlen == 0)
        //     continue;
        std.debug.print("code: {b}, symbol: {c}, bitlen: {d}\n", .{ i, t.sym, t.bitlen });
    }
}

//TODO: make the huffman tree canonical. This requires doing 2 things:
// 1. codes are in order of their bit length
// 2. for codes with the same bit length, they are ordered by their bit
// representation. eg 'C' > 'A' and 'k' < 'z'

pub fn main() !void {
    var buf: [256]u8 = [_]u8{0} ** 256;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const str = "aaaabbbbccccddddeeeeeffffffgghh";

    freq_table(str, buf[0..]);
    // std.debug.print("freq_table: {s}\n", .{buf[0..]});
    const root = try build_tree(buf[0..], allocator);
    var huff_table = [_]HuffEntry{.{ .sym = 0, .bitlen = 0 }} ** 16;
    canonical(root, huff_table[0..]);
    // var bl_count = [_]u8{0} ** 15;
    // traverse(root, bl_count[0..]);
    // std.debug.print("{any}\n", .{bl_count});
    const ex = [_]u8{ 0b10100011, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x5a };
    var in_mem = std.io.fixedBufferStream(ex[0..]);
    std.debug.print("{b:0>8}\n", .{ex});
    try decode(in_mem, huff_table[0..]);
}
