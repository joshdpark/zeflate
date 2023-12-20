/// Play with creating a huffman tree representation, first doing the naive
/// method of just using a basic tree with pointers, to building the canonical
/// huffman tree and then representing the tree in smaller and smaller ways that
/// can speed up the compression/decompression
const std = @import("std");
const PriorityQueue = std.PriorityQueue;
const Allocator = std.mem.Allocator;
const Order = std.math.Order;

const huffnode = struct {
    code: u8,
    freq: u32,
    // leaf nodes have null branches since huffman trees are always full
    branch: ?[2]*huffnode,
};

/// keeping track of both the bitlen (the depth of the tree) as well as the
/// doing a postorder traversal of the tree. Should write results to an
/// associated list of bitlens and their codes.
/// TODO: add in the associated list
fn postorder(root: *huffnode) void {
    var stack: [100]*huffnode = undefined;
    var bitlens = [_]u8{0} ** 100;
    var items: usize = 0;
    stack[items] = root;
    items += 1;
    while (true) {
        if (items == 0)
            break;
        // pop the stack
        const node = stack[items - 1];
        const bitlen = &bitlens[items - 1];
        items -= 1;
        if (node.branch) |branch| {
            bitlen.* += 1;
            // add branches to stack
            for (branch) |b| {
                stack[items] = b;
                bitlens[items] = bitlen.*;
                items += 1;
            }
        } else {
            std.debug.print("bitlen: {d}, code: {c}\n", .{ bitlen.*, node.code });
        }
    }
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

fn decode(reader: anytype, root: *huffnode) void {
    var bit_stream = std.io.bitReader(.little, reader);
    var node = root;
    while (true) {
        if (node.branch) |branch| {
            const bit = bit_stream.readBitsNoEof(u1, 1) catch
                return;
            std.debug.print("{b}", .{bit});
            node = branch[bit];
        } else {
            std.debug.print(" code: {c}\n", .{node.code});
            node = root; // go back to the root node
        }
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
    const str = "aaaabbbccccccdddddddeeeeeeeeeee";

    freq_table(str, buf[0..]);
    // std.debug.print("freq_table: {s}\n", .{buf[0..]});
    const root = try build_tree(buf[0..], allocator);
    postorder(root);
    const ex = [_]u8{0b0011110};
    var in_mem = std.io.fixedBufferStream(ex[0..]);
    std.debug.print("{b:0>8}\n", .{ex});
    decode(in_mem.reader(), root);
}
