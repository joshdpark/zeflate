/// https://fgiesen.wordpress.com/2018/02/20/reading-bits-in-far-too-many-ways-part-2/
/// A general byte reader is essentially the first part of this blog post. This
/// implementation is variant 2-4;
const std = @import("std");
const io = std.io;
const assert = std.debug.assert;

const width_to_mask_table = blk: {
    var table: [64]u64 = undefined;
    inline for (&table, 1..) |*val, i| {
        val.* = (1 << @as(u6, @truncate(i))) - 1;
    }
    table[63] = std.math.maxInt(u64);
    break :blk table;
};

test width_to_mask_table {
    // uncomment to see all the masks
    // std.debug.print("\n", .{});
    // for (width_to_mask_table, 0..) |item, i| {
    //     std.debug.print("i: {d: >2} {b:0>64}\n", .{ i, item });
    // }
    try std.testing.expect(width_to_mask_table[2] == 0x7);
}

fn get64bits(reader: anytype) u64 {
    // var result: u64 = 0;
    // var i: u6 = 0;
    // while (i < 8) : (i += 1) {
    //     const byte: u64 = reader.readByte() catch
    //         return 0;
    //     result |= byte << (i * 8);
    // }
    // return result;
    return reader.readInt(u64, .little) catch 0;
}

test get64bits {
    const str = [_]u8{0xf0} ** 20;
    var r = io.fixedBufferStream(str[0..]);
    var bf = io.bufferedReader(r.reader());
    const s = bf.reader();
    std.debug.print("bitbuf: {b:0>64}", .{get64bits(&s)});
}

/// the implementation for this variant uses 2 buffers, a bit buffer for consuming, and a
/// lookahead buffer. The use of these two buffers allows for a rotation so that you
/// can consume a large amount of bits without having to stop and wait for an io action.
fn Variant2(comptime ReaderType: type) type {
    return struct {
        bitbuf: u64 = 0,
        bitcount: i32 = 0,
        lookahead: u64 = 0,
        have_lookahead: bool = false,
        reader: ReaderType,

        const Self = @This();

        fn init(self: *Self) void {
            self.bitbuf = get64bits(self.reader);
            self.bitcount = 64;
        }

        fn ensure_lookahead(self: *Self) void {
            if (!self.have_lookahead) {
                self.lookahead = get64bits(self.reader);
                self.have_lookahead = true;
            }
        }

        fn peek(self: *Self, count: i32) u64 {
            assert(self.bitcount >= 1);
            assert(count >= 1 and count <= 64);
            if (count <= self.bitcount) {
                // (count - 1) -> see test for width_to_mask_table
                return self.bitbuf & width_to_mask_table[@as(usize, @intCast(count)) - 1];
            } else {
                self.ensure_lookahead();
                // combine bitbuf with lookahead through effectively a rotate
                var next_bits = self.bitbuf;
                next_bits |= self.lookahead << @as(u6, @intCast(self.bitcount));
                return next_bits & width_to_mask_table[@as(usize, @intCast(count)) - 1];
            }
        }

        fn consume(self: *Self, count: i32) void {
            assert(self.bitcount >= 1);
            assert(count >= 0 and count <= 64);

            if (count < self.bitcount) {
                self.bitbuf >>= @as(u6, @intCast(count));
                self.bitcount -= @as(u6, @intCast(count));
            } else {
                self.ensure_lookahead();

                // advance into lookahead buffer
                const lookahead_consumed: usize = @as(usize, @intCast(count)) - @as(usize, @intCast(self.bitcount));
                self.bitbuf = self.lookahead >> @as(u6, @intCast(lookahead_consumed));
                self.bitcount = @as(u6, @intCast(64 - lookahead_consumed));
                self.have_lookahead = false;
            }

            assert(self.bitcount >= 1);
        }

        fn getbits(self: *Self, count: i32) u64 {
            const result = self.peek(count);
            self.consume(count);
            return result;
        }
    };
}

pub fn variant2(reader: anytype) Variant2(@TypeOf(reader)) {
    return .{ .reader = reader };
}

test Variant2 {
    const str = [_]u8{0xff} ** 256;
    var r = io.fixedBufferStream(str[0..]);
    var bf = io.bufferedReader(r.reader());
    const s = bf.reader();
    var br = variant2(s);
    br.init();
    const bitnums = [_]usize{ 42, 23, 38, 21, 64 };
    std.debug.print("\n", .{});
    inline for (bitnums) |n| {
        std.debug.print("{b:0>64}\n", .{br.getbits(n)});
        const ws = [_]u8{' '} ** (64 - n) ++ "^~~";
        std.debug.print("{s}\n", .{ws});
    }
}

fn Variant3(comptime ReaderType: type) type {
    return struct {
        bitptr: usize = 0, // index to current byte
        bitbuf: u64 = 0, // last 64 bits we read
        bitpos: usize = 0, // how may of those bits we've consumed
        reader: ReaderType,

        const Self = @This();

        pub fn refill(self: *Self) !void {
            assert(self.bitpos <= 64);

            // advance index by number of full bytes consumed
            self.bitptr += self.bitpos >> 3;

            // refill
            self.bitbuf = try self.read64se();

            // leftover bits that weren't consumed
            self.bitpos &= 7;
        }

        pub fn peek(self: *Self, count: usize) u64 {
            assert(self.bitpos + count <= 64);
            assert(count >= 1 and count <= 64 - 7);

            // shift out bits we've already consumed
            const remaining: u64 = self.bitbuf >> @truncate(self.bitpos);

            // return the bottom 'count' bits
            // const mask: u64 = (@as(u64, 1) << @as(u6, @truncate(count))) - 1;
            // return remaining & mask;
            return blk: {
                var rem = remaining;
                var r: u64 = 0;
                for (0..count) |_| {
                    r <<= 1;
                    r |= rem & 1;
                    rem >>= 1;
                }
                break :blk r;
            };
        }

        pub fn consume(self: *Self, count: usize) void {
            self.bitpos += count;
        }

        pub fn getbits(self: *Self, count: usize) !u64 {
            if (self.bitpos + count > 64)
                try self.refill();
            const result = self.peek(count);
            self.consume(count);
            return result;
        }

        // se: small endian
        fn read64se(self: *@This()) !u64 {
            try self.reader.seekTo(self.bitptr);
            return self.reader.reader().readInt(u64, .little);
        }
    };
}

pub fn variant3(reader: anytype) Variant3(@TypeOf(reader)) {
    return .{ .reader = reader };
}

test Variant3 {
    const str = [_]u8{0x99} ** 256;
    var r = io.fixedBufferStream(str[0..]);
    var br = variant3(r);
    try br.refill();
    const bitnums = [_]usize{ 7, 8, 10, 14, 50 };
    std.debug.print("\n", .{});
    inline for (bitnums) |n| {
        std.debug.print("{b:0>64}\n", .{br.getbits(n) catch 0});
        const ws = [_]u8{' '} ** (64 - n) ++ "^~~";
        std.debug.print("{s} bitpos:{d}\n", .{ ws, br.bitpos });
    }
}

fn Variant4(comptime ReaderType: type) type {
    return struct {
        bitptr: usize = 0, // ptr to the stream
        bitbuf: u64 = 0, // bit buffer
        bitcount: u6 = 0, // number of bits in bitbuf
        reader: ReaderType,

        const Self = @This();

        pub fn refill(self: *Self) void {
            // grab the next word and insert them right above the current top
            self.bitbuf |= self.read64() << self.bitcount;

            // advance the ptr for next iteration
            self.bitptr += (63 - self.bitcount) >> 3;

            // update the bitcount
            self.bitcount |= 56; // 0b111000; bitcount is in [56,63)
        }

        fn read64(self: *Self) u64 {
            self.reader.seekTo(self.bitptr) catch return;
            return self.reader.reader().readInt(u64, .little) catch 0;
        }

        fn peek(self: *Self, count: u6) u64 {
            assert(count >= 0 and count <= 56);
            assert(count <= self.bitcount);

            const mask: u64 = (@as(u64, 1) << count) - 1;

            return self.bitbuf & mask;
        }

        fn consume(self: *Self, count: u6) void {
            assert(count <= self.bitcount);

            self.bitbuf >>= count;
            self.bitcount -= count;
        }

        fn getbits(self: *Self, count: u6) u64 {
            if (count > self.bitcount)
                self.refill();
            const bits = self.peek(count);
            self.consume(count);
            return bits;
        }
    };
}

pub fn variant4(reader: anytype) Variant4(@TypeOf(reader)) {
    return .{ .reader = reader };
}

test variant4 {
    const str = [_]u8{0xff} ** 256;
    var r = io.fixedBufferStream(str[0..]);
    var br = variant4(r);
    br.refill();
    const bitnums = [_]usize{ 7, 8, 10, 14, 30 };
    std.debug.print("\n", .{});
    inline for (bitnums) |n| {
        std.debug.print("{b:0>64}\n", .{br.getbits(n)});
        const ws = [_]u8{' '} ** (64 - n) ++ "^~~";
        std.debug.print("{s}\n", .{ws});
    }
}
