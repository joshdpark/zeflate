/// https://fgiesen.wordpress.com/2018/02/20/reading-bits-in-far-too-many-ways-part-2/
const std = @import("std");
const io = std.io;
const assert = std.debug.assert;

pub fn BitReader(comptime ReaderType: type) type {
    return struct {
        reader: ReaderType,
        buf: [4096]u8 = undefined,
        bitptr: usize = 0, // ptr to the stream
        bitbuf: u64 = 0, // bit buffer
        bitcount: u6 = 0, // number of bits in bitbuf

        const Self = @This();

        // initialize the buf and the bitbuf
        pub fn initialize(self: *Self) !void {
            _ = try self.reader.read(self.buf[0..]);
            self.refill();
        }

        pub fn refill(self: *Self) void {
            // grab the next word and insert them right above the current top
            self.bitbuf |= self.read64() << self.bitcount;

            // advance the ptr for next iteration
            self.bitptr += (63 - self.bitcount) >> 3;

            // update the bitcount
            self.bitcount |= 56; // 0b111000; bitcount is in [56,63)
        }

        fn read64(self: *Self) u64 {
            if (self.bitptr + 8 > self.buf.len) {
                _ = self.lookahead();
            }
            return @bitCast(self.buf[self.bitptr..][0..8].*);
        }

        // read from stream into the buf
        fn lookahead(self: *Self) usize {
            const remaining = self.buf.len - self.bitptr;
            @memcpy(self.buf[0..remaining], self.buf[self.bitptr..][0..remaining]);
            // read into the top buffer
            const left = self.reader.read(self.buf[remaining..]) catch @panic("read error");
            self.bitptr = 0; // reset bitptr
            return left;
        }

        pub fn peek_msb(self: *Self, n: u6) u64 {
            assert(n > 0);

            return @bitReverse(self.bitbuf << (63 - n + 1));
        }

        pub fn peek_lsb(self: *Self, count: u6) u16 {
            assert(count >= 0 and count <= 56);
            assert(count <= self.bitcount);

            const mask: u64 = (@as(u64, 1) << count) - 1;
            return @intCast(self.bitbuf & mask);
        }

        pub fn consume(self: *Self, count: u6) void {
            assert(count <= self.bitcount);

            self.bitbuf >>= count;
            self.bitcount -= count;
        }

        pub fn getbits(self: *Self, count: u6) u64 {
            const bits = self.peek_lsb(count);
            self.consume(count);
            return bits;
        }
    };
}

pub fn bitReader(reader: anytype) BitReader(@TypeOf(reader)) {
    return .{ .reader = reader };
}
