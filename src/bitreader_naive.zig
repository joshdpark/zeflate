const std = @import("std");
const assert = std.debug.assert;

fn BitReader(comptime ReaderType: type) type {
    return struct {
        buf: u64 = 0,
        bitcount: u6 = 0,
        reader: ReaderType,

        const Self = @This();

        pub inline fn refill(self: *Self) void {
            while (self.bitcount < 56) {
                const byte: u64 = self.reader.readByte() catch {
                    //TODO: handle end of stream error
                    return;
                };
                self.buf |= byte << self.bitcount;
                self.bitcount += 8;
            }
        }

        // get n lowest bits in the bitbuf in lsb order.
        pub inline fn peek_lsb(self: *Self, n: u6) u64 {
            const mask = (@as(u64, 1) << n) - 1;
            return self.buf & mask;
        }

        // get n lowest bits in the bitbuf in msb order.
        pub inline fn peek_msb(self: *Self, n: u6) u64 {
            var code: u64 = 0;
            var buf = self.buf;
            for (0..n) |_| {
                code <<= 1;
                code |= buf & 1;
                buf >>= 1;
            }
            return code;
        }

        pub inline fn consume(self: *Self, n: u6) void {
            if (n <= self.bitcount)
                self.refill();
            self.buf >>= n;
            self.bitcount -= n;
        }

        pub fn getbits(self: *Self, n: u6) u64 {
            if (n == 0)
                return 0;
            if (self.bitcount < n)
                self.refill();
            const value: u64 = self.peek_lsb(n);
            self.consume(n);
            return value;
        }

        pub fn getcode(self: *Self, n: u6) u64 {
            if (self.bitcount < n)
                self.refill();
            var code: u64 = 0;
            for (0..n) |_| {
                code <<= 1;
                code |= self.buf & 1;
                self.buf >>= 1;
            }
            self.bitcount -= n;
            return code;
        }
    };
}

pub fn bitReader(reader: anytype) BitReader(@TypeOf(reader)) {
    return .{ .reader = reader };
}
