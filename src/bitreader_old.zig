/// read bits into a buffer
const std = @import("std");
const io = std.io;
const debug = std.debug;
const expect = std.testing.expect;

pub const BitReader = struct {
    bitbuf: u64,
    bitcount: u6, // number of bits in bitbuf

    pub fn init() BitReader {
        return .{ .bitbuf = 0, .bitcount = 0 };
    }

    /// read from stream until the bitbuf is filled up or there's an error
    pub fn refill(self: *BitReader, stream: anytype) void {
        while (self.bitcount < 56) {
            const byte: u64 = stream.readByte() catch return;
            self.bitbuf |= byte << self.bitcount;
            self.bitcount += 8;
        }
    }

    /// peek at `count` bits; reverse the bits
    inline fn peek(self: *BitReader, count: u6) u64 {
        return self.bitbuf & ((@as(u64, 1) << count) - 1);
    }

    /// consume count bits in the buffer
    inline fn consume(self: *BitReader, count: u6) void {
        self.bitbuf >>= count;
        self.bitcount -= count;
    }

    pub fn getbits(self: *BitReader, count: u6) u64 {
        const bits = self.peek(count);
        self.consume(count);
        return bits;
    }

    pub fn printbuf(self: *BitReader) void {
        var i: u6 = 0;
        while (i < 8) {
            const v = (self.bitbuf >> (i * 8)) & ((1 << 8) - 1);
            debug.print("byte {}: {b:0>8}\n", .{ i, v });
            i += 1;
        }
    }
};

test BitReader {
    const str = [_]u8{ 0x37, 0xFF, 0xF0, 0xA0, 0x0A };
    var r = io.fixedBufferStream(&str);
    var bf = io.bufferedReader(r.reader());
    const s = bf.reader();
    var bt = BitReader.init();
    bt.refill(&s);
    const fivebits = @as(u5, @truncate(bt.peek(5)));
    debug.print("fivebits: {b:0>5}\n", .{fivebits});
    try expect(fivebits == 0b11101);
    bt.consume(5);
    const twobits = @as(u2, @truncate(bt.peek(2)));
    debug.print("twobits: {b:0>2}\n", .{twobits});
    try expect(twobits == 0b10);
}
