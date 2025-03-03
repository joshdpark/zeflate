# A personal project for implementing DEFLATE

As a way to get familiar with zig and compression, this repository was my
attempt at trying to implement the [DEFLATE RFC](https://www.rfc-editor.org/rfc/rfc1951) from first principles, but
without having any experience in c or in systems programming.

Most of this implementation was naively done but there were parts where I
implemented parts of papers specifically written by Moffat. The bitreader was
inspired by posts by
[fgeisen](https://fgiesen.wordpress.com/2018/02/19/reading-bits-in-far-too-many-ways-part-1/).

Other inspiration came from `libdeflate` and from the encodes forum.
