const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const parser = @import("parser.zig");
const Program = parser.Parser.Program;

pub const Target = struct {
    ptr: *anyopaque,
    compileFn: *const fn (ptr: *anyopaque, allocator: Allocator, program: Program) Output,

    fn compile(self: Target, allocator: Allocator, program: Program) Output {
        return self.compileFn(self.ptr, allocator, program);
    }
};

pub const Output = struct {
    allocator: std.mem.Allocator,
    code: []u8,

    pub fn init(allocator: std.mem.Allocator, code: []u8) Output {
        const output: Output = Output{
            .allocator = allocator,
            .code = code,
        };

        return output;
    }

    pub fn deinit(self: *Output) void {
        self.allocator.free(self.code);
    }
};

pub fn compileProgram(target: Target, allocator: Allocator, program: Program) Output {
    return target.compile(allocator, program);
}
