const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const Program = parser.Parser.Program;
const compiler = @import("compiler.zig");
const c_target = @import("targets.zig").c_target;
const target = c_target;

pub fn main() !void {
    var heap = std.heap.HeapAllocator.init();
    defer heap.deinit();

    const allocator = heap.allocator();

    var files = [_][]const u8{"examples/hello world.lm"};

    var prser: parser.Parser = parser.Parser.initFiles(allocator, &files);

    var program: Program = prser.parse();
    defer program.deinit();

    var output: compiler.Output = compiler.compileProgram(target.target(), allocator, program);
    defer output.deinit();

    saveOutputToFile("examples/bin/hello world.c", output);
}

fn saveOutputToFile(path: []const u8, output: compiler.Output) void {
    var file = std.fs.cwd().createFile(path, .{}) catch unreachable;
    defer file.close();

    var buf_writer = std.io.bufferedWriter(file.writer());
    var out_stream = buf_writer.writer();

    _ = out_stream.writeAll(output.code) catch unreachable;

    buf_writer.flush() catch unreachable;
}
