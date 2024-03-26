const std = @import("std");

const Level = enum {
    info,
    warn,
    error_,

    fn toString(self: Level) []const u8 {
        return switch (self) {
            .info => "INFO",
            .warn => "WARN",
            .error_ => "ERROR",
        };
    }
};

const Loc = struct {
    file: []const u8,
    line: usize,
    col: usize,
};

const Msg = struct {
    level: Level,
    text: []const u8,
    loc: Loc,

    fn init(level: Level, text: []const u8, loc: Loc) Msg {
        return .{ .level = level, .text = text, .loc = loc };
    }

    fn Log(self: Msg) void {
        std.debug.print("{s}:{}:{}: {s}: {s}\n", .{
            self.loc.file,
            self.loc.line,
            self.loc.col,
            self.level.toString(),
            self.text,
        });
    }
};
