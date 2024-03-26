const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const assert = std.debug.assert;
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const parseNumLit = parser.Parser.parseNumLit;

const Program = parser.Parser.Program;
const Module = parser.Parser.Module;
const Expr = parser.Parser.Expr;
const Stmt = parser.Parser.Stmt;
const Number = parser.Parser.Number;
const ProcCall = parser.Parser.ProcCall;

pub const TypeChecker = struct {
    allocator: mem.Allocator,
    arena: heap.ArenaAllocator,

    pub fn init(allocator: mem.Allocator) TypeChecker {
        const type_checker: TypeChecker = .{
            .allocator = allocator,
            .arena = heap.ArenaAllocator.init(allocator),
        };

        return type_checker;
    }

    pub fn deinit(self: *TypeChecker) void {
        self.arena.deinit();
    }

    pub const SignedIntType = enum {
        i8,
        i16,
        i32,
        i64,
    };

    pub const UnsignedIntType = enum {
        u8,
        u16,
        u32,
        u64,
    };

    pub const IntType = union(enum) {
        signed: SignedIntType,
        unsigned: UnsignedIntType,
        comptime_num: Number,
    };

    pub const FloatType = enum {
        f32,
        f64,
    };

    pub const NumberType = union(enum) {
        int: IntType,
        float: FloatType,
    };

    pub const Type = struct {
        modifiers_mask: u64,
        as: TypeAs,
    };

    const TypeModifier = enum(u64) {
        none = 0x0,
        constant = 0x1,
    };

    const TypeAs = union(enum) {
        bool: void,
        number: NumberType,
        string: void,
        pointer: *Type,
    };

    pub fn parseType(lxer: *lexer.Lexer) *Type {
        _ = lxer;
        unreachable;
    }

    fn getProcType(proc_name: []const u8) *Type {
        _ = proc_name;
        unreachable;
    }

    pub fn checkProcCall(proc_call: ProcCall) *Type {
        _ = proc_call;
        unreachable;
    }

    fn getVarType(var_name: []const u8) *Type {
        _ = var_name;
        unreachable;
    }

    fn checkBinary(binary: Expr.Binary) *Type {
        _ = binary;
        unreachable;
    }

    pub fn getExprType(self: *TypeChecker, expr: *Expr) *Type {
        var result: *Type = self.arena.allocator().create(Type) catch unreachable;

        switch (expr.as) {
            .num => |num| {
                result.* = .{ .number = .{ .comptime_num = .{parseNumLit(num)} } };
            },
            .bool => {
                result.* = .{ .bool = {} };
            },
            .lit_str => {
                result.* = .{ .string = {} };
            },
            .proc_call => |proc_call| {
                result.* = checkProcCall(proc_call);
            },
            .binary => |binary| {
                result.* = checkBinary(binary);
            },
            .var_read => {
                result.* = getVarType(expr.var_read.name);
            },
        }

        return result;
    }

    pub fn checkStmtType(stmt: Stmt) void {
        _ = stmt;
        unreachable;
    }
};
