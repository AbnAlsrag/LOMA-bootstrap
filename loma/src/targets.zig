const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const parser = @import("parser.zig");
const Program = parser.Parser.Program;
const Module = parser.Parser.Module;
const TopLevel = parser.Parser.TopLevel;
const TopLevelAs = parser.Parser.TopLevelAs;
const ProcDecl = parser.Parser.ProcDecl;
const VarDecl = parser.Parser.VarDecl;
const VarAssign = parser.Parser.VarAssign;
const VarAssignType = parser.Parser.VarAssignType;
const Block = parser.Parser.Block;
const Stmt = parser.Parser.Stmt;
const Condition = parser.Parser.Condition;
const If = parser.Parser.If;
const Else = parser.Parser.Else;
const Switch = parser.Parser.Switch;
const Cases = parser.Parser.Cases;
const Case = parser.Parser.Case;
const CaseBody = parser.Parser.CaseBody;
const While = parser.Parser.While;
const For = parser.Parser.For;
const Range = parser.Parser.Range;
const Defer = parser.Parser.Defer;
const Binary = parser.Parser.Binary;
const BinaryOp = parser.Parser.BinaryOp;
const Number = parser.Parser.Number;
const Expr = parser.Parser.Expr;
const ProcCallArgs = parser.Parser.ProcCallArgs;
const ProcCallArg = parser.Parser.ProcCallArg;
const VarRead = parser.Parser.VarRead;
const compiler = @import("compiler.zig");
const Output = compiler.Output;
const Target = compiler.Target;

pub const c_target = struct {
    const DEFER_STACK_MAX = 5000;

    var defer_stack: [DEFER_STACK_MAX]Defer = undefined;
    var defer_stack_size: usize = 0;

    fn pushDefer(defer_: Defer) void {
        assert(defer_stack_size < DEFER_STACK_MAX);

        defer_stack[defer_stack_size] = defer_;
        defer_stack_size += 1;
    }

    fn popDefer() Defer {
        assert(defer_stack_size > 0);

        defer_stack_size -= 1;
        return defer_stack[defer_stack_size];
    }

    fn compileFn(ptr: *anyopaque, allocator: Allocator, program: Program) Output {
        _ = ptr;
        var output: Output = Output{
            .allocator = allocator,
            .code = undefined,
        };

        var buf: std.ArrayList(u8) = std.ArrayList(u8).init(allocator);

        for (program.modules[0..program.module_count]) |module| {
            c_target.compileModule(&buf, module);
        }

        output.code = buf.toOwnedSlice() catch unreachable;

        return output;
    }

    fn compileModule(buffer: *std.ArrayList(u8), module: Module) void {
        buffer.writer().print("#include <stdlib.h>\n", .{}) catch unreachable;
        buffer.writer().print("#include <stdio.h>\n", .{}) catch unreachable;
        buffer.writer().print("#include <stdbool.h>\n", .{}) catch unreachable;
        buffer.writer().print("\n", .{}) catch unreachable;

        if (module.begin == null) return;

        var top: ?*TopLevel = module.begin;
        while (top) |t| {
            c_target.compileTopLevel(buffer, t);
            top = t.next;
        }
    }

    fn compileTopLevel(buffer: *std.ArrayList(u8), top: *TopLevel) void {
        switch (top.as) {
            .proc_decl => |proc_decl| {
                c_target.compileProcDecl(buffer, proc_decl);
            },
            .var_decl => |var_decl| {
                c_target.compileVarDecl(buffer, var_decl);
            },
        }

        buffer.writer().print("\n", .{}) catch unreachable;
    }

    fn compileProcDecl(buffer: *std.ArrayList(u8), proc_decl: ProcDecl) void {
        buffer.writer().print("int {s}() ", .{proc_decl.name}) catch unreachable;

        c_target.compileBlock(buffer, proc_decl.body, 1);
    }

    fn compileBlock(buffer: *std.ArrayList(u8), block: Block, tabs: usize) void {
        buffer.writer().print("{c}\n", .{'{'}) catch unreachable;

        var stmt: ?*Stmt = block.begin;
        while (stmt) |s| {
            c_target.compileStmt(buffer, s, tabs);
            stmt = s.next;
        }

        for (0..block.defer_count) |_| {
            c_target.compileDefer(buffer, c_target.popDefer(), tabs);
        }

        c_target.writeTabs(buffer, tabs - 1);

        buffer.writer().print("{c}", .{'}'}) catch unreachable;
    }

    fn compileDefer(buffer: *std.ArrayList(u8), defer_: Defer, tabs: usize) void {
        c_target.compileStmt(buffer, defer_.stmt, tabs);
    }

    fn compileStmt(buffer: *std.ArrayList(u8), stmt: *Stmt, tabs: usize) void {
        switch (stmt.as) {
            .expr => |expr| {
                c_target.writeTabs(buffer, tabs);

                c_target.compileExpr(buffer, expr);
                buffer.writer().print(";", .{}) catch unreachable;
            },
            .if_ => |if_| {
                c_target.writeTabs(buffer, tabs);

                c_target.compileIf(buffer, if_, tabs);
            },
            .switch_ => |switch_| {
                c_target.writeTabs(buffer, tabs);

                c_target.compileSwitch(buffer, switch_, tabs);
            },
            .while_ => |while_| {
                c_target.writeTabs(buffer, tabs);

                c_target.compileWhile(buffer, while_, tabs);
            },
            .for_ => |for_| {
                c_target.writeTabs(buffer, tabs);

                c_target.compileFor(buffer, for_, tabs);
            },
            .var_decl => |var_decl| {
                c_target.writeTabs(buffer, tabs);

                c_target.compileVarDecl(buffer, var_decl);
            },
            .var_assign => |var_assign| {
                c_target.writeTabs(buffer, tabs);

                c_target.compileVarAssign(buffer, var_assign);
            },
            .defer_ => |defer_| {
                c_target.pushDefer(defer_);
                return;
            },
            .block => |block| {
                c_target.writeTabs(buffer, tabs);

                c_target.compileBlock(buffer, block, tabs + 1);
            },
        }

        buffer.writer().print("\n", .{}) catch unreachable;
    }

    fn compileIf(buffer: *std.ArrayList(u8), if_: If, tabs: usize) void {
        buffer.writer().print("if (", .{}) catch unreachable;

        c_target.compileCondition(buffer, if_.condition);

        buffer.writer().print(") ", .{}) catch unreachable;

        c_target.compileBlock(buffer, if_.body, tabs + 1);

        if (if_.else_) |else_| {
            c_target.compileElse(buffer, else_, tabs);
        }

        buffer.writer().print("\n", .{}) catch unreachable;
    }

    fn compileElse(buffer: *std.ArrayList(u8), else_: Else, tabs: usize) void {
        buffer.writer().print(" else ", .{}) catch unreachable;

        c_target.compileBlock(buffer, else_.body, tabs + 1);
    }

    fn compileSwitch(buffer: *std.ArrayList(u8), switch_: Switch, tabs: usize) void {
        buffer.writer().print("switch (", .{}) catch unreachable;

        c_target.compileExpr(buffer, switch_.main_cond);

        buffer.writer().print(") {c}\n", .{'{'}) catch unreachable;

        c_target.compileSwitchCases(buffer, switch_.cases, tabs);

        c_target.writeTabs(buffer, tabs);

        buffer.writer().print("{c}", .{'}'}) catch unreachable;
    }

    fn compileSwitchCases(buffer: *std.ArrayList(u8), cases: Cases, tabs: usize) void {
        for (cases.cases[0..cases.amount]) |case| {
            c_target.compileSwitchCase(buffer, case, tabs);
        }
    }

    fn compileSwitchCase(buffer: *std.ArrayList(u8), case: Case, tabs: usize) void {
        c_target.writeTabs(buffer, tabs);

        if (case.condition) |condition| {
            buffer.writer().print("case ", .{}) catch unreachable;
            c_target.compileCondition(buffer, condition);
            buffer.writer().print(":\n", .{}) catch unreachable;
        } else {
            buffer.writer().print("default:\n", .{}) catch unreachable;
        }

        c_target.compileSwitchCaseBody(buffer, case.body, tabs + 1);
    }

    fn compileSwitchCaseBody(buffer: *std.ArrayList(u8), body: CaseBody, tabs: usize) void {
        var stmt: ?*Stmt = body.begin;
        while (stmt) |s| {
            c_target.compileStmt(buffer, s, tabs);
            stmt = s.next;
        }

        c_target.writeTabs(buffer, tabs);

        buffer.writer().print("break;\n", .{}) catch unreachable;
    }

    fn compileWhile(buffer: *std.ArrayList(u8), while_: While, tabs: usize) void {
        buffer.writer().print("while (", .{}) catch unreachable;

        c_target.compileCondition(buffer, while_.condition);

        buffer.writer().print(") ", .{}) catch unreachable;

        c_target.compileBlock(buffer, while_.body, tabs + 1);
    }

    fn compileCondition(buffer: *std.ArrayList(u8), condition: Condition) void {
        c_target.compileExpr(buffer, condition.expr);
    }

    fn compileFor(buffer: *std.ArrayList(u8), for_: For, tabs: usize) void {
        buffer.writer().print("for (", .{}) catch unreachable;

        c_target.compileRange(buffer, for_.range);

        buffer.writer().print(") ", .{}) catch unreachable;

        c_target.compileBlock(buffer, for_.body, tabs + 1);
    }

    fn compileRange(buffer: *std.ArrayList(u8), range: Range) void {
        buffer.writer().print("int i =", .{}) catch unreachable;

        c_target.compileExpr(buffer, range.low);

        buffer.writer().print(";", .{}) catch unreachable;

        buffer.writer().print("i < ", .{}) catch unreachable;

        c_target.compileExpr(buffer, range.high);

        buffer.writer().print(";", .{}) catch unreachable;

        buffer.writer().print("i++", .{}) catch unreachable;
    }

    fn compileVarDecl(buffer: *std.ArrayList(u8), var_decl: VarDecl) void {
        buffer.writer().print("int {s} = ", .{var_decl.name}) catch unreachable;

        c_target.compileExpr(buffer, var_decl.value);

        buffer.writer().print(";", .{}) catch unreachable;

        buffer.writer().print("\n", .{}) catch unreachable;
    }

    fn compileVarAssign(buffer: *std.ArrayList(u8), var_assign: VarAssign) void {
        buffer.writer().print("{s}", .{var_assign.name}) catch unreachable;

        c_target.compileVarAssignType(buffer, var_assign.type);

        if (var_assign.type != .inc and var_assign.type != .dec) {
            c_target.compileExpr(buffer, var_assign.value);
        }

        buffer.writer().print(";", .{}) catch unreachable;

        buffer.writer().print("\n", .{}) catch unreachable;
    }

    fn compileVarAssignType(buffer: *std.ArrayList(u8), var_assign_type: VarAssignType) void {
        switch (var_assign_type) {
            .eq => {
                buffer.writer().print(" = ", .{}) catch unreachable;
            },
            .inc => {
                buffer.writer().print(" += 1", .{}) catch unreachable;
            },
            .dec => {
                buffer.writer().print(" -= 1", .{}) catch unreachable;
            },
            .add => {
                buffer.writer().print(" += ", .{}) catch unreachable;
            },
            .sub => {
                buffer.writer().print(" -= ", .{}) catch unreachable;
            },
            .mul => {
                buffer.writer().print(" *= ", .{}) catch unreachable;
            },
            .div => {
                buffer.writer().print(" /= ", .{}) catch unreachable;
            },
        }
    }

    fn compileExpr(buffer: *std.ArrayList(u8), expr: *Expr) void {
        switch (expr.as) {
            .num => |num| {
                c_target.compileLitNum(buffer, num);
            },
            .bool => |boolean| {
                switch (boolean) {
                    .true => {
                        buffer.writer().print("true", .{}) catch unreachable;
                    },
                    .false => {
                        buffer.writer().print("false", .{}) catch unreachable;
                    },
                }
            },
            .lit_str => |lit_str| {
                c_target.compileLitStr(buffer, lit_str);
            },
            .proc_call => |proc_call| {
                buffer.writer().print("{s}(", .{proc_call.name}) catch unreachable;
                c_target.compileProcCallArgs(buffer, proc_call.args);
                buffer.writer().print(")", .{}) catch unreachable;
            },
            .binary => |binary| {
                buffer.writer().print("(", .{}) catch unreachable;
                c_target.compileExpr(buffer, binary.lhs);
                buffer.writer().print(")", .{}) catch unreachable;

                switch (binary.op) {
                    .plus => {
                        buffer.writer().print("+", .{}) catch unreachable;
                    },
                    .minus => {
                        buffer.writer().print("-", .{}) catch unreachable;
                    },
                    .mul => {
                        buffer.writer().print("*", .{}) catch unreachable;
                    },
                    .div => {
                        buffer.writer().print("/", .{}) catch unreachable;
                    },
                    .deq => {
                        buffer.writer().print("==", .{}) catch unreachable;
                    },
                    .neq => {
                        buffer.writer().print("!=", .{}) catch unreachable;
                    },
                    .gt => {
                        buffer.writer().print(">", .{}) catch unreachable;
                    },
                    .ge => {
                        buffer.writer().print(">=", .{}) catch unreachable;
                    },
                    .lt => {
                        buffer.writer().print("<", .{}) catch unreachable;
                    },
                    .le => {
                        buffer.writer().print("<=", .{}) catch unreachable;
                    },
                }

                buffer.writer().print("(", .{}) catch unreachable;
                c_target.compileExpr(buffer, binary.rhs);
                buffer.writer().print(")", .{}) catch unreachable;
            },
            .var_read => |var_read| {
                buffer.writer().print("{s}", .{var_read.name}) catch unreachable;
            },
        }
    }

    fn compileLitNum(buffer: *std.ArrayList(u8), lit_num: Number) void {
        switch (lit_num) {
            .int => |int| {
                buffer.writer().print("{}", .{int}) catch unreachable;
            },
            .float => |float| {
                buffer.writer().print("{d}", .{float}) catch unreachable;
            },
        }
    }

    fn compileLitStr(buffer: *std.ArrayList(u8), lit_str: []const u8) void {
        buffer.writer().print("\"", .{}) catch unreachable;

        for (lit_str) |char| {
            switch (char) {
                0x07 => {
                    buffer.writer().print("\\a", .{}) catch unreachable;
                },
                0x08 => {
                    buffer.writer().print("\\b", .{}) catch unreachable;
                },
                0x0C => {
                    buffer.writer().print("\\f", .{}) catch unreachable;
                },
                '\n' => {
                    buffer.writer().print("\\n", .{}) catch unreachable;
                },
                '\r' => {
                    buffer.writer().print("\\r", .{}) catch unreachable;
                },
                '\t' => {
                    buffer.writer().print("\\t", .{}) catch unreachable;
                },
                0x0B => {
                    buffer.writer().print("\\v", .{}) catch unreachable;
                },
                '\\' => {
                    buffer.writer().print("\\\\", .{}) catch unreachable;
                },
                '\"' => {
                    buffer.writer().print("\\\"", .{}) catch unreachable;
                },
                '\'' => {
                    buffer.writer().print("\\\'", .{}) catch unreachable;
                },
                0x00 => {
                    buffer.writer().print("\\0", .{}) catch unreachable;
                },
                else => {
                    buffer.writer().print("{c}", .{char}) catch unreachable;
                },
            }
        }

        buffer.writer().print("\"", .{}) catch unreachable;
    }

    fn compileProcCallArgs(buffer: *std.ArrayList(u8), args: ProcCallArgs) void {
        var current: usize = 0;

        if (args.amount == 0) return;

        c_target.compileProcCallArg(buffer, args.args[current]);
        current += 1;

        while (current < args.amount) : (current += 1) {
            buffer.writer().print(", ", .{}) catch unreachable;
            c_target.compileProcCallArg(buffer, args.args[current]);
        }
    }

    fn compileProcCallArg(buffer: *std.ArrayList(u8), arg: ProcCallArg) void {
        c_target.compileExpr(buffer, arg.value);
    }

    fn writeTabs(buffer: *std.ArrayList(u8), amount: usize) void {
        for (0..amount) |_| {
            buffer.writer().print("    ", .{}) catch unreachable;
        }
    }

    pub fn target() Target {
        return .{
            .ptr = undefined,
            .compileFn = compileFn,
        };
    }
};
