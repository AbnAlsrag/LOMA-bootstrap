const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const assert = std.debug.assert;
const lexer = @import("lexer.zig");
const types = @import("types.zig");
const Type = types.TypeChecker.Type;

pub const Parser = struct {
    allocator: mem.Allocator,
    modules: [Program.MAX_MODULES][]const u8 = undefined,
    module_count: usize = 0,

    pub fn init(allocator: mem.Allocator, src: []const u8) Parser {
        _ = src;
        _ = allocator;
        //FIXME: FIX THIS SHIT
        // var parser: Parser = Parser{
        //     .lxer = lexer.Lexer.init(src),
        //     .curr_tok = undefined,
        //     .allocator = allocator,
        //     .arena = heap.ArenaAllocator.init(allocator),
        // };

        // parser.curr_tok = parser.lxer.nextToken();

        // return parser;
    }

    pub fn initFiles(allocator: mem.Allocator, paths: [][]const u8) Parser {
        var parser: Parser = .{
            .allocator = allocator,
            .modules = undefined,
            .module_count = paths.len,
        };

        mem.copy([]const u8, &parser.modules, paths);

        return parser;
    }

    pub fn initLexer(allocator: mem.Allocator, lxer: lexer.Lexer) Parser {
        return .{
            .lxer = lxer,
            .allocator = allocator,
            .arena = heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *Parser) void {
        _ = self;
    }

    pub const Program = struct {
        const MAX_MODULES = 1000;

        allocator: mem.Allocator,
        modules: [MAX_MODULES]Module = undefined,
        module_count: usize = 0,
        type_checker: types.TypeChecker,

        fn initFiles(allocator: mem.Allocator, paths: [][]const u8) Program {
            var program: Program = .{
                .allocator = allocator,
                .type_checker = types.TypeChecker.init(allocator),
            };

            for (paths) |path| {
                program.pushModule(Module.initFile(allocator, &program.type_checker, path));
            }

            return program;
        }

        pub fn deinit(self: *Program) void {
            for (self.modules[0..self.module_count]) |*module| {
                module.deinit();
            }

            self.type_checker.deinit();
        }

        fn pushModule(self: *Program, module: Module) void {
            assert(self.module_count <= MAX_MODULES);

            self.modules[self.module_count] = module;
            self.module_count += 1;
        }
    };

    pub const Module = struct {
        path: []const u8,
        lxer: lexer.Lexer,
        allocator: mem.Allocator,
        arena: heap.ArenaAllocator,
        begin: ?*TopLevel = null,
        end: ?*TopLevel = null,
        type_checker: *types.TypeChecker,

        fn initFile(allocator: mem.Allocator, type_checker: *types.TypeChecker, path: []const u8) Module {
            var module: Module = undefined;

            module.begin = null;
            module.end = null;
            module.path = path;
            module.allocator = allocator;
            module.arena = heap.ArenaAllocator.init(allocator);
            module.lxer = lexer.Lexer.initFile(allocator, path);
            module.type_checker = type_checker;

            return module;
        }

        fn deinit(self: *Module) void {
            self.lxer.deinit();
            self.arena.deinit();
        }

        fn pushTop(self: *Module, top: *TopLevel) void {
            if (self.end == null) {
                assert(self.begin == null);
                self.begin = top;
                self.end = top;
            } else {
                assert(self.begin != null);
                self.end.?.next = top;
                self.end = top;
            }
        }

        fn parseModule(self: *Module) void {
            while (self.lxer.peekToken(1, null).type != .eof) {
                self.pushTop(self.parseTopLevel());
            }
        }

        fn parseTopLevel(self: *Module) *TopLevel {
            var top: *TopLevel = self.arena.allocator().create(TopLevel) catch unreachable;
            top.next = null;

            if (self.lxer.peekToken(3, null).type == .lparen) {
                top.as = TopLevelAs{ .proc_decl = self.parseProcDecl() };
            } else if (self.lxer.peekToken(3, null).type == .eq) {
                top.as = TopLevelAs{ .var_decl = self.parseVarDecl() };
            } else {
                @panic("[ERR] Unkown top level statement");
            }

            return top;
        }

        fn parseProcDecl(self: *Module) ProcDecl {
            var proc_decl: ProcDecl = undefined;

            var tok: lexer.Token = self.lxer.expectToken(.ident);
            proc_decl.return_type = tok.text;

            tok = self.lxer.expectToken(.ident);
            proc_decl.name = tok.text;

            proc_decl.args = self.parseProcDeclParams();

            proc_decl.body = self.parseBlock();

            return proc_decl;
        }

        fn parseProcDeclParams(self: *Module) ProcDeclParams {
            _ = self.lxer.expectToken(.lparen);

            var params: ProcDeclParams = ProcDeclParams.init();

            var tok: lexer.Token = self.lxer.peekToken(1, null);

            if (tok.type != .eof and tok.type != .rparen) {
                const param: ProcDeclParam = self.parseProcDeclParam();

                params.pushParam(param);
            }

            tok = self.lxer.peekToken(1, null);
            while (tok.type != .eof and tok.type != .rparen) {
                _ = self.lxer.expectToken(.comma);

                const param: ProcDeclParam = self.parseProcDeclParam();

                params.pushParam(param);

                tok = self.lxer.peekToken(1, null);
            }

            _ = self.lxer.expectToken(.rparen);

            return params;
        }

        fn parseProcDeclParam(self: *Module) ProcDeclParam {
            var param: ProcDeclParam = undefined;

            var tok: lexer.Token = self.lxer.expectToken(.ident);
            param.type = tok.text;

            tok = self.lxer.expectToken(.ident);
            param.name = tok.text;

            return param;
        }

        fn parseBlock(self: *Module) Block {
            var block: Block = .{ .begin = null, .end = null };

            _ = self.lxer.expectToken(.lcurly);

            var tok: lexer.Token = self.lxer.peekToken(1, null);
            while (tok.type != .eof and tok.type != .rcurly) {
                const stmt: *Stmt = self.parseStmt();
                block.pushStmt(stmt);
                tok = self.lxer.peekToken(1, null);
            }

            {
                var stmt: ?*Stmt = block.begin;
                while (stmt) |s| {
                    switch (s.as) {
                        .defer_ => block.addDefer(),
                        else => {},
                    }
                    stmt = s.next;
                }
            }

            _ = self.lxer.expectToken(.rcurly);

            return block;
        }

        fn parseStmt(self: *Module) *Stmt {
            var stmt: *Stmt = self.arena.allocator().create(Stmt) catch unreachable;
            stmt.* = .{ .as = undefined, .next = null };

            switch (self.lxer.peekToken(1, null).type) {
                .ident => {
                    const tok: lexer.Token = self.lxer.peekToken(2, null);
                    if (tok.type == .eq or tok.type == .plus_eq or tok.type == .minus_eq or
                        tok.type == .mul_eq or tok.type == .div_eq or tok.type == .plus_plus or
                        tok.type == .minus_minus)
                    {
                        stmt.as = .{ .var_assign = self.parseVarAssign() };

                        self.type_checker.checkStmtType(stmt);
                        return stmt;
                    } else if (tok.type == .ident) {
                        stmt.as = .{ .var_decl = self.parseVarDecl() };

                        self.type_checker.checkStmtType(stmt);
                        return stmt;
                    }
                },
                .if_ => {
                    stmt.as = .{ .if_ = self.parseIf() };

                    self.type_checker.checkStmtType(stmt);
                    return stmt;
                },
                .switch_ => {
                    stmt.as = .{ .switch_ = self.parseSwitch() };

                    self.type_checker.checkStmtType(stmt);
                    return stmt;
                },
                .while_ => {
                    stmt.as = .{ .while_ = self.parseWhile() };

                    self.type_checker.checkStmtType(stmt);
                    return stmt;
                },
                .for_ => {
                    stmt.as = .{ .for_ = self.parseFor() };

                    self.type_checker.checkStmtType(stmt);
                    return stmt;
                },
                .defer_ => {
                    stmt.as = .{ .defer_ = self.parseDefer() };

                    self.type_checker.checkStmtType(stmt);
                    return stmt;
                },
                .lcurly => {
                    stmt.as = .{ .block = self.parseBlock() };

                    self.type_checker.checkStmtType(stmt);
                    return stmt;
                },
                else => {},
            }

            {
                stmt.as = .{ .expr = self.parseExpr() };
                _ = self.lxer.expectToken(.semi_colon);

                return stmt;
            }
        }

        fn parseDefer(self: *Module) Defer {
            var defer_: Defer = undefined;

            _ = self.lxer.expectToken(.defer_);

            defer_.stmt = self.parseStmt();

            return defer_;
        }

        fn parseIf(self: *Module) If {
            var if_: If = undefined;

            _ = self.lxer.expectToken(.if_);

            if_.condition = self.parseCondition();
            if_.body = self.parseBlock();
            if_.else_ = null;

            if (self.lxer.tokenIs(.else_)) {
                if_.else_ = self.parseElse();
            }

            return if_;
        }

        fn parseElse(self: *Module) Else {
            var else_: Else = undefined;

            _ = self.lxer.expectToken(.else_);

            if (self.lxer.tokenIs(.if_)) {
                var block: Block = .{};

                var stmt: *Stmt = self.arena.allocator().create(Stmt) catch unreachable;

                stmt.* = .{ .as = .{ .if_ = self.parseIf() } };

                block.pushStmt(stmt);

                else_.body = block;
            } else {
                else_.body = self.parseBlock();
            }

            return else_;
        }

        fn parseSwitch(self: *Module) Switch {
            var switch_: Switch = undefined;

            _ = self.lxer.expectToken(.switch_);

            switch_.main_cond = self.parseExpr();

            switch_.cases = self.parseCases();

            return switch_;
        }

        fn parseCases(self: *Module) Cases {
            var cases: Cases = .{};

            _ = self.lxer.expectToken(.lcurly);

            while (self.parseCase()) |case| {
                cases.pushCase(case);
            }

            _ = self.lxer.expectToken(.rcurly);

            return cases;
        }

        fn parseCase(self: *Module) ?Case {
            var case: Case = undefined;

            const tok: lexer.Token = self.lxer.peekToken(1, null);

            if (tok.type == .rcurly) return null;

            if (self.lxer.tokenIs(.case)) {
                self.lxer.skipToken();
                case.condition = self.parseCondition();
            } else if (self.lxer.tokenIs(.default)) {
                self.lxer.skipToken();
                case.condition = null;
            } else {
                tok.loc.printLoc();
                std.debug.print(" error: expected case or default but got {s}", .{tok.text});
            }

            case.body = self.parseCaseBody();

            return case;
        }

        fn parseCaseBody(self: *Module) CaseBody {
            var case_body: CaseBody = .{};

            _ = self.lxer.expectToken(.colon);

            var tok: lexer.Token = self.lxer.peekToken(1, null);

            while (tok.type != .rcurly and tok.type != .case and tok.type != .default) {
                case_body.pushStmt(self.parseStmt());
                tok = self.lxer.peekToken(1, null);
            }

            return case_body;
        }

        fn parseCondition(self: *Module) Condition {
            var condition: Condition = undefined;

            condition.expr = self.parseExpr();

            return condition;
        }

        fn parseWhile(self: *Module) While {
            var while_: While = undefined;

            _ = self.lxer.expectToken(.while_);

            while_.condition = self.parseCondition();
            while_.body = self.parseBlock();

            return while_;
        }

        fn parseFor(self: *Module) For {
            var for_: For = undefined;

            _ = self.lxer.expectToken(.for_);

            for_.range = self.parseRange();
            for_.body = self.parseBlock();

            return for_;
        }

        fn parseRange(self: *Module) Range {
            var range: Range = undefined;

            range.low = self.parseExpr();

            _ = self.lxer.expectToken(.dot_dot);

            range.high = self.parseExpr();

            return range;
        }

        fn parseVarDecl(self: *Module) VarDecl {
            var var_decl: VarDecl = undefined;

            var tok: lexer.Token = self.lxer.expectToken(.ident);
            var_decl.type = tok.text;

            tok = self.lxer.expectToken(.ident);
            var_decl.name = tok.text;

            _ = self.lxer.expectToken(.eq);

            var_decl.value = self.parseExpr();

            _ = self.lxer.expectToken(.semi_colon);

            return var_decl;
        }

        fn getVarAssignType(tok: lexer.Token) VarAssignType {
            return switch (tok.type) {
                .eq => .eq,
                .plus_plus => .inc,
                .minus_minus => .dec,
                .plus_eq => .add,
                .minus_eq => .sub,
                .mul_eq => .mul,
                .div_eq => .div,
                else => unreachable,
            };
        }

        fn parseVarAssign(self: *Module) VarAssign {
            var var_assign: VarAssign = undefined;

            var_assign.name = self.lxer.expectToken(.ident).text;

            if (!(self.lxer.tokenIs(.eq) or self.lxer.tokenIs(.plus_eq) or self.lxer.tokenIs(.minus_eq) or
                self.lxer.tokenIs(.mul_eq) or self.lxer.tokenIs(.div_eq) or self.lxer.tokenIs(.plus_plus) or self.lxer.tokenIs(.minus_minus)))
            {
                unreachable;
            }

            var_assign.type = getVarAssignType(self.lxer.nextToken());

            if (var_assign.type != .inc and var_assign.type != .dec) {
                var_assign.value = self.parseExpr();
            }

            _ = self.lxer.expectToken(.semi_colon);

            return var_assign;
        }

        fn isOp(tok: lexer.Token) bool {
            switch (tok.type) {
                .plus, .minus, .mul, .div, .deq, .neq, .gt, .ge, .lt, .le => return true,
                else => return false,
            }
        }

        fn getOperatorInfo(op: lexer.Token) OpInfo {
            switch (op.type) {
                .plus => return OpInfo{ .prec = 1 },
                .minus => return OpInfo{ .prec = 1 },
                .mul => return OpInfo{ .prec = 2 },
                .div => return OpInfo{ .prec = 2 },
                .deq => return OpInfo{ .prec = 0 },
                .neq => return OpInfo{ .prec = 0 },
                .gt => return OpInfo{ .prec = 0 },
                .ge => return OpInfo{ .prec = 0 },
                .lt => return OpInfo{ .prec = 0 },
                .le => return OpInfo{ .prec = 0 },
                else => unreachable,
            }
        }

        fn getOp(tok: lexer.Token) BinaryOp {
            switch (tok.type) {
                .plus => return .plus,
                .minus => return .minus,
                .mul => return .mul,
                .div => return .div,
                .deq => return .deq,
                .neq => return .neq,
                .gt => return .gt,
                .ge => return .ge,
                .lt => return .lt,
                .le => return .le,
                else => {
                    std.debug.print("Unexpected token: {}\n", .{tok});
                    unreachable;
                },
            }
        }

        fn parseExpr(self: *Module) *Expr {
            return self.parseExprPrec(0);
        }

        fn parseExprPrec(self: *Module, prec: Precedence) *Expr {
            var left: *Expr = self.parsePrimaryExpr();

            while (true) {
                const tok: lexer.Token = self.lxer.peekToken(1, null);

                if (tok.type == .eof) break;

                if (!isOp(tok)) break;

                const op: BinaryOp = getOp(tok);
                const op_info: OpInfo = getOperatorInfo(tok);

                if (op_info.prec < prec) break;

                _ = self.lxer.nextToken();
                const right: *Expr = self.parseExprPrec(op_info.prec + 1);

                const result: *Expr = self.arena.allocator().create(Expr) catch unreachable;
                result.* = .{ .as = .{ .binary = .{ .op = op, .lhs = left, .rhs = right } } };

                left = result;
            }

            left.type = self.type_checker.getExprType(left);

            return left;
        }

        fn parsePrimaryExpr(self: *Module) *Expr {
            const tok = self.lxer.peekToken(1, null);
            var result: *Expr = undefined;

            switch (tok.type) {
                .num => {
                    result = self.arena.allocator().create(Expr) catch unreachable;
                    result.* = .{ .as = .{ .num = self.parseNumLit() } };
                },
                .true => {
                    result = self.arena.allocator().create(Expr) catch unreachable;
                    result.* = .{ .as = .{ .bool = .true } };
                    self.lxer.skipToken();
                },
                .false => {
                    result = self.arena.allocator().create(Expr) catch unreachable;
                    result.* = .{ .as = .{ .bool = .false } };
                    self.lxer.skipToken();
                },
                .str => {
                    result = self.arena.allocator().create(Expr) catch unreachable;
                    result.* = .{ .as = .{ .lit_str = self.parseStrLit() } };
                },
                .lparen => {
                    self.lxer.skipToken();
                    result = self.parseExpr();
                    if (!self.lxer.tokenIs(.rparen)) {
                        @panic("Expected ')' after expression in parentheses");
                    }
                },
                .ident => {
                    self.lxer.skipToken();
                    if (self.lxer.peekToken(1, null).type == .lparen) {
                        result = self.arena.allocator().create(Expr) catch unreachable;
                        result.* = .{ .as = .{ .proc_call = self.parseProcCall(tok.text) } };
                    } else {
                        result = self.arena.allocator().create(Expr) catch unreachable;
                        result.* = .{ .as = .{ .var_read = self.parseVarRead(tok.text) } };
                    }
                },
                else => {
                    std.debug.print("Unexpected token: {}", .{tok});
                    unreachable;
                },
            }

            return result;
        }

        fn parseProcCall(self: *Module, name: []const u8) ProcCall {
            var proc_call: ProcCall = undefined;
            proc_call.name = name;
            proc_call.args = self.parseProcCallArgs();

            return proc_call;
        }

        fn parseVarRead(self: *Module, name: []const u8) VarRead {
            _ = self;
            var var_read: VarRead = undefined;

            var_read.name = name;

            return var_read;
        }

        fn parseProcCallArgs(self: *Module) ProcCallArgs {
            var args: ProcCallArgs = .{};

            _ = self.lxer.expectToken(.lparen);

            var tok: lexer.Token = self.lxer.peekToken(1, null);

            if (tok.type != .eof and tok.type != .rparen) {
                const arg: ProcCallArg = self.parseProcCallArg();

                args.pushArg(arg);
            }

            tok = self.lxer.peekToken(1, null);
            while (tok.type != .eof and tok.type != .rparen) {
                _ = self.lxer.expectToken(.comma);

                const arg: ProcCallArg = self.parseProcCallArg();

                args.pushArg(arg);

                tok = self.lxer.peekToken(1, null);
            }

            _ = self.lxer.expectToken(.rparen);

            return args;
        }

        fn parseProcCallArg(self: *Module) ProcCallArg {
            const arg: ProcCallArg = .{
                .value = self.parseExpr(),
            };

            return arg;
        }

        fn parseStrLit(self: *Module) []const u8 {
            const token: lexer.Token = self.lxer.expectToken(.str);

            const result: []u8 = self.arena.allocator().alloc(u8, token.text.len) catch unreachable;

            var current: usize = 0;
            var result_curr: usize = 0;
            while (current < token.text.len) {
                switch (token.text[current]) {
                    '\\' => {
                        current += 1;
                        if (current >= token.text.len) {
                            @panic("Unterminated string literal");
                        }

                        switch (token.text[current]) {
                            'a' => result[result_curr] = 0x07,
                            'b' => result[result_curr] = 0x08,
                            'e' => result[result_curr] = 0x1B,
                            'f' => result[result_curr] = 0x0C,
                            'n' => result[result_curr] = '\n',
                            'r' => result[result_curr] = '\r',
                            't' => result[result_curr] = '\t',
                            'v' => result[result_curr] = 0x0B,
                            '\\' => result[result_curr] = '\\',
                            '\"' => result[result_curr] = '\"',
                            '\'' => result[result_curr] = '\'',
                            else => @panic("Invalid escape sequence"),
                        }
                    },
                    else => result[result_curr] = token.text[current],
                }

                current += 1;
                result_curr += 1;
            }

            return result[0..result_curr];
        }

        pub fn parseNumLit(self: *Module) Number {
            const tok: lexer.Token = self.lxer.expectToken(.num);
            var is_float: bool = false;

            for (tok.text) |char| {
                if (char == '.') {
                    is_float = true;
                    break;
                }
            }

            if (is_float) {
                return .{ .float = std.fmt.parseFloat(f64, tok.text) catch unreachable };
            } else {
                return .{ .int = std.fmt.parseInt(i128, tok.text, 0) catch unreachable };
            }
        }
    };

    pub const Number = union(enum) {
        int: i128,
        float: f64,
    };

    pub const Bool = enum(u1) {
        false,
        true,
    };

    pub const VarRead = struct {
        name: []const u8,
    };

    pub const Expr = struct {
        type: *Type = undefined,
        as: ExprAs,
    };

    pub const ExprAs = union(enum) {
        num: Number,
        bool: Bool,
        lit_str: []const u8,
        proc_call: ProcCall,
        binary: Binary,
        var_read: VarRead,
    };

    pub const BinaryOp = enum {
        plus,
        minus,
        mul,
        div,
        deq,
        neq,
        gt,
        ge,
        lt,
        le,
    };

    pub const Binary = struct {
        op: BinaryOp,
        lhs: *Expr,
        rhs: *Expr,
    };

    const OpInfo = struct {
        prec: Precedence,
    };

    pub const Condition = struct {
        expr: *Expr,
    };

    pub const Else = struct {
        body: Block,
    };

    pub const If = struct {
        condition: Condition,
        body: Block,
        else_: ?Else,
    };

    pub const CaseBody = struct {
        begin: ?*Stmt = null,
        end: ?*Stmt = null,

        fn pushStmt(self: *CaseBody, stmt: *Stmt) void {
            if (self.end == null) {
                assert(self.begin == null);
                self.begin = stmt;
                self.end = stmt;
            } else {
                assert(self.begin != null);
                self.end.?.next = stmt;
                self.end = stmt;
            }
        }
    };

    pub const Case = struct {
        condition: ?Condition,
        body: CaseBody,
    };

    pub const Cases = struct {
        const MAX_CASES = 500;

        cases: [MAX_CASES]Case = [_]Case{undefined} ** MAX_CASES,
        amount: u8 = 0,

        fn pushCase(self: *Cases, case: Case) void {
            assert(self.amount < MAX_CASES);

            self.cases[self.amount] = case;
            self.amount += 1;
        }
    };

    pub const Switch = struct {
        main_cond: *Expr,
        cases: Cases,
    };

    pub const While = struct {
        condition: Condition,
        body: Block,
    };

    pub const Range = struct {
        low: *Expr,
        high: *Expr,
    };

    pub const For = struct {
        range: Range,
        body: Block,
    };

    pub const VarDecl = struct {
        name: []const u8,
        type: []const u8,
        value: *Expr,
    };

    pub const VarAssignType = enum(u8) {
        eq,
        inc,
        dec,
        add,
        sub,
        mul,
        div,
    };

    pub const VarAssign = struct {
        name: []const u8,
        type: VarAssignType,
        value: *Expr,
    };

    pub const Defer = struct {
        stmt: *Stmt,
    };

    pub const Stmt = struct {
        as: StmtAs,
        next: ?*Stmt = null,
    };

    pub const StmtAs = union(enum) {
        expr: *Expr,
        if_: If,
        switch_: Switch,
        while_: While,
        for_: For,
        var_decl: VarDecl,
        var_assign: VarAssign,
        defer_: Defer,
        block: Block,
    };

    pub const ProcCallArg = struct {
        value: *Expr,
    };

    pub const ProcCallArgs = struct {
        args: [ProcDeclParams.MAX_PARAM_COUNT]ProcCallArg = [_]ProcCallArg{undefined} ** ProcDeclParams.MAX_PARAM_COUNT,
        amount: u8 = 0,

        fn init() ProcCallArgs {
            return .{};
        }

        fn pushArg(self: *ProcCallArgs, arg: ProcCallArg) void {
            assert(self.amount < ProcDeclParams.MAX_PARAM_COUNT);

            self.args[self.amount] = arg;
            self.amount += 1;
        }
    };

    pub const ProcCall = struct {
        name: []const u8,
        args: ProcCallArgs,
    };

    pub const Block = struct {
        begin: ?*Stmt = null,
        end: ?*Stmt = null,
        defer_count: usize = 0,

        fn pushStmt(self: *Block, stmt: *Stmt) void {
            if (self.end == null) {
                assert(self.begin == null);
                self.begin = stmt;
                self.end = stmt;
            } else {
                assert(self.begin != null);
                self.end.?.next = stmt;
                self.end = stmt;
            }
        }

        fn addDefer(self: *Block) void {
            self.defer_count += 1;
        }
    };

    pub const ProcDeclParam = struct {
        name: []const u8,
        type: []const u8,
    };

    pub const ProcDeclParams = struct {
        const MAX_PARAM_COUNT = 128;

        params: [MAX_PARAM_COUNT]ProcDeclParam = [_]ProcDeclParam{undefined} ** MAX_PARAM_COUNT,
        amount: u8 = 0,

        fn init() ProcDeclParams {
            return .{};
        }

        fn pushParam(self: *ProcDeclParams, param: ProcDeclParam) void {
            assert(self.amount < MAX_PARAM_COUNT);

            self.params[self.amount] = param;
            self.amount += 1;
        }
    };

    pub const ProcDecl = struct {
        name: []const u8,
        args: ProcDeclParams,
        return_type: []const u8,
        body: Block,
    };

    pub const TopLevelAs = union(enum) {
        proc_decl: ProcDecl,
        var_decl: VarDecl,
    };

    pub const TopLevel = struct {
        as: TopLevelAs,
        next: ?*TopLevel,
    };

    const Precedence = u8;
    const PRECEDENCE_MIN = 0;
    const PRECEDENCE_MAX = 4;
    const NO_PRECEDENCE = 5;

    pub fn parse(self: *Parser) Program {
        const program: Program = self.parseProgram();

        return program;
    }

    pub fn parseProgram(self: *Parser) Program {
        var program: Program = Program.initFiles(self.allocator, self.modules[0..self.module_count]);

        for (program.modules[0..program.module_count]) |*module| {
            module.parseModule();
        }

        return program;
    }
};
