const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;

pub const TokenType = enum(u8) {
    ident,
    if_,
    else_,
    switch_,
    case,
    default,
    while_,
    do,
    for_,
    defer_,
    export_,
    num,
    str,
    true,
    false,
    lparen,
    rparen,
    lcurly,
    rcurly,
    colon,
    semi_colon,
    comma,
    dot,
    dot_dot,
    plus,
    plus_plus,
    minus,
    minus_minus,
    mul,
    div,
    eq,
    plus_eq,
    minus_eq,
    mul_eq,
    div_eq,
    deq,
    neq,
    gt,
    ge,
    lt,
    le,
    eof,
    token_type_max,
};

pub const TokenLoc = struct {
    path: []const u8,
    line: usize,
    col: usize,

    fn init(path: []const u8, line: usize, col: usize) TokenLoc {
        return .{ .path = path, .line = line, .col = col };
    }

    pub fn printLoc(self: TokenLoc) void {
        std.debug.print("{s}:{}:{}", .{ self.path, self.line, self.col });
    }
};

pub const Token = struct {
    type: TokenType,
    text: []const u8,
    loc: TokenLoc,
};

pub const Lexer = struct {
    path: []const u8,
    src: []const u8,
    allocator: ?mem.Allocator = null,
    current: usize = 0,
    current_line: usize = 0,
    current_col: usize = 0,

    pub fn init(src: []const u8) Lexer {
        return .{ .path = "", .src = src };
    }

    pub fn initFile(allocator: mem.Allocator, path: []const u8) Lexer {
        var file = std.fs.cwd().openFile(path, .{}) catch {
            @panic("[ERROR] error while parsing lasm file");
        };
        defer file.close();

        var buf_reader = std.io.bufferedReader(file.reader());
        var in_stream = buf_reader.reader();

        const buffer: []u8 = in_stream.readAllAlloc(allocator, 5000) catch {
            @panic("[ERROR] error while parsing lasm file");
        };

        return .{ .path = path, .src = buffer, .allocator = allocator };
    }

    pub fn deinit(self: *Lexer) void {
        if (self.allocator) |alloc| {
            alloc.free(self.src);
        }
    }

    pub fn nextToken(self: *Lexer) Token {
        while (self.current < self.src.len) {
            const c: u8 = self.src[self.current];
            if (isAlpha(c)) {
                return self.parseAlphaNum();
            } else if (isNum(c)) {
                return self.parseNum();
            } else if (c == '"') {
                return self.parseStr();
            } else if (c == '(') {
                self.current += 1;
                self.current_col += 1;
                return Token{ .type = .lparen, .text = "(", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
            } else if (c == ')') {
                self.current += 1;
                self.current_col += 1;
                return Token{ .type = .rparen, .text = ")", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
            } else if (c == '{') {
                self.current += 1;
                self.current_col += 1;
                return Token{ .type = .lcurly, .text = "{", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
            } else if (c == '}') {
                self.current += 1;
                self.current_col += 1;
                return Token{ .type = .rcurly, .text = "}", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
            } else if (c == ':') {
                self.current += 1;
                self.current_col += 1;
                return Token{ .type = .colon, .text = ":", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
            } else if (c == ';') {
                self.current += 1;
                self.current_col += 1;
                return Token{ .type = .semi_colon, .text = ";", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
            } else if (c == ',') {
                self.current += 1;
                self.current_col += 1;
                return Token{ .type = .comma, .text = ",", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
            } else if (c == '.') {
                self.current += 1;
                self.current_col += 1;

                if (self.src[self.current] == '.') {
                    self.current += 1;
                    self.current_col += 1;

                    return Token{ .type = .dot_dot, .text = "..", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                } else {
                    return Token{ .type = .dot, .text = ".", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                }
            } else if (c == '+') {
                self.current += 1;
                self.current_col += 1;

                if (self.src[self.current] == '+') {
                    self.current += 1;
                    self.current_col += 1;

                    return Token{ .type = .plus_plus, .text = "++", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                } else if (self.src[self.current] == '=') {
                    self.current += 1;
                    self.current_col += 1;

                    return Token{ .type = .plus_eq, .text = "+=", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                } else {
                    return Token{ .type = .plus, .text = "+", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                }
            } else if (c == '-') {
                self.current += 1;
                self.current_col += 1;

                if (self.src[self.current] == '-') {
                    self.current += 1;
                    self.current_col += 1;

                    return Token{ .type = .minus_minus, .text = "--", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                } else if (self.src[self.current] == '=') {
                    self.current += 1;
                    self.current_col += 1;

                    return Token{ .type = .minus_eq, .text = "-=", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                } else {
                    return Token{ .type = .minus, .text = "-", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                }
            } else if (c == '*') {
                self.current += 1;
                self.current_col += 1;

                if (self.src[self.current] == '=') {
                    self.current += 1;
                    self.current_col += 1;

                    return Token{ .type = .mul_eq, .text = "*=", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                } else {
                    return Token{ .type = .mul, .text = "*", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                }
            } else if (c == '/') {
                self.current += 1;
                self.current_col += 1;

                if (self.src[self.current] == '=') {
                    self.current += 1;
                    self.current_col += 1;

                    return Token{ .type = .div_eq, .text = "/=", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                } else {
                    return Token{ .type = .div, .text = "/", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                }
            } else if (c == '=') {
                self.current += 1;
                self.current_col += 1;

                if (self.src[self.current] == '=') {
                    self.current += 1;
                    self.current_col += 1;

                    return Token{ .type = .deq, .text = "==", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                } else {
                    return Token{ .type = .eq, .text = "=", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                }
            } else if (c == '!') {
                self.current += 1;
                self.current_col += 1;

                if (self.src[self.current] == '=') {
                    self.current += 1;
                    self.current_col += 1;

                    return Token{ .type = .neq, .text = "!=", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                }
            } else if (c == '>') {
                self.current += 1;
                self.current_col += 1;

                if (self.src[self.current] == '=') {
                    self.current += 1;
                    self.current_col += 1;

                    return Token{ .type = .ge, .text = ">=", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                } else {
                    return Token{ .type = .gt, .text = ">", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                }
            } else if (c == '<') {
                self.current += 1;
                self.current_col += 1;

                if (self.src[self.current] == '=') {
                    self.current += 1;
                    self.current_col += 1;

                    return Token{ .type = .le, .text = "<=", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                } else {
                    return Token{ .type = .lt, .text = "<", .loc = TokenLoc.init(self.path, self.current_line, self.current_col) };
                }
            } else if (isWhiteSpace(c)) {
                self.current += 1;
                self.current_col += 1;
                if (c == '\n') {
                    self.current_line += 1;
                    self.current_col = 0;
                }
            } else if (c == '#') {
                while (self.current < self.src.len and self.src[self.current] != '\n') {
                    self.current += 1;
                }
            } else {
                unreachable;
            }
        }

        return Token{ .type = .eof, .text = "", .loc = .{ .path = self.path, .line = 0, .col = 0 } };
    }

    pub fn peekToken(self: *Lexer, amount: usize, parsed: ?*usize) Token {
        assert(amount > 0);

        const old_current = self.current;
        defer self.current = old_current;

        var tok: Token = undefined;

        for (0..amount) |_| {
            tok = self.nextToken();

            if (parsed) |p| {
                p.* += 1;
            }

            if (tok.type == .eof) break;
        }

        return tok;
    }

    pub fn skipToken(self: *Lexer) void {
        _ = self.nextToken();
    }

    pub fn expectToken(self: *Lexer, expected: TokenType) Token {
        const tok = self.peekToken(1, null);

        if (expected == tok.type) {
            self.skipToken();
            return tok;
        } else {
            tok.loc.printLoc();
            std.debug.print(" error: expected {s} but got {s}\n", .{ @tagName(expected), tok.text });
            unreachable;
        }
    }

    pub fn tokenIs(self: *Lexer, expected: TokenType) bool {
        return if (self.peekToken(1, null).type == expected) true else false;
    }

    fn parseAlphaNum(self: *Lexer) Token {
        var tok: Token = .{
            .type = .ident,
            .text = undefined,
            .loc = TokenLoc.init(self.path, self.current_line, self.current_col),
        };
        const start: usize = self.current;

        while (self.current < self.src.len and !isWhiteSpace(self.src[self.current]) and isAlphaNum(self.src[self.current])) : (self.current += 1) {}

        tok.text = self.src[start..self.current];

        if (mem.eql(u8, tok.text, "if")) {
            tok.type = .if_;
        } else if (mem.eql(u8, tok.text, "else")) {
            tok.type = .else_;
        } else if (mem.eql(u8, tok.text, "switch")) {
            tok.type = .switch_;
        } else if (mem.eql(u8, tok.text, "case")) {
            tok.type = .case;
        } else if (mem.eql(u8, tok.text, "default")) {
            tok.type = .default;
        } else if (mem.eql(u8, tok.text, "while")) {
            tok.type = .while_;
        } else if (mem.eql(u8, tok.text, "do")) {
            //TODO: ADD DO WHILE BLOCK
            tok.type = .do;
        } else if (mem.eql(u8, tok.text, "for")) {
            tok.type = .for_;
        } else if (mem.eql(u8, tok.text, "defer")) {
            tok.type = .defer_;
        } else if (mem.eql(u8, tok.text, "export")) {
            //TODO: ADD MODULE SYSTEM AND EXPORT
            tok.type = .export_;
        } else if (mem.eql(u8, tok.text, "true")) {
            tok.type = .true;
        } else if (mem.eql(u8, tok.text, "false")) {
            tok.type = .false;
        }

        return tok;
    }

    fn parseNum(self: *Lexer) Token {
        var tok: Token = .{
            .type = .num,
            .text = undefined,
            .loc = TokenLoc.init(self.path, self.current_line, self.current_col),
        };
        const start: usize = self.current;

        while (self.current < self.src.len and !isWhiteSpace(self.src[self.current]) and
            (isNum(self.src[self.current]) or self.src[self.current] == '.'))
        {
            if (self.src[self.current] == '.' and self.current < self.src.len and !isNum(self.src[self.current + 1])) break;
            self.current += 1;
        }

        tok.text = self.src[start..self.current];

        return tok;
    }

    fn parseStr(self: *Lexer) Token {
        var tok: Token = .{
            .type = .str,
            .text = undefined,
            .loc = TokenLoc.init(self.path, self.current_line, self.current_col),
        };

        self.current += 1;
        const start = self.current;

        while (self.current < self.src.len and self.src[self.current] != '"') : (self.current += 1) {}

        self.current += 1;

        tok.text = self.src[start..(self.current - 1)];

        return tok;
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    fn isNum(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isAlphaNum(c: u8) bool {
        return isAlpha(c) or isNum(c);
    }

    fn isWhiteSpace(c: u8) bool {
        return c == ' ' or c == '\n' or c == '\r' or c == '\t';
    }
};
