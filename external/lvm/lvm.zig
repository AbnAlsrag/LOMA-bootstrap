// ██╗     ██╗   ██╗███╗   ███╗
// ██║     ██║   ██║████╗ ████║
// ██║     ██║   ██║██╔████╔██║
// ██║     ╚██╗ ██╔╝██║╚██╔╝██║
// ███████╗ ╚████╔╝ ██║ ╚═╝ ██║
// ╚══════╝  ╚═══╝  ╚═╝     ╚═╝
// This Software Was Made by ABN ALSRAG
// LVM

// This software is provided 'as-is', without any express or implied
// warranty.  In no event will the authors be held liable for any damages
// arising from the use of this software.

// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:

// 1. The origin of this software must not be misrepresented; you must not
//     claim that you wrote the original software. If you use this software
//     in a product, an acknowledgment in the product documentation would be
//     appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//     misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.

// TODO: REMOVE ALL THE CHATCH UNREACHABLE AND ADD BETTER ERROR HANDLING

const std = @import("std");
const mem = std.mem;

const MAGIC = 0x45564F4C;
const VERSION = 0;

// TODO: MAKE STACK VARIABLE LENGTH
const STACK_MAX = 1024;

const REGISTER_SIZE = 8;

pub const Word = u64;
pub const Register = Word;

pub fn wordFromI64(data: i64) Word {
    return @bitCast(data);
}

pub fn wordFromU64(data: u64) Word {
    return data;
}

pub fn wordFromF64(data: f64) Word {
    return @bitCast(data);
}

pub fn i64FromWord(word: Word) i64 {
    return @bitCast(word);
}

pub fn u64FromWord(word: Word) u64 {
    return word;
}

pub fn f64FromWord(word: Word) f64 {
    return @bitCast(word);
}

pub fn printWord(word: Word) void {
    std.debug.print("WORD[i64: {}, u64: {}, f64: {}, ptr: 0x{x}]\n", .{ i64FromWord(word), word, f64FromWord(word), word });
}

pub const Machine = struct {
    pub const Config = struct {
        allocator: std.mem.Allocator,
        program: ?Program = null,
    };

    config: Config,

    program: Program,
    stack: [STACK_MAX]Word = [_]Word{0} ** STACK_MAX,
    sp: usize = 0,

    hlt: bool = false,

    pub fn init(config: Config) Machine {
        var machine: Machine = Machine{
            .config = config,
            .program = undefined,
        };

        if (config.program) |prog| {
            machine.program = prog;
        } else {
            machine.program = Program.init(config.allocator);
        }

        return machine;
    }

    pub fn deinit(self: *Machine) void {
        if (self.config.program == null) {
            self.program.deinit();
        }
    }

    pub fn run(self: *Machine, max_insts: isize) void {
        var i: isize = max_insts;

        while (!self.hlt and i != 0 and self.program.insts.items.len > 0) {
            self.program.executeInst(self) catch |err| {
                std.debug.print("[ERROR] {s} caused by instruction {}\n", .{ @errorName(err), self.program.pc });
                @panic("");
            };

            i -= 1;
        }
    }
};

pub const Compiler = struct {
    pub const Target = struct {
        ptr: *anyopaque,
        compileFn: *const fn (ptr: *anyopaque, program: Program) Output,

        fn compile(self: Target, program: Program) Output {
            return self.compileFn(self.ptr, program);
        }
    };

    pub const x86_64_nasm_linux_target = struct {
        fn compileFn(ptr: *anyopaque, program: Program) Output {
            _ = ptr;
            var output: Output = Output{
                .allocator = program.allocator,
                .code = undefined,
            };

            var buf: std.ArrayList(u8) = std.ArrayList(u8).init(output.allocator);
            defer buf.deinit();

            buf.writer().print("%define EXIT_OK 0\n", .{}) catch unreachable;

            buf.writer().print("%define SYS_EXIT 60\n", .{}) catch unreachable;
            buf.writer().print("%define SYS_WRITE 1\n", .{}) catch unreachable;

            buf.writer().print("segment .text\n", .{}) catch unreachable;
            buf.writer().print("global _start\n", .{}) catch unreachable;

            buf.writer().print("print_debug:\n", .{}) catch unreachable;
            buf.writer().print("push    rbp\n", .{}) catch unreachable;
            buf.writer().print("mov     rbp, rsp\n", .{}) catch unreachable;
            buf.writer().print("sub     rsp, 64\n", .{}) catch unreachable;
            buf.writer().print("mov     QWORD [rbp-56], rdi\n", .{}) catch unreachable;
            buf.writer().print("mov     QWORD [rbp-48], 0\n", .{}) catch unreachable;
            buf.writer().print("mov     QWORD [rbp-40], 0\n", .{}) catch unreachable;
            buf.writer().print("mov     QWORD [rbp-32], 0\n", .{}) catch unreachable;
            buf.writer().print("mov     QWORD [rbp-24], 0\n", .{}) catch unreachable;
            buf.writer().print("mov     QWORD [rbp-8], 1\n", .{}) catch unreachable;
            buf.writer().print("mov     eax, 32\n", .{}) catch unreachable;
            buf.writer().print("sub     rax, QWORD [rbp-8]\n", .{}) catch unreachable;
            buf.writer().print("mov     BYTE [rbp-48+rax], 10\n", .{}) catch unreachable;
            buf.writer().print(".L2:\n", .{}) catch unreachable;
            buf.writer().print("mov     rcx, QWORD [rbp-56]\n", .{}) catch unreachable;
            buf.writer().print("mov  rdx, -3689348814741910323\n", .{}) catch unreachable;
            buf.writer().print("mov     rax, rcx\n", .{}) catch unreachable;
            buf.writer().print("mul     rdx\n", .{}) catch unreachable;
            buf.writer().print("shr     rdx, 3\n", .{}) catch unreachable;
            buf.writer().print("mov     rax, rdx\n", .{}) catch unreachable;
            buf.writer().print("sal     rax, 2\n", .{}) catch unreachable;
            buf.writer().print("add     rax, rdx\n", .{}) catch unreachable;
            buf.writer().print("add     rax, rax\n", .{}) catch unreachable;
            buf.writer().print("sub     rcx, rax\n", .{}) catch unreachable;
            buf.writer().print("mov     rdx, rcx\n", .{}) catch unreachable;
            buf.writer().print("mov     eax, edx\n", .{}) catch unreachable;
            buf.writer().print("lea     edx, [rax+48]\n", .{}) catch unreachable;
            buf.writer().print("mov     eax, 31\n", .{}) catch unreachable;
            buf.writer().print("sub     rax, QWORD [rbp-8]\n", .{}) catch unreachable;
            buf.writer().print("mov     BYTE [rbp-48+rax], dl\n", .{}) catch unreachable;
            buf.writer().print("add     QWORD [rbp-8], 1\n", .{}) catch unreachable;
            buf.writer().print("mov     rax, QWORD [rbp-56]\n", .{}) catch unreachable;
            buf.writer().print("mov  rdx, -3689348814741910323\n", .{}) catch unreachable;
            buf.writer().print("mul     rdx\n", .{}) catch unreachable;
            buf.writer().print("mov     rax, rdx\n", .{}) catch unreachable;
            buf.writer().print("shr     rax, 3\n", .{}) catch unreachable;
            buf.writer().print("mov     QWORD [rbp-56], rax\n", .{}) catch unreachable;
            buf.writer().print("cmp     QWORD [rbp-56], 0\n", .{}) catch unreachable;
            buf.writer().print("jne     .L2\n", .{}) catch unreachable;
            buf.writer().print("mov     eax, 32\n", .{}) catch unreachable;
            buf.writer().print("sub     rax, QWORD [rbp-8]\n", .{}) catch unreachable;
            buf.writer().print("lea     rdx, [rbp-48]\n", .{}) catch unreachable;
            buf.writer().print("lea     rcx, [rdx+rax]\n", .{}) catch unreachable;
            buf.writer().print("mov     rax, QWORD [rbp-8]\n", .{}) catch unreachable;
            buf.writer().print("mov     rdx, rax\n", .{}) catch unreachable;
            buf.writer().print("mov     rsi, rcx\n", .{}) catch unreachable;
            buf.writer().print("mov     edi, 1\n", .{}) catch unreachable;
            buf.writer().print("mov     rax, SYS_WRITE\n", .{}) catch unreachable;
            buf.writer().print("syscall\n", .{}) catch unreachable;
            buf.writer().print("nop\n", .{}) catch unreachable;
            buf.writer().print("leave\n", .{}) catch unreachable;
            buf.writer().print("ret\n", .{}) catch unreachable;

            buf.writer().print("_start:\n", .{}) catch unreachable;

            for (program.insts.items) |inst| {
                switch (inst.opcode) {
                    .push => {
                        buf.writer().print("push {}\n", .{inst.operand}) catch unreachable;
                    },
                    .pop => {
                        buf.writer().print("pop\n", .{}) catch unreachable;
                    },
                    .add => {
                        buf.writer().print("pop rcx\n", .{}) catch unreachable;
                        buf.writer().print("pop rdx\n", .{}) catch unreachable;
                        buf.writer().print("add rcx, rdx\n", .{}) catch unreachable;
                        buf.writer().print("push rcx\n", .{}) catch unreachable;
                    },
                    .sub => {
                        buf.writer().print("pop rcx\n", .{}) catch unreachable;
                        buf.writer().print("pop rdx\n", .{}) catch unreachable;
                        buf.writer().print("sub rcx, rdx\n", .{}) catch unreachable;
                        buf.writer().print("push rcx\n", .{}) catch unreachable;
                    },
                    .jmp => {
                        unreachable;
                    },
                    .hlt => {
                        buf.writer().print("mov rdi, EXIT_OK\n", .{}) catch unreachable;
                        buf.writer().print("mov rax, SYS_EXIT\n", .{}) catch unreachable;
                        buf.writer().print("syscall\n", .{}) catch unreachable;
                    },
                    .print_debug => {
                        buf.writer().print("pop rdi\n", .{}) catch unreachable;
                        buf.writer().print("call print_debug\n", .{}) catch unreachable;
                    },
                    .max_insts => {
                        @panic("[ERROR] UNREACHABLE");
                    },
                }
            }

            output.code = buf.toOwnedSlice() catch unreachable;

            return output;
        }

        pub fn target() Target {
            return Target{
                .ptr = undefined,
                .compileFn = compileFn,
            };
        }
    };

    //TODO: REFACTOR THIS SHIT
    //TODO: ADD DEINIT FN THAT DEALLOCATES THE BUFFER
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
    };

    pub fn compile(program: Program, target: Target) Output {
        return target.compile(program);
    }
};

pub const Linker = struct {
    pub fn link(allocator: std.mem.Allocator, modules: []const Module) Module {
        _ = modules;
        var module = Module.init(allocator);
        _ = module;

        @panic("[ERROR] UNIMPLEMENTED");

        // return module;
    }
};

pub const Opcode = enum(u8) {
    push,
    pop,
    dup,
    add,
    sub,
    jmp,
    call,
    syscal,
    ld8,
    hlt,
    print_debug,
    max_insts,
};

pub const InstDef = struct {
    opcode: Opcode,
    name: []const u8,
    takes_operand: bool,
};

pub const Module = struct {
    pub const Operand = union(enum) {
        symbol: []const u8,
        word: Word,
    };

    pub const Inst = struct {
        opcode: Opcode,
        operand: Operand,
    };

    pub const Symbol = struct {
        name: []const u8,
        loc: Word,
        public: bool,
    };

    const ResolveSymbolError = error{
        SymbolNotFound,
    };

    pub const LinkingError = error{
        ModuleNotLinked,
    };

    allocator: std.mem.Allocator,
    insts: std.ArrayList(Inst),
    symbols: std.ArrayList(Symbol),

    linked: bool = false,

    pub fn init(allocator: std.mem.Allocator) Module {
        const module: Module = Module{
            .allocator = allocator,
            .insts = std.ArrayList(Inst).init(allocator),
            .symbols = std.ArrayList(Symbol).init(allocator),
        };

        return module;
    }

    pub fn deinit(self: *Module) void {
        self.insts.deinit();
        self.symbols.deinit();
    }

    pub fn pushInst(self: *Module, inst: Inst) void {
        self.insts.append(inst) catch unreachable;
    }

    //TODO: ADD ERROR CHECKING FOR DUPLICATES
    pub fn pushSymbol(self: *Module, symbol: Symbol) void {
        self.symbols.append(symbol) catch unreachable;
    }

    fn findSymbol(self: *Module, symbol: []const u8) ResolveSymbolError!*Symbol {
        for (self.symbols.items) |*s| {
            if (std.mem.eql(u8, s.name, symbol)) {
                return s;
            }
        }

        return ResolveSymbolError.SymbolNotFound;
    }

    pub fn resolveSymbols(self: *Module) ResolveSymbolError!void {
        var resolved: bool = true;

        for (self.insts.items) |*inst| {
            if (inst.operand == .symbol) {
                if (self.findSymbol(inst.operand.symbol)) |symbol| {
                    inst.operand = .{ .word = symbol.loc };
                } else |_| {
                    resolved = false;
                }
            }
        }

        self.linked = resolved;
    }

    //FIXME: THERE IS A BUG THAT I CAN'T DEINIT PROGRAN
    pub fn toProgram(self: *Module) LinkingError!Program {
        var program: Program = Program.init(self.allocator);

        if (!self.linked) {
            return LinkingError.ModuleNotLinked;
        }

        for (self.insts.items) |inst| {
            program.pushInst(.{ .opcode = inst.opcode, .operand = inst.operand.word });
        }

        return program;
    }

    pub fn saveToFile(self: *Module, path: []const u8) void {
        _ = self;
        _ = path;
        @panic("[ERROR] UNIMPLEMENTED");
    }

    pub fn loadFromFile(self: *Module, path: []const u8) void {
        _ = self;
        _ = path;
        @panic("[ERROR] UNIMPLEMENTED");
    }
};

pub const Program = struct {
    pub const Inst = struct {
        opcode: Opcode,
        operand: Word,
    };

    pub const Exeception = error{
        StackOverflow,
        StackUnderflow,
    };

    allocator: std.mem.Allocator,
    insts: std.ArrayList(Inst),
    pc: usize = 0,

    pub fn init(allocator: std.mem.Allocator) Program {
        const program: Program = Program{
            .allocator = allocator,
            .insts = std.ArrayList(Inst).init(allocator),
        };

        return program;
    }

    pub fn deinit(self: *Program) void {
        self.insts.deinit();
    }

    pub fn pushInst(self: *Program, inst: Inst) void {
        self.insts.append(inst) catch unreachable;
    }

    fn executeInst(self: *Program, machine: *Machine) Exeception!void {
        const inst: Inst = self.insts.items[self.pc];

        switch (inst.opcode) {
            .push => {
                if (machine.sp >= STACK_MAX) {
                    return Exeception.StackOverflow;
                }

                machine.stack[machine.sp] = inst.operand;
                machine.sp += 1;

                self.pc += 1;
            },
            .pop => {
                if (machine.sp <= 0) {
                    return Exeception.StackUnderflow;
                }

                machine.sp -= 1;

                self.pc += 1;
            },
            .dup => {
                if (machine.sp >= STACK_MAX) {
                    return Exeception.StackOverflow;
                }

                if (machine.sp <= 0) {
                    return Exeception.StackUnderflow;
                }

                machine.stack[machine.sp] = machine.stack[machine.sp - 1];
                machine.sp += 1;

                self.pc += 1;
            },
            .add => {
                if (machine.sp < 2) {
                    return Exeception.StackUnderflow;
                }

                const a: Word = machine.stack[machine.sp - 1];
                const b: Word = machine.stack[machine.sp - 2];
                machine.stack[machine.sp - 2] = a + b;

                machine.sp -= 1;
                self.pc += 1;
            },
            .sub => {
                if (machine.sp < 2) {
                    return Exeception.StackUnderflow;
                }

                const a: Word = machine.stack[machine.sp - 1];
                const b: Word = machine.stack[machine.sp - 2];
                machine.stack[machine.sp - 2] = b - a;

                machine.sp -= 1;
                self.pc += 1;
            },
            .jmp => {
                self.pc = inst.operand;
            },
            .hlt => {
                machine.hlt = true;
            },
            .print_debug => {
                const word: Word = machine.stack[machine.sp - 1];
                printWord(word);

                machine.sp -= 1;
                self.pc += 1;
            },
            .max_insts => {
                @panic("[ERROR] UNREACHABLE");
            },
        }
    }

    //TODO: UNIMPLEMENT
    pub fn saveToFile(self: Machine, path: []const u8) !void {
        _ = path;
        _ = self;
        // var file = try std.fs.cwd().createFile(path, .{});
        // defer file.close();

        // var buf_writer = std.io.bufferedWriter(file.writer());
        // var out_stream = buf_writer.writer();

        // const meta_data = MetaData.init(self.program_size, 0);

        // _ = try out_stream.writeInt(u32, meta_data.magic, .Little);
        // _ = try out_stream.writeInt(u16, meta_data.version, .Little);
        // _ = try out_stream.writeInt(u64, meta_data.program_size, .Little);
        // _ = try out_stream.writeInt(u64, meta_data.memory_size, .Little);

        // _ = try out_stream.writeAll(@as([*]const u8, @ptrCast(&self.program))[0 .. self.program_size * @sizeOf(Inst)]);

        // try buf_writer.flush();
    }

    //TODO: UNIMPLEMENT
    pub fn loadFromFile(self: *Machine, path: []const u8) !void {
        _ = path;
        _ = self;
        // var file = try std.fs.cwd().openFile(path, .{});
        // defer file.close();

        // var buf_reader = std.io.bufferedReader(file.reader());
        // var in_stream = buf_reader.reader();

        // var meta_data: MetaData = undefined;

        // meta_data.magic = try in_stream.readInt(u32, .Little);
        // meta_data.version = try in_stream.readInt(u16, .Little);
        // meta_data.program_size = try in_stream.readInt(u64, .Little);
        // meta_data.memory_size = try in_stream.readInt(u64, .Little);

        // self.program_size = meta_data.program_size;

        // _ = try in_stream.readAll(@as([*]u8, @ptrCast(&self.program))[0 .. self.program_size * @sizeOf(Inst)]);
    }
};
