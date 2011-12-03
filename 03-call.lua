local bc = require"codegen"

local f0 = bc.Proto(bc.Proto.VARARG)
local f1 = f0:child(bc.Proto.VARARG)

local a1 = f1:param("a")
local r1 = f1:reg()
local l1 = f1:list(1)
f1:op_getglobal(r1, "print")
f1:op_move(l1[1], a1)
f1:op_varg(l1, 0)
f1:op_call(r1, 1, l1)

local c1 = f0:reg()
local l1 = f0:list(2)
f0:op_closure(c1, f1)
f0:op_loadk(l1[1], "answer")
f0:op_loadk(l1[2], 42)
f0:op_call(c1, 1, l1)

local dump = bc.Dump(f0, "=03-call.lua")
local code = assert(loadstring(dump:pack()))
code()


