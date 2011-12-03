local bc = require"codegen"

local f0 = bc.Proto(bc.Proto.VARARG)

local f1 = f0:child()
local r1 = f1:reg()
f1:op_loadk(r1, "forty-two")
f1:op_return(r1)

local c1 = f0:reg()
local r1 = f0:reg()
local a1 = f0:list(1)
f0:op_closure(c1, f1)
f0:op_call(c1, 2)
f0:op_getglobal(r1, "print")
f0:op_move(a1[1], c1)
f0:op_call(r1, 1, a1)

local dump = bc.Dump(f0, "=02-return.lua")
local code = assert(loadstring(dump:pack()))
code()


