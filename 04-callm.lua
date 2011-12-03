local bc = require"codegen"

local f0 = bc.Proto(bc.Proto.VARARG)
local f1 = f0:child(bc.Proto.VARARG)

local p1 = f1:param('a')
local l1 = f1:list(2)
f1:op_loadk(l1[1],'answer')
f1:op_loadk(l1[2], 42)
f1:op_return(l1)

local r1 = f0:reg()
local r2 = f0:reg()
f0:op_getglobal(r1, "print")
f0:op_closure(r2, f1)
f0:op_call(r2, 0, 1)
f0:op_call(r1, 1, 0)

local dump = bc.Dump(f0, "=04-callm.lua")
local code = assert(loadstring(dump:pack()))
code()


