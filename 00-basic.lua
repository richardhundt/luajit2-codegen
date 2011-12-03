local bc = require"codegen"

local main = bc.Proto(bc.Proto.VARARG)
local base = main:reg()
local args = main:list(2)
local var1 = main:var("a")

main:op_loadk(var1, 41)
main:op_add(var1, 1, var1)
main:op_getglobal(base, "print")
main:op_loadk(args[1], "answer")
main:op_move(args[2], var1)
main:op_call(base, 1, args)

local func = main:child()
local base = func:reg()
local args = func:list(2)
local var1 = func:var("b")
local test = func:var("a")

--func:op_loadk(a, 1)
--func:op_loadk(b, 2)
--func:op_comp('LE', b, a, "skip")
func:op_test(false, test, "skip")
func:op_loadk(var1, 69)
func:op_getglobal(base, "print")
func:op_loadk(args[1], "answer")
func:op_move(args[2], var1)
func:op_call(base, 1, args)
func:here("skip")

local args = main:list(0)
local dest = main:reg()
main:op_closure(dest, func)
main:op_call(dest, 1, args)

local dump = bc.Dump(main, "=00-basic.lua")
local code = assert(loadstring(dump:pack()))
code()


