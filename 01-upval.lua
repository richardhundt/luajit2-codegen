local bc = require"codegen"

local main = bc.Proto(bc.Proto.VARARG)
local var1 = main:var("a")
main:op_loadk(var1, "Hello World!")

local fun0 = main:child()

local fun1 = fun0:child()
local base = fun1:reg()
local args = fun1:list(1)
fun1:op_getglobal(base, "print")
fun1:op_getupval(args[1], "a")
fun1:op_call(base, 1, args)

fun1:op_setupval("a", 42)

fun1:op_getglobal(base, "print")
fun1:op_getupval(args[1], "a")
fun1:op_call(base, 1, args)

local base = fun0:reg()
fun0:op_closure(base, fun1)
fun0:op_call(base)

local base = main:reg()
main:op_closure(base, fun0)
main:op_call(base)

local b = main:var("b")
main:op_loadk(b, 42)
main:op_ne(b, 42, "out")

local f = main:reg()
local a = main:list(1)
main:op_getglobal(f, "print")
main:op_loadk(a[1], "OK")
main:op_call(f, 1, a)
main:here("out")

local dump = bc.Dump(main, "=01-upval.lua")
local code = assert(loadstring(dump:pack()))
code()


