local bc = require"codegen"

local main = bc.Proto(bc.Proto.VARARG)
local args = main:list(2)
main:op_loadk(args[1], 1)
main:op_loadk(args[2], 10)
local loop = main:op_fori(args[1], args[2])
local v = main:var('v')
main:op_loadk(v, 1)
main:op_add(v, 41)
main:op_forl(loop)

local dump = bc.Dump(main, "=05-loops.lua")
local code = assert(loadstring(dump:pack()))
code()



