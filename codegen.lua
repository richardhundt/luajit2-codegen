--[=[
 dump   = header proto+ 0U
 header = ESC 'L' 'J' versionB flagsU [namelenU nameB*]
 proto  = lengthU pdata
 pdata  = phead bcinsW* kgc* knum* uvdataH* [debugB*]
 phead  = flagsB numparamsB framesizeB numuvB numkgcU numknU numbcU
          [debuglenU [firstlineU numlineU]]
 kgc    = kgctypeU { ktab | (loU hiU) | (rloU rhiU iloU ihiU) | strB* }
 knum   = intU0 | (loU1 hiU)
 ktab   = narrayU nhashU karray* khash*
 karray = ktabk
 khash  = ktabk ktabk
 ktabk  = ktabtypeU { intU | (loU hiU) | strB* }
 uvdata = register index with high bit set if local to outer
 debug  = lninfoV* uvals vars '\0'
 uvals  = nameB* '\0'
 vars   = nameB* '\0' startU endU

 B = 8 bit,
 H = 16 bit,
 W = 32 bit,
 V = B, H or W,
 U = ULEB128 of W, U0/U1 = ULEB128 of W+1,
]=]

local DEBUG = true
local bit = require 'bit'

local typeof = getmetatable

local function enum(t)
   for i=0,#t do t[t[i]] = i end
   return t
end

local IDGEN = 0
local function genid()
   IDGEN = IDGEN + 1
   return '_'..IDGEN
end

local Class = { }
function Class:__call(...)
   local obj = setmetatable({ }, self)
   if self.__init then self.__init(obj, ...) end
   return obj
end
local function class(proto)
   proto.__index = proto
   function proto:__tostring()
      return self.__name
   end
   return setmetatable(proto, Class)
end

-- forward declarations
local Buf, Ins, Reg, List, Rest, Proto, Dump, KNum, KObj

local BC = enum {
   [0] = 'ISLT', 'ISGE', 'ISLE', 'ISGT', 'ISEQV', 'ISNEV', 'ISEQS','ISNES',
   'ISEQN', 'ISNEN', 'ISEQP', 'ISNEP', 'ISTC', 'ISFC', 'IST', 'ISF', 'MOV',
   'NOT', 'UNM', 'LEN', 'ADDVN', 'SUBVN', 'MULVN', 'DIVVN', 'MODVN', 'ADDNV',
   'SUBNV', 'MULNV', 'DIVNV', 'MODNV', 'ADDVV', 'SUBVV', 'MULVV', 'DIVVV',
   'MODVV', 'POW', 'CAT', 'KSTR', 'KCDATA', 'KSHORT', 'KNUM', 'KPRI', 'KNIL',
   'UGET', 'USETV', 'USETS', 'USETN', 'USETP', 'UCLO', 'FNEW', 'TNEW', 'TDUP',
   'GGET', 'GSET', 'TGETV', 'TGETS', 'TGETB', 'TSETV', 'TSETS', 'TSETB',
   'TSETM', 'CALLM', 'CALL', 'CALLMT', 'CALLT', 'ITERC', 'ITERN', 'VARG',
   'ISNEXT', 'RETM', 'RET', 'RET0', 'RET1', 'FORI', 'JFORI', 'FORL', 'IFORL',
   'JFORL', 'ITERL', 'IITERL', 'JITERL', 'LOOP', 'ILOOP', 'JLOOP', 'JMP',
   'FUNCF', 'IFUNCF', 'JFUNCF', 'FUNCV', 'IFUNCV', 'JFUNCV', 'FUNCC', 'FUNCCW',
}

local OP_FOLD = {
   ADD = function(a, b) return a + b end;
   SUB = function(a, b) return a - b end;
   MUL = function(a, b) return a * b end;
   DIV = function(a, b) return a / b end;
   MOD = function(a, b) return a % b end;
   POW = function(a, b) return a ^ b end;
}

local BC_ABC = 0
local BC_AD  = 1
local BC_AJ  = 2

local BC_MODE = {
   [0] = 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1,
   1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1,
   1, 0, 0, 0, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 2, 2, 1, 2, 2, 1, 2, 1,
   1, 1, 1, 1, 1, 1, 1,
}

local VKNIL   = 0
local VKFALSE = 1
local VKTRUE  = 2

local NO_JMP = bit.bnot(0)

local KOBJ = enum {
   [0] = "CHILD", "TAB", "I64", "U64", "COMPLEX", "STR",
}
local KTAB = enum {
   [0] = "NIL", "FALSE", "TRUE", "INT", "NUM", "STR",
}

local FOR_IDX   = "(for index)";
local FOR_STOP  = "(for limit)";
local FOR_STEP  = "(for step)";
local FOR_GEN   = "(for generator)";
local FOR_STATE = "(for state)";
local FOR_CTL   = "(for control)";

Buf = class { __name = 'Buf' }
function Buf:__init(tab)
   if tab ~= nil then
      for i=1, #tab do self[#self + 1] = tab[i] end
   end
end
function Buf:put(v, o)
   table.insert(self, o or #self + 1, v)
end
function Buf:put_uint8(v, o)
   if not o then o = #self + 1 end
   self:put(v % 256, o)
end
function Buf:put_uint16(v, o)
   if not o then o = #self + 1 end
   self:put(v % 256, o)
   v = math.floor(v / 256)
   self:put(v % 256, o + 1)
end
function Buf:put_uint32(v, o)
   if not o then o = #self + 1 end
   self:put(v % 256, o)
   v = math.floor(v / 256)
   self:put(v % 256, o + 1)
   v = math.floor(v / 256)
   self:put(v % 256, o + 2)
   v = math.floor(v / 256)
   self:put(v % 256, o + 3)
end
function Buf:put_uleb128(v, o)
   if not o then o = #self + 1 end
   local i = 0
   repeat
      local b = bit.band(v, 0x7f)
      v = bit.rshift(v, 7)
      if v ~= 0 then
         b = bit.bor(b, 0x80)
      end
      self:put(b, o + i)
      i = i + 1
   until v == 0
end
function Buf:put_literal(v, o)
   if not o then o = #self + 1 end
   for i=1, #v do
      self:put(string.byte(v, i), o + i - 1)
   end
end
function Buf:pack()
   local out = { }
   for i=1, #self do
      out[#out + 1] = string.char(self[i])
   end
   return table.concat(out)
end

Ins = class{ __name = 'Ins' }
function Ins:__init(op, a, b, c)
   self[1] = op
   self[2] = a or 0
   self[3] = b or 0
   self[4] = c or 0
end
function Ins:__tostring()
   local buf = { BC[self[1]] }
   for i=2, 4 do
      buf[#buf + 1] = tostring(self[i])
   end
   return 'Ins{'..table.concat(buf,',')..'}'
end
function Ins:sync(ctx, pc)
   for i=1, #self do
      if type(self[i]) == 'table' and self[i].sync then
         self[i]:sync(ctx, pc)
      end
   end
end
function Ins:alloc(ctx)
   for i=2, #self do
      local arg = self[i]
      if type(arg) == 'table' and arg.idx then
         self[i] = arg.idx
      elseif typeof(arg) == List then
         if arg.varg then
            self[i] = #ctx.params
         else
            self[i] = #arg + 1
         end
      else
         assert(type(arg) == "number")
      end
   end
end
function Ins:write(buf)
   local op, a = self[1], self[2]
   buf:put(op)
   buf:put(a)
   local mode = BC_MODE[op]
   if mode == BC_ABC then
      local b, c = self[3], self[4]
      buf:put(c)
      buf:put(b)
   elseif mode == BC_AD then
      local d = self[3]
      buf:put_uint16(d)
   elseif mode == BC_AJ then
      local j = self[3]
      buf:put_uint16(j + 0x8000)
   else
      error("bad instruction ["..tostring(op).."] (op mode unknown)")
   end
end

KObj = class { __name = 'KObj' }
function KObj:__init(v)
   self[1] = v
end
function KObj:write(buf)
   local t, v = type(self[1]), self[1]
   if t == "string" then
      self:write_string(buf, v)
   elseif t == 'table' then
      if typeof(v) == Proto then
         self:write_proto(buf, v)
      else
         self:write_table(buf, v)
      end
   end
end
function KObj:write_string(buf, v)
   buf:put_uleb128(KOBJ.STR + #v)
   for i=1, #v do
      buf:put(string.byte(v, i))
   end
end
function KObj:write_table(buf, v)
   local seen = { }
   for i, v in ipairs(v) do
      seen[i] = true
   end
end

KNum = class { __name = 'KNum' }
function KNum:__init(v)
   self[1] = v
end
function KNum:write(buf)
   local v, s, m, e = self[1], 0
   if v < 0 then s, v = 1, -v end

   if v == 0 then
      m, e = 0, 0
   elseif v == 1/0 then
      m, e = 0, 0x7ff -- inf
   elseif v ~= v then
      m, e = 1, 0x7ff -- NaN
   else
      m, e = math.frexp(v) -- we lose precision here :(
      m, e = (m * 2 - 1) * 2^52, e + 1022
   end

   local u64 = s * 2^64 + e * 2^52 + m
   local u32_lo = u64 % 0x100000000
   local u32_hi = math.floor(u64 / 0x100000000)

   buf:put_uleb128(1 + 2 * u32_lo) -- 33 bits with lsb set
   if u32_lo >= 0x80000000 then
      buf[-1] = bit.bor(buf[-1], 0x10)
   end
   buf:put_uleb128(u32_hi)
end

Reg = class {
   __name = 'Reg';
   bot = 0x100;
   top = -1;
   idx = -1;
}
function Reg:__init(proto)
   self.proto = proto
end
function Reg:sync(ctx, pc)
   if ctx == self.proto then
      if self.bot > 0xff then
         self.bot = pc
         ctx.vstack[#ctx.vstack + 1] = self
      end
      self.top = pc
   else
      self.top = 65536
   end
end

List = class { __name = 'List' }
function List:__tostring()
   local buf = { }
   for i=1, #self do
      buf[#buf + 1] = tostring(self[i])
   end
   return 'List{'..table.concat(buf,',')..'}'
end
function List:__init(proto)
   self.proto = proto
end
function List:sync(ctx, pc)
   for i=1, #self do self[i]:sync(ctx, pc) end
end

Rest = class { __name = 'Rest' }
function Rest:__init(list)
   self[1] = list
end
function Rest:sync(ctx, pc)
   local list = args[1]
   list:sync(ctx, pc)
end

Proto = class {
   __name = 'Proto';
   CHILD  = 0x01; -- Has child prototypes.
   VARARG = 0x02; -- Vararg function.
   FFI    = 0x04; -- Uses BC_KCDATA for FFI datatypes.
   NOJIT  = 0x08; -- JIT disabled for this function.
   ILOOP  = 0x10; -- Patched bytecode with ILOOP etc.
}

function Proto:__init(flags, outer)
   self.flags  = flags or 0
   self.outer  = outer
   self.params = { }
   self.upvals = { }
   self.vstack = { }
   self.code   = { }
   self.kobj   = { }
   self.knum   = { }
   self.debug  = { }
   self.lninfo = { }
   self.uvinfo = { }
   self.locals = { }
   self.labels = { }
   self.kcache = { }
   self.currline  = 1
   self.firstline = 1
   self.numlines  = 1
   self.framesize = 0
end
function Proto:child(flags)
   self.flags = bit.bor(self.flags, Proto.CHILD)
   local child = Proto(flags, self)
   self.kobj[child] = #self.kobj
   self.kobj[#self.kobj + 1] = child
   return child
end
function Proto:const(val)
   if type(val) == 'string' then
      if not self.kcache[val] then
         local item = KObj(val)
         item.idx = #self.kobj
         self.kcache[val] = item
         self.kobj[#self.kobj + 1] = item
      end
   elseif type(val) == 'number' then
      if not self.kcache[val] then
         local item = KNum(val)
         item.idx = #self.knum
         self.kcache[val] = item
         self.knum[#self.knum + 1] = item
      end
   end
   return self.kcache[val].idx
end
function Proto:line(ln)
   self.currline = ln
   if ln > self.currline then
      self.numlines = ln
   end
end
function Proto:emit(op, a, b, c)
   local ins = Ins(op, a, b, c)
   self.code[#self.code + 1] = ins
   ins:sync(self, #self.code)
   self.lninfo[#self.lninfo + 1] = self.currline
   return ins
end
function Proto:write(buf)
   -- allocate all registers first so that upvalues
   -- can be referenced correctly from child protos 
   self:alloc_vars()

   local has_child
   if bit.band(self.flags, Proto.CHILD) ~= 0 then
      has_child = true
      -- recursively write children
      for i=1, #self.kobj do
         local o = self.kobj[i]
         if typeof(o) == Proto then
            o:write(buf)
         end
      end
   end

   local ofs = #buf
   if has_child then
      self:emit(BC.UCLO, 0, 0) -- close upvals
   end
   self:emit(BC.RET0, 0, 1)
   self:write_head(buf)
   self:write_body(buf)

   local len = #buf - ofs
   buf:put_uleb128(len, ofs + 1) -- length of the proto

   if DEBUG then
      local fun = assert(loadstring(buf:pack()))
      require("jit.bc").dump(fun)
   end
end
function Proto:write_head(buf)
   buf:put(self.flags)
   buf:put(#self.params)
   buf:put(self.framesize)
   buf:put(#self.upvals)
   buf:put_uleb128(#self.kobj)
   buf:put_uleb128(#self.knum)
   buf:put_uleb128(#self.code)
   self.ofsdbg = #buf
   buf:put_uleb128(self.firstline)
   buf:put_uleb128(self.numlines)
end
function Proto:write_body(buf)
   for i=1, #self.code do
      self.code[i]:write(buf)
   end
   for i=1, #self.upvals do
      local uval = self.upvals[i]
      if self.outer == uval.proto then
         buf:put_uint16(bit.bor(uval.idx, 0x8000))
      else
         self.outer:upval(uval.name)
         buf:put_uint16(uval.idx)
      end
   end
   for i=#self.kobj, 1, -1 do
      local o = self.kobj[i]
      if typeof(o) == Proto then
         buf:put_uleb128(KOBJ.CHILD)
      else
         self.kobj[i]:write(buf)
      end
   end
   for i=1, #self.knum do
      self.knum[i]:write(buf)   
   end
   self:write_debug(buf)
end
function Proto:write_debug(buf)
   local ofs = #buf
   local first = self.firstline
   if self.numlines < 256 then
      for i=1, #self.lninfo do
         local delta = self.lninfo[i] - first
         buf:put_uint8(delta)
      end
   elseif self.numlines < 65536 then 
      for i=1, #self.lninfo do
         local delta = self.lninfo[i] - first
         buf:put_uint16(delta)
      end
   else
      for i=1, #self.lninfo do
         local delta = self.lninfo[i] - first
         buf:put_uint32(delta)
      end
   end
   for i=1, #self.upvals do
      local uval = self.upvals[i]
      buf:put_literal(uval.name.."\0")
   end
   local lastpc = 0
   for i=1, #self.locals do
      local var = self.locals[i]
      local startpc, endpc = (var.bot or 0), (var.top or 0) + 1
      buf:put_literal(var.name.."\0")
      buf:put_uleb128(startpc - lastpc)
      buf:put_uleb128(endpc - startpc)
      lastpc = startpc
   end
   buf:put_uleb128(#buf - ofs, self.ofsdbg + 1)
end
function Proto:alloc_vars()
   local vstack, free, size = self.vstack, 0, 0

   -- stack-based register allocation?
   for i=1, #vstack do
      local reg = vstack[i]
      for j=i-1, 1, -1 do
         if vstack[j].top < reg.bot then
            free = vstack[j].idx
         else
            break
         end
      end
      reg.idx = free
      if free > size then
         size = free
      end
      free = free + 1
   end

   self.framesize = size

   for pc=1, #self.code do
      self.code[pc]:alloc(self)
   end
end
function Proto:reg()
   return Reg(self)
end
function Proto:var(name, reg)
   if not reg then reg = self:reg() end
   reg.name = name
   self.locals[name] = reg
   self.locals[#self.locals + 1] = reg
   return reg
end
function Proto:param(...)
   local reg = self:var(...)
   reg:sync(self, 0)
   self.params[#self.params + 1] = reg
   return reg
end
function Proto:list(size)
   assert(size ~= nil, "list constructor needs a size")
   local list = List(self)
   for i=1, size or 0 do
      list[#list + 1] = self:reg()
   end
   return list
end
function Proto:rest(list)
   return Rest(list)
end
function Proto:upval(name)
   if not self.upvals[name] then
      local proto, upval = self.outer
      while proto do
         if proto.locals[name] then
            upval = proto.locals[name]
            break
         end
         proto = proto.outer
      end
      if not upval then
         error("not found upvalue:"..name)
      end
      self.upvals[name] = upval
      self.upvals[#self.upvals + 1] = upval
   end
   return self.upvals[name]
end
function Proto:here(name)
   if name == nil then name = genid() end
   if self.labels[name] then
      -- forward jump
      local offs = self.labels[name]
      self.code[offs][3] = #self.code - offs
   else
      -- backward jump
      self.labels[name] = #self.code - 1
   end
   return name
end
function Proto:goto(name)
   if self.labels[name] then
      -- backward jump
      local offs = self.labels[name]
      return self:emit(BC.JMP, self:reg(), offs - #self.code)
   else
      -- forward jump
      self.labels[name] = #self.code + 1
      return self:emit(BC.JMP, self:reg(), 'JUMP')
   end
end
function Proto:op_jump(delta)
   return self:emit(BC.JMP, self:reg(), delta)
end
-- branch if condition
function Proto:op_test(cond, a, here)
   return self:emit(cond and BC.IST or BC.ISF, 0, a), self:goto(here)
end
-- branch if comparison
function Proto:op_comp(cond, a, b, here)
   if cond == 'LE' then
      cond = 'GE'
      a, b = b, a
   elseif cond == 'LT' then
      cond = 'GT'
      a, b = b, a
   elseif cond == 'EQ' or cond == 'NE' then
      -- invert, since we jump if not condition
      cond = cond == 'EQ' and 'NE' or 'EQ'
      local tb = type(b)
      if tb == 'nil' or tb == 'boolean' then
         cond = cond..'P'
         if tb == 'nil' then
            b = VKNIL
         else
            b = b == true and VKTRUE or VKFALSE
         end
      elseif tb == 'number' then
         cond = cond..'N'
      elseif tb == 'string' then
         cond = cond..'S'
      else
         cond = cond..'V'
      end
   end
   return self:emit(BC['IS'..cond], a, b), self:goto(here)
end
function Proto:op_eq(a, b, here)
   return self:op_comp('EQ', a, b, here)
end
function Proto:op_ne(a, b, here)
   return self:op_comp('NE', a, b, here)
end
function Proto:op_le(a, b, here)
   return self:op_comp('LE', a, b, here)
end
function Proto:op_ge(a, b, here)
   return self:op_comp('GE', a, b, here)
end
function Proto:op_lt(a, b, here)
   return self:op_comp('LT', a, b, here)
end
function Proto:op_gt(a, b, here)
   return self:op_comp('GT', a, b, here)
end
function Proto:op_arith(op, dest, var1, var2)
   local t1, t2 = type(var1), type(var2)
   if t1 == 'number' and t2 == 'number' then
      return self:op_loadk(dest, OP_FOLD[op](var1, var2))
   elseif t1 == 'number' and t2 == 'table' then
      return self:emit(BC[op..'VN'], dest, var2, self:const(var1))
   elseif t1 == 'table' and t2 == 'number' then
      return self:emit(BC[op..'VN'], dest, var1, self:const(var2))
   else
      return self:emit(BC[op..'VV'], dest, var1, var2)
   end
end
function Proto:op_add(dest, var1, var2)
   return self:op_arith('ADD', dest, var1, var2)
end
function Proto:op_sub(dest, var1, var2)
   return self:op_arith('SUB', dest, var1, var2)
end
function Proto:op_mul(dest, var1, var2)
   return self:op_arith('MUL', dest, var1, var2)
end
function Proto:op_div(dest, var1, var2)
   return self:op_arith('DIV', dest, var1, var2)
end
function Proto:op_mod(dest, var1, var2)
   return self:op_arith('MOD', dest, var1, var2)
end
function Proto:op_pow(dest, var1, var2)
   return self:op_arith('POW', dest, var1, var2)
end
function Proto:op_getglobal(dest, name)
   return self:emit(BC.GGET, dest, self:const(name))
end
function Proto:op_setglobal(from, name)
   return self:emit(BC.GSET, from, self:const(name))
end
function Proto:op_move(dest, src)
   return self:emit(BC.MOV, dest, src)
end
function Proto:op_loadk(dest, val)
   local tv = type(val)
   if tv == 'nil' then
      return self:emit(BC.KPRI, dest, VKNIL)
   elseif tv == 'boolean' then
      return self:emit(BC.KPRI, dest, val and VKTRUE or VKFALSE)
   elseif tv == 'string' then
      return self:emit(BC.KSTR, dest, self:const(val))
   elseif tv == 'number' then
      if val < 0xff then
         return self:emit(BC.KSHORT, dest, val)
      else
         return self:emit(BC.KNUM, dest, self:const(val))
      end
   else
      error("cannot LOADK: "..tostring(val))
   end
end
function Proto:op_newtable(dest, narry, nhash)
   if narry then
      if narry < 3 then
         narry = 3
      elseif narry > 0x7ff then
         narry = 0x7ff
      end
   else
      narry = 0
   end
   if nhash then
      nhash = math.ceil(nhash / 2)
   else
      nhash = 0
   end
   return self:emit(BC.TNEW, dest, bit.bor(narry, bit.lshift(nhash, 11)))
end
function Proto:op_settable(dest, key, val)
   if type(key) == 'string' then
      return self:emit(BC.TSETS, dest, val, self:const(key))
   elseif type(key) == 'number' and key < 0xff then
      return self:emit(BC.TSETB, dest, val, key)
   else
      return self:emit(BC.TSETV, dest, val, key)
   end
end
function Proto:op_gettable(dest, tab, key)
   if type(key) == 'string' then
      return self:emit(BC.TGETS, dest, tab, self:const(key))
   elseif type(key) == 'number' and key < 0xff then
      return self:emit(BC.TGETB, dest, tab, key)
   else
      return self:emit(BC.TGETV, dest, tab, key)
   end
end
function Proto:op_closure(dest, proto)
   if not proto.idx then
      local idx
      for i=1, #self.kobj do
         if self.kobj[i] == proto then
            idx = i - 1
         end
      end
      if not idx then
         error("proto not a child of self")
      end
      proto.idx = idx
   end
   return self:emit(BC.FNEW, dest, proto.idx)
end
function Proto:op_close(jump)
   return self:emit(BC.UCLO, 0, jump or 0)
end
function Proto:op_setupval(name, val)
   local tv = type(val)
   local slot = self:upval(name)
   if tv == 'string' then
      return self:emit(BC.USETS, slot, self:const(val))
   elseif tv == 'number' then
      return self:emit(BC.USETN, slot, self:const(val))
   elseif tv == 'nil' or tv == 'boolean' then
      local pri
      if tv == 'nil' then
         pri = VKNIL
      else
         pri = val and VKTRUE or VKFALSE
      end
      return self:emit(BC.USETP, slot, pri)
   else
      assert(typeof(val) == Reg, 'virtual register expected')
      return self:emit(BC.USETV, slot, val)
   end
end
function Proto:op_getupval(dest, name)
   local slot = self:upval(name)
   return self:emit(BC.UGET, dest, slot)
end
function Proto:op_return(what)
   if what == nil then
      return self:emit(BC.RET0, 0, 1)
   elseif typeof(what) == Reg then
      return self:emit(BC.RET1, what, 2)
   else
      assert(typeof(what) == List, "List expected")
      local prev = self.code[#self.code]
      if prev[1] == BC.CALL or prev[1] == BC.CALLM then
         return self:emit(BC.RETM, what[1], what)
      else
         return self:emit(BC.RET, what[1], what)
      end
   end
end
function Proto:op_varg(list, want)
   local base = self:reg()
   list[#list + 1] = base
   list.varg = true
   return self:emit(BC.VARG, base, want or 1, list)
end
function Proto:op_call(base, want, args)
   if args == 0 then
      return self:emit(BC.CALLM, base, want or 1, #self.params)
   elseif type(args) == 'table' and args.varg then
      return self:emit(BC.CALLM, base, want or 1, args)
   else
      return self:emit(BC.CALL, base, want or 1, args or 1)
   end
end
function Proto:op_tailcall(base, args)

end

Dump = class {
   __name = 'Dump';
   HEAD_1 = 0x1b;
   HEAD_2 = 0x4c;
   HEAD_3 = 0x4a;
   VERS   = 0x01;
   BE     = 0x01;
   STRIP  = 0x02;
   FFI    = 0x04;
}
function Dump:__init(main, name, flags)
   self.main  = main or Proto()
   self.name  = name
   self.flags = flags or 0
end
function Dump:write_header(buf)
   buf:put(Dump.HEAD_1)
   buf:put(Dump.HEAD_2)
   buf:put(Dump.HEAD_3)
   buf:put(Dump.VERS)
   buf:put(self.flags)
   local name = self.name
   if bit.band(self.flags, Dump.STRIP) == 0 then
      if not name then
         name = '(binary)'
      end
      buf:put_uleb128(#name)
      buf:put_literal(name)
   end
end
function Dump:write_footer(buf)
   buf:put(0x00)
end
function Dump:pack()
   local buf = Buf()
   self:write_header(buf)
   self.main:write(buf)
   self:write_footer(buf)
   return buf:pack()
end

module(...)

_M.Buf   = Buf;
_M.Ins   = Ins;
_M.Reg   = Reg;
_M.List  = List;
_M.Rest  = Rest;
_M.KNum  = KNum;
_M.KObj  = KObj;
_M.Proto = Proto;
_M.Dump  = Dump;

