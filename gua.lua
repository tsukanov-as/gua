-- MIT License

-- Copyright (c) 2021 tsukanov-as

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

setfenv(1, {_G = _G})

local string_byte = _G.string.byte
local string_sub = _G.string.sub
local string_rep = _G.string.rep
local string_format = _G.string.format
local table_concat = _G.table.concat
local table_sort = _G.table.sort
local tonumber = _G.tonumber
local assert = _G.assert
local setmetatable = _G.setmetatable
local pairs = _G.pairs
local ipairs = _G.ipairs
local tostring = _G.tostring
local error = _G.error
local type = _G.type
local print = _G.print

local Type = {
    __call = function(self, t)
        if self.init then
            t = self.init(self, t)
        end
        return setmetatable(t, self)
    end;
}
setmetatable(Type, Type)

local List = Type{
    __tostring = function(t)
        local res = {}
        for i, v in ipairs(t) do
            if type(v) == "string" then
                res[i] = string_format("%q", v)
            else
                res[i] = tostring(v)
            end
        end
        return "{" .. table_concat(res, ", ") .. "}"
    end;
}

local Set = Type{
    __tostring = function(t)
        local res = {}
        for k in pairs(t) do
            if type(k) == "string" then
                res[#res+1] = string_format("%q", k)
            else
                res[#res+1] = tostring(k)
            end
        end
        table_sort(res)
        return "{" .. table_concat(res, ", ") .. "}"
    end;
    init = function(self, t)
        local set = {}
        for _, v in ipairs(t) do
            set[v] = v
        end
        return set
    end;
}

local Fields = Type{
    __tostring = function(t)
        local res = {}
        for i, v in ipairs(t) do
            res[#res+1] = string_format("%d:%q", i, v)
        end
        table_sort(res)
        return "{" .. table_concat(res, ", ") .. "}"
    end;
    init = function(self, t)
        local fields = {}
        for i, v in ipairs(t) do
            fields[i] = v
            fields[v] = i
        end
        return fields
    end;
}

local nodes = {
    ["module"  ] = Fields{"Type", "Pos", "Len", "Body", "Comments"},
    ["value"   ] = Fields{"Type", "Pos", "Len", "Value"},
    ["field"   ] = Fields{"Type", "Pos", "Len", "Args", "Name"},
    ["index"   ] = Fields{"Type", "Pos", "Len", "Expr"},
    ["id"      ] = Fields{"Type", "Pos", "Len", "Name", "Tail", "Args", "Self"},
    ["table"   ] = Fields{"Type", "Pos", "Len", "List"},
    ["pair"    ] = Fields{"Type", "Pos", "Len", "Left", "Right"},
    ["list"    ] = Fields{"Type", "Pos", "Len", "List"},
    ["paren"   ] = Fields{"Type", "Pos", "Len", "Expr"},
    ["unop"    ] = Fields{"Type", "Pos", "Len", "Op", "Expr"},
    ["binop"   ] = Fields{"Type", "Pos", "Len", "Left", "Op", "Right"},
    ["call"    ] = Fields{"Type", "Pos", "Len", "ID"},
    ["set"     ] = Fields{"Type", "Pos", "Len", "Left", "Right"},
    ["let"     ] = Fields{"Type", "Pos", "Len", "Left", "Right"},
    ["inc"     ] = Fields{"Type", "Pos", "Len", "ID", "Expr"},
    ["dec"     ] = Fields{"Type", "Pos", "Len", "ID", "Expr"},
    ["if"      ] = Fields{"Type", "Pos", "Len", "Expr", "Then", "Else"},
    ["block"   ] = Fields{"Type", "Pos", "Len", "Body"},
    ["for"     ] = Fields{"Type", "Pos", "Len", "Expr", "Body"},
    ["for_to"  ] = Fields{"Type", "Pos", "Len", "ID", "From", "Limit", "Step", "Body"},
    ["for_in"  ] = Fields{"Type", "Pos", "Len", "IDs", "INs", "Body"},
    ["return"  ] = Fields{"Type", "Pos", "Len", "List"},
    ["break"   ] = Fields{"Type", "Pos", "Len"},
    ["continue"] = Fields{"Type", "Pos", "Len"},
    ["label"   ] = Fields{"Type", "Pos", "Len", "Name"},
    ["func"    ] = Fields{"Type", "Pos", "Len", "Name", "Params", "Body", "Receiver"},
    ["nop"     ] = Fields{"Type", "Pos", "Len"},
    ["vararg"  ] = Fields{"Type", "Pos", "Len"},
    ["param"   ] = Fields{"Type", "Pos", "Len", "Name"},
    ["params"  ] = Fields{"Type", "Pos", "Len", "List"},
    ["switch"  ] = Fields{"Type", "Pos", "Len", "Expr", "Cases", "Default"},
    ["case"    ] = Fields{"Type", "Pos", "Len", "List", "Expr", "Body"},
}

local Node = Type{
    __tostring = List.__tostring;
    __index = function(t, k)
        if type(k) == "string" then
            local _type = t[1]
            if k == "Type" then
                return _type
            end
            return t[nodes[_type][k]]
        end
        return nil
    end;
}

local KEYWORDS = Set{"break", "continue", "else", "false", "for", "func", "if", "in", "nil", "return", "true", "switch", "case", "default"}
local LITERALS = Set{"str", "chr", "num", "true", "false", "nil"}
local REL_OPS = Set{"==", "!=", "<", ">", "<=", ">="}
local MUL_OPS = Set{"*", "/", "%"}
local ADD_OPS = Set{"+", "-"}
local UNR_OPS = Set{"+", "-", "!", "#"}

local LF = 0x0A
local ALPHA = 1
local DIGIT = 2
local SPACE = 3

local MAP = {[string_byte'"'] = "str", [string_byte"'"] = "chr"}
local HEX = {}
do
    local sym = "()[]{}*:;.,/+-=<>#%|&!^"
    for i = 1, #sym do
        MAP[string_byte(sym, i)] = string_sub(sym, i, i)
    end
    for i = 0x01, 0x20 do
        MAP[i] = SPACE
    end
    for i = 0x30, 0x39 do
        MAP[i] = DIGIT
    end
    local abc = "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    for i = 1, #abc do
        MAP[string_byte(abc, i)] = ALPHA
    end
    local hex = "ABCDEFabcdef0123456789"
    for i = 1, #hex do
        HEX[string_byte(hex, i)] = true
    end
end

-------------------------------------------------------------------------------
-- parser state
local p_path = nil
local p_src = nil
local p_line = 1
local p_curpos = 0
local p_tokpos = 0 -- current token pos
local p_endpos = 0 -- previous token end pos
local p_chr = nil
local p_tok = nil
local p_lit = ''
local p_val = nil
local p_comments = List{}
local p_vars = {}
local p_scope = {}
local p_level = 0
local p_skip_id_check = false
local p_skipped_id_name
local p_continue = List{}
local p_looplevel = 0
-------------------------------------------------------------------------------

local function next()
    p_curpos = p_curpos + 1
    p_chr = string_byte(p_src, p_curpos)
    return p_chr
end

local function scan()
    repeat
        p_tok = MAP[p_chr]
        p_lit = ''
        p_val = nil
        p_endpos = p_curpos
        while p_tok == SPACE do
            if p_chr == LF then
                p_line = p_line + 1
            end
            p_tok = MAP[next()]
        end
        p_tokpos = p_curpos
        if p_tok == ALPHA then
            local beg = p_curpos
            repeat
                p_tok = MAP[next()]
            until p_tok ~= ALPHA and p_tok ~= DIGIT
            p_lit = string_sub(p_src, beg, p_curpos-1)
            p_tok = KEYWORDS[p_lit] or "id"
            if p_tok == "true" then
                p_val = true
            elseif p_tok == "false" then
                p_val = false
            end
        elseif p_tok == DIGIT then
            local beg = p_curpos
            local base = 10
            if p_chr == 0x30 then -- 0
                next()
                if p_chr == 0x58 or p_chr == 0x78 then -- X, x
                    next()
                    beg = p_curpos
                    assert(HEX[p_chr], 'expected hex digit at pos: ' .. p_curpos)
                    repeat
                        next()
                    until not HEX[p_chr]
                    base = 16
                    assert(p_curpos-beg <= 8)
                elseif p_chr == 0x42 or p_chr == 0x62 then -- B, b
                    next()
                    beg = p_curpos
                    assert(p_chr == 0x30 or p_chr == 0x31, 'expected bin digit at pos: ' .. p_curpos)
                    repeat
                        next()
                    until p_chr ~= 0x30 and p_chr ~= 0x31
                    base = 2
                    assert(p_curpos-beg <= 32)
                end
            end
            if base == 10 then
                p_tok = MAP[p_chr]
                while p_tok == DIGIT do
                    p_tok = MAP[next()]
                end
                if p_chr == 0x2E then -- .
                    repeat
                        p_tok = MAP[next()]
                    until p_tok ~= DIGIT
                end
                if p_chr == 0x45 or p_chr == 0x65 then -- E, e
                    next()
                    if p_chr == 0x2B or p_chr == 0x2D then -- +, -
                        next()
                    end
                    p_tok = MAP[p_chr]
                    assert(p_tok == DIGIT, 'expected digit at pos: ' .. p_curpos)
                    repeat
                        p_tok = MAP[next()]
                    until p_tok ~= DIGIT
                end
            end
            p_tok = "num"
            p_lit = string_sub(p_src, beg, p_curpos-1)
            p_val = assert(tonumber(p_lit, base))
        elseif p_tok == "str" then
            local beg = p_curpos
            repeat
                if next() == 0x5C then -- \
                    next()
                end
            until p_chr == 0x22 or p_chr == LF or p_chr == nil
            assert(p_chr == 0x22, 'expected " at pos: ' .. p_curpos)
            p_lit = string_sub(p_src, beg+1, p_curpos-1)
            p_val = p_lit
            next()
        elseif p_tok == "chr" then
            local beg = p_curpos
            next()
            next()
            assert(p_chr == 0x27, "expected ' at pos: " .. p_curpos)
            p_lit = string_sub(p_src, beg+1, p_curpos-1)
            p_val = string_byte(p_lit)
            next()
        elseif p_tok == ":" then
            next()
            if p_chr == 0x3D then
                p_tok = ":="
                next()
            elseif p_chr == 0x3A then
                p_tok = "::"
                next()
            end
        elseif p_tok == "=" then
            next()
            if p_chr == 0x3D then
                p_tok = "=="
                next()
            end
        elseif p_tok == "+" then
            next()
            if p_chr == 0x3D then
                p_tok = "+="
                next()
            end
        elseif p_tok == "-" then
            next()
            if p_chr == 0x2D then
                local beg = p_curpos
                repeat
                    next()
                until p_chr == LF or p_chr == nil
                p_lit = string_sub(p_src, beg+1, p_curpos-1)
                p_comments[p_line] = p_lit
                p_tok = "--"
            elseif p_chr == 0x3D then
                p_tok = "-="
                next()
            end
        elseif p_tok == "<" then
            next()
            if p_chr == 0x3D then
                p_tok = "<="
                next()
            end
        elseif p_tok == ">" then
            next()
            if p_chr == 0x3D then
                p_tok = ">="
                next()
            end
        elseif p_tok == "&" then
            next()
            if p_chr == 0x26 then
                p_tok = "&&"
                next()
            end
        elseif p_tok == "|" then
            next()
            if p_chr == 0x7C then
                p_tok = "||"
                next()
            end
        elseif p_tok == "!" then
            next()
            if p_chr == 0x3D then
                p_tok = "!="
                next()
            end
        elseif p_tok == "." then
            next()
            if p_chr == 0x2E then
                p_tok = ".."
                next()
                if p_chr == 0x2E then
                    p_tok = "..."
                    next()
                end
            elseif p_chr == 0x5B then
                p_tok = ".["
                next()
            elseif p_chr == 0x7B then
                p_tok = ".{"
                next()
            end
        else
            next()
        end
    until p_tok ~= "--"
    return p_tok
end

local function expect(t, l)
    if p_tok ~= t then
        local str
        if p_tok == "num" or p_tok == "str" or p_tok == "chr" or p_tok == "id" then
            str = p_lit
        else
            str = tostring(p_tok)
        end
        error(string_format("expected '%s', found '%s' in line %d", tostring(t), str, p_line), l or 2)
    end
end

local function skip(t)
    expect(t, 3)
    return scan()
end

local function open_scope(vars)
    p_vars = vars or {}
    p_level = p_level + 1
    p_scope[p_level] = p_vars
end

local function close_scope()
    p_level = p_level - 1
    p_vars = p_scope[p_level]
end

local function find_var(name)
    local var = p_vars[name]
    local i = p_level
    while var == nil and i > 1 do
        i = i - 1
        var = p_scope[i][name]
    end
    return var, i
end

local function errorf(notef, ...)
    error(string_format(notef, ...) .. " in line " .. p_line, 2)
end

local function assertf(expr, notef, ...)
    if not expr then
        error(string_format(notef, ...) .. " in line " .. p_line, 2)
    end
    return expr
end

local parse_expr
local parse_table, parse_list

local function parse_tail(call)
    local tail, i = List{}, 0
    while true do
        local pos = p_tokpos
        local dot = (p_tok == ".")
        if dot or p_tok == "::" then
            scan()
            if KEYWORDS[p_lit] == nil then
                expect("id")
            end
            local name = p_lit
            local args
            local last = p_tokpos
            scan()
            if p_tok == "(" then
                scan()
                args = List{}
                while p_tok ~= ")" do
                    args[#args+1] = parse_expr()
                    if p_tok ~= "," then
                        break
                    end
                    scan()
                end
                last = p_tokpos
                skip(")")
                call = true
            elseif p_tok == "str" then
                args = List{Node{"value", p_tokpos, p_endpos-pos, p_val}}
                last = p_tokpos
                scan()
                call = true
            elseif p_tok == ".{" then
                args = List{parse_table()}
                last = p_tokpos
                scan()
                call = true
            elseif p_tok == ".[" then
                args = List{parse_list()}
                last = p_tokpos
                scan()
                call = true
            else
                call = false
            end
            local item = Node{"field", pos, last - pos, args or false, name, dot}
            i = i + 1
            tail[i] = item
        elseif p_tok == "[" then
            scan()
            assertf(p_tok ~= "]", "expected expression, found ']'")
            local expr = parse_expr()
            skip("]")
            i = i + 1
            tail[i] = Node{"index", pos, p_endpos-pos, expr}
        else
            break
        end
    end
    return i > 0 and tail, call
end

local function parse_id()
    local pos = p_tokpos
    local name = p_lit
    if p_skip_id_check then
        p_skipped_id_name = name
        p_skip_id_check = false
    else
        assertf(find_var(name), "undeclared variable '%s'", name)
    end
    local call = false
    local args, tail = false, false
    scan()
    if p_tok == "(" then
        scan()
        args = List{}
        while p_tok ~= ")" do
            args[#args+1] = parse_expr()
            if p_tok ~= "," then
                break
            end
            scan()
        end
        skip(")")
        call = true
    elseif p_tok == "str" then
        args = List{Node{"value", p_tokpos, p_endpos-pos, p_val}}
        scan()
        call = true
    elseif p_tok == ".{" then
        args = List{parse_table()}
        scan()
        call = true
    elseif p_tok == ".[" then
        args = List{parse_list()}
        scan()
        call = true
    end
    tail, call = parse_tail(call)
    local node = Node{"id", pos, p_endpos-pos, name, tail, args}
    return node, call
end

local function parse_paren()
    local pos = p_tokpos
    skip("(")
    local expr = parse_expr()
    skip(")")
    return Node{"paren", pos, p_endpos-pos, expr}
end

parse_table = function()
    local pos = p_tokpos
    local list = List{}
    scan() -- skip "{" or ".{"
    while p_tok ~= "}" do
        local key_pos = p_tokpos
        local left
        if p_tok == "id" then
            left = Node{"id", key_pos, #p_lit, p_lit, false, false}
            scan()
        elseif p_tok == "[" then
            scan()
            local expr = parse_expr()
            skip("]")
            left = Node{"index", key_pos, p_endpos-key_pos, expr}
        else
            left = parse_expr()
        end
        expect(":")
        scan()
        local right = parse_expr()
        list[#list+1] = Node{"pair", pos, p_endpos-pos, left, right}
        if p_tok ~= "," then
            break
        end
        scan()
    end
    skip("}")
    return Node{"table", pos, p_endpos-pos, list}
end

parse_list = function()
    local pos = p_tokpos
    local list = List{}
    scan() -- skip "[" or ".["
    while p_tok ~= "]" do
        list[#list+1] = parse_expr()
        if p_tok ~= "," then
            break
        end
        scan()
    end
    skip("]")
    return Node{"list", pos, p_endpos-pos, list}
end

local parse_func

local function parse_operand()
    local node
    if LITERALS[p_tok] then
        node = Node{"value", p_tokpos, #p_lit, p_val}
        scan()
    elseif p_tok == "id" then
        node = parse_id()
    elseif p_tok == "(" then
        node = parse_paren()
    elseif p_tok == "{" then
        node = parse_table()
    elseif p_tok == "[" then
        node = parse_list()
    elseif p_tok == "func" then
        node = parse_func(true)
    elseif p_tok == "..." then
        node = Node{"vararg", p_tokpos, 3}
        scan()
    else
        errorf("expected operand, found '%s'", p_tok)
    end
    return node
end

local function parse_pow()
    local pos = p_tokpos
    local left = parse_operand()
    while p_tok == "^" do
        scan()
        local right = parse_operand()
        left = Node{"binop", pos, p_endpos-pos, left, "^", right}
    end
    return left
end

local function parse_unary()
    local pos = p_tokpos
    local expr
    if UNR_OPS[p_tok] then
        local op = p_tok
        scan()
        local right = parse_pow()
        expr = Node{"unop", pos, p_endpos-pos, op, right}
    else
        expr = parse_pow()
    end
    return expr
end

local function parse_mul()
    local pos = p_tokpos
    local left = parse_unary()
    while MUL_OPS[p_tok] do
        local op = p_tok
        scan()
        local right = parse_unary()
        left = Node{"binop", pos, p_endpos-pos, left, op, right}
    end
    return left
end

local function parse_add()
    local pos = p_tokpos
    local left = parse_mul()
    while ADD_OPS[p_tok] do
        local op = p_tok
        scan()
        local right = parse_mul()
        left = Node{"binop", pos, p_endpos-pos, left, op, right}
    end
    return left
end

local function parse_cat()
    local pos = p_tokpos
    local left = parse_add()
    while p_tok == ".." do
        scan()
        local right = parse_add()
        left = Node{"binop", pos, p_endpos-pos, left, "..", right}
    end
    return left
end

local function parse_rel()
    local pos = p_tokpos
    local left = parse_cat()
    while REL_OPS[p_tok] do
        local op = p_tok
        scan()
        local right = parse_cat()
        left = Node{"binop", pos, p_endpos-pos, left, op, right}
    end
    return left
end

local function parse_and()
    local pos = p_tokpos
    local left = parse_rel()
    while p_tok == "&&" do
        local op = p_tok
        scan()
        local right = parse_rel()
        left = Node{"binop", pos, p_endpos-pos, left, op, right}
    end
    return left
end

parse_expr = function()
    local pos = p_tokpos
    local left = parse_and()
    while p_tok == "||" do
        local op = p_tok
        scan()
        local right = parse_and()
        left = Node{"binop", pos, p_endpos-pos, left, op, right}
    end
    return left
end

local parse_block, parse_body

local function parse_set_or_call()
    local pos = p_tokpos
    local name = p_lit
    p_skip_id_check = true
    local id, call = parse_id()
    if call then
        assertf(find_var(name), "undeclared variable '%s'", name)
        return Node{"call", pos, p_endpos-pos, id}
    end
    if p_tok == "+=" then
        assertf(find_var(name), "undeclared variable '%s'", name)
        scan()
        local expr = parse_add()
        return Node{"inc", pos, p_endpos-pos, id, expr}
    end
    if p_tok == "-=" then
        assertf(find_var(name), "undeclared variable '%s'", name)
        scan()
        local expr = parse_add()
        return Node{"dec", pos, p_endpos-pos, id, expr}
    end
    local left = List{id}
    while p_tok == "," do
        scan()
        expect("id")
        p_skip_id_check = true
        id, call = parse_id()
        assertf(not call, "unexpected call")
        left[#left+1] = id
    end
    if p_tok == "=" then
        for _, id in ipairs(left) do
            name = id[4]
            assertf(find_var(name), "undeclared variable '%s'", name)
        end
        scan()
        local right = List{}
        while true do
            right[#right+1] = parse_expr()
            if p_tok ~= "," then
                break
            end
            scan()
        end
        return Node{"set", pos, p_endpos-pos, left, right}
    end
    if p_tok == ":=" then
        local allow = false
        for _, id in ipairs(left) do
            assertf(not id[5], "unexpected tail")
            assertf(not id[6], "unexpected call")
            assertf(not id[7], "unexpected self")
            local var, level = find_var(id[4])
            if not var then
                allow = true
            elseif level ~= p_level then
                errorf("variable shadowing is prohibited, you need to change the name '%s'", p_skipped_id_name)
            end
        end
        assertf(allow, "no new variables on left side of ':='")
        scan()
        local right = List{}
        while true do
            right[#right+1] = parse_expr()
            if p_tok ~= "," then
                break
            end
            scan()
        end
        for _, v in ipairs(left) do
            p_vars[v[4]] = v
        end
        return Node{"let", pos, p_endpos-pos, left, right}
    end
    expect("=")
end

local function parse_if()
    local pos = p_tokpos
    skip("if")
    local expr = parse_expr()
    local body = parse_block()
    local else_body = false
    if p_tok == "else" then
        scan()
        if p_tok == "if" then
            else_body = parse_if()
        else
            else_body = parse_block()
        end
    end
    return Node{"if", pos, p_endpos-pos, expr, body, else_body}
end

local function parse_switch()
    local pos = p_tokpos
    skip("switch")
    local expr = false
    local cases = List{}
    if p_tok == "{" then
        scan()
        while p_tok == "case" do
            scan()
            local case_expr = parse_expr()
            skip(":")
            local body = parse_body()
            cases[#cases+1] = Node{"case", pos, p_endpos-pos, false, case_expr, body}
        end
    else
        expr = parse_expr()
        skip("{")
        expect("case")
        while p_tok == "case" do
            scan()
            local list = List{}
            while true do
                list[#list+1] = parse_expr()
                if p_tok ~= "," then
                    break
                end
                scan()
            end
            local case_expr = false
            if p_tok == ";" then
                scan()
                case_expr = parse_expr()
            end
            skip(":")
            local body = parse_body()
            cases[#cases+1] = Node{"case", pos, p_endpos-pos, list, case_expr, body}
        end
    end
    local default = false
    if p_tok == "default" then
        scan()
        skip(":")
        default = parse_body()
    end
    skip("}")
    return Node{"switch", pos, p_endpos-pos, expr, cases, default}
end

local function parse_for()
    local pos = p_tokpos
    p_looplevel = p_looplevel + 1
    skip("for")
    if p_tok == "{" then
        local body = parse_block(nil, true)
        return Node{"for", pos, p_endpos-pos, false, body}
    end
    p_skip_id_check = true
    local expr = parse_expr()
    if expr[1] ~= "id" or expr[5] or expr[6] then
        assertf(find_var(p_skipped_id_name), "undeclared variable '%s'", p_skipped_id_name)
        local body = parse_block(nil, true)
        return Node{"for", pos, p_endpos-pos, expr, body}
    end
    local vars = {[expr[4]] = expr}
    if p_tok == ":=" then
        assertf(not find_var(p_skipped_id_name), "variable shadowing is prohibited, you need to change the name '%s'", p_skipped_id_name)
        scan()
        local from = parse_expr()
        skip(",")
        local to = parse_expr()
        local by
        if p_tok == "," then
            scan()
            by = parse_expr()
        end
        local body = parse_block(vars, true)
        return Node{"for_to", pos, p_endpos-pos, expr, from, to, by or false, body}
    end
    assertf(not find_var(expr[4]), "variable shadowing is prohibited, you need to change the name '%s'", expr[4])
    local ids = List{expr}
    while p_tok == "," do
        scan()
        expect("id")
        local name = p_lit
        assertf(not find_var(name), "variable shadowing is prohibited, you need to change the name '%s'", name)
        local id = Node{"id", p_tokpos, #name, name, false, false}
        ids[#ids+1] = id
        assertf(vars[name] == nil, "re-declaring variable '%s'", name)
        vars[name] = id
        scan()
    end
    expect("in")
    local ins = List{}
    scan()
    ins[#ins+1] = parse_expr()
    if p_tok == "," then
        scan()
        ins[#ins+1] = parse_expr()
    end
    if p_tok == "," then
        scan()
        ins[#ins+1] = parse_expr()
    end
    local body = parse_block(vars, true)
    return Node{"for_in", pos, p_endpos-pos, ids, ins, body}
end

local function parse_return()
    local pos = p_tokpos
    skip("return")
    local list = List{}
    if p_tok ~= "}" then
        while true do
            list[#list+1] = parse_expr()
            if p_tok ~= "," then
                break
            end
            scan()
        end
    end
    expect("}")
    return Node{"return", pos, p_endpos-pos, list}
end

local function parse_break()
    local pos = p_tokpos
    skip("break")
    expect("}")
    return Node{"break", pos, 5}
end

local function parse_continue()
    local pos = p_tokpos
    skip("continue")
    expect("}")
    p_continue[p_looplevel] = true
    return Node{"continue", pos, 8}
end

local function parse_params()
    local pos = p_tokpos
    skip("(")
    local list = List{}
    while true do
        if p_tok == "..." then
            local id = Node{"param", p_tokpos, 3, "..."}
            list[#list+1] = id
            p_vars["..."] = id
            scan()
            break
        end
        if p_tok ~= "id" then
            break
        end
        local id = Node{"param", p_tokpos, #p_lit, p_lit}
        list[#list+1] = id
        assertf(p_vars[p_lit] == nil, "parameter '%s' is already declared", p_lit)
        p_vars[p_lit] = id
        scan()
        if p_tok ~= "," then
            break
        end
        scan()
    end
    skip(")")
    return Node{"params", pos, p_endpos-pos, list}
end

parse_func = function(lambda)
    local pos = p_tokpos
    local name = false
    local receiver = false
    local vars
    skip("func")
    if not lambda then
        expect("id")
        name = p_lit
        scan()
        if p_tok == "." then
            scan()
            expect("id")
            assertf(find_var(name), "undeclared variable '%s'", name)
            receiver = name
            name = p_lit
            scan()
            vars = {["self"] = Node{"id", 0, 0, "self", false, false}}
        else
            assertf(not find_var(name), "re-declaring variable '%s'", name)
        end
    end
    open_scope(vars)
    local params = parse_params()
    local body = parse_block()
    close_scope()
    local node = Node{"func", pos, p_endpos-pos, name, params, body, receiver}
    if not lambda then
        p_vars[name] = node
    end
    return node
end

local function parse_statement()
    if p_tok == "id" then
        return parse_set_or_call()
    elseif p_tok == "func" then
        return parse_func()
    elseif p_tok == "if" then
        return parse_if()
    elseif p_tok == "{" then
        return parse_block()
    elseif p_tok == "for" then
        return parse_for()
    elseif p_tok == "return" then
        return parse_return()
    elseif p_tok == "break" then
        return parse_break()
    elseif p_tok == "continue" then
        return parse_continue()
    elseif p_tok == "switch" then
        return parse_switch()
    elseif p_tok == ";" then
        scan()
        return Node{"nop", p_curpos, 1}
    end
    return nil
end

parse_body = function(vars)
    open_scope(vars)
    local body = List{}
    while true do
        local stmt = parse_statement()
        if stmt == nil then
            break
        end
        body[#body+1] = stmt
    end
    close_scope()
    return body
end

parse_block = function(vars, loop)
    local pos = p_tokpos
    skip("{")
    local body = parse_body(vars)
    skip("}")
    if loop and p_continue[p_looplevel] then
        body[#body+1] = Node{"label", 0, 0, "continue"}
        p_continue[p_looplevel] = false
        p_looplevel = p_looplevel - 1
    end
    return Node{"block", pos, p_endpos-pos, body}
end

local function parse_module(src, vars)
    p_src = src
    p_line = 1
    p_curpos = 0
    p_tokpos = 0
    p_endpos = 0
    p_chr = nil
    p_tok = nil
    p_lit = ''
    p_val = nil
    p_comments = List{}
    p_vars = {}
    p_level = 0
    p_scope = List{}
    p_continue = List{}
    p_looplevel = 0
    next()
    scan()
    local pos = p_tokpos
    local body = parse_body(vars)
    local module = Node{"module", pos, p_endpos-pos, body, p_comments}
    expect(nil)
    return module
end

local v_res = {}
local v_level = 0
local LUA_OPS = setmetatable({
    ["!"] = "not ";
    ["&&"] = "and";
    ["||"] = "or";
    ["!="] = "~=";
}, {
    __index = function(t, k)
        return k
    end;
})

local function space()
    return string_rep("    ", v_level)
end

local visit_expr

local function visit_value(node)
    local v = node[4]
    if type(v) == "string" then
        v_res[#v_res+1] = '"' .. v .. '"'
    else
        v_res[#v_res+1] = tostring(v)
    end
end

local function visit_field(node)
    local args = node[4]
    if args then
        if node[6] then
            v_res[#v_res+1] = ":" .. node[5] .. "("
        else
            v_res[#v_res+1] = "." .. node[5] .. "("
        end
        if #args > 0 then
            for _, v in ipairs(args) do
                visit_expr(v)
                v_res[#v_res+1] = ", "
            end
            v_res[#v_res] = ""
        end
        v_res[#v_res+1] = ")"
    else
        v_res[#v_res+1] = "." .. node[5]
    end
end

local function visit_index(node)
    v_res[#v_res+1] = "["
    visit_expr(node[4])
    v_res[#v_res+1] = "]"
end

local function visit_id(node)
    v_res[#v_res+1] = node[4]
    local tail, args = node[5], node[6]
    if tail then
        for _, v in ipairs(tail) do
            if v[1] == "field" then
                visit_field(v)
            else
                visit_index(v)
            end
        end
    end
    if args then
        v_res[#v_res+1] = "("
        if #args > 0 then
            for _, v in ipairs(args) do
                visit_expr(v)
                v_res[#v_res+1] = ", "
            end
            v_res[#v_res] = ""
        end
        v_res[#v_res+1] = ")"
    end
end

local function visit_pair(node)
    local key = node[4]
    if key[1] == "id" and not key[5] and not key[6] then
        visit_expr(key)
    elseif key[1] == "index" then
        visit_index(key)
    else
        v_res[#v_res+1] = "["
        visit_expr(key)
        v_res[#v_res+1] = "]"
    end
    v_res[#v_res+1] = " = "
    visit_expr(node[5])
end

local function visit_table(node)
    v_res[#v_res+1] = "{\n"
    v_level = v_level + 1
    for _, v in ipairs(node[4]) do
        v_res[#v_res+1] = space()
        if v[1] == "pair" then
            visit_pair(v)
            v_res[#v_res+1] = ";\n"
        else
            visit_expr(v)
            v_res[#v_res+1] = ",\n"
        end
    end
    v_level = v_level - 1
    v_res[#v_res+1] = space() .. "}"
end

local function visit_list(node)
    v_res[#v_res+1] = "{"
    local list = node[4]
    if #list > 0 then
        for _, v in ipairs(list) do
            visit_expr(v)
            v_res[#v_res+1] = ", "
        end
        v_res[#v_res] = ""
    end
    v_res[#v_res+1] = "}"
end

local function visit_paren(node)
    v_res[#v_res+1] = "("
    visit_expr(node[4])
    v_res[#v_res+1] = ")"
end

local function visit_unop(node)
    local op = LUA_OPS[node[4]]
    if op ~= "+" then
        v_res[#v_res+1] = LUA_OPS[node[4]]
    end
    visit_expr(node[5])
end

local function visit_binop(node)
    visit_expr(node[4])
    v_res[#v_res+1] = " " .. LUA_OPS[node[5]] .. " "
    visit_expr(node[6])
end

local visit_func

visit_expr = function(node)
    local t = node[1]
    if t == "id" then
        visit_id(node)
    elseif t == "binop" then
        visit_binop(node)
    elseif t == "unop" then
        visit_unop(node)
    elseif t == "paren" then
        visit_paren(node)
    elseif t == "value" then
        visit_value(node)
    elseif t == "table" then
        visit_table(node)
    elseif t == "list" then
        visit_list(node)
    elseif t == "func" then
        visit_func(node, true)
    elseif t == "vararg" then
        v_res[#v_res+1] = "..."
    else
        errorf("unknown node type: '%s'", t)
    end
end

local visit_stmt

local function visit_body(node)
    v_level = v_level + 1
    for _, v in ipairs(node) do
        visit_stmt(v)
    end
    v_level = v_level - 1
end

local function visit_block(node)
    visit_body(node[4])
end

local function visit_call(node)
    v_res[#v_res+1] = space()
    visit_id(node[4])
    v_res[#v_res+1] = "\n"
end

local function visit_set(node)
    v_res[#v_res+1] = space()
    for _, v in ipairs(node[4]) do
        visit_id(v)
        v_res[#v_res+1] = ", "
    end
    v_res[#v_res] = " = "
    for _, v in ipairs(node[5]) do
        visit_expr(v)
        v_res[#v_res+1] = ", "
    end
    v_res[#v_res] = "\n"
end

local function visit_inc(node)
    v_res[#v_res+1] = space()
    visit_id(node[4])
    v_res[#v_res+1] = " = "
    visit_id(node[4])
    v_res[#v_res+1] = " + "
    visit_expr(node[5])
    v_res[#v_res+1] = "\n"
end

local function visit_dec(node)
    v_res[#v_res+1] = space()
    visit_id(node[4])
    v_res[#v_res+1] = " = "
    visit_id(node[4])
    v_res[#v_res+1] = " - "
    visit_expr(node[5])
    v_res[#v_res+1] = "\n"
end

local function visit_let(node)
    v_res[#v_res+1] = space() .. "local "
    for _, v in ipairs(node[4]) do
        visit_id(v)
        v_res[#v_res+1] = ", "
    end
    v_res[#v_res] = " = "
    for _, v in ipairs(node[5]) do
        visit_expr(v)
        v_res[#v_res+1] = ", "
    end
    v_res[#v_res] = "\n"
end

local function visit_if(node)
    v_res[#v_res+1] = space() .. "if "
    visit_expr(node[4])
    v_res[#v_res+1] = " then\n"
    visit_block(node[5])
    local _else = node[6]
    while _else do
        v_res[#v_res+1] = space() .. "else"
        if _else[1] == "if" then
            v_res[#v_res+1] = "if "
            visit_expr(_else[4])
            v_res[#v_res+1] = " then\n"
            visit_block(_else[5])
            _else = _else[6]
        else
            v_res[#v_res+1] = "\n"
            visit_block(_else)
            _else = nil
        end
    end
    v_res[#v_res+1] = space() .. "end\n"
end

local function visit_switch(node)
    local expr = node[4]
    if expr then
        v_res[#v_res+1] = space() .. "do\n"
        v_level = v_level + 1
        v_res[#v_res+1] = space() .. "local case = "
        visit_expr(expr)
        v_res[#v_res+1] = "\n" .. space()
        for _, case in ipairs(node[5]) do
            v_res[#v_res+1] = "if "
            local case_exp = case[5]
            if case_exp then
                v_res[#v_res+1] = "("
            end
            for _, item in ipairs(case[4]) do
                v_res[#v_res+1] = "case == "
                if item[1] == "binop" then
                    v_res[#v_res+1] = "("
                    visit_expr(item)
                    v_res[#v_res+1] = ")"
                else
                    visit_expr(item)
                end
                v_res[#v_res+1] = " or "
            end
            if case_exp then
                v_res[#v_res] = ") and "
                visit_expr(case_exp)
            else
                v_res[#v_res] = ""
            end
            v_res[#v_res+1] = " then\n"
            visit_body(case[6])
            v_res[#v_res+1] = space() .. "else"
        end
        local default = node[6]
        if default then
            v_res[#v_res+1] = "\n"
            visit_body(default)
        else
            v_res[#v_res] = ""
        end
        v_res[#v_res+1] = space() .. "end\n"
        v_level = v_level - 1
        v_res[#v_res+1] = space() .. "end\n"
    else
        v_res[#v_res+1] = space()
        for _, case in ipairs(node[5]) do
            v_res[#v_res+1] = "if "
            visit_expr(case[5])
            v_res[#v_res+1] = " then\n"
            visit_body(case[6])
            v_res[#v_res+1] = space() .. "else"
        end
        local default = node[6]
        if default then
            v_res[#v_res+1] = "\n"
            visit_body(default)
        else
            v_res[#v_res] = ""
        end
        v_res[#v_res+1] = space() .. "end\n"
    end
end

local function visit_for(node)
    v_res[#v_res+1] = space() .. "while "
    local expr = node[4]
    if expr then
        visit_expr(expr)
    else
        v_res[#v_res+1] = "true"
    end
    v_res[#v_res+1] = " do\n"
    visit_block(node[5])
    v_res[#v_res+1] = space() .. "end\n"
end

local function visit_for_to(node)
    v_res[#v_res+1] = space() .. "for "
    visit_id(node[4])
    v_res[#v_res+1] = " = "
    visit_expr(node[5])
    v_res[#v_res+1] = ", "
    visit_expr(node[6])
    local step = node[7]
    if step then
        v_res[#v_res+1] = ", "
        visit_expr(step)
    end
    v_res[#v_res+1] = " do\n"
    visit_block(node[8])
    v_res[#v_res+1] = space() .. "end\n"
end

local function visit_for_in(node)
    v_res[#v_res+1] = space() .. "for "
    for _, v in ipairs(node[4]) do
        visit_id(v)
        v_res[#v_res+1] = ", "
    end
    v_res[#v_res] = " in "
    for _, v in ipairs(node[5]) do
        visit_expr(v)
        v_res[#v_res+1] = ", "
    end
    v_res[#v_res] = " do\n"
    visit_block(node[6])
    v_res[#v_res+1] = space() .. "end\n"
end

local function visit_return(node)
    local list = node[4]
    v_res[#v_res+1] = space() .. "return"
    if list then
        v_res[#v_res+1] = " "
        for _, v in ipairs(list) do
            visit_expr(v)
            v_res[#v_res+1] = ", "
        end
        v_res[#v_res] = ""
    end
    v_res[#v_res+1] = "\n"
end

local function visit_break(node)
    v_res[#v_res+1] = space() .. "break\n"
end

local function visit_continue(node)
    v_res[#v_res+1] = space() .. "goto continue\n"
end

local function visit_label(node)
    v_res[#v_res+1] = space() .. "::" .. node[4] .. "::\n"
end

local function visit_params(node)
    local t = {}
    for _, v in ipairs(node[4]) do
        t[#t+1] = v[4]
    end
    v_res[#v_res+1] = "("..table_concat(t, ", ")..")\n"
end

visit_func = function(node, lambda)
    if lambda then
        v_res[#v_res+1] = "function"
    elseif node[7] then
        v_res[#v_res+1] = space() .. "function " .. node[7] .. ":" .. node[4]
    else
        v_res[#v_res+1] = space() .. "local function " .. node[4]
    end
    visit_params(node[5])
    visit_block(node[6])
    v_res[#v_res+1] = space() .. "end"
    if not lambda then
        v_res[#v_res+1] = "\n"
    end
end

local function visit_nop(node)
    v_res[#v_res] = ";\n"
end

visit_stmt = function(node)
    local t = node[1]
    if t == "call" then
        visit_call(node)
    elseif t == "set" then
        visit_set(node)
    elseif t == "inc" then
        visit_inc(node)
    elseif t == "dec" then
        visit_dec(node)
    elseif t == "let" then
        visit_let(node)
    elseif t == "if" then
        visit_if(node)
    elseif t == "switch" then
        visit_switch(node)
    elseif t == "block" then
        v_res[#v_res+1] = space() .. "do\n"
        visit_block(node)
        v_res[#v_res+1] = space() .. "end\n"
    elseif t == "for" then
        visit_for(node)
    elseif t == "for_to" then
        visit_for_to(node)
    elseif t == "for_in" then
        visit_for_in(node)
    elseif t == "return" then
        visit_return(node)
    elseif t == "break" then
        visit_break(node)
    elseif t == "continue" then
        visit_continue(node)
    elseif t == "label" then
        visit_label(node)
    elseif t == "func" then
        visit_func(node)
    elseif t == "nop" then
        visit_nop(node)
    else
        errorf("unknown node type: %s", t)
    end
end

local function visit_module(node, level)
    v_res = {}
    v_level = level or 0
    for _, v in ipairs(node[4]) do
        visit_stmt(v)
    end
    return table_concat(v_res)
end

return {
    Node = Node;
    nodes = nodes;
    parse_module = parse_module;
    visit_module = visit_module;
}
