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
    ["module"] =   Fields{"Type", "Body", "Comments"},
    ["value"] =    Fields{"Type", "Pos", "Len", "Value"},
    ["field"] =    Fields{"Type", "Pos", "Len", "Args", "Name"},
    ["index"] =    Fields{"Type", "Pos", "Len", "Expr"},
    ["id"] =       Fields{"Type", "Pos", "Len", "Name", "Tail", "Args", "Self"},
    ["table"] =    Fields{"Type", "Pos", "Len", "List"},
    ["pair"] =     Fields{"Type", "Pos", "Len", "Left", "Right"},
    ["list"] =     Fields{"Type", "Pos", "Len", "List"},
    ["paren"] =    Fields{"Type", "Pos", "Len", "Expr"},
    ["unop"] =     Fields{"Type", "Pos", "Len", "Op", "Expr"},
    ["binop"] =    Fields{"Type", "Pos", "Len", "Left", "Op", "Right"},
    ["call"] =     Fields{"Type", "Pos", "Len", "ID"},
    ["set"] =      Fields{"Type", "Pos", "Len", "Left", "Right"},
    ["let"] =      Fields{"Type", "Pos", "Len", "Left", "Right"},
    ["if"] =       Fields{"Type", "Pos", "Len", "Expr", "Then", "Else"},
    ["body"] =     Fields{"Type", "Pos", "Len", "Body"},
    ["for"] =      Fields{"Type", "Pos", "Len", "Expr", "Body"},
    ["for_to"] =   Fields{"Type", "Pos", "Len", "ID", "From", "Limit", "Step", "Body"},
    ["for_in"] =   Fields{"Type", "Pos", "Len", "IDs", "INs", "Body"},
    ["return"] =   Fields{"Type", "Pos", "Len", "List"},
    ["break"] =    Fields{"Type", "Pos", "Len"},
    ["continue"] = Fields{"Type", "Pos", "Len"},
    ["func"] =     Fields{"Type", "Pos", "Len", "Name", "Params", "Body"},
    ["nop"] =      Fields{"Type", "Pos", "Len"},
    ["param"] =    Fields{"Type", "Pos", "Len", "Name"},
    ["params"] =   Fields{"Type", "Pos", "Len", "List"}
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

local KEYWORDS = Set{"break", "continue", "else", "false", "for", "func", "if", "in", "nil", "return", "true"}
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
do
    local sym = "()[]{}*:;.,/+-=<>#%|&!"
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
local p_vars = List{}
local p_scope = {}
local p_level = 0
local p_left -- cheatcode
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
            repeat
                p_tok = MAP[next()]
            until p_tok ~= DIGIT
            if MAP[p_chr] == "." then
                repeat
                    p_tok = MAP[next()]
                until p_tok ~= DIGIT
            end
            p_tok = "num"
            p_lit = string_sub(p_src, beg, p_curpos-1)
            p_val = tonumber(p_lit)
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
            repeat
                if next() == 0x5C then -- \
                    next()
                end
            until p_chr == 0x27 or p_chr == LF or p_chr == nil
            assert(p_chr == 0x27, "expected ' at pos: " .. p_curpos)
            p_lit = string_sub(p_src, beg+1, p_curpos-1)
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
        else
            next()
        end
    until p_tok ~= "--"
    return p_tok
end

local function expect(t, l)
    if p_tok ~= t then
        error("expected " .. tostring(t) .. ", found " .. tostring(p_tok), l or 2)
    end
end

local function skip(t)
    expect(t, 3)
    return scan()
end

local function open_scope(vars)
    p_vars = vars or List{}
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
    error(string_format(notef, ...) .. " at line: " .. p_line, 2)
end

local parse_expr

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
                    args[#args+1] = assert(parse_expr())
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
                call = true
            else
                call = false
            end
            local item = Node{"field", pos, last - pos, args or false, name, dot}
            i = i + 1
            tail[i] = item
        elseif p_tok == "[" then
            if scan() == "]" then
                errorf("expected expression, found '%s'", p_tok)
            end
            local expr = assert(parse_expr())
            skip("]")
            i = i + 1
            tail[i] = Node{"index", pos, p_endpos-pos, expr}
        else
            break
        end
    end
    return i > 0 and tail, call
end

local function parse_id(check)
    local pos = p_tokpos
    local name = p_lit
    if check then
        assert(find_var(name))
    end
    local call = false
    local args, tail = false, false
    scan()
    if p_tok == "(" then
        scan()
        args = List{}
        while p_tok ~= ")" do
            args[#args+1] = assert(parse_expr())
            if p_tok ~= "," then
                break
            end
            scan()
        end
        skip(")")
        call = true
    elseif p_tok == "str" then
        args = List{Node{"value", p_tokpos, p_endpos-pos, p_val}}
        call = true
    end
    tail, call = parse_tail(call)
    local node = Node{"id", pos, p_endpos-pos, name, tail, args}
    return node, call
end

local function parse_paren()
    local pos = p_tokpos
    skip("(")
    local expr = assert(parse_expr())
    skip(")")
    return Node{"paren", pos, p_endpos-pos, expr}
end

local function parse_table()
    local pos = p_tokpos
    local list = List{}
    skip("{")
    while p_tok ~= "}" do
        if p_tok == "id" then
            local id = parse_id(false)
            -- cheatcode
            p_left = id
            p_tokpos = id[2]
        end
        local left = assert(parse_expr())
        if p_tok == ":" then
            local line = p_line
            scan()
            if left[1] == "id" and (left[5] or left[6]) then
                errorf("unexpected ':' at pos: ", line)
            end
            local right = assert(parse_expr())
            list[#list+1] = Node{"pair", pos, p_endpos-pos, left, right}
        else
            list[#list+1] = left
        end
        if p_tok ~= "," and p_tok ~= ";" then
            break
        end
        scan()
    end
    skip("}")
    return Node{"table", pos, p_endpos-pos, list}
end

local function parse_list()
    local pos = p_tokpos
    local list = List{}
    skip("[")
    while p_tok ~= "]" do
        list[#list+1] = assert(parse_expr())
        if p_tok ~= "," then
            break
        end
        scan()
    end
    skip("]")
    return Node{"list", pos, p_endpos-pos, list}
end

local function parse_operand()
    local node
    if LITERALS[p_tok] then
        node = Node{"value", p_tokpos, #p_lit, p_val}
        scan()
    elseif p_tok == "id" then
        node = parse_id(true)
    elseif p_tok == "(" then
        node = parse_paren()
    elseif p_tok == "{" then
        node = parse_table()
    elseif p_tok == "[" then
        node = parse_list()
    else
        errorf("expected operand, found '%s'", p_tok)
    end
    return node
end

local function parse_unary()
    local pos = p_tokpos
    local expr
    if UNR_OPS[p_tok] then
        local op = p_tok
        scan()
        local right = parse_operand()
        expr = Node{"unop", pos, p_endpos-pos, op, right}
    else
        expr = parse_operand()
    end
    return expr
end

local function parse_mul()
    local pos = p_tokpos
    local left = p_left
    if left then
        -- cheatcode
        p_left = nil
        pos = left[2]
    else
        left = parse_unary()
    end
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

local function parse_rel()
    local pos = p_tokpos
    local left = parse_add()
    while REL_OPS[p_tok] do
        local op = p_tok
        scan()
        local right = parse_add()
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

local parse_body

local function parse_set_or_call()
    local pos = p_tokpos
    local name = p_lit
    local id, call = parse_id(false)
    if call then
        assert(find_var(name))
        return Node{"call", pos, p_endpos-pos, id}
    end
    local left = List{id}
    while p_tok == "," do
        scan()
        expect("id")
        id = Node{"id", p_tokpos, #p_lit, p_lit, false, false}
        left[#left+1] = id
        scan()
    end
    if p_tok == "=" then
        assert(find_var(name))
        scan()
        local right = List{}
        while true do
            right[#right+1] = assert(parse_expr())
            if p_tok ~= "," then
                break
            end
            scan()
        end
        return Node{"set", pos, p_endpos-pos, left, right}
    elseif p_tok == ":=" then
        local var, level = find_var(name)
        assert(not var or level == p_level)
        scan()
        local right = List{}
        while true do
            right[#right+1] = assert(parse_expr())
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
    local body = parse_body()
    local else_body = false
    if p_tok == "else" then
        scan()
        if p_tok == "if" then
            else_body = parse_if()
        else
            else_body = parse_body()
        end
    end
    return Node{"if", pos, p_endpos-pos, expr, body, else_body}
end

local function parse_for()
    local pos = p_tokpos
    skip("for")
    local id, call
    if p_tok == "{" then
        local body = parse_body()
        return Node{"for", pos, p_endpos-pos, false, body}
    end
    if p_tok == "id" then
        id, call = parse_id(false)
        -- cheatcode
        p_left = id
        p_tokpos = id[2]
    end
    local expr = assert(parse_expr())
    if expr ~= id or call or id[5] then
        assert(find_var(id[4]))
        local body = parse_body()
        return Node{"for", pos, p_endpos-pos, expr, body}
    end
    assert(id)
    local vars = List{[id[4]] = id}
    if p_tok == ":=" then
        scan()
        local from = parse_expr()
        skip(",")
        local to = parse_expr()
        local by
        if p_tok == "," then
            scan()
            by = parse_expr()
        end
        local body = parse_body(vars)
        return Node{"for_to", pos, p_endpos-pos, id, from, to, by or false, body}
    end
    local ids = List{id}
    while p_tok == "," do
        scan()
        expect("id")
        id = Node{"id", p_tokpos, #p_lit, p_lit, false, false}
        ids[#ids+1] = id
        vars[p_lit] = id
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
    local body = parse_body(vars)
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
    return Node{"continue", pos, 8}
end

local function parse_params()
    local pos = p_tokpos
    skip("(")
    local list = List{}
    while p_tok == "id" do
        local id = Node{"param", p_tokpos, #p_lit, p_lit}
        list[#list+1] = id
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

local function parse_function()
    local pos = p_tokpos
    skip("func")
    expect("id")
    local name = p_lit
    assert(not find_var(name))
    scan()
    open_scope()
    local params = parse_params()
    local body = parse_body()
    close_scope()
    local node = Node{"func", pos, p_endpos-pos, name, params, body}
    p_vars[name] = node
    return node
end

local function parse_statement()
    if p_tok == "id" then
        return parse_set_or_call()
    elseif p_tok == "func" then
        return parse_function()
    elseif p_tok == "if" then
        return parse_if()
    elseif p_tok == "{" then
        return parse_body()
    elseif p_tok == "for" then
        return parse_for()
    elseif p_tok == "return" then
        return parse_return()
    elseif p_tok == "break" then
        return parse_break()
    elseif p_tok == "continue" then
        return parse_continue()
    elseif p_tok == ";" then
        scan()
        return Node{"nop", p_curpos, 1}
    end
    return nil
end

parse_body = function(vars)
    local pos = p_tokpos
    skip("{")
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
    skip("}")
    return Node{"body", pos, p_endpos-pos, body}
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
    p_vars = List{}
    p_level = 0
    p_scope = List{}
    next()
    scan()
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
    local module = Node{"module", body, p_comments}
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
        for _, v in ipairs(args) do
            visit_expr(v)
            v_res[#v_res+1] = ","
        end
        v_res[#v_res] = ")"
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
    v_res[#v_res + 1] = node[4]
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
        for _, v in ipairs(args) do
            visit_expr(v)
            v_res[#v_res+1] = ", "
        end
        v_res[#v_res] = ")"
    end
end

local function visit_pair(node)
    visit_expr(node[4])
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
    v_res[#v_res+1] = LUA_OPS[node[4]]
    visit_expr(node[5])
end

local function visit_binop(node)
    visit_expr(node[4])
    v_res[#v_res+1] = " " .. LUA_OPS[node[5]] .. " "
    visit_expr(node[6])
end

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
    else
        errorf("unknown node type: '%s'", t)
    end
end

local visit_stmt

local function visit_body(node)
    v_level = v_level + 1
    for _, v in ipairs(node[4]) do
        visit_stmt(v)
    end
    v_level = v_level - 1
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
    visit_body(node[5])
    local _else = node[6]
    while _else do
        v_res[#v_res+1] = space() .. "else"
        if _else[1] == "if" then
            v_res[#v_res+1] = "if "
            visit_expr(_else[4])
            v_res[#v_res+1] = " then\n"
            visit_body(_else[5])
            _else = _else[6]
        else
            v_res[#v_res+1] = "\n"
            visit_body(_else)
            _else = nil
        end
    end
    v_res[#v_res+1] = space() .. "end\n"
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
    visit_body(node[5])
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
    visit_body(node[8])
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
    visit_body(node[6])
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
    v_res[#v_res+1] = space() .. "continue\n"
end

local function visit_params(node)
    local t = {}
    for _, v in ipairs(node[4]) do
        t[#t+1] = v[4]
    end
    v_res[#v_res+1] = "("..table_concat(t, ", ")..")\n"
end

local function visit_func(node)
    v_res[#v_res+1] = space() .. "local function " .. node[4]
    visit_params(node[5])
    visit_body(node[6])
    v_res[#v_res+1] = space() .. "end\n"
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
    elseif t == "let" then
        visit_let(node)
    elseif t == "if" then
        visit_if(node)
    elseif t == "body" then
        visit_body(node)
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
    for _, v in ipairs(node[2]) do
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
