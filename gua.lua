_G.setfenv(1, {
    _G = _G;
})
local string_byte = _G.string.byte
local string_sub = _G.string.sub
local string_rep = _G.string.rep
local string_format = _G.string.format
local string_find = _G.string.find
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
            t = self:init(t)
        end
        return setmetatable(t, self)
    end;
}
setmetatable(Type, Type)
local Hex = Type{
    __tostring = function(t)
        return string_format("0x%02X", t[1])
    end;
}
local Raw = Type{
    __tostring = function(t)
        local s = t[1]
        local i = 0
        local x = nil
        while true do
            x = string_rep("=", i)
            if not string_find(s, "[" .. x .. "[", 1, true) and not string_find(s, "]" .. x .. "]", 1, true) then
                break
            end
            i = i + 1
        end
        return string_format(" [%s[%s]%s]", x, s, x)
    end;
}
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
                res[#res + 1] = string_format("%q", k)
            else
                res[#res + 1] = tostring(k)
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
            res[#res + 1] = string_format("%d:%q", i, v)
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
    ["module"] = Fields{"Type", "Pos", "Len", "Body", "Comments"};
    ["value"] = Fields{"Type", "Pos", "Len", "Value"};
    ["field"] = Fields{"Type", "Pos", "Len", "Args", "Name", "Dot", "Paren"};
    ["index"] = Fields{"Type", "Pos", "Len", "Expr"};
    ["id"] = Fields{"Type", "Pos", "Len", "Name", "Tail", "Args", "Const", "Paren"};
    ["table"] = Fields{"Type", "Pos", "Len", "List"};
    ["pair"] = Fields{"Type", "Pos", "Len", "Left", "Right"};
    ["list"] = Fields{"Type", "Pos", "Len", "List"};
    ["paren"] = Fields{"Type", "Pos", "Len", "Expr"};
    ["unop"] = Fields{"Type", "Pos", "Len", "Op", "Expr"};
    ["binop"] = Fields{"Type", "Pos", "Len", "Left", "Op", "Right"};
    ["call"] = Fields{"Type", "Pos", "Len", "ID"};
    ["set"] = Fields{"Type", "Pos", "Len", "Left", "Right"};
    ["let"] = Fields{"Type", "Pos", "Len", "Left", "Right"};
    ["inc"] = Fields{"Type", "Pos", "Len", "ID", "Expr"};
    ["dec"] = Fields{"Type", "Pos", "Len", "ID", "Expr"};
    ["if"] = Fields{"Type", "Pos", "Len", "Expr", "Then", "Else"};
    ["block"] = Fields{"Type", "Pos", "Len", "Body"};
    ["for"] = Fields{"Type", "Pos", "Len", "Expr", "Body"};
    ["for_to"] = Fields{"Type", "Pos", "Len", "ID", "From", "Limit", "Step", "Body"};
    ["for_in"] = Fields{"Type", "Pos", "Len", "IDs", "INs", "Body"};
    ["return"] = Fields{"Type", "Pos", "Len", "List"};
    ["break"] = Fields{"Type", "Pos", "Len", "Expr"};
    ["continue"] = Fields{"Type", "Pos", "Len"};
    ["label"] = Fields{"Type", "Pos", "Len", "Name"};
    ["func"] = Fields{"Type", "Pos", "Len", "Name", "Params", "Body", "Receiver", "Dot"};
    ["nop"] = Fields{"Type", "Pos", "Len"};
    ["vararg"] = Fields{"Type", "Pos", "Len"};
    ["param"] = Fields{"Type", "Pos", "Len", "Name"};
    ["params"] = Fields{"Type", "Pos", "Len", "List"};
    ["switch"] = Fields{"Type", "Pos", "Len", "Expr", "Cases", "Default"};
    ["case"] = Fields{"Type", "Pos", "Len", "List", "Expr", "Body"};
    ["const"] = Fields{"Type", "Pos", "Len", "List"};
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
local KEYWORDS = Set{"break", "continue", "else", "false", "for", "func", "if", "in", "nil", "return", "true", "switch", "case", "default", "const"}
local RESERVED = Set{"local", "function", "while", "do", "end", "repeat", "until", "and", "or", "not", "then", "elseif"}
local LITERALS = Set{"str", "chr", "raw", "num", "true", "false", "nil"}
local REL_OPS = Set{"==", "!=", "<", ">", "<=", ">="}
local MUL_OPS = Set{"*", "/", "%"}
local ADD_OPS = Set{"+", "-"}
local UNR_OPS = Set{"+", "-", "!", "#"}
local MAP = {
    [0x22] = "str";
    [0x27] = "chr";
    [0x60] = "raw";
}
local HEX = {}
local ALPHA_OR_DIGIT = {}
do
    local byte, sub = string_byte, string_sub
    local sym = "()[]{}*:;.,/+-=<>#%|&!^"
    for i = 1, #sym do
        MAP[byte(sym, i)] = sub(sym, i, i)
    end
    for i = 0x01, 0x20 do
        MAP[i] = 3
    end
    for i = 0x30, 0x39 do
        MAP[i] = 2;
        ALPHA_OR_DIGIT[i] = true
    end
    local abc = "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    for i = 1, #abc do
        MAP[byte(abc, i)] = 1;
        ALPHA_OR_DIGIT[byte(abc, i)] = true
    end
    local hex = "ABCDEFabcdef0123456789"
    for i = 1, #hex do
        HEX[byte(hex, i)] = true
    end
end
local p_path = nil
local p_src = nil
local p_line = 1
local p_curpos = 0
local p_tokpos = 0
local p_endpos = 0
local p_chr = nil
local p_tok = nil
local p_lit = ""
local p_val = nil
local p_comments = List{}
local p_vars = {}
local p_scope = {}
local p_level = 0
local p_skip_id_check = false
local p_skipped_id_name = nil
local p_continue = List{}
local p_looplevel = 0
local function errorf(notef, ...)
    if p_path then
        error(p_path .. ":" .. p_line .. ": " .. string_format(notef, ...), 2)
    else
        error(string_format(notef, ...) .. " in line " .. p_line, 2)
    end
end
local function scan()
    local byte = string_byte
    local src = p_src
    local pos = p_curpos
    local chr = p_chr
    local tok = nil
    repeat
        tok = MAP[chr]
        p_lit = ""
        p_val = nil
        p_endpos = pos
        while tok == 3 do
            if chr == 0x0A then
                p_line = p_line + 1
            end
            pos = pos + 1;
            chr = byte(src, pos)
            tok = MAP[chr]
        end
        p_tokpos = pos
        do
            local case = tok
            if case == 1 then
                local AD = ALPHA_OR_DIGIT
                local beg = pos
                repeat
                    pos = pos + 1;
                    chr = byte(src, pos);
                until not AD[chr]
                p_lit = string_sub(src, beg, pos - 1)
                tok = KEYWORDS[p_lit] or "id"
                if tok == "true" then
                    p_val = true
                elseif tok == "false" then
                    p_val = false
                end
            elseif case == 2 then
                local beg = pos
                local base = 10
                if chr == 0x30 then
                    pos = pos + 1;
                    chr = byte(src, pos)
                    do
                        local case = chr
                        if case == 0x58 or case == 0x78 then
                            pos = pos + 1;
                            chr = byte(src, pos)
                            beg = pos
                            if not HEX[chr] then
                                errorf("expected hex digit, found '%c'", chr)
                            end
                            repeat
                                pos = pos + 1;
                                chr = byte(src, pos)
                            until not HEX[chr]
                            base = 16
                            if pos - beg > 8 then
                                errorf("integer greater than 32 bits", tok)
                            end
                        elseif case == 0x42 or case == 0x62 then
                            pos = pos + 1;
                            chr = byte(src, pos)
                            beg = pos
                            if chr ~= 0x30 and chr ~= 0x31 then
                                errorf("expected bin digit, found '%c'", chr)
                            end
                            repeat
                                pos = pos + 1;
                                chr = byte(src, pos)
                            until chr ~= 0x30 and chr ~= 0x31
                            base = 2
                            if pos - beg > 32 then
                                errorf("integer greater than 32 bits", tok)
                            end
                        end
                    end
                end
                if base == 10 then
                    while MAP[chr] == 2 do
                        pos = pos + 1;
                        chr = byte(src, pos)
                    end
                    if chr == 0x2E then
                        repeat
                            pos = pos + 1;
                            chr = byte(src, pos)
                        until MAP[chr] ~= 2
                    end
                    if chr == 0x45 or chr == 0x65 then
                        pos = pos + 1;
                        chr = byte(src, pos)
                        if chr == 0x2B or chr == 0x2D then
                            pos = pos + 1;
                            chr = byte(src, pos)
                        end
                        if MAP[chr] ~= 2 then
                            errorf("expected digit, found '%s'", tok)
                        end
                        repeat
                            pos = pos + 1;
                            chr = byte(src, pos)
                        until MAP[chr] ~= 2
                    end
                end
                tok = "num"
                p_lit = string_sub(src, beg, pos - 1)
                if base == 10 then
                    p_val = tonumber(p_lit)
                else
                    p_val = Hex{tonumber(p_lit, base)}
                end
                if p_val == nil then
                    errorf("malformed number '%s'", p_lit)
                end
            elseif case == "str" then
                local beg = pos
                repeat
                    pos = pos + 1;
                    chr = byte(src, pos)
                    if chr == 0x5C then
                        pos = pos + 2;
                        chr = byte(src, pos)
                    end
                until chr == 0x22 or chr == 0x0A or chr == nil
                if chr ~= 0x22 then
                    errorf("expected \", found EOL")
                end
                p_lit = string_sub(src, beg + 1, pos - 1)
                p_val = p_lit
                pos = pos + 1;
                chr = byte(src, pos)
            elseif case == "raw" then
                local beg = pos
                repeat
                    pos = pos + 1;
                    chr = byte(src, pos)
                    if chr == 0x0A then
                        p_line = p_line + 1
                    end
                until chr == 0x60 or chr == nil
                if chr ~= 0x60 then
                    errorf("expected `, found '%c'", chr)
                end
                p_lit = string_sub(src, beg + 1, pos - 1)
                p_val = Raw{p_lit}
                pos = pos + 1;
                chr = byte(src, pos)
            elseif case == "chr" then
                local beg = pos
                pos = pos + 1;
                chr = byte(src, pos)
                if chr == 0x5C then
                    beg = pos
                    pos = pos + 1;
                    chr = byte(src, pos)
                end
                pos = pos + 1;
                chr = byte(src, pos)
                if chr ~= 0x27 then
                    errorf("expected ', found '%c'", chr)
                end
                p_lit = string_sub(src, beg + 1, pos - 1)
                p_val = Hex{string_byte(p_lit)}
                pos = pos + 1;
                chr = byte(src, pos)
            elseif case == ":" then
                pos = pos + 1;
                chr = byte(src, pos)
                if chr == 0x3D then
                    tok = ":="
                    pos = pos + 1;
                    chr = byte(src, pos)
                elseif chr == 0x3A then
                    tok = "::"
                    pos = pos + 1;
                    chr = byte(src, pos)
                end
            elseif case == "=" then
                pos = pos + 1;
                chr = byte(src, pos)
                if chr == 0x3D then
                    tok = "=="
                    pos = pos + 1;
                    chr = byte(src, pos)
                end
            elseif case == "+" then
                pos = pos + 1;
                chr = byte(src, pos)
                if chr == 0x3D then
                    tok = "+="
                    pos = pos + 1;
                    chr = byte(src, pos)
                end
            elseif case == "-" then
                pos = pos + 1;
                chr = byte(src, pos)
                if chr == 0x3D then
                    tok = "-="
                    pos = pos + 1;
                    chr = byte(src, pos)
                end
            elseif case == "/" then
                pos = pos + 1;
                chr = byte(src, pos)
                if chr == 0x2F then
                    local beg = pos
                    repeat
                        pos = pos + 1;
                        chr = byte(src, pos)
                    until chr == 0x0A or chr == nil
                    p_lit = string_sub(src, beg + 1, pos - 1)
                    p_comments[p_line] = p_lit
                    tok = "//"
                end
            elseif case == "<" then
                pos = pos + 1;
                chr = byte(src, pos)
                if chr == 0x3D then
                    tok = "<="
                    pos = pos + 1;
                    chr = byte(src, pos)
                end
            elseif case == ">" then
                pos = pos + 1;
                chr = byte(src, pos)
                if chr == 0x3D then
                    tok = ">="
                    pos = pos + 1;
                    chr = byte(src, pos)
                end
            elseif case == "&" then
                pos = pos + 1;
                chr = byte(src, pos)
                if chr == 0x26 then
                    tok = "&&"
                    pos = pos + 1;
                    chr = byte(src, pos)
                end
            elseif case == "|" then
                pos = pos + 1;
                chr = byte(src, pos)
                if chr == 0x7C then
                    tok = "||"
                    pos = pos + 1;
                    chr = byte(src, pos)
                end
            elseif case == "!" then
                pos = pos + 1;
                chr = byte(src, pos)
                if chr == 0x3D then
                    tok = "!="
                    pos = pos + 1;
                    chr = byte(src, pos)
                end
            elseif case == "." then
                pos = pos + 1;
                chr = byte(src, pos)
                if chr == 0x2E then
                    tok = ".."
                    pos = pos + 1;
                    chr = byte(src, pos)
                    if chr == 0x2E then
                        tok = "..."
                        pos = pos + 1;
                        chr = byte(src, pos)
                    end
                elseif chr == 0x5B then
                    tok = ".["
                    pos = pos + 1;
                    chr = byte(src, pos)
                elseif chr == 0x7B then
                    tok = ".{"
                    pos = pos + 1;
                    chr = byte(src, pos)
                end
            elseif (case == nil) and (chr ~= nil) then
                errorf("unknown symbol '%c'", chr)
            else
                pos = pos + 1;
                chr = byte(src, pos)
            end
        end
    until tok ~= "//"
    p_curpos, p_chr, p_tok = pos, chr, tok
    return p_tok
end
local function expect(t, l)
    if p_tok ~= t then
        local str = nil
        if p_tok == "num" or p_tok == "str" or p_tok == "chr" or p_tok == "id" then
            str = p_lit
        else
            str = tostring(p_tok)
        end
        if p_path then
            error(string_format("%s:%d: expected '%s', found '%s'", p_path, p_line, tostring(t), str), l or 2)
        else
            error(string_format("expected '%s', found '%s' in line %d", tostring(t), str, p_line), l or 2)
        end
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
    if name == "_" then
        return nil
    end
    local v = p_vars[name]
    local i = p_level
    while v == nil and i > 1 do
        i = i - 1
        v = p_scope[i][name]
    end
    return v, i
end
local function assertf(expr, notef, ...)
    if not expr then
        if p_path then
            error(p_path .. ":" .. p_line .. ": " .. string_format(notef, ...), 2)
        else
            error(string_format(notef, ...) .. " in line " .. p_line, 2)
        end
    end
    return expr
end
local parse_expr = nil
local parse_table, parse_list = nil, nil
local function parse_tail(call)
    local tail, i = List{}, 0
    while true do
        local pos = p_tokpos
        local dot = (p_tok == ".")
        do
            local case = p_tok
            if case == "." or case == "::" then
                scan()
                if KEYWORDS[p_lit] == nil then
                    expect("id")
                end
                local name = p_lit
                local args = false
                local paren = false
                local last = p_tokpos
                scan()
                do
                    local case = p_tok
                    if case == "(" then
                        scan()
                        args = List{}
                        while p_tok ~= ")" do
                            args[#args + 1] = parse_expr()
                            if p_tok ~= "," then
                                break
                            end
                            scan()
                        end
                        last = p_tokpos
                        skip(")")
                        call = true
                        paren = true
                    elseif case == "str" then
                        args = List{Node{"value", p_tokpos, p_endpos - pos, p_val}}
                        last = p_tokpos
                        scan()
                        call = true
                    elseif case == "chr" then
                        args = List{Node{"value", p_tokpos, p_endpos - pos, p_val}}
                        last = p_tokpos
                        scan()
                        call = true
                        paren = true
                    elseif case == ".{" then
                        args = List{parse_table()}
                        last = p_tokpos
                        scan()
                        call = true
                    elseif case == ".[" then
                        args = List{parse_list()}
                        last = p_tokpos
                        scan()
                        call = true
                    else
                        call = false
                    end
                end
                if args and dot and (KEYWORDS[name] or RESERVED[name]) then
                    errorf("name '%s' cannot be used in a method call", name)
                end
                local item = Node{"field", pos, last - pos, args or false, name, dot, paren}
                i = i + 1
                tail[i] = item
            elseif case == "[" then
                scan()
                assertf(p_tok ~= "]", "expected expression, found ']'")
                local expr = parse_expr()
                skip("]")
                i = i + 1
                tail[i] = Node{"index", pos, p_endpos - pos, expr}
            else
                break
            end
        end
    end
    return i > 0 and tail, call
end
local function parse_id()
    local pos = p_tokpos
    local name = p_lit
    local _const = false
    assertf(not RESERVED[name], "name '%s' is reserved", name)
    if p_skip_id_check then
        p_skipped_id_name = name
        p_skip_id_check = false
    else
        local id = assertf(find_var(name), "undeclared variable '%s'", name)
        _const = id[7]
    end
    local call = false
    local paren = false
    local args, tail = false, false
    scan()
    if not _const then
        do
            local case = p_tok
            if case == "(" then
                scan()
                args = List{}
                while p_tok ~= ")" do
                    args[#args + 1] = parse_expr()
                    if p_tok ~= "," then
                        break
                    end
                    scan()
                end
                skip(")")
                call = true
                paren = true
            elseif case == "str" then
                args = List{Node{"value", p_tokpos, p_endpos - pos, p_val}}
                scan()
                call = true
            elseif case == "chr" then
                args = List{Node{"value", p_tokpos, p_endpos - pos, p_val}}
                scan()
                call = true
                paren = true
            elseif case == ".{" then
                args = List{parse_table()}
                call = true
            elseif case == ".[" then
                args = List{parse_list()}
                call = true
            end
        end
        tail, call = parse_tail(call)
    end
    local node = Node{"id", pos, p_endpos - pos, name, tail, args, _const, paren}
    return node, call
end
local function parse_paren()
    local pos = p_tokpos
    skip("(")
    local expr = parse_expr()
    skip(")")
    return Node{"paren", pos, p_endpos - pos, expr}
end
parse_table = function()
    local pos = p_tokpos
    local list = List{}
    scan()
    while p_tok ~= "}" do
        local key_pos = p_tokpos
        local left = nil
        if p_tok == "id" or KEYWORDS[p_lit] then
            left = Node{"id", key_pos, #p_lit, p_lit, false, false}
            scan()
        elseif p_tok == "[" then
            scan()
            local expr = parse_expr()
            skip("]")
            left = Node{"index", key_pos, p_endpos - key_pos, expr}
        else
            left = parse_expr()
        end
        expect(":")
        scan()
        local right = parse_expr()
        list[#list + 1] = Node{"pair", pos, p_endpos - pos, left, right}
        if p_tok ~= "," then
            break
        end
        scan()
    end
    skip("}")
    return Node{"table", pos, p_endpos - pos, list}
end
parse_list = function()
    local pos = p_tokpos
    local list = List{}
    scan()
    while p_tok ~= "]" do
        list[#list + 1] = parse_expr()
        if p_tok ~= "," then
            break
        end
        scan()
    end
    skip("]")
    return Node{"list", pos, p_endpos - pos, list}
end
local parse_func = nil
local function parse_operand()
    local node = nil
    do
        local case = p_tok
        if case == "str" then
            local pos, len, val = p_tokpos, #p_lit, p_val
            scan()
            local tail = parse_tail()
            node = Node{"value", pos, len, val, tail}
        elseif case == "id" then
            node = parse_id()
        elseif case == "(" then
            node = parse_paren()
        elseif case == "{" then
            node = parse_table()
        elseif case == "[" then
            node = parse_list()
        elseif case == "func" then
            node = parse_func(true)
        elseif case == "..." then
            node = Node{"vararg", p_tokpos, 3}
            scan()
        else
            if LITERALS[p_tok] then
                node = Node{"value", p_tokpos, #p_lit, p_val}
                scan()
            else
                errorf("expected operand, found '%s'", p_tok)
            end
        end
    end
    return node
end
local function parse_pow()
    local pos = p_tokpos
    local left = parse_operand()
    while p_tok == "^" do
        scan()
        local right = parse_operand()
        left = Node{"binop", pos, p_endpos - pos, left, "^", right}
    end
    return left
end
local function parse_unary()
    local pos = p_tokpos
    local expr = nil
    if UNR_OPS[p_tok] then
        local op = p_tok
        scan()
        local right = parse_pow()
        expr = Node{"unop", pos, p_endpos - pos, op, right}
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
        left = Node{"binop", pos, p_endpos - pos, left, op, right}
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
        left = Node{"binop", pos, p_endpos - pos, left, op, right}
    end
    return left
end
local function parse_cat()
    local pos = p_tokpos
    local left = parse_add()
    while p_tok == ".." do
        scan()
        local right = parse_add()
        left = Node{"binop", pos, p_endpos - pos, left, "..", right}
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
        left = Node{"binop", pos, p_endpos - pos, left, op, right}
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
        left = Node{"binop", pos, p_endpos - pos, left, op, right}
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
        left = Node{"binop", pos, p_endpos - pos, left, op, right}
    end
    return left
end
local parse_block, parse_body = nil, nil
local function parse_set_or_call()
    local pos = p_tokpos
    local name = p_lit
    p_skip_id_check = true
    local id, call = parse_id()
    if call then
        assertf(find_var(name), "undeclared variable '%s'", name)
        return Node{"call", pos, p_endpos - pos, id}
    end
    if p_tok == "+=" then
        assertf(find_var(name), "undeclared variable '%s'", name)
        scan()
        local expr = parse_add()
        return Node{"inc", pos, p_endpos - pos, id, expr}
    end
    if p_tok == "-=" then
        assertf(find_var(name), "undeclared variable '%s'", name)
        scan()
        local expr = parse_add()
        return Node{"dec", pos, p_endpos - pos, id, expr}
    end
    local left = List{id}
    while p_tok == "," do
        scan()
        expect("id")
        p_skip_id_check = true
        id, call = parse_id()
        assertf(not call, "unexpected call")
        left[#left + 1] = id
    end
    if p_tok == "=" then
        for _, _id in ipairs(left) do
            name = _id[4]
            local v = assertf(find_var(name), "undeclared variable '%s'", name)
            assertf(not v[7], "cannot assign to '%s' (declared const)", name)
        end
        scan()
        local right = List{}
        while true do
            right[#right + 1] = parse_expr()
            if p_tok ~= "," then
                break
            end
            scan()
        end
        return Node{"set", pos, p_endpos - pos, left, right}
    end
    if p_tok == ":=" then
        local allow = false
        for _, _id in ipairs(left) do
            assertf(not _id[5], "unexpected tail")
            assertf(not _id[6], "unexpected call")
            assertf(not _id[7], "unexpected self")
            local v, level = find_var(_id[4])
            if not v then
                allow = true
            elseif level ~= p_level or v[7] then
                errorf("variable shadowing is prohibited, you need to change the name '%s'", p_skipped_id_name)
            end
        end
        assertf(allow, "no new variables on left side of ':='")
        scan()
        local right = List{}
        while true do
            right[#right + 1] = parse_expr()
            if p_tok ~= "," then
                break
            end
            scan()
        end
        for _, v in ipairs(left) do
            p_vars[v[4]] = v
        end
        return Node{"let", pos, p_endpos - pos, left, right}
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
    return Node{"if", pos, p_endpos - pos, expr, body, else_body}
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
            cases[#cases + 1] = Node{"case", pos, p_endpos - pos, false, case_expr, body}
        end
    else
        expr = parse_expr()
        skip("{")
        expect("case")
        while p_tok == "case" do
            scan()
            local list = List{}
            while true do
                list[#list + 1] = parse_expr()
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
            cases[#cases + 1] = Node{"case", pos, p_endpos - pos, list, case_expr, body}
        end
    end
    local _default = false
    if p_tok == "default" then
        scan()
        skip(":")
        _default = parse_body()
    end
    skip("}")
    return Node{"switch", pos, p_endpos - pos, expr, cases, _default}
end
local function parse_for()
    local pos = p_tokpos
    p_looplevel = p_looplevel + 1
    skip("for")
    if p_tok == "{" then
        local body = parse_block(nil, true)
        return Node{"for", pos, p_endpos - pos, false, body}
    end
    p_skip_id_check = true
    local expr = parse_expr()
    if p_tok == "{" or expr[1] ~= "id" or expr[5] or expr[6] then
        assertf(find_var(p_skipped_id_name), "undeclared variable '%s'", p_skipped_id_name)
        local body = parse_block(nil, true)
        return Node{"for", pos, p_endpos - pos, expr, body}
    end
    local vars = {
        [expr[4]] = expr;
    }
    if p_tok == ":=" then
        assertf(not find_var(p_skipped_id_name), "variable shadowing is prohibited, you need to change the name '%s'", p_skipped_id_name)
        scan()
        local from = parse_expr()
        skip(",")
        local to = parse_expr()
        local by = nil
        if p_tok == "," then
            scan()
            by = parse_expr()
        end
        local body = parse_block(vars, true)
        return Node{"for_to", pos, p_endpos - pos, expr, from, to, by or false, body}
    end
    assertf(not find_var(p_skipped_id_name), "variable shadowing is prohibited, you need to change the name '%s'", p_skipped_id_name)
    local ids = List{expr}
    while p_tok == "," do
        scan()
        expect("id")
        local name = p_lit
        assertf(not RESERVED[name], "name '%s' is reserved", name)
        assertf(not find_var(name), "variable shadowing is prohibited, you need to change the name '%s'", name)
        local id = Node{"id", p_tokpos, #name, name, false, false}
        ids[#ids + 1] = id
        assertf(vars[name] == nil, "re-declaring variable '%s'", name)
        vars[name] = id
        scan()
    end
    expect("in")
    local ins = List{}
    scan()
    ins[#ins + 1] = parse_expr()
    if p_tok == "," then
        scan()
        ins[#ins + 1] = parse_expr()
    end
    if p_tok == "," then
        scan()
        ins[#ins + 1] = parse_expr()
    end
    local body = parse_block(vars, true)
    return Node{"for_in", pos, p_endpos - pos, ids, ins, body}
end
local function parse_return()
    local pos = p_tokpos
    skip("return")
    local list = List{}
    if p_tok ~= "}" then
        while true do
            list[#list + 1] = parse_expr()
            if p_tok ~= "," then
                break
            end
            scan()
        end
    end
    if p_tok and p_tok ~= "case" then
        expect("}")
    end
    return Node{"return", pos, p_endpos - pos, list}
end
local function parse_break()
    local pos = p_tokpos
    assert(p_looplevel > 0, "no loop to break")
    skip("break")
    local expr = false
    if p_tok == "if" then
        scan()
        expr = parse_expr()
    end
    expect("}")
    return Node{"break", pos, 5, expr}
end
local function parse_continue()
    local pos = p_tokpos
    assert(p_looplevel > 0, "no loop to continue")
    skip("continue")
    expect("}")
    p_continue[p_looplevel] = true
    return Node{"continue", pos, 8}
end
local function parse_const()
    local pos = p_tokpos
    skip("const")
    if p_tok == "id" then
        local name = p_lit
        assertf(not RESERVED[name], "name '%s' is reserved", name)
        assertf(not find_var(name), "variable shadowing is prohibited, you need to change the name '%s'", name)
        local id = Node{"id", p_tokpos, #name, name, false, false, nil}
        local list = List{id}
        scan()
        skip("=")
        assertf(LITERALS[p_tok], "expected value")
        id[7] = Node{"value", p_tokpos, #p_lit, p_val}
        p_vars[name] = id
        scan()
        return Node{"const", pos, p_endpos - pos, list}
    end
    if p_tok == "(" then
        scan()
        local list = List{}
        while p_tok == "id" do
            local name = p_lit
            assertf(not RESERVED[name], "name '%s' is reserved", name)
            assertf(not find_var(name), "variable shadowing is prohibited, you need to change the name '%s'", name)
            local id = Node{"id", p_tokpos, #name, name, false, false, nil}
            list[#list + 1] = id
            scan()
            skip("=")
            assertf(LITERALS[p_tok], "expected value")
            id[7] = Node{"value", p_tokpos, #p_lit, p_val}
            p_vars[name] = id
            scan()
        end
        skip(")")
        return Node{"const", pos, p_endpos - pos, list}
    end
    expect("id")
end
local function parse_params()
    local pos = p_tokpos
    skip("(")
    local list = List{}
    while true do
        if p_tok == "..." then
            local id = Node{"param", p_tokpos, 3, "..."}
            list[#list + 1] = id
            p_vars["..."] = id
            scan()
            break
        end
        if p_tok ~= "id" then
            break
        end
        local id = Node{"param", p_tokpos, #p_lit, p_lit}
        list[#list + 1] = id
        assertf(p_vars[p_lit] == nil, "parameter '%s' is already declared", p_lit)
        p_vars[p_lit] = id
        scan()
        if p_tok ~= "," then
            break
        end
        scan()
    end
    skip(")")
    return Node{"params", pos, p_endpos - pos, list}
end
parse_func = function(lambda)
    local pos = p_tokpos
    local name = false
    local receiver = false
    local dot = false
    local vars = nil
    skip("func")
    if not lambda then
        expect("id")
        name = p_lit
        scan()
        if p_tok == "." then
            dot = true
            scan()
            expect("id")
            assertf(find_var(name), "undeclared variable '%s'", name)
            receiver = name
            name = p_lit
            scan()
            vars = {
                ["self"] = Node{"id", 0, 0, "self", false, false};
            }
        elseif p_tok == "::" then
            scan()
            expect("id")
            assertf(find_var(name), "undeclared variable '%s'", name)
            receiver = name
            name = p_lit
            scan()
        else
            assertf(not find_var(name), "re-declaring variable '%s'", name)
        end
        p_vars[name] = {}
    end
    open_scope(vars)
    local params = parse_params()
    local body = parse_block()
    close_scope()
    local node = Node{"func", pos, p_endpos - pos, name, params, body, receiver, dot}
    if not lambda then
        p_vars[name] = node
    end
    return node
end
local function parse_statement()
    do
        local case = p_tok
        if case == "id" then
            return parse_set_or_call()
        elseif case == "func" then
            return parse_func()
        elseif case == "if" then
            return parse_if()
        elseif case == "{" then
            return parse_block()
        elseif case == "for" then
            return parse_for()
        elseif case == "return" then
            return parse_return()
        elseif case == "break" then
            return parse_break()
        elseif case == "continue" then
            return parse_continue()
        elseif case == "switch" then
            return parse_switch()
        elseif case == "const" then
            return parse_const()
        elseif case == ";" then
            scan()
            return Node{"nop", p_curpos, 1}
        end
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
        body[#body + 1] = stmt
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
        body[#body + 1] = Node{"label", 0, 0, "continue"}
        p_continue[p_looplevel] = false
        p_looplevel = p_looplevel - 1
    end
    return Node{"block", pos, p_endpos - pos, body}
end
local function parse_module(src, path, vars)
    p_path = path
    p_src = src
    p_line = 1
    p_curpos = 0
    p_tokpos = 0
    p_endpos = 0
    p_chr = nil
    p_tok = nil
    p_lit = ""
    p_val = nil
    p_comments = List{}
    p_vars = {
        _G = {"id", 0, 0, "_G", false, false};
    }
    p_level = 1
    p_scope = List{p_vars}
    p_continue = List{}
    p_looplevel = 0
    p_curpos = p_curpos + 1;
    p_chr = string_byte(p_src, p_curpos)
    scan()
    local pos = p_tokpos
    local body = parse_body(vars)
    local module = Node{"module", pos, p_endpos - pos, body, p_comments}
    if p_tok ~= nil then
        errorf("unexpected '%s'", p_tok)
    end
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
local function add(s)
    v_res[#v_res + 1] = s
end
local visit_expr = nil
local function visit_field(node)
    local args = node[4]
    local paren = node[7]
    local name = node[5]
    if args then
        if node[6] then
            add(":" .. name)
        else
            if KEYWORDS[name] or RESERVED[name] then
                add("[\"" .. name .. "\"]")
            else
                add("." .. name)
            end
        end
        if paren then
            add("(")
        end
        if #args > 0 then
            for _, v in ipairs(args) do
                visit_expr(v)
                add(", ")
            end
            v_res[#v_res] = ""
        end
        if paren then
            add(")")
        end
    else
        if KEYWORDS[name] or RESERVED[name] then
            add("[\"" .. name .. "\"]")
        else
            add("." .. name)
        end
    end
end
local function visit_index(node)
    add("[")
    visit_expr(node[4])
    add("]")
end
local function visit_value(node)
    local v = node[4]
    if type(v) == "string" then
        local tail = node[5]
        if tail then
            add("(\"" .. v .. "\")")
            for _, _v in ipairs(tail) do
                if _v[1] == "field" then
                    visit_field(_v)
                else
                    visit_index(_v)
                end
            end
        else
            add("\"" .. v .. "\"")
        end
    else
        add(tostring(v))
    end
end
local function visit_id(node)
    local tail, args, _const, paren = node[5], node[6], node[7], node[8]
    if _const and _const[1] == "value" then
        visit_value(_const)
        return
    end
    add(node[4])
    if args then
        if paren then
            add("(")
        end
        if #args > 0 then
            for _, v in ipairs(args) do
                visit_expr(v)
                add(", ")
            end
            v_res[#v_res] = ""
        end
        if paren then
            add(")")
        end
    end
    if tail then
        for _, v in ipairs(tail) do
            if v[1] == "field" then
                visit_field(v)
            else
                visit_index(v)
            end
        end
    end
end
local function visit_pair(node)
    local key = node[4]
    if key[1] == "id" and not key[5] and not key[6] then
        local name = key[4]
        if KEYWORDS[name] or RESERVED[name] then
            add("[\"")
            visit_expr(key)
            add("\"]")
        else
            visit_expr(key)
        end
    elseif key[1] == "index" then
        visit_index(key)
    else
        add("[")
        visit_expr(key)
        add("]")
    end
    add(" = ")
    visit_expr(node[5])
end
local function visit_table(node)
    if #node[4] == 0 then
        add("{}")
        return
    end
    add("{\n")
    v_level = v_level + 1
    for _, v in ipairs(node[4]) do
        add(space())
        if v[1] == "pair" then
            visit_pair(v)
            add(";\n")
        else
            visit_expr(v)
            add(",\n")
        end
    end
    v_level = v_level - 1
    add(space() .. "}")
end
local function visit_list(node)
    add("{")
    local list = node[4]
    if #list > 0 then
        for _, v in ipairs(list) do
            visit_expr(v)
            add(", ")
        end
        v_res[#v_res] = ""
    end
    add("}")
end
local function visit_paren(node)
    add("(")
    visit_expr(node[4])
    add(")")
end
local function visit_unop(node)
    local op = LUA_OPS[node[4]]
    if op ~= "+" then
        add(LUA_OPS[node[4]])
    end
    visit_expr(node[5])
end
local function visit_binop(node)
    visit_expr(node[4])
    add(" " .. LUA_OPS[node[5]] .. " ")
    visit_expr(node[6])
end
local visit_func = nil
visit_expr = function(node)
    do
        local case = node[1]
        if case == "id" then
            visit_id(node)
        elseif case == "binop" then
            visit_binop(node)
        elseif case == "unop" then
            visit_unop(node)
        elseif case == "paren" then
            visit_paren(node)
        elseif case == "value" then
            visit_value(node)
        elseif case == "table" then
            visit_table(node)
        elseif case == "list" then
            visit_list(node)
        elseif case == "func" then
            visit_func(node, true)
        elseif case == "vararg" then
            add("...")
        else
            errorf("unknown node type: '%s'", node[1])
        end
    end
end
local visit_stmt = nil
local function visit_body(node, skip_break)
    v_level = v_level + 1
    for _, v in ipairs(node) do
        visit_stmt(v, skip_break)
    end
    v_level = v_level - 1
end
local function visit_block(node, skip_break)
    visit_body(node[4], skip_break)
end
local function visit_call(node)
    add(space())
    visit_id(node[4])
    add("\n")
end
local function visit_set(node)
    add(space())
    for _, v in ipairs(node[4]) do
        visit_id(v)
        add(", ")
    end
    v_res[#v_res] = " = "
    for _, v in ipairs(node[5]) do
        visit_expr(v)
        add(", ")
    end
    v_res[#v_res] = "\n"
end
local function visit_inc(node)
    add(space())
    visit_id(node[4])
    add(" = ")
    visit_id(node[4])
    add(" + ")
    visit_expr(node[5])
    add("\n")
end
local function visit_dec(node)
    add(space())
    visit_id(node[4])
    add(" = ")
    visit_id(node[4])
    add(" - ")
    visit_expr(node[5])
    add("\n")
end
local function visit_let(node)
    add(space() .. "local ")
    for _, v in ipairs(node[4]) do
        visit_id(v)
        add(", ")
    end
    v_res[#v_res] = " = "
    for _, v in ipairs(node[5]) do
        visit_expr(v)
        add(", ")
    end
    v_res[#v_res] = "\n"
end
local function visit_if(node)
    add(space() .. "if ")
    visit_expr(node[4])
    add(" then\n")
    visit_block(node[5])
    local _else = node[6]
    while _else do
        add(space() .. "else")
        if _else[1] == "if" then
            add("if ")
            visit_expr(_else[4])
            add(" then\n")
            visit_block(_else[5])
            _else = _else[6]
        else
            add("\n")
            visit_block(_else)
            _else = nil
        end
    end
    add(space() .. "end\n")
end
local function visit_switch(node)
    local expr = node[4]
    if expr then
        add(space() .. "do\n")
        v_level = v_level + 1
        add(space() .. "local case = ")
        visit_expr(expr)
        add("\n" .. space())
        for _, _case in ipairs(node[5]) do
            add("if ")
            local case_exp = _case[5]
            if case_exp then
                add("(")
            end
            for _, item in ipairs(_case[4]) do
                add("case == ")
                if item[1] == "binop" then
                    add("(")
                    visit_expr(item)
                    add(")")
                else
                    visit_expr(item)
                end
                add(" or ")
            end
            if case_exp then
                v_res[#v_res] = ") and ("
                visit_expr(case_exp)
                add(")")
            else
                v_res[#v_res] = ""
            end
            add(" then\n")
            visit_body(_case[6])
            add(space() .. "else")
        end
        local _default = node[6]
        if _default then
            add("\n")
            visit_body(_default)
        else
            v_res[#v_res] = ""
        end
        add(space() .. "end\n")
        v_level = v_level - 1
        add(space() .. "end\n")
    else
        add(space())
        for _, _case in ipairs(node[5]) do
            add("if ")
            visit_expr(_case[5])
            add(" then\n")
            visit_body(_case[6])
            add(space() .. "else")
        end
        local _default = node[6]
        if _default then
            add("\n")
            visit_body(_default)
        else
            v_res[#v_res] = ""
        end
        add(space() .. "end\n")
    end
end
local function visit_for(node)
    local expr = node[4]
    local block = node[5]
    local body = block[4]
    if not expr and #body > 0 then
        local last = body[#body]
        if last[1] == "break" then
            local break_expr = last[4]
            if break_expr then
                add(space() .. "repeat\n")
                visit_block(block, true)
                add(space() .. "until ")
                visit_expr(break_expr)
                add("\n")
                return
            end
        end
    end
    add(space() .. "while ")
    if expr then
        visit_expr(expr)
    else
        add("true")
    end
    add(" do\n")
    visit_block(block)
    add(space() .. "end\n")
end
local function visit_for_to(node)
    add(space() .. "for ")
    visit_id(node[4])
    add(" = ")
    visit_expr(node[5])
    add(", ")
    visit_expr(node[6])
    local step = node[7]
    if step then
        add(", ")
        visit_expr(step)
    end
    add(" do\n")
    visit_block(node[8])
    add(space() .. "end\n")
end
local function visit_for_in(node)
    add(space() .. "for ")
    for _, v in ipairs(node[4]) do
        visit_id(v)
        add(", ")
    end
    v_res[#v_res] = " in "
    for _, v in ipairs(node[5]) do
        visit_expr(v)
        add(", ")
    end
    v_res[#v_res] = " do\n"
    visit_block(node[6])
    add(space() .. "end\n")
end
local function visit_return(node)
    local list = node[4]
    add(space() .. "return")
    if list then
        add(" ")
        for _, v in ipairs(list) do
            visit_expr(v)
            add(", ")
        end
        v_res[#v_res] = ""
    end
    add("\n")
end
local function visit_break(node)
    local expr = node[4]
    if expr then
        add(space() .. "if ")
        visit_expr(expr)
        add(" then\n")
        v_level = v_level + 1
        add(space() .. "break\n")
        v_level = v_level - 1
        add(space() .. "end\n")
    else
        add(space() .. "break\n")
    end
end
local function visit_continue(node)
    add(space() .. "goto continue\n")
end
local function visit_label(node)
    add(space() .. "::" .. node[4] .. "::\n")
end
local function visit_params(node)
    local t = {}
    for _, v in ipairs(node[4]) do
        t[#t + 1] = v[4]
    end
    add("(" .. table_concat(t, ", ") .. ")\n")
end
visit_func = function(node, lambda)
    if lambda then
        add("function")
    elseif node[7] then
        if node[8] then
            add(space() .. "function " .. node[7] .. ":" .. node[4])
        else
            add(space() .. "function " .. node[7] .. "." .. node[4])
        end
    else
        add(space() .. "local function " .. node[4])
    end
    visit_params(node[5])
    visit_block(node[6])
    add(space() .. "end")
    if not lambda then
        add("\n")
    end
end
local function visit_nop(node)
    v_res[#v_res] = ";\n"
end
visit_stmt = function(node, skip_break)
    do
        local case = node[1]
        if case == "call" then
            visit_call(node)
        elseif case == "set" then
            visit_set(node)
        elseif case == "inc" then
            visit_inc(node)
        elseif case == "dec" then
            visit_dec(node)
        elseif case == "let" then
            visit_let(node)
        elseif case == "if" then
            visit_if(node)
        elseif case == "switch" then
            visit_switch(node)
        elseif case == "block" then
            add(space() .. "do\n")
            visit_block(node)
            add(space() .. "end\n")
        elseif case == "for" then
            visit_for(node)
        elseif case == "for_to" then
            visit_for_to(node)
        elseif case == "for_in" then
            visit_for_in(node)
        elseif case == "return" then
            visit_return(node)
        elseif case == "break" then
            if not skip_break then
                visit_break(node)
            end
        elseif case == "continue" then
            visit_continue(node)
        elseif case == "label" then
            visit_label(node)
        elseif case == "func" then
            visit_func(node)
        elseif case == "nop" then
            visit_nop(node)
        elseif case == "const" then
        else
            errorf("unknown node type: %s", node[1])
        end
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
do
    local fn = _G.arg[1]
    if fn then
        local os, io, pcall, print = _G.os, _G.io, _G.pcall, _G.print
        local src = io.open(fn, "r"):read("a")
        local r, m = pcall(parse_module, src, fn)
        if r then
            local res = visit_module(m, 0)
            local out = _G.arg[2]
            if out then
                io.open(out, "w"):write(res)
            else
                io.open(fn .. ".lua", "w"):write(res)
            end
            print(os.clock())
        else
            print(m)
            os.exit(1)
        end
    end
end
return {
    Node = Node;
    nodes = nodes;
    parse_module = parse_module;
    visit_module = visit_module;
}
