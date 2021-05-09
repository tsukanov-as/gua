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
local Hex = Type({
    __tostring = function(t)
        return string_format("0x%02X", t[1])
    end;
})
local Raw = Type({
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
})
local List = Type({
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
})
local Set = Type({
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
})
local Fields = Type({
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
})
local nodes = {
    ["module"] = Fields({"Type", "Pos", "Len", "Body", "Comments"});
    ["value"] = Fields({"Type", "Pos", "Len", "Value"});
    ["field"] = Fields({"Type", "Pos", "Len", "Args", "Name", "Dot"});
    ["index"] = Fields({"Type", "Pos", "Len", "Expr"});
    ["id"] = Fields({"Type", "Pos", "Len", "Name", "Tail", "Args", "Const"});
    ["table"] = Fields({"Type", "Pos", "Len", "List"});
    ["pair"] = Fields({"Type", "Pos", "Len", "Left", "Right"});
    ["list"] = Fields({"Type", "Pos", "Len", "List"});
    ["paren"] = Fields({"Type", "Pos", "Len", "Expr"});
    ["unop"] = Fields({"Type", "Pos", "Len", "Op", "Expr"});
    ["binop"] = Fields({"Type", "Pos", "Len", "Left", "Op", "Right"});
    ["call"] = Fields({"Type", "Pos", "Len", "ID"});
    ["set"] = Fields({"Type", "Pos", "Len", "Left", "Right"});
    ["let"] = Fields({"Type", "Pos", "Len", "Left", "Right"});
    ["inc"] = Fields({"Type", "Pos", "Len", "ID", "Expr"});
    ["dec"] = Fields({"Type", "Pos", "Len", "ID", "Expr"});
    ["if"] = Fields({"Type", "Pos", "Len", "Expr", "Then", "Else", "Left", "Right"});
    ["block"] = Fields({"Type", "Pos", "Len", "Body"});
    ["for"] = Fields({"Type", "Pos", "Len", "Expr", "Body"});
    ["for_to"] = Fields({"Type", "Pos", "Len", "ID", "From", "Limit", "Step", "Body"});
    ["for_in"] = Fields({"Type", "Pos", "Len", "IDs", "INs", "Body"});
    ["return"] = Fields({"Type", "Pos", "Len", "List"});
    ["break"] = Fields({"Type", "Pos", "Len", "Expr"});
    ["continue"] = Fields({"Type", "Pos", "Len"});
    ["label"] = Fields({"Type", "Pos", "Len", "Name"});
    ["func"] = Fields({"Type", "Pos", "Len", "Name", "Params", "Body", "Receiver"});
    ["receiver"] = Fields({"Type", "Pos", "Len", "Name", "ID"});
    ["nop"] = Fields({"Type", "Pos", "Len"});
    ["vararg"] = Fields({"Type", "Pos", "Len"});
    ["param"] = Fields({"Type", "Pos", "Len", "Name"});
    ["params"] = Fields({"Type", "Pos", "Len", "List"});
    ["switch"] = Fields({"Type", "Pos", "Len", "Expr", "Cases", "Default"});
    ["case"] = Fields({"Type", "Pos", "Len", "List", "Expr", "Body"});
    ["const"] = Fields({"Type", "Pos", "Len", "List"});
}
local Node = Type({
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
})
local KEYWORDS = Set({"break", "continue", "else", "false", "for", "func", "if", "in", "nil", "return", "true", "switch", "case", "default", "const"})
local RESERVED = Set({"local", "function", "while", "do", "end", "repeat", "until", "and", "or", "not", "then", "elseif"})
local LITERALS = Set({"str", "chr", "raw", "num", "true", "false", "nil"})
local REL_OPS = Set({"==", "!=", "<", ">", "<=", ">="})
local MUL_OPS = Set({"*", "/", "%"})
local ADD_OPS = Set({"+", "-"})
local UNR_OPS = Set({"+", "-", "!", "#"})
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
local p_len = 0
local p_line = 1
local p_curpos = 0
local p_tokpos = 0
local p_endpos = 0
local p_chr = nil
local p_tok = nil
local p_lit = ""
local p_val = nil
local p_comments = List({})
local p_vars = {}
local p_scope = {}
local p_level = 0
local p_left = false
local p_continue = List({})
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
        for i = pos + 1, p_len + 1 do
            if tok ~= 3 then
                break
            end
            if chr == 0x0A then
                p_line = p_line + 1
            end
            pos = i;
            chr = byte(src, pos)
            tok = MAP[chr]
        end
        p_tokpos = pos
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
                p_val = Hex({tonumber(p_lit, base)})
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
            p_val = Raw({p_lit})
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
            p_val = Hex({string_byte(p_lit)})
            pos = pos + 1;
            chr = byte(src, pos)
        elseif (case == nil) and (chr ~= nil) then
            errorf("unknown symbol '%c'", chr)
        else
            local old = chr
            pos = pos + 1;
            chr = byte(src, pos)
            local case = chr
            if case == 0x3D then
                local case = old
                if case == 0x3A then
                    tok = ":=";
                    pos = pos + 1;
                    chr = byte(src, pos)
                elseif case == 0x3D then
                    tok = "==";
                    pos = pos + 1;
                    chr = byte(src, pos)
                elseif case == 0x2B then
                    tok = "+=";
                    pos = pos + 1;
                    chr = byte(src, pos)
                elseif case == 0x2D then
                    tok = "-=";
                    pos = pos + 1;
                    chr = byte(src, pos)
                elseif case == 0x3C then
                    tok = "<=";
                    pos = pos + 1;
                    chr = byte(src, pos)
                elseif case == 0x3E then
                    tok = ">=";
                    pos = pos + 1;
                    chr = byte(src, pos)
                elseif case == 0x21 then
                    tok = "!=";
                    pos = pos + 1;
                    chr = byte(src, pos)
                end
            elseif case == 0x3A then
                if old == 0x3A then
                    tok = "::"
                    pos = pos + 1;
                    chr = byte(src, pos)
                end
            elseif case == 0x2F then
                if old == 0x2F then
                    local beg = pos
                    repeat
                        pos = pos + 1;
                        chr = byte(src, pos)
                    until chr == 0x0A or chr == nil
                    p_lit = string_sub(src, beg + 1, pos - 1)
                    p_comments[p_line] = p_lit
                    tok = "//"
                end
            elseif case == 0x26 then
                if old == 0x26 then
                    tok = "&&"
                    pos = pos + 1;
                    chr = byte(src, pos)
                end
            elseif case == 0x7C then
                if old == 0x7C then
                    tok = "||"
                    pos = pos + 1;
                    chr = byte(src, pos)
                end
            elseif case == 0x2E then
                if old == 0x2E then
                    tok = ".."
                    pos = pos + 1;
                    chr = byte(src, pos)
                    if chr == 0x2E then
                        tok = "..."
                        pos = pos + 1;
                        chr = byte(src, pos)
                    end
                end
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
local parse_expr = nil
local parse_table, parse_list = nil, nil
local function parse_tail(call)
    local tail, i = List({}), 0
    while true do
        local pos = p_tokpos
        local dot = (p_tok == ".")
        local case = p_tok
        if case == "." or case == "::" then
            scan()
            if KEYWORDS[p_lit] == nil then
                expect("id")
            end
            local name = p_lit
            local args = false
            local last = p_tokpos
            scan()
            if p_tok == "(" then
                scan()
                args = List({})
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
            else
                call = false
            end
            if args and dot and (KEYWORDS[name] or RESERVED[name]) then
                errorf("name '%s' cannot be used in a method call", name)
            end
            local item = Node({"field", pos, last - pos, args or false, name, dot})
            i = i + 1
            tail[i] = item
        elseif case == "[" then
            scan()
            if p_tok == "]" then
                errorf("expected expression, found ']'")
            end
            local expr = parse_expr()
            skip("]")
            i = i + 1
            tail[i] = Node({"index", pos, p_endpos - pos, expr})
        else
            break
        end
    end
    return i > 0 and tail, call
end
local function parse_id(must_exist)
    local pos = p_tokpos
    local name = p_lit
    if RESERVED[name] then
        errorf("name '%s' is reserved", name)
    end
    local id = find_var(name)
    if must_exist and not id then
        errorf("undeclared variable '%s'", name)
    end
    local _const = id and id[7] or false
    local call = false
    local args, tail = false, false
    scan()
    if not _const then
        if p_tok == "(" then
            scan()
            args = List({})
            while p_tok ~= ")" do
                args[#args + 1] = parse_expr()
                if p_tok ~= "," then
                    break
                end
                scan()
            end
            skip(")")
            call = true
        end
        tail, call = parse_tail(call)
    end
    local node = Node({"id", pos, p_endpos - pos, name, tail, args, _const})
    return node, call
end
local function parse_paren()
    local pos = p_tokpos
    skip("(")
    local expr = parse_expr()
    skip(")")
    return Node({"paren", pos, p_endpos - pos, expr})
end
parse_table = function()
    local pos = p_tokpos
    local list = List({})
    skip("{")
    while p_tok ~= "}" do
        local key_pos = p_tokpos
        local left = nil
        if p_tok == "id" or KEYWORDS[p_lit] then
            left = Node({"id", key_pos, #p_lit, p_lit, false, false})
            scan()
        elseif p_tok == "[" then
            scan()
            local expr = parse_expr()
            skip("]")
            left = Node({"index", key_pos, p_endpos - key_pos, expr})
        else
            left = parse_expr()
        end
        expect(":")
        scan()
        local right = parse_expr()
        list[#list + 1] = Node({"pair", pos, p_endpos - pos, left, right})
        if p_tok ~= "," then
            break
        end
        scan()
    end
    skip("}")
    return Node({"table", pos, p_endpos - pos, list})
end
parse_list = function()
    local pos = p_tokpos
    local list = List({})
    skip("[")
    while p_tok ~= "]" do
        list[#list + 1] = parse_expr()
        if p_tok ~= "," then
            break
        end
        scan()
    end
    skip("]")
    return Node({"list", pos, p_endpos - pos, list})
end
local parse_func = nil
local function parse_operand()
    local node = p_left
    if node then
        p_left = false
        return node
    end
    local case = p_tok
    if case == "str" then
        local pos, len, val = p_tokpos, #p_lit, p_val
        scan()
        local tail = parse_tail()
        return Node({"value", pos, len, val, tail})
    elseif case == "id" then
        return parse_id(true)
    elseif case == "(" then
        return parse_paren()
    elseif case == "{" then
        return parse_table()
    elseif case == "[" then
        return parse_list()
    elseif case == "func" then
        return parse_func(true)
    elseif case == "..." then
        local node = Node({"vararg", p_tokpos, 3})
        scan()
        return node
    else
        if LITERALS[p_tok] then
            local node = Node({"value", p_tokpos, #p_lit, p_val})
            scan()
            return node
        end
        errorf("expected operand, found '%s'", p_tok)
    end
end
local function parse_pow()
    local pos = p_tokpos
    local left = parse_operand()
    while p_tok == "^" do
        scan()
        local right = parse_operand()
        left = Node({"binop", pos, p_endpos - pos, left, "^", right})
    end
    return left
end
local function parse_unary()
    local pos = p_tokpos
    local expr = nil
    if not p_left and UNR_OPS[p_tok] then
        local op = p_tok
        scan()
        local right = parse_pow()
        expr = Node({"unop", pos, p_endpos - pos, op, right})
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
        left = Node({"binop", pos, p_endpos - pos, left, op, right})
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
        left = Node({"binop", pos, p_endpos - pos, left, op, right})
    end
    return left
end
local function parse_cat()
    local pos = p_tokpos
    local left = parse_add()
    while p_tok == ".." do
        scan()
        local right = parse_add()
        left = Node({"binop", pos, p_endpos - pos, left, "..", right})
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
        left = Node({"binop", pos, p_endpos - pos, left, op, right})
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
        left = Node({"binop", pos, p_endpos - pos, left, op, right})
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
        left = Node({"binop", pos, p_endpos - pos, left, op, right})
    end
    return left
end
local parse_block, parse_body = nil, nil
local function check_new_id(check_exist, name, tail, args)
    if check_exist then
        local v = find_var(name)
        if v then
            if v[7] then
                errorf("shadowing of constant is prohibited, you need to change the name '%s'", name)
            else
                errorf("shadowing of variable is prohibited, you need to change the name '%s'", name)
            end
        end
    end
    if RESERVED[name] then
        errorf("name '%s' is reserved", name)
    end
    if tail then
        errorf("unexpected tail after '%s'", name)
    end
    if args then
        errorf("unexpected args after '%s'", name)
    end
end
local function parse_set_or_call()
    local pos = p_tokpos
    local name = p_lit
    local id, call = parse_id()
    if call then
        if not find_var(name) then
            errorf("undeclared variable '%s'", name)
        end
        return Node({"call", pos, p_endpos - pos, id})
    end
    if p_tok == "+=" then
        if not find_var(name) then
            errorf("undeclared variable '%s'", name)
        end
        scan()
        local expr = parse_add()
        return Node({"inc", pos, p_endpos - pos, id, expr})
    end
    if p_tok == "-=" then
        if not find_var(name) then
            errorf("undeclared variable '%s'", name)
        end
        scan()
        local expr = parse_add()
        return Node({"dec", pos, p_endpos - pos, id, expr})
    end
    local left = List({id})
    while p_tok == "," do
        scan()
        expect("id")
        id, call = parse_id()
        if call then
            errorf("unexpected call")
        end
        left[#left + 1] = id
    end
    if p_tok == "=" then
        for _, _id in ipairs(left) do
            name = _id[4]
            local v = find_var(name)
            if not v then
                errorf("undeclared variable '%s'", name)
            end
            if v[7] then
                errorf("cannot assign to '%s' (declared const)", name)
            end
        end
        scan()
        local right = List({})
        while true do
            right[#right + 1] = parse_expr()
            if p_tok ~= "," then
                break
            end
            scan()
        end
        return Node({"set", pos, p_endpos - pos, left, right})
    end
    if p_tok == ":=" then
        local allow = false
        for _, _id in ipairs(left) do
            name = _id[4]
            check_new_id(false, name, _id[5], _id[6])
            local v, level = find_var(name)
            if not v then
                allow = true
            elseif v[7] then
                errorf("shadowing of constant is prohibited, you need to change the name '%s'", name)
            elseif level ~= p_level then
                errorf("shadowing of variable is prohibited, you need to change the name '%s'", name)
            end
        end
        if not allow then
            errorf("no new variables on left side of ':='")
        end
        scan()
        local right = List({})
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
        return Node({"let", pos, p_endpos - pos, left, right})
    end
    expect("=")
end
local parse_if = nil
local function continue_parse_if(pos, left, right)
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
    return Node({"if", pos, p_endpos - pos, expr, body, else_body, left, right})
end
parse_if = function()
    local pos = p_tokpos
    skip("if")
    if p_tok ~= "id" then
        return continue_parse_if(pos)
    end
    local id = parse_id()
    local id_name = id[4]
    local vars = nil
    local left = nil
    local case = p_tok
    if case == ":=" then
        check_new_id(true, id_name, id[5], id[6])
        vars = {
            [id_name] = id;
        }
        left = List({id})
        scan()
    elseif case == "," then
        check_new_id(true, id_name, id[5], id[6])
        vars = {
            [id_name] = id;
        }
        left = List({id})
        while p_tok == "," do
            scan()
            expect("id")
            local name = p_lit
            check_new_id(true, name)
            local next_id = Node({"id", p_tokpos, #name, name, false, false})
            left[#left + 1] = next_id
            if vars[name] then
                errorf("re-declaring variable '%s'", name)
            end
            vars[name] = next_id
            scan()
        end
        skip(":=")
    else
        if not find_var(id_name) then
            errorf("undeclared variable '%s'", id_name)
        end
        p_left = id
        return continue_parse_if(pos)
    end
    local right = List({})
    while true do
        right[#right + 1] = parse_expr()
        if p_tok ~= "," then
            break
        end
        scan()
    end
    skip(";")
    open_scope(vars)
    local node = continue_parse_if(pos, left, right)
    close_scope()
    return node
end
local function parse_switch()
    local pos = p_tokpos
    skip("switch")
    local expr = false
    local cases = List({})
    if p_tok == "{" then
        scan()
        while p_tok == "case" do
            scan()
            local case_expr = parse_expr()
            skip(":")
            local body = parse_body()
            cases[#cases + 1] = Node({"case", pos, p_endpos - pos, false, case_expr, body})
        end
    else
        expr = parse_expr()
        skip("{")
        expect("case")
        while p_tok == "case" do
            scan()
            local list = List({})
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
            cases[#cases + 1] = Node({"case", pos, p_endpos - pos, list, case_expr, body})
        end
    end
    local _default = false
    if p_tok == "default" then
        scan()
        skip(":")
        _default = parse_body()
    end
    skip("}")
    return Node({"switch", pos, p_endpos - pos, expr, cases, _default})
end
local function parse_for()
    local pos = p_tokpos
    p_looplevel = p_looplevel + 1
    skip("for")
    if p_tok == "{" then
        local body = parse_block(nil, true)
        return Node({"for", pos, p_endpos - pos, false, body})
    end
    if p_tok ~= "id" then
        local expr = parse_expr()
        local body = parse_block(nil, true)
        return Node({"for", pos, p_endpos - pos, expr, body})
    end
    local id = parse_id()
    local id_name = id[4]
    local ids = nil
    local vars = nil
    local case = p_tok
    if case == ":=" then
        check_new_id(true, id_name, id[5], id[6])
        vars = {
            [id_name] = id;
        }
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
        return Node({"for_to", pos, p_endpos - pos, id, from, to, by or false, body})
    elseif case == "," then
        check_new_id(true, id_name, id[5], id[6])
        vars = {
            [id_name] = id;
        }
        ids = List({id})
        while p_tok == "," do
            scan()
            expect("id")
            local name = p_lit
            check_new_id(true, name)
            local next_id = Node({"id", p_tokpos, #name, name, false, false})
            ids[#ids + 1] = next_id
            if vars[name] then
                errorf("re-declaring variable '%s'", name)
            end
            vars[name] = next_id
            scan()
        end
        skip("in")
    elseif case == "in" then
        check_new_id(true, id_name, id[5], id[6])
        vars = {
            [id_name] = id;
        }
        ids = List({id})
        scan()
    else
        p_left = id
        local expr = parse_expr()
        if not find_var(id_name) then
            errorf("undeclared variable '%s'", id_name)
        end
        local body = parse_block(nil, true)
        return Node({"for", pos, p_endpos - pos, expr, body})
    end
    local ins = List({})
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
    return Node({"for_in", pos, p_endpos - pos, ids, ins, body})
end
local function parse_return()
    local pos = p_tokpos
    skip("return")
    local list = List({})
    if p_tok ~= "}" then
        while true do
            list[#list + 1] = parse_expr()
            if p_tok ~= "," then
                break
            end
            scan()
        end
    end
    if p_tok and p_tok ~= "case" and p_tok ~= "default" then
        expect("}")
    end
    return Node({"return", pos, p_endpos - pos, list})
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
    return Node({"break", pos, 5, expr})
end
local function parse_continue()
    local pos = p_tokpos
    assert(p_looplevel > 0, "no loop to continue")
    skip("continue")
    expect("}")
    p_continue[p_looplevel] = true
    return Node({"continue", pos, 8})
end
local function parse_const_item()
    local name = p_lit
    if RESERVED[name] then
        errorf("name '%s' is reserved", name)
    end
    local v = find_var(name)
    if v then
        if v[7] then
            errorf("shadowing of constant is prohibited, you need to change the name '%s'", name)
        else
            errorf("shadowing of variable is prohibited, you need to change the name '%s'", name)
        end
    end
    local id = Node({"id", p_tokpos, #name, name, false, false, nil})
    scan()
    skip("=")
    if not LITERALS[p_tok] then
        errorf("expected value")
    end
    id[7] = Node({"value", p_tokpos, #p_lit, p_val})
    p_vars[name] = id
    scan()
    return id
end
local function parse_const()
    local pos = p_tokpos
    skip("const")
    if p_tok == "id" then
        local list = List({parse_const_item()})
        return Node({"const", pos, p_endpos - pos, list})
    end
    if p_tok == "(" then
        scan()
        local list = List({})
        while p_tok == "id" do
            list[#list + 1] = parse_const_item()
            if p_tok == ";" then
                scan()
            end
        end
        skip(")")
        return Node({"const", pos, p_endpos - pos, list})
    end
    expect("id")
end
local function parse_params()
    local pos = p_tokpos
    skip("(")
    local list = List({})
    while true do
        if p_tok == "..." then
            local id = Node({"param", p_tokpos, 3, "..."})
            list[#list + 1] = id
            p_vars["..."] = id
            scan()
            break
        end
        if p_tok ~= "id" then
            break
        end
        local id = Node({"param", p_tokpos, #p_lit, p_lit})
        list[#list + 1] = id
        if p_vars[p_lit] then
            errorf("parameter '%s' is already declared", p_lit)
        end
        p_vars[p_lit] = id
        scan()
        if p_tok ~= "," then
            break
        end
        scan()
    end
    skip(")")
    return Node({"params", pos, p_endpos - pos, list})
end
parse_func = function(lambda)
    local pos = p_tokpos
    local name = false
    local receiver = false
    local vars = nil
    skip("func")
    if not lambda then
        if p_tok == "(" then
            local receiver_pos = p_tokpos
            scan()
            expect("id")
            local receiver_name = p_lit
            scan()
            expect("id")
            local receiver_type = p_lit
            local id = find_var(receiver_type)
            if not id then
                errorf("undeclared variable '%s'", receiver_type)
            end
            if id[7] then
                errorf("'%s' is a constant", receiver_type)
            end
            scan()
            skip(")")
            receiver = Node({"receiver", receiver_pos, p_endpos - receiver_pos, receiver_name, receiver_type})
            vars = {
                ["self"] = Node({"id", 0, 0, "self", false, false});
                [receiver_name] = Node({"id", 0, 0, "receiver_name", false, false});
            }
        end
        expect("id")
        name = p_lit
        scan()
        p_vars[name] = {}
    end
    open_scope(vars)
    local params = parse_params()
    local body = parse_block()
    close_scope()
    local node = Node({"func", pos, p_endpos - pos, name, params, body, receiver})
    if not lambda then
        p_vars[name] = node
    end
    return node
end
local function parse_statement()
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
        return Node({"nop", p_curpos, 1})
    end
    return nil
end
parse_body = function(vars)
    open_scope(vars)
    local body = List({})
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
        body[#body + 1] = Node({"label", 0, 0, "continue"})
        p_continue[p_looplevel] = false
        p_looplevel = p_looplevel - 1
    end
    return Node({"block", pos, p_endpos - pos, body})
end
local function parse_module(src, path, vars)
    p_path = path
    p_src = src
    p_len = #src
    p_line = 1
    p_curpos = 0
    p_tokpos = 0
    p_endpos = 0
    p_chr = nil
    p_tok = nil
    p_lit = ""
    p_val = nil
    p_comments = List({})
    p_vars = {
        _G = {"id", 0, 0, "_G", false, false};
    }
    p_level = 1
    p_scope = List({p_vars})
    p_continue = List({})
    p_looplevel = 0
    p_curpos = p_curpos + 1;
    p_chr = string_byte(p_src, p_curpos)
    scan()
    local pos = p_tokpos
    local body = parse_body(vars)
    local module = Node({"module", pos, p_endpos - pos, body, p_comments})
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
local function emit(s)
    v_res[#v_res + 1] = s
end
local emit_expr = nil
local function emit_field(node)
    local name = node[5]
    local args = node[4]
    if args then
        if node[6] then
            emit(":" .. name)
        else
            if KEYWORDS[name] or RESERVED[name] then
                emit("[\"" .. name .. "\"]")
            else
                emit("." .. name)
            end
        end
        emit("(")
        if #args > 0 then
            for _, v in ipairs(args) do
                emit_expr(v)
                emit(", ")
            end
            v_res[#v_res] = ""
        end
        emit(")")
    else
        if KEYWORDS[name] or RESERVED[name] then
            emit("[\"" .. name .. "\"]")
        else
            emit("." .. name)
        end
    end
end
local function emit_index(node)
    emit("[")
    emit_expr(node[4])
    emit("]")
end
local function emit_value(node)
    local v = node[4]
    if type(v) == "string" then
        local tail = node[5]
        if tail then
            emit("(\"" .. v .. "\")")
            for _, _v in ipairs(tail) do
                if _v[1] == "field" then
                    emit_field(_v)
                else
                    emit_index(_v)
                end
            end
        else
            emit("\"" .. v .. "\"")
        end
    else
        emit(tostring(v))
    end
end
local function emit_id(node)
    local _const = node[7]
    if _const and _const[1] == "value" then
        emit_value(_const)
        return
    end
    emit(node[4])
    local args = node[6]
    if args then
        emit("(")
        if #args > 0 then
            for _, v in ipairs(args) do
                emit_expr(v)
                emit(", ")
            end
            v_res[#v_res] = ""
        end
        emit(")")
    end
    local tail = node[5]
    if tail then
        for _, v in ipairs(tail) do
            if v[1] == "field" then
                emit_field(v)
            else
                emit_index(v)
            end
        end
    end
end
local function emit_pair(node)
    local key = node[4]
    local case = key[1]
    if (case == "id") and (not key[5] and not key[6]) then
        local name = key[4]
        if KEYWORDS[name] or RESERVED[name] then
            emit("[\"")
            emit_expr(key)
            emit("\"]")
        else
            emit_expr(key)
        end
    elseif case == "index" then
        emit_index(key)
    else
        emit("[")
        emit_expr(key)
        emit("]")
    end
    emit(" = ")
    emit_expr(node[5])
end
local function emit_table(node)
    if #node[4] == 0 then
        emit("{}")
        return
    end
    emit("{\n")
    v_level = v_level + 1
    for _, v in ipairs(node[4]) do
        emit(space())
        if v[1] == "pair" then
            emit_pair(v)
            emit(";\n")
        else
            emit_expr(v)
            emit(",\n")
        end
    end
    v_level = v_level - 1
    emit(space() .. "}")
end
local function emit_list(node)
    emit("{")
    local list = node[4]
    if #list > 0 then
        for _, v in ipairs(list) do
            emit_expr(v)
            emit(", ")
        end
        v_res[#v_res] = ""
    end
    emit("}")
end
local function emit_paren(node)
    emit("(")
    emit_expr(node[4])
    emit(")")
end
local function emit_unop(node)
    local op = LUA_OPS[node[4]]
    if op ~= "+" then
        emit(op)
    end
    emit_expr(node[5])
end
local function emit_binop(node)
    emit_expr(node[4])
    emit(" " .. LUA_OPS[node[5]] .. " ")
    emit_expr(node[6])
end
local emit_func = nil
emit_expr = function(node)
    local case = node[1]
    if case == "id" then
        emit_id(node)
    elseif case == "binop" then
        emit_binop(node)
    elseif case == "unop" then
        emit_unop(node)
    elseif case == "paren" then
        emit_paren(node)
    elseif case == "value" then
        emit_value(node)
    elseif case == "table" then
        emit_table(node)
    elseif case == "list" then
        emit_list(node)
    elseif case == "func" then
        emit_func(node, true)
    elseif case == "vararg" then
        emit("...")
    else
        errorf("unknown node type: '%s'", node[1])
    end
end
local emit_stmt = nil
local function emit_body(node, skip_break)
    v_level = v_level + 1
    for _, v in ipairs(node) do
        emit_stmt(v, skip_break)
    end
    v_level = v_level - 1
end
local function emit_block(node, skip_break)
    emit_body(node[4], skip_break)
end
local function emit_call(node)
    emit(space())
    emit_id(node[4])
    emit("\n")
end
local function emit_set(node)
    emit(space())
    for _, v in ipairs(node[4]) do
        emit_id(v)
        emit(", ")
    end
    v_res[#v_res] = " = "
    for _, v in ipairs(node[5]) do
        emit_expr(v)
        emit(", ")
    end
    v_res[#v_res] = "\n"
end
local function emit_inc(node)
    emit(space())
    emit_id(node[4])
    emit(" = ")
    emit_id(node[4])
    emit(" + ")
    emit_expr(node[5])
    emit("\n")
end
local function emit_dec(node)
    emit(space())
    emit_id(node[4])
    emit(" = ")
    emit_id(node[4])
    emit(" - ")
    emit_expr(node[5])
    emit("\n")
end
local function emit_let(node)
    emit(space() .. "local ")
    for _, v in ipairs(node[4]) do
        emit_id(v)
        emit(", ")
    end
    v_res[#v_res] = " = "
    for _, v in ipairs(node[5]) do
        emit_expr(v)
        emit(", ")
    end
    v_res[#v_res] = "\n"
end
local function emit_if(node)
    local left = node[7]
    if left then
        local right = node[8]
        emit(space() .. "local ")
        for _, v in ipairs(left) do
            emit(v[4])
            emit(", ")
        end
        v_res[#v_res] = " = "
        for _, v in ipairs(right) do
            emit_expr(v)
            emit(", ")
        end
        v_res[#v_res] = "\n"
    end
    emit(space() .. "if ")
    emit_expr(node[4])
    emit(" then\n")
    emit_block(node[5])
    local _else = node[6]
    while _else do
        emit(space() .. "else")
        if _else[1] == "if" then
            emit("if ")
            emit_expr(_else[4])
            emit(" then\n")
            emit_block(_else[5])
            _else = _else[6]
        else
            emit("\n")
            emit_block(_else)
            _else = nil
        end
    end
    emit(space() .. "end\n")
end
local function emit_switch(node)
    local expr = node[4]
    if expr then
        emit(space() .. "local case = ")
        emit_expr(expr)
        emit("\n" .. space())
        for _, _case in ipairs(node[5]) do
            emit("if ")
            local case_exp = _case[5]
            if case_exp then
                emit("(")
            end
            for _, item in ipairs(_case[4]) do
                emit("case == ")
                if item[1] == "binop" then
                    emit("(")
                    emit_expr(item)
                    emit(")")
                else
                    emit_expr(item)
                end
                emit(" or ")
            end
            if case_exp then
                v_res[#v_res] = ") and ("
                emit_expr(case_exp)
                emit(")")
            else
                v_res[#v_res] = ""
            end
            emit(" then\n")
            emit_body(_case[6])
            emit(space() .. "else")
        end
        local _default = node[6]
        if _default then
            emit("\n")
            emit_body(_default)
        else
            v_res[#v_res] = ""
        end
        emit(space() .. "end\n")
    else
        emit(space())
        for _, _case in ipairs(node[5]) do
            emit("if ")
            emit_expr(_case[5])
            emit(" then\n")
            emit_body(_case[6])
            emit(space() .. "else")
        end
        local _default = node[6]
        if _default then
            emit("\n")
            emit_body(_default)
        else
            v_res[#v_res] = ""
        end
        emit(space() .. "end\n")
    end
end
local function emit_for(node)
    local expr, block = node[4], node[5]
    local body = block[4]
    if not expr and #body > 0 then
        local last = body[#body]
        if last[1] == "break" then
            local break_expr = last[4]
            if break_expr then
                emit(space() .. "repeat\n")
                emit_block(block, true)
                emit(space() .. "until ")
                emit_expr(break_expr)
                emit("\n")
                return
            end
        end
    end
    emit(space() .. "while ")
    if expr then
        emit_expr(expr)
    else
        emit("true")
    end
    emit(" do\n")
    emit_block(block)
    emit(space() .. "end\n")
end
local function emit_for_to(node)
    emit(space() .. "for ")
    emit_id(node[4])
    emit(" = ")
    emit_expr(node[5])
    emit(", ")
    emit_expr(node[6])
    local step = node[7]
    if step then
        emit(", ")
        emit_expr(step)
    end
    emit(" do\n")
    emit_block(node[8])
    emit(space() .. "end\n")
end
local function emit_for_in(node)
    emit(space() .. "for ")
    for _, v in ipairs(node[4]) do
        emit_id(v)
        emit(", ")
    end
    v_res[#v_res] = " in "
    for _, v in ipairs(node[5]) do
        emit_expr(v)
        emit(", ")
    end
    v_res[#v_res] = " do\n"
    emit_block(node[6])
    emit(space() .. "end\n")
end
local function emit_return(node)
    emit(space() .. "return")
    local list = node[4]
    if list then
        emit(" ")
        for _, v in ipairs(list) do
            emit_expr(v)
            emit(", ")
        end
        v_res[#v_res] = ""
    end
    emit("\n")
end
local function emit_break(node)
    local expr = node[4]
    if expr then
        emit(space() .. "if ")
        emit_expr(expr)
        emit(" then\n")
        v_level = v_level + 1
        emit(space() .. "break\n")
        v_level = v_level - 1
        emit(space() .. "end\n")
    else
        emit(space() .. "break\n")
    end
end
local function emit_continue(node)
    emit(space() .. "goto continue\n")
end
local function emit_label(node)
    emit(space() .. "::" .. node[4] .. "::\n")
end
local function emit_params(node)
    local t = {}
    for _, v in ipairs(node[4]) do
        t[#t + 1] = v[4]
    end
    emit("(" .. table_concat(t, ", ") .. ")\n")
end
emit_func = function(node, lambda)
    local receiver = node[7]
    if lambda then
        emit("function")
        emit_params(node[5])
    elseif receiver then
        emit(space() .. "function " .. receiver[5] .. ":" .. node[4])
        emit_params(node[5])
        v_level = v_level + 1
        emit(space() .. "local " .. receiver[4] .. " = self\n")
        v_level = v_level - 1
    else
        emit(space() .. "local function " .. node[4])
        emit_params(node[5])
    end
    emit_block(node[6])
    emit(space() .. "end")
    if not lambda then
        emit("\n")
    end
end
local function emit_nop(node)
    v_res[#v_res] = ";\n"
end
emit_stmt = function(node, skip_break)
    local case = node[1]
    if case == "call" then
        emit_call(node)
    elseif case == "set" then
        emit_set(node)
    elseif case == "inc" then
        emit_inc(node)
    elseif case == "dec" then
        emit_dec(node)
    elseif case == "let" then
        emit_let(node)
    elseif case == "if" then
        emit_if(node)
    elseif case == "switch" then
        emit_switch(node)
    elseif case == "block" then
        emit(space() .. "do\n")
        emit_block(node)
        emit(space() .. "end\n")
    elseif case == "for" then
        emit_for(node)
    elseif case == "for_to" then
        emit_for_to(node)
    elseif case == "for_in" then
        emit_for_in(node)
    elseif case == "return" then
        emit_return(node)
    elseif case == "break" then
        if not skip_break then
            emit_break(node)
        end
    elseif case == "continue" then
        emit_continue(node)
    elseif case == "label" then
        emit_label(node)
    elseif case == "func" then
        emit_func(node)
    elseif case == "nop" then
        emit_nop(node)
    elseif case == "const" then
    else
        errorf("unknown node type: %s", node[1])
    end
end
local function emit_module(node, level)
    v_res = {}
    v_level = level or 0
    for _, v in ipairs(node[4]) do
        emit_stmt(v)
    end
    return table_concat(v_res)
end
do
    local fn = _G.arg[1]
    if fn then
        local os, io, pcall, print, load = _G.os, _G.io, _G.pcall, _G.print, _G.loadstring or _G.load
        local src = io.open(fn, "r"):read("*a")
        local r, m = pcall(parse_module, src, fn)
        if r then
            local res = emit_module(m, 0)
            local out = _G.arg[2]
            if out then
                local f = io.open(out, "w")
                f:write(res)
                f:close()
                print(os.clock())
            else
                local f, err = load(res)
                if err then
                    print(err)
                    os.exit(1)
                end
                f()
            end
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
    emit_module = emit_module;
}
