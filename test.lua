local gua = require "gua"

local function test(name)
    local vars = {
        print = {"id", 0, 0, "print", false, false};
        pairs = {"id", 0, 0, "pairs", false, false};
        next =  {"id", 0, 0, "next", false, false};
    }
    return function(src)
        local r, m = pcall(gua.parse_module, src, vars)
        assert(r, m)
        return function(want)
            local res = gua.visit_module(m, 1)
            if res ~= want then
                print("failed: " .. name .. "\nres:\n" .. res .. "\nast:\n" .. tostring(m))
            end
        end
    end
end

test "let_01"
[[
    x := 1
]]
[[
    local x = 1
]]

test "let_02"
[[
    x, y := 1, 2
]]
[[
    local x, y = 1, 2
]]

test "let_03"
[[
    x := 1 + 2
]]
[[
    local x = 1 + 2
]]

test "set_01"
[[
    x := 1
    x = 2
]]
[[
    local x = 1
    x = 2
]]

test "set_02"
[[
    x := 1
    x = 1 + 2
]]
[[
    local x = 1
    x = 1 + 2
]]

test "func_01"
[[
    func foo(p1, p2) {
        x := 1
        p1 = 2
        p2 = 3
    }
]]
[[
    local function foo(p1, p2)
        local x = 1
        p1 = 2
        p2 = 3
    end
]]

test "func_02"
[[
    func foo(p1, p2,) {
        x := 1
        p1 = 2
        p2 = 3
    }
]]
[[
    local function foo(p1, p2)
        local x = 1
        p1 = 2
        p2 = 3
    end
]]

test "return_01"
[[
    func foo(p) {
        return
    }
]]
[[
    local function foo(p)
        return
    end
]]

test "return_02"
[[
    func foo(p) {
        return p
    }
]]
[[
    local function foo(p)
        return p
    end
]]

test "return_03"
[[
    func foo(p1, p2) {
        x := 1
        return p1, p2, x
    }
]]
[[
    local function foo(p1, p2)
        local x = 1
        return p1, p2, x
    end
]]

test "if_01"
[[
    x := 2
    if x > 1 {
        x = 3
    }
]]
[[
    local x = 2
    if x > 1 then
        x = 3
    end
]]

test "if_02"
[[
    x := 2
    if x > 1 {
        x = 3
    } else {
        x = 4
    }
]]
[[
    local x = 2
    if x > 1 then
        x = 3
    else
        x = 4
    end
]]

test "if_03"
[[
    x := 2
    if x > 1 {
        x = 3
    } else if x > 2 {
        x = 5
    } else if x > 3 {
        x = 6
    } else {
        x = 4
    }
]]
[[
    local x = 2
    if x > 1 then
        x = 3
    elseif x > 2 then
        x = 5
    elseif x > 3 then
        x = 6
    else
        x = 4
    end
]]

test "for_01"
[[
    x := 1
    for x < 10 {
        x = x + 1
    }
]]
[[
    local x = 1
    while x < 10 do
        x = x + 1
    end
]]

test "for_02"
[[
    x := 1
    for {
        x = x + 1
    }
]]
[[
    local x = 1
    while true do
        x = x + 1
    end
]]

test "for_03"
[[
    for x := 1, 10 {
        print(x)
    }
]]
[[
    for x = 1, 10 do
        print(x)
    end
]]

test "for_04"
[[
    for x := 1, 10, 2 {
        print(x)
    }
]]
[[
    for x = 1, 10, 2 do
        print(x)
    end
]]

test "for_05"
[[
    y := []
    for k, v in pairs(y) {
        print(k, v)
    }
]]
[[
    local y = {}
    for k, v in pairs(y) do
        print(k, v)
    end
]]

test "for_06"
[[
    y := []
    for k, v in next, y, nil {
        print(k, v)
    }
]]
[[
    local y = {}
    for k, v in next, y, nil do
        print(k, v)
    end
]]

test "exp_01"
[[
    x := 1 + 2 * 3 / 4 % 5
]]
[[
    local x = 1 + 2 * 3 / 4 % 5
]]

test "exp_02"
[[
    a, b, c := nil, nil, nil
    x := !a && b || c
]]
[[
    local a, b, c = nil, nil, nil
    local x = not a and b or c
]]

test "exp_03"
[[
    a, b, c := nil, nil, nil
    x := a > 1 && a >= 1 && b < 2 && b <= 2 && c == 3 && c != 3
]]
[[
    local a, b, c = nil, nil, nil
    local x = a > 1 and a >= 1 and b < 2 and b <= 2 and c == 3 and c ~= 3
]]

test "exp_04"
[[
    a := {
        one: 1;
        tow: 2;
        3,
        4,
    }
]]
[[
    local a = {
        one = 1;
        tow = 2;
        3,
        4,
    }
]]

test "exp_05"
[[
    a := [1, 2, 3]
]]
[[
    local a = {1, 2, 3}
]]

test "exp_06"
[[
    a := []
    a.push(1)
    a.push = nil
    x := a.push(1)
    x := a.push
]]
[[
    local a = {}
    a:push(1)
    a.push = nil
    local x = a:push(1)
    local x = a.push
]]

test "exp_07"
[[
    a := []
    a::push(1)
    a::push = nil
    x := a::push(1)
    x := a::push
]]
[[
    local a = {}
    a.push(1)
    a.push = nil
    local x = a.push(1)
    local x = a.push
]]

test "exp_08"
[[
    a := true && false && nil && 12.3 && "test"
]]
[[
    local a = true and false and nil and 12.3 and "test"
]]

test "exp_09"
[[
    a := (1 + 2) * 3
]]
[[
    local a = (1 + 2) * 3
]]

test "exp_10"
[[
    a := []
    a[#a + 1] = 1
]]
[[
    local a = {}
    a[#a + 1] = 1
]]

test "exp_11"
[[
    a := 1 + -2 ^ 3 * 4^5
]]
[[
    local a = 1 + -2 ^ 3 * 4 ^ 5
]]

test "exp_12"
[[
    a := "abc" .. "def" .. 123 + 1
]]
[[
    local a = "abc" .. "def" .. 123 + 2
]]

print("OK.", os.clock())