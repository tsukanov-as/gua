local gua = require "gua"

local function test(name)
    local vars = {
        print = {"id", 0, 0, "print", false, false};
        pairs = {"id", 0, 0, "pairs", false, false};
        next =  {"id", 0, 0, "next", false, false};
        mock =  {"id", 0, 0, "mock", false, false};
    }
    return function(src)
        local r, m = pcall(gua.parse_module, src, vars)
        if not r then
            error("failed: " .. name .. "\nerror: " .. m)
        end
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

test "func_03"
[[
    x := {
        field: 1
    }
    func x.foo(p1, p2) {
        self.bar(p2, p1)
    }
]]
[[
    local x = {
        field = 1;
    }
    function x:foo(p1, p2)
        self:bar(p2, p1)
    end
]]

test "func_04"
[[
    func foo(p1, p2, ...) {
        print(p2, p1, ...)
    }
]]
[[
    local function foo(p1, p2, ...)
        print(p2, p1, ...)
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

test "for_07"
[[
    y := 0
    for y ^ 2 {
        print(y)
    }
]]
[[
    local y = 0
    while y ^ 2 do
        print(y)
    end
]]

test "for_08"
[[
    for i := 1, 10 {
        if i > 5 {
            continue
        }
        print("i = ", i)
        for j := 11, 20 {
            if j > 15 {
                continue
            }
            print("j = ", j)
        }
    }
]]
[[
    for i = 1, 10 do
        if i > 5 then
            goto continue
        end
        print("i = ", i)
        for j = 11, 20 do
            if j > 15 then
                goto continue
            end
            print("j = ", j)
            ::continue::
        end
        ::continue::
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
        one: 1,
        tow: 2,
        list: [1, 2, 3],
    }
]]
[[
    local a = {
        one = 1;
        tow = 2;
        list = {1, 2, 3};
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
    y := a.push
]]
[[
    local a = {}
    a:push(1)
    a.push = nil
    local x = a:push(1)
    local y = a.push
]]

test "exp_07"
[[
    a := []
    a::push(1)
    a::push = nil
    x := a::push(1)
    y := a::push
]]
[[
    local a = {}
    a.push(1)
    a.push = nil
    local x = a.push(1)
    local y = a.push
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
    local a = "abc" .. "def" .. 123 + 1
]]

test "exp_13"
[[
    x1 := 1e3
    x2 := 1e-3
    x3 := 1.e3
    x4 := 1.e-3
    x5 := 1.2e3
    x6 := 1.2e-3
]]
[[
    local x1 = 1000
    local x2 = 0.001
    local x3 = 1000
    local x4 = 0.001
    local x5 = 1200
    local x6 = 0.0012
]]

test "exp_14"
[[
    x1 := 0xAF
    x2 := 0x1F
    x3 := 0XABCDEF
    x4 := 0Xabcdef
    x5 := 0X23456789
    x6 := 0XFFFFFFFF
    x7 := 0b0001
    x8 := 0b0011
]]
[[
    local x1 = 175
    local x2 = 31
    local x3 = 11259375
    local x4 = 11259375
    local x5 = 591751049
    local x6 = 4294967295
    local x7 = 1
    local x8 = 3
]]

test "exp_15"
[[
    z := 1
    x := func(y) {
        return y + z
    }
]]
[[
    local z = 1
    local x = function(y)
        return y + z
    end
]]

test "exp_15"
[=[
    x := {
        name1: 1,
        "name2": 2,
        3: 4,
        true: 5,
        [next]: 6,
        [next()]: 7,
        [mock.field]: 8,
        [mock.field()]: 9,
        [[mock.field(1, 2)]]: 10,
    }
]=]
[[
    local x = {
        name1 = 1;
        ["name2"] = 2;
        [3] = 4;
        [true] = 5;
        [next] = 6;
        [next()] = 7;
        [mock.field] = 8;
        [mock:field()] = 9;
        [{mock:field(1, 2)}] = 10;
    }
]]

test "exp_16"
[[
    x := 'A'
    y := '''
]]
[[
    local x = 65
    local y = 39
]]

test "exp_17"
[[
    x := -1
    y := +1
]]
[[
    local x = -1
    local y = 1
]]

test "exp_18"
[[
    func foo(s) {
        print(s)
    }
    x := foo "hello"
]]
[[
    local function foo(s)
        print(s)
    end
    local x = foo("hello")
]]

test "exp_19"
[[
    y := {}
    func y.foo(s) {
        print(s)
    }
    x := y.foo "hello"
]]
[[
    local y = {
    }
    function y:foo(s)
        print(s)
    end
    local x = y:foo("hello")
]]

test "exp_20"
[[
    func Struct(t) {
        print(t)
    }
    x := Struct.{
        one: 1,
        two: 2,
    }
]]
[[
    local function Struct(t)
        print(t)
    end
    local x = Struct({
        one = 1;
        two = 2;
    })
]]

test "exp_21"
[[
    func List(t) {
        print(t)
    }
    x := List.[1, 2, 3]
]]
[[
    local function List(t)
        print(t)
    end
    local x = List({1, 2, 3})
]]

test "exp_22"
[[
    x := 1
    x, y := 1, 2
]]
[[
    local x = 1
    local x, y = 1, 2
]]

test "inc_01"
[[
    x := 0
    x += 2
]]
[[
    local x = 0
    x = x + 2
]]

test "dec_01"
[[
    x := 0
    x -= 2
]]
[[
    local x = 0
    x = x - 2
]]

print("OK.", os.clock())