# gua
go+lua=gua (experimental programming language)

[![test](https://github.com/tsukanov-as/gua/actions/workflows/build.yml/badge.svg)](https://github.com/tsukanov-as/gua/actions/workflows/build.yml)

## WAT

### transpiler from gua:
<details>
<summary>Example</summary>

```go
std := _G
setmt := std.setmetatable

Type := {}
func (t Type) __call(x) {
    x = x || {}
    x.__index = x
    return setmt(x, t)
}
setmt(Type, Type)

Color := Type([0, 0, 0])

func (c Color) __tostring() {
    return std::string::format("[%d, %d, %d]", c[1], c[2], c[3])
}

Point := Type({
    x: 0,
    y: 0,
    c: nil,
})

func (p Point) move(dx, dy) {
    p.x += dx
    p.y += dy
}

func (p Point) __tostring() {
    return std::string::format("x: %d, y: %d, c: %s", p.x, p.y, std::tostring(p.c))
}

p := Point({
    x: 10,
    y: 10,
    c: Color(),
})

p.c[1] = 255

for p.x < 20 {
    p.move(1, 0)
}

std::print(p)
```

</details>

### to lua:

<details>
<summary>Example</summary>

```lua
local std = _G
local setmt = std.setmetatable
local Type = {}
function Type:__call(x)
    local t = self
    x = x or {}
    x.__index = x
    return setmt(x, t)
end
setmt(Type, Type)
local Color = Type({0, 0, 0})
function Color:__tostring()
    local c = self
    return std.string.format("[%d, %d, %d]", c[1], c[2], c[3])
end
local Point = Type({
    x = 0;
    y = 0;
    c = nil;
})
function Point:move(dx, dy)
    local p = self
    p.x = p.x + dx
    p.y = p.y + dy
end
function Point:__tostring()
    local p = self
    return std.string.format("x: %d, y: %d, c: %s", p.x, p.y, std.tostring(p.c))
end
local p = Point({
    x = 10;
    y = 10;
    c = Color();
})
p.c[1] = 255
while p.x < 20 do
    p:move(1, 0)
end
std.print(p)
```

</details>

## HOWTO

* run gua:
```sh
$ lua gua.lua test.gua
```

* transpile to lua:
```sh
$ lua gua.lua test.gua test.lua
```

## FEATURES

* go-style syntax
* go-style if
* go/lua-style `for`
* js-style map/list literals
* `switch`
* `const`
* `continue`
* global variables are prohibited except for `_G`
* shadowing of variable is prohibited
* `.` instead of `:` for method calls with `self`
* `::` instead of `.` for method calls without `self` (like namespaces)
* only `"` for strings
