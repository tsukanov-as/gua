# gua
go+lua=gua (experimental programming language)

[![test](https://github.com/tsukanov-as/gua/actions/workflows/build.yml/badge.svg)](https://github.com/tsukanov-as/gua/actions/workflows/build.yml)

transpiler from gua:
```go
std := _G
setmt := std.setmetatable

ColorMeta := {
    __tostring: func(self) {
        return std::string::format("[%d, %d, %d]", self[1], self[2], self[3])
    }
}

Point := {
    x: 0,
    y: 0,
    c: setmt([0, 0, 0], ColorMeta),
}

func (p Point) move(dx, dy) {
    p.x += dx
    p.y += dy
}

PointMeta := {
    __index: Point,
    __tostring: func(self) {
        return std::string::format("x: %d, y: %d, c: %s", self.x, self.y, std::tostring(self.c))
    }
}

p := setmt({}, PointMeta)
p.c[1] = 255

for p.x < 20 {
    p.move(1, 0)
}

std::print(p)
```
to lua:
```lua
local std = _G
local setmt = std.setmetatable
local ColorMeta = {
    __tostring = function(self)
        return std.string.format("[%d, %d, %d]", self[1], self[2], self[3])
    end;
}
local Point = {
    x = 0;
    y = 0;
    c = setmt({0, 0, 0}, ColorMeta);
}
function Point:move(dx, dy)
    local p = self
    p.x = p.x + dx
    p.y = p.y + dy
end
local PointMeta = {
    __index = Point;
    __tostring = function(self)
        return std.string.format("x: %d, y: %d, c: %s", self.x, self.y, std.tostring(self.c))
    end;
}
local p = setmt({}, PointMeta)
p.c[1] = 255
while p.x < 20 do
    p:move(1, 0)
end
std.print(p)
```
for more examples, see [test.lua](./test.lua)