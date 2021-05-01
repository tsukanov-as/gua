# gua
go+lua=gua (experimental programming language)

transpiler from gua:
```go
p := {
    x: 10,
    y: 10,
    color: [255, 0, 0],
}
func p.move(dx, dy) {
    self.x += dx
    self.y += dy
}
for p.x < 20 {
    p.move(1, 0)
}
```
to lua:
```lua
local p = {
    x = 10;
    y = 10;
    color = {255, 0, 0};
}
function p:move(dx, dy)
    self.x = self.x + dx
    self.y = self.y + dy
end
while p.x < 20 do
    p:move(1, 0)
end
```
for more examples, see [test.lua](./test.lua)