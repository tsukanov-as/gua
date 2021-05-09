local gua = require "gua"
local f = io.open("gua.gua", "r")
local src = f:read("*a")
f:close()

local parse_module = gua.parse_module

for _ = 1, 1e3 do
    parse_module(src)
end

print(os.clock())