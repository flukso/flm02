#! /usr/bin/env lua

require 'socket'

local fd = assert(io.open('/dev/ttyUSB0', 'w'))

for i = tonumber(arg[1]), tonumber(arg[2]) do
  local data = 'pls ' .. arg[3] .. ':' .. string.sub(string.rep('0', 10) .. i, -10, -1) .. '\n'
  fd:write(data)
  socket.select(nil, nil, 0.001)

end

fd:close()
