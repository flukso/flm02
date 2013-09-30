
local nixio 	= require "nixio"
local uci 	= require "luci.model.uci".cursor()
local mosq 	= require 'mosquitto'
local dbg       = require 'dbg'  

-- Mosquitto parameters
local MOSQ_ID = DEAMON
local MOSQ_CLN_SESSION = true
local MOSQ_HOST = 'localhost'
local MOSQ_PORT = 1883
local MOSQ_KEEPALIVE = 300
local MOSQ_TIMEOUT = 0
local MOSQ_MAX_PKTS = 10
local MOSQ_QOS = 0
local MOSQ_RETAIN = true 

local fifo = '/tmp/run/spid/uart/in'
local clean_packet = {}

function jeenode_packet(group_id, node_id, payload)                                         
  packet = {}
  
  -- First a Sync
  packet["0x2D"] = 0x2D
  
  -- TODO: Check whether group_id is between bounds (0 and 212 or something similar)
  packet["G"] = tonumber(group_id)  -- 1 byte
  
  -- Set the 3bit CDA from HDR                                                                     
  temp_header = nixio.bit.lshift(0, 7) -- Useless for now, but leave it in for clarity CTL flags (only 1 for ACK messages)
  temp_header = nixio.bit.bor(temp_header, nixio.bit.lshift(1, 6)) -- DST flags (1 for node, or 0 for a broadcast)       
  temp_header = nixio.bit.bor(temp_header, nixio.bit.lshift(0, 5)) -- ACK flags (1 if ACK is requested)                  

  -- Set the 5bit Destination or origin in the HDR                                                                        
  -- TODO: Check if node_id is between 0 and 30                                                                           
  temp_header = nixio.bit.bor(temp_header, node_id ) -- bit_field_NODEID                                                 
  packet["HDR"] = temp_header

  -- Set the Payload LEN (length) (1 byte)
  -- TODO: Check that the payload is x bytes and format it as such (1 byte for now)  
  packet["LEN"] = 1                                                                                     

  -- Get the Payload                                                                                                      
  packet["PAYLOAD"] = payload                                                                                  

  -- We don't need to do the CRC                                                                                          
  return packet
end

function jeenode_send(packet)
  print("Sending packet:")
  dbg.vardump(packet)

  print("Buffer:")
  buffer = tostring(packet["0x2D"]):char() .. tostring(packet["G"]):char() .. tostring(packet["HDR"]):char() .. tostring(packet["LEN"]):char() .. tostring(packet["PAYLOAD"]):char()
  -- Need to add a linefeed
  --buffer = buffer .. tostring(0x0a):char()
  --dbg.vardump(nixio.hexlify(buffer))

  -- Open a write to a fifo (made also read, so it won't block me)
  flags = nixio.open_flags("nonblock","rdwr")
  file = nixio.open(fifo, flags)
  assert(file, nixio.strerror(nixio.errno()))

  local nr_of_bytes_written = file:write(buffer)
  print("Wrote: " .. nr_of_bytes_written .. ' to fifo')
  assert(file, nixio.strerror(nixio.errno()))
  --file:sync() 
  
  file:close()
  --os.execute('echo -e "\\x2D\\xD4\\x4C\\x01\\x04" > /var/run/spid/uart/in')
  --os.exit(0)
end

-- Load the configuration data (a sensor/actuator == a sensor/actuator on a jeenode, e.g. temperature)
local params = {}
uci:foreach("flukso", "actuator", 
  function(s)
    if s["topic_in"] then
      if params[s["topic_in"]] == nil then 
        params[s["topic_in"]] = {}
      end
      table.insert(params[s["topic_in"]], s) -- add it to the other actuators for this topic
    end 
  end)
--dbg.vardump(params["/internal_light"])

-- Connect to the mqtt broker
mosq.init()
local mqtt = mosq.new(MOSQ_ID, MOSQ_CLN_SESSION)
print("Connecting to local MQTT broker")
while not mqtt:connect(MOSQ_HOST, MOSQ_PORT, MOSQ_KEEPALIVE) do
  print(".")
end
print("Connected to local MQTT broker")

local nr_message = 0
-- Setting the action to execute when a message comes in
mqtt:set_callback(mosq.ON_MESSAGE, function(mid, topic_in, message, qos, retain)
    print("MQTT received a message with topic " .. topic_in) 
    -- Lookup actuator configuration settings
    -- dbg.vardump(params[topic_in])
    for key,value in pairs(params[topic_in]) do
      group_id = params[topic_in][key]["group_id"]
      node_id = params[topic_in][key]["node_id"]
      payload = message:gsub("\[[%d]+\,([%d]+)\,\".*\"\]","%1")
           
      -- Sent MQTT payload to Jeenode Payload
      local packet = jeenode_packet(group_id, node_id, payload)
      jeenode_send(packet)
    end
  end
)


-- Subscribe to local MQTT topics
mqtt:subscribe("#", 0)    -- TODO: don't know what the "0" is about 

-- service the mosquitto loop
while true do 
  if not mqtt:loop(MOSQ_TIMEOUT, MOSQ_MAX_PKTS) then
    mqtt:reconnect()
  end
end

