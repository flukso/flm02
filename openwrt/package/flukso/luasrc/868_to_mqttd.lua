
local nixio 	= require "nixio"
local fs 	= require "nixio.fs"
local uci 	= require "luci.model.uci".cursor()
local mosq 	= require 'mosquitto'
local dbg       = require 'dbg'  

function number_from_bitfield(payload, packet_cursor, bitfield, type)
  -- retain only certain bits (equal to 1 in bitfield) in payload to calculate the result
  local result_number = 0
  local bit_fields = {'0x01','0x02','0x04','0x08',
                      '0x10','0x20','0x40','0x80',
                      '0x0100','0x0200','0x0400','0x0800',
                      '0x1000','0x2000','0x4000','0x8000',
                      '0x00010000','0x00020000','0x00040000','0x00080000',
  		      '0x00100000','0x00200000','0x00400000','0x00800000',
  		      '0x01000000','0x02000000','0x04000000','0x08000000',
  		      '0x10000000','0x20000000','0x40000000','0x80000000'}
  local bit_power = 1
  if type == 'uint8' then
    max_bits = 8
    if not bitfield then
     bitfield = '0xff'
    end
    number = payload[packet_cursor]
  elseif type == 'uint16' then
    max_bits = 16
    if not bitfield then
     bitfield = '0xffff'
    end
    number = payload[packet_cursor] + 255 * payload[packet_cursor + 1]
  elseif type == 'uint32' then
    if not bitfield then
     bitfield = '0xffffffff'
    end
    max_bits = 32
    number = payload[packet_cursor] + 255 * payload[packet_cursor + 1] + 255 * 255 * payload[packet_cursor + 2] +255 * 255 * 255 * payload[packet_cursor + 3]
  end
  dbg.vardump(number)
  dbg.vardump(bitfield)
  dbg.vardump(type)
  for bit_pos = 1, max_bits do
    if nixio.bit.check(bitfield, bit_fields[bit_pos]) then
      if nixio.bit.check(number, bit_fields[bit_pos]) then
        result_number = result_number + 2^(bit_power-1)
      end
      bit_power = bit_power + 1
    end
  end
  return result_number 
end

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
 
local fifo = '/tmp/run/spid/uart/out'
local clean_packet = {}

-- Connect to the mqtt broker
mosq.init()
local mqtt = mosq.new(MOSQ_ID, MOSQ_CLN_SESSION)
mqtt:connect(MOSQ_HOST, MOSQ_PORT, MOSQ_KEEPALIVE)

-- Load the configuration data (a sensor == a sensor on a jeenode, e.g. temperature)
local params = {}
uci:foreach("flukso", "sensor", 
  function(s)
    if s["jeenode_id"] then
      if params[tonumber(s["jeenode_id"])] == nil then 
        params[tonumber(s["jeenode_id"])] = {}
      end
      table.insert(params[tonumber(s["jeenode_id"])], s) -- add it to the other sensors for the same jeenode
    end 
  end)
dbg.vardump(params)

-- Open non-blocking fifo file
local flags = nixio.open_flags("nonblock","rdonly")
local file = nixio.open(fifo, flags)
assert(file, nixio.strerror(nixio.errno()))

-- Flush the fifo, don't want the old packets / garbage
io.write ("Emptying fifo ")
while file:read(8192) do io.write(".") end 
io.write("\n")

while true do
  local poll_file = {fd = file, events=nixio.poll_flags('in'),revents=0}
  local poll = nixio.poll({poll_file}, 2000)
  if poll == 1 then 
   
    -- Set message receive_timestamp as receiving time of message
    -- TODO: Ask Flukso to add a microseconds lib, or cross-compile a C/C++ lib for this
    local receive_timestamp = os.time()
    if receive_timestamp < 1.377e9 then -- time should be reasonable, otherwise it's probably 1970 anyway 
      print("The flukso meter receive_timestamp is off, did it sync well?")
      os.exit(0)
    end
    print("\n****************************\nThe flukso meter receive_timestamp is: " .. receive_timestamp)
    
    -- First byte from FIFO is Group ID
    local bytes_read = file:read(4096)
    if bytes_read then
      print(bytes_read:len() .. " bytes read from fifo") 

      -- Get the Group ID
      clean_packet["G"] = string.sub(bytes_read,1,1):byte()
    
      -- Get the 3bit CDA from HDR
      local byte_read = string.sub(bytes_read,2,2):byte()
      clean_packet["HDR"] = {}
      local bit_flag_CTL = '0x80'  -- must be set to zero (only 1 for ACK messages)
      local bit_flag_DST = '0x40'  -- 1 if send to a node, 0 is a broadcast
      local bit_flag_ACK = '0x20'  -- 1 is ACK was requested
      clean_packet["HDR"]["CTL"] = nixio.bit.check(byte_read, bit_flag_CTL)
      clean_packet["HDR"]["DST"] = nixio.bit.check(byte_read, bit_flag_DST)
      clean_packet["HDR"]["ACK"] = nixio.bit.check(byte_read, bit_flag_ACK)
  
      -- Get the 5bit Destination or origin from HDR
      local bit_field_NODEID = '0x1f'
      clean_packet["HDR"]["NODEID"] = nixio.bit.band(byte_read, bit_field_NODEID)   
    
      -- Get the Payload LEN (length)
      clean_packet["LEN"] = string.sub(bytes_read,3,3):byte() 
      
      -- Get the Payload
      clean_packet["PAYLOAD"] = {bytes_read:byte(3 + 1, 3 + clean_packet["LEN"])}
      dbg.vardump(clean_packet["PAYLOAD"])
      
      -- TODO: Check the CRC
      clean_packet["CRC"] = {}
      clean_packet["CRC"][1] = string.sub(bytes_read, 4 + clean_packet["LEN"], 4 + clean_packet["LEN"]):byte() 
      clean_packet["CRC"][2] = string.sub(bytes_read, 5 + clean_packet["LEN"], 5 + clean_packet["LEN"]):byte()
    
      -- Calculate the values sent in the payload
      local sensor_values = {}
      
      if params[tonumber(clean_packet["HDR"]["NODEID"])] then -- makes sure a sending, but undefined jeenode_id doesn't crash this process
        local nr_sensors = #(params[tonumber(clean_packet["HDR"]["NODEID"])])
        for i = 1, nr_sensors do  -- for each sensor in the received jeenode packet
        
          -- Get sensor configuration from uci
          local sensor_config = params[tonumber(clean_packet["HDR"]["NODEID"])][i] 
          
          -- Calculate the values for this sensor
          -- TODO: Check whether lua reads this in correctly for large values (usigned probably mapped to signed)
          packet_cursor = tonumber(sensor_config["value_start_byte"])
          sensor_values[sensor_config["sensor_name"]] = number_from_bitfield(clean_packet["PAYLOAD"], packet_cursor, sensor_config["bit_field"], sensor_config["sensor_type"])
       
          -- Setup the mqtt packet contents
          local mqtt_topic = string.format(sensor_config["mqtt_topic"], sensor_config["id"])
          local mqtt_payload = string.format(sensor_config["mqtt_payload"], receive_timestamp, sensor_values[sensor_config["sensor_name"]], sensor_config["sensor_unit"])

          -- Check whether connection is ok, and send the mqtt packet
          if not mqtt:loop(MOSQ_TIMEOUT, MOSQ_MAX_PKTS) then       
            mqtt:reconnect() 
          end   
          mqtt:publish(mqtt_topic, mqtt_payload, MOSQ_QOS, MOSQ_RETAIN)
          print("Sent to MQTT: " .. mqtt_topic .. " : " .. mqtt_payload)  
        end
        
        -- print the packet contents, and values for debugging purposes 
        -- dbg.vardump(clean_packet)
        dbg.vardump(sensor_values)
        
      end
    end
  else 
    -- print("Poll was not 1")
  end
end

