#!/bin/sh

# Copyright (C) 2014 bart@flukso.net
# Add custom flukso.net bridge parameters to /etc/mosquitto/mosquitto.conf defaults

NOW=$(date)
CONFDIR=/var/run/mosquitto
CONFFILE=$CONFDIR/flukso.conf

CACERT=$(uci -q get flukso.daemon.cacert)
ADDRESS=$(uci -q get flukso.mqtt.bridge_address)
PORT=$(uci -q get flukso.mqtt.bridge_port)
KEEPALIVE=$(uci -q get flukso.mqtt.bridge_keepalive)
TIMEOUT=$(uci -q get flukso.mqtt.bridge_restart_timeout)
USERNAME=$(uci -q get system.@system[0].device)
PASSWORD=$(uci -q get system.@system[0].key)

mkdir -p $CONFDIR

echo "# Mosquitto conf file generated on $NOW by $0" > $CONFFILE
echo "connection flukso" >> $CONFFILE
echo "address $ADDRESS:$PORT" >> $CONFFILE
echo "username $USERNAME" >> $CONFFILE
echo "password $PASSWORD" >> $CONFFILE
echo "bridge_cafile $CACERT" >> $CONFFILE
echo "start_type automatic" >> $CONFFILE
echo "try_private false" >> $CONFFILE
echo "cleansession true" >> $CONFFILE
echo "keepalive_interval $KEEPALIVE" >> $CONFFILE
echo "restart_timeout $TIMEOUT" >> $CONFFILE
echo 'topic /device/+/tmpo/sync out 0 "" ""' >> $CONFFILE
echo 'topic /sensor/+/tmpo/# out 0 "" ""' >> $CONFFILE

