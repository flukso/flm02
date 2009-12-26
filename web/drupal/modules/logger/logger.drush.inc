<?php
// $Id$

/**
 * Implementation of hook_drush_command().
 */
function logger_drush_command() {
  $items = array();

  $items['logger create'] = array(
    'callback'    => '_logger_create',
    'description' => 'Create a new sensor node entry.',
    'arguments'   => array(
      'serial'      => 'Sensor node serial number.',
      'country'     => 'Destination country.',
      'uid'         => 'User ID.',
    ),
  );

  $items['logger assign'] = array(
    'callback'    => '_logger_assign',
    'description' => 'Assign a sensor node to a user.',
    'arguments'   => array(
      'serial'      => 'Sensor node serial number.',
      'uid'         => 'User ID.',
    ),
  );

  $items['logger alias'] = array(
    'callback'    => '_logger_alias',
    'description' => 'Create aliases for meter/sensorIDs.',
    'arguments'   => array(
      'meter'       => 'Create an alias for one specific meterID.',
      'permissions' => 'Set non-default permissions for this alias.',
    ),
  );

  return $items;
}

/**
 * Drush command callbacks.
 */
function _logger_create($serial, $country ="", $uid = 0) {
  // guard against duplicating entries for the same S/N
  $count = db_result(db_query("SELECT COUNT(device) FROM {logger_devices} WHERE serial = %d", $serial));
  if ($count > 0) {
    drush_set_error('LOGGER_CREATE_SERIAL_DUPLICATE', dt('The S/N: @serial already exists.', array('@serial' => $serial)));
  }
  else { 
    if ($uid > 0) {
      $result = db_query("INSERT INTO {logger_users} (uid, private) VALUES (%d, %d)", $uid, 0);
      if (!$result) drush_set_error('LOGGER_CREATE_USERS_ENTRY', dt('Error creating a user entry for @uid.', array('@uid' => $uid)));
    }

    // create an entry in the {logger_devices} table
    $device  = md5(uniqid(rand(), TRUE));
    $sha     = md5(uniqid(rand(), TRUE));
    $created = time();

    $result = db_query("INSERT INTO {logger_devices} (device, serial, uid, sha, created, country) VALUES ('%s', %d, %d, '%s', %d, '%s')", $device, $serial, $uid, $sha, $created, $country);
    if (!$result) drush_set_error('LOGGER_CREATE_DEVICE_ENTRY', dt('Error creating a device entry for @device.', array('@device' => $device)));

    if (!drush_get_error()) drush_log(dt('Successfully created the device/key: @device / @key for S/N: @serial', array('@device' => $device, '@key' => $sha, '@serial' => $serial)), 'ok');

    // create an entry in the {logger_meters} table
    for ($i = 0; $i < 4; $i++) {
      if ($i == 0) {
        $type = 'electricity';
      }
      else {
        $type = 'undefined';
      }

      $permissions = 62;
      $meter = md5(uniqid(rand(), TRUE));
      $alias = md5(uniqid(rand(), TRUE));

      $path = new stdClass();
        $path->root = DRUPAL_ROOT .'/'. drupal_get_path('module', 'logger');
        $path->base = $path->root .'/data/base/';
        $path->night = $path->root .'/data/night/';
        $path->alias_base = $path->root .'/alias/base/';
        $path->alias_night = $path->root .'/alias/night/';

      $result = db_query("INSERT INTO {logger_meters} (meter, uid, device, created, type) VALUES ('%s', %d, '%s', %d, '%s')", $meter, $uid, $device, $created, $type); 
      $insert = db_query("INSERT INTO {logger_aliases} (alias, meter, permissions) VALUES ('%s', '%s', %d)", $alias, $meter, $permissions);

      if (!($result && $insert)) drush_set_error('LOGGER_CREATE_METER_ENTRY', dt('Error creating meter entry for @meter.', array('@meter' => $meter)));

      // create the meter base rrd
      if (!file_exists($path->base . $meter .'.rrd')) {
        $command = $path->root .'/rrdtool create '. $path->base . $meter .'.rrd -b 1199487600 -s 60 DS:meter:DERIVE:8640000:0:20 RRA:AVERAGE:0.5:1:120 RRA:AVERAGE:0.5:15:192 RRA:AVERAGE:0.5:1440:60 RRA:AVERAGE:0.5:10080:520';
        system($command, $return);
        if ($return == 0) {
          system('ln ' . $path->base . $meter . '.rrd ' . $path->alias_base . $alias);
        }
        else {
          drush_set_error('LOGGER_CREATE_RRD_BASE_ERROR', dt('Error creating the base @meter rrd.', array('@meter' => $meter)));
        }
      }

      // create the meter night rrd
      if (!file_exists($path->night . $meter .'.rrd')) {
        $command = $path->root .'/rrdtool create '. $path->night . $meter .'.rrd -b 1199487600 -s 86400 DS:meter:GAUGE:8640000:0:20 RRA:AVERAGE:0.5:1:60 RRA:AVERAGE:0.5:7:520';
        system($command, $return);
        if ($return == 0) {
          system('ln ' . $path->night . $meter . '.rrd ' . $path->alias_night . $alias);
        }
        else {
          drush_set_error('LOGGER_CREATE_RRD_NIGHT_ERROR', dt('Error creating the night @meter rrd.', array('@meter' => $meter)));
        }
      }

      if (!drush_get_error()) drush_log(dt('Successfully created the meter: @meter with alias: @alias and type: @type', array('@meter' => $meter, '@alias' => $alias, '@type' => $type)), 'ok');
    }
  }
}

function _logger_assign($serial, $country, $uid) {
  // check the existence of S/N
  $device = db_result(db_query("SELECT device FROM {logger_devices} WHERE serial = %d", $serial));
  if ($device == '') {
    drush_set_error('LOGGER_ASSIGN_SERIAL_NON_EXISTENT', dt('The S/N: @serial does not exist.', array('@serial' => $serial)));
  }
  else {
    db_query("UPDATE {logger_devices} SET uid = %d, country = '%s' WHERE serial = %d", $uid, $country, $serial);   
    db_query("UPDATE {logger_meters} SET uid = %d WHERE device = '%s'", $uid, $device);
    
    $result = db_query("INSERT INTO {logger_users} (uid, private) VALUES (%d, %d)", $uid, 0);
    if (!$result) drush_log(dt('uid: @uid already exists in the {logger_users} table', array('@uid' => $uid)), 'notice');
  }

  if (!drush_get_error()) drush_log(dt('Successfully assigned uid: @uid to S/N: @serial', array('@uid' => $uid, '@serial' => $serial)), 'ok');

}

function _logger_alias($meter = "", $permissions = 62) {
  if ($meter == "") {
    $result = db_query("SELECT meter FROM {logger_meters}");
    while ($meter = db_fetch_object($result)) {
      $count = db_result(db_query("SELECT COUNT(meter) FROM {logger_aliases} WHERE meter = '%s'", $meter->meter));
      if ($count == 0) {
        $alias = md5(uniqid(rand(), TRUE));
        $insert = db_query("INSERT INTO {logger_aliases} (alias, meter, permissions) VALUES ('%s', '%s', %d)", $alias, $meter->meter, $permissions);

        if (!$insert) {
          drush_set_error('LOGGER_CREATE_ALIAS_ENTRY', dt('Error creating alias entry for @meter.', array('@meter' => $meter->meter)));
        }
        else {
          drush_log(dt('Created an entry in {logger_aliases} with alias: @alias and meter: @meter', array('@alias' => $alias , '@meter' => $meter->meter)), 'ok');

          $root_path = DRUPAL_ROOT .'/'. drupal_get_path('module', 'logger');
          system('ln ' . $root_path . '/data/base/' . $meter->meter . '.rrd ' . $root_path . '/alias/base/' . $alias);
          system('ln ' . $root_path . '/data/night/' . $meter->meter . '.rrd ' . $root_path . '/alias/night/' . $alias);
        }
      }
    }
  }
}

