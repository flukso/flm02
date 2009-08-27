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
      'version'     => 'Software version number.',
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

  return $items;
}

/**
 * Drush command callbacks.
 */
function _logger_create($serial, $version = 0, $uid = 0) {
  // guard against duplicating entries for the same S/N
  $count = db_result(db_query("SELECT COUNT(device) FROM {logger_devices} WHERE serial = %d", $serial));
  if ($count > 0) {
    drush_set_error('LOGGER_CREATE_SERIAL_DUPLICATE', dt('The S/N: @serial already exists.', array('@serial' => $serial)));
  }
  else { 

    // create an entry in the {logger_devices} table
    $device  = md5(uniqid(rand(), TRUE));
    $sha     = md5(uniqid(rand(), TRUE));
    $created = time();

    $result = db_query("INSERT INTO {logger_devices} (device, serial, uid, sha, created, version) VALUES ('%s', %d, %d, '%s', %d, %d)", $device, $serial, $uid, $sha, $created, $version);
    if (!$result) drush_set_error('LOGGER_CREATE_DEVICE_ENTRY', dt('Error creating a device entry for @device.', array('@device' => $device)));

    // create an entry in the {logger_meters} table
    $meter = md5(uniqid(rand(), TRUE));
    $path = new stdClass();
      $path->root = DRUPAL_ROOT .'/'. drupal_get_path('module', 'logger');
      $path->base = $path->root .'/data/base/';
      $path->night = $path->root .'/data/night/';
    $result = db_query("INSERT INTO {logger_meters} (meter, uid, device, created) VALUES ('%s', %d, '%s', %d)", $meter, $uid, $device, $created);
    if (!$result) drush_set_error('LOGGER_CREATE_METER_ENTRY', dt('Error creating meter entry for @meter.', array('@meter' => $meter)));

    // create the meter base rrd
    if (!file_exists($path->base . $meter .'.rrd')) {
      $command = $path->root .'/rrdtool create '. $path->base . $meter .'.rrd -b 1199487600 -s 60 DS:meter:DERIVE:8640000:-2:2 RRA:AVERAGE:0.5:1:120 RRA:AVERAGE:0.5:15:192 RRA:AVERAGE:0.5:1440:60 RRA:AVERAGE:0.5:10080:520';
      system($command, $return);
      if ($return <> 0) drush_set_error('LOGGER_CREATE_RRD_BASE_ERROR', dt('Error creating the base @meter rrd.', array('@meter' => $meter)));
    }

    // create the meter night rrd
    if (!file_exists($path->night . $meter .'.rrd')) {
      $command = $path->root .'/rrdtool create '. $path->night . $meter .'.rrd -b 1199487600 -s 86400 DS:meter:GAUGE:8640000:-2:2 RRA:AVERAGE:0.5:1:60 RRA:AVERAGE:0.5:7:520';
      system($command, $return);
      if ($return <> 0) drush_set_error('LOGGER_CREATE_RRD_NIGHT_ERROR', dt('Error creating the night @meter rrd.', array('@meter' => $meter)));
    }
  }

  if (!drush_get_error()) drush_log(dt('Successfully created the device/key: @device / @key and meter: @meter entries for S/N: @serial', array('@device' => $device, '@key' => $sha, '@meter' => $meter, '@serial' => $serial)), 'ok');
}

function _logger_assign($serial, $uid) {
  // check the existence of S/N
  $device = db_result(db_query("SELECT device FROM {logger_devices} WHERE serial = %d", $serial));
  if ($device == '') {
    drush_set_error('LOGGER_ASSIGN_SERIAL_NON_EXISTENT', dt('The S/N: @serial does not exist.', array('@serial' => $serial)));
  }
  else {
    db_query("UPDATE {logger_devices} SET uid = %d WHERE serial = %d", $uid, $serial);   
    db_query("UPDATE {logger_meters} SET uid = %d WHERE device = '%s'", $uid, $device);
  }

  if (!drush_get_error()) drush_log(dt('Successfully assigned uid: @uid to S/N: @serial', array('@uid' => $uid, '@serial' => $serial)), 'ok');

}
