<?php
/**
 * Callbacks for accessing RRDs.
 *
 * Copyright (c) 2008-2009 jokamajo.org
 *               2010-2011 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
 *               2010 Fraunhofer Institut ITWM (www.itwm.fraunhofer.de)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

//Constants
//Some constants need to be redefined because this file can be called from xmlrpc.php
//drupal_get_path does not work, because xmlrpc.php is called from xmlrpc.php.
define('ROOT_PATH',  'sites/all/modules/logger');
define('RRDTOOL',    ROOT_PATH . '/rrdtool');
define('DATA_PATH',  ROOT_PATH . '/data');

define('SECOND', 1);
define('MINUTE', 60);
define('HOUR',   60 * MINUTE);
define('DAY',    24 * HOUR);
define('WEEK',    7 * DAY);
define('MONTH',  30 * DAY);
define('YEAR',   52 * WEEK);

define('METER', 'meter');

/**
 * Creates an RRD for storing sensors' measurements in minute resolution.
 *
 * @param $meter  The sensor's id.
 * @return the creation command execution code.
 */
function logger_rrd_base_create($meter) {

  $step = 1 * MINUTE;
  $archives = logger_get_storage_periods();

  return _logger_rrd_create($meter, $step, 'DERIVE', $archives, 'base');
}

/**
 * Returns an array of measurement storage periods, indexed by time resolution.
 *
 * @return the array of storage periods.
 */
function logger_get_storage_periods() {
  return array(
     //resolution   storage period
     1 * MINUTE =>   1 * DAY,
    15 * MINUTE =>   1 * WEEK,
     1 * DAY    => 365 * DAY,
     1 * WEEK   =>  10 * YEAR);
}

/**
 * Creates an RRD for storing sensors' night measurements in daily resolution.
 *
 * @param $meter  The sensor's id.
 * @return the creation command execution code.
 */
function logger_rrd_night_create($meter) {

  $step = 24 * HOUR;
  $archives = array(
    //resolution    storage period
    1 * DAY    => 365 * DAY,
    1 * WEEK   =>  10 * YEAR);

  return _logger_rrd_create($meter, $step, 'GAUGE', $archives, 'night');
}

/**
 * Creates an RRD for storing sensors' measurements.
 *
 * @param $meter     The sensor's id.
 * @param $step      The base interval in seconds with which data will be fed into the RRD.
 * @param $ds_type   The RRD DS type.
 * @param $archives  An array of RRD archives' properties formatted as (resolution => storage period).
 * @param $subdir    The subdirectory where the rrd file should be placed. (NULL means the default location)
 * @return the creation command execution code.
 */
function _logger_rrd_create($meter, $step, $ds_type, $archives, $subdir) {

  $return = 0;
  $file_path = _logger_rrd_file($meter, $subdir);
  $start = 1199487600; //Fri, 04 Jan 2008 23:00:00 GMT

  $ds = _logger_rrd_meter_ds($ds_type);

  if (!file_exists($file_path)) {

    $command = RRDTOOL . " create $file_path " .
      "--start $start " .
      "--step $step " .
      "$ds " .
      _logger_rrd_rras($step, $archives);

    system($command, $return);
  }
  return $return;
}

/**
 * Creates a RRD DS definition.
 *
 * @param $type  The DS type.
 * @return the DS definition.
 */
function _logger_rrd_meter_ds($type) {

  $name = METER;
  $timeout = 100 * DAY;
  $min = 0;
  $max = 20;

  return "DS:$name:$type:$timeout:$min:$max";
}

/**
 * Creates a list of RRD archives' definitions.
 *
 * @param $step      The base interval in seconds with which data will be fed into the RRD.
 * @param $archives  The array of RRD archives' properties formatted as (resolution => storage period).
 * @return the list of RRD archives' definitions.
 */
function _logger_rrd_rras($step, $archives) {
  
  $rras = '';
  foreach($archives as $resolution => $storage_period) {

    $rows = $resolution / $step;
    $slots = $storage_period / $resolution;

    $rras .= "RRA:AVERAGE:0.5:$rows:$slots ";
  }
  return $rras;
}

/**
 * Feeds the sensors' RRDs with night consumption.
 *
 * @param $meter  The sensor's id.
 * @param $start  The night period start time.
 * @param $end    The night period end time.
 * @return the update command execution code.
 */
function logger_rrd_night_update($meter, $start, $end) {

  $file_path = _logger_rrd_file($meter);
  $resolution = 15 * MINUTE;

  $command = RRDTOOL . " fetch $file_path " .
    "AVERAGE " .
    "--resolution $resolution " .
    "--start $start " .
    "--end $end " .

    "| tail -n 12 | awk -F': ' '{SUM += $2} END {print SUM/12}'";

  $value = (float) shell_exec($command);

  return logger_rrd_update($meter, array($end => $value), 'night');
}

/**
 * Feeds the sensors' RRDs with measurements.
 *
 * @param $meter   The sensor's id.
 * @param $values  An array of measurements formatted as (timestamp => value).
 * @param $subdir  The subdirectory where the rrd file is placed.
 * @return the update command execution code.
 */
function logger_rrd_update($meter, $values, $subdir = NULL) {

  $return = 0;
  $file_path = _logger_rrd_file($meter, $subdir);

  $command = RRDTOOL . " update $file_path ";

  ksort($values);

  foreach($values as $timestamp => $value) {
    $command .= " $timestamp:$value";
  }

  system($command, $return);

  return $return;
}

/**
 * Queries the RRDs for sensor's measurements.
 *
 * @param $interval  The time interval. This argument is used to select the RRA.
 * @param $sensor    The sensor.
 * @param $unit      The power unit.
 * @param $offset    The user timezone offset.
 * @param $period    An array containing a period of time to be queried.
 * @param $step      The amount of time aggregated in a single point.
 * @return the array of sensor's measurements.
 */
function logger_rrd_query_sensor($interval, $sensor, $unit, $offset, $period, $step) {

  $series_id = METER;
  $def = _logger_rrd_sensor_def($interval, $series_id, $sensor, $unit);

  $latest = _logger_rrd_latest_timestamp($interval, $sensor);

  return _logger_rrd_export($interval, $def, $series_id, $latest, $offset, true, $period, $step);
}

/**
 * Returns the timestamp of the latest update of a sensor.
 *
 * @param $interval  The time interval.
 * @param $sensor    The sensor.
 * @return the latest update timestamp.
 */
function _logger_rrd_latest_timestamp($interval, $sensor) {

  $file_path = _logger_rrd_file($sensor->meter, $interval);

  $command = RRDTOOL . " last $file_path";
  exec($command, $lines);

  return $lines[0];
}

/**
 * Queries the RRDs for sensors' aggregated measurements.
 *
 * @param $interval  The time interval.
 * @param $sensors   The array of sensors.
 * @param $unit      The power unit.
 * @param $offset    The user timezone offset.
 * @param $period    An array containing a period of time to be queried.
 * @param $step      The amount of time aggregated in a single point.
 * @return the array of sensor's measurements.
 */
function logger_rrd_query_agg($interval, $sensors, $unit, $offset, $period, $step) {

  //Latest measurements are not considered, in order to tolerate heartbeat delays.
  $end_time = time() - 15 * MINUTE;
  $def = "";
  $variables = "";

  $i = 1;
  foreach ($sensors as $sensor) {

    $sensor_cdef = METER . $i;

    $def .= _logger_rrd_sensor_def($interval, $sensor_cdef, $sensor, $unit) .

      //Considers unknown measurements to be zero
      "CDEF:completeseries$i=" . "$sensor_cdef,UN,0,$sensor_cdef,IF " .

      //Do not consider latest minutes
      "CDEF:filtered$i=TIME,$end_time,GT,UNKN,completeseries$i,IF ";

    $variables .= ($i > 1 ? ',' : '') . "filtered$i";
    $i++;
  }

  $operators = str_repeat(",+", count($sensors) - 1);

  $series_id = METER . '0';

  //Sum all measurements of a particular time
  $def .= "CDEF:$series_id=$variables$operators ";

  return _logger_rrd_export($interval, $def, $series_id, time(), $offset, false, $period, $step);
}

/**
 * Queries the total energy consumption during the specified time period, in watt-hour.
 *
 * @param $sensors  The sensors whose measurements are to be summed up.
 * @param $unit     The energy unit.
 * @param $interval The time interval menu option.
 * @param $offset   The user timezone offset.
 * @param $period   An array containing a period of time to be queried.
 * @param $step     The amount of time aggregated in a single point.
 * @return the summed power consumption.
 */
function logger_rrd_query_energy($sensors, $unit, $interval, $offset, $period, $step) {

  $total = 0;

  foreach ($sensors as $sensor) {
    $measurements = logger_rrd_query_sensor($interval, $sensor, $unit, $offset, $period, $step);

    foreach($measurements as $timestamp => $value) {
      $total += $value * $step;
    }
  }
  return $total;
}

/**
 * Creates a DEF tag to represent sensor's measurements plotted in a chart series.
 *
 * @param $interval       The time interval.
 * @param $series_id      The series id.
 * @param $sensor         The sensor.
 * @param $unit           The unit.
 * @return the DEF tag.
 */
function _logger_rrd_sensor_def($interval, $series_id, $sensor, $unit) {

  global $user;
  $factor = _logger_rrd_get_factor($unit);


  if($sensor->private > 0 && $sensor->uid != $user->uid) {
    return null;
  }

  $file_path = _logger_rrd_file($sensor->meter, $interval);

  return "DEF:data$series_id=" . "$file_path:meter:AVERAGE " .
    "CDEF:$series_id=" . "data$series_id," . $factor . ",* ";
}

/**
 * Composes RRD file path.
 *
 * @param $meter     The sensor id.
 * @param $interval  The time interval.
 * @return the RRD file path.
 */
function _logger_rrd_file($meter, $interval) {

  $path = DATA_PATH . ($interval == 'night'? '/night' : '/base');
  return "$path/$meter.rrd";
}

/**
 * Exports the sensors' measurements from the RRDs to an array.
 *
 * @param $def           The rrd def tag.
 * @param $exported_cdef The exported cdef.
 * @param $latest        The latest update time.
 * @param $offset        The user timezone offset.
 * @param $include_nan   Whether NaN values should be included in the result.
 * @param $period        An array containing a period of time to be queried.
 * @param $step          The amount of time aggregated in a single point.
 * @return the array of sensors' measurements.
 */
function _logger_rrd_export($interval, $def, $exported_cdef, $latest, $offset, $include_nan, $period, $step) {

  if (!$def) {
    return array();
  }

  $start = $period['start'];
  $end = $period['end'];

  //time period MUST be within the RRA and both $start and $end must be multiples of $step
  $start = $start - ($start % $step);
  $end = $end - ($end % $step);

  $maxrows = 1 + ($end - $start) / $step;
  $maxrows = $maxrows < 10? 10: $maxrows; //RRDTool exports at least 8 rows

  $command = RRDTOOL . ' xport ' .
    "--start $start " .
    "--end $end " .
    "--step $step " .
    "--maxrows $maxrows " .
    "$def " .
    "XPORT:$exported_cdef";

  exec($command, $lines);

  return _logger_rrd_parse_exported_lines($lines, $offset);
}

/**
 * Parses output of command rrdtool xport into an array.
 * 
 * @param $lines   The lines to be exported.
 * @param $offset  The user timezone offset.
 * @return the array of sensors' measurements.
 */
function _logger_rrd_parse_exported_lines($lines, $offset) {

  $data = array();

  foreach($lines as $line) {
    $line = strtolower($line);

    if (strpos($line, '<row>') > 0) {

      $line = str_replace(array('<row><t>', '</v></row>'), '',  $line);
      $line = str_replace('</t><v>', ':', $line);
      $div_pos = strpos($line, ':');

      $timestamp = trim(substr($line, 0, $div_pos)) + $offset;
      $value = trim(substr($line, $div_pos + 1));

      if ($value == 'nan') {

        if ($include_nan) {
          $data[$timestamp] = null;
        }

      } else {
        $data[$timestamp] = $value;
      }
    }
  }
  return $data;
}

function _logger_rrd_get_factor($unit) {

  $factors = array(
    'kw'    => 0.001,
    'kwh'   => 31536,
    'eur'   => 5676, // 18 EURcent/kWh
    'aud'   => 5991, // 19 AUDcent/kWh
    'watt'  => 3600, // 1Wh/s = 3600 W

    'Wh'    => 1,
    'kWh'   => 0.001
  );
  return $factors[$unit];
}
