<?php
//
// logger.admin.inc : callbacks for logger account and admin pages
// Copyright (c) 2010 flukso.net
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//
// $Id$
//

/**
 * Callback function for the user/x/sensors page
 */
function _logger_account_sensors($type = 'electricity') {
  global $user;
  $rows = array();

  $result = db_query("SELECT lm.meter, lm.function 
                      FROM {logger_meters} lm
                      WHERE lm.uid = %d AND lm.type = '%s'
                      ORDER BY lm.function", $user->uid, $type);

  while ($sensor = db_fetch_object($result)) {
    $row = array();
    $row[] = $sensor->meter;
    $row[] = $type;
    $row[] = $sensor->function;

    $rows[] = $row;
  }

  return theme('logger_account_sensors_list', $rows);
}

/**
 * Theme function for displaying the user's sensors
 *
 * @param $items
 *   An array of table rows.
 *
 * @ingroup themeable
 */
function theme_logger_account_sensors_list($items) {
  if (count($items) > 0) {
    $headers = array(t('Sensor'), t('Type'), t('Function'));
    $output  = theme('table', $headers, $items);
  }
  else {
    $output = t('No sensors available.');
  }

  return $output;
}

/**
 * Callback function for the user/x/privacy page
 */
function _logger_account_privacy() {
  $output .= drupal_get_form('_logger_account_privacy_form');
  return $output;
}

/**
 * Generates the privacy form.
 */
function _logger_account_privacy_form() {
  global $user;

  $form['privacy'] = array(
    '#title' => t('Set your preferred privacy mode'),
    '#type' => 'radios',
    '#options' => array(
      t('Shared within Flukso'),
      t('Private'),
    ),
    '#description' => t("When selecting Private mode, your data stream will not be drawn on other people's graph and all statistics will be marked as 'prv'. Conversely, nobody else's data stream will be drawn on your chart."),
    '#default_value' => db_result(db_query("SELECT private
                                            FROM {logger_users}
                                            WHERE uid = %d", $user->uid)),
  );
  $form['submit'] = array(
    '#type' => 'submit',
    '#value' => t('Submit'),
  );
  return $form;
}

/**
 * Process settings form submissions.
 */
function _logger_account_privacy_form_submit($form, &$form_state) {
  global $user;

  // the built-in validation function already checks elements with options
  $private = $form_state['values']['privacy'];
  db_query("UPDATE {logger_users}
            SET private = %d
            WHERE uid = %d", $private, $user->uid);

  // page redirection to the dashboard after form submission
  $form_state['redirect'] = 'logger';
}

/**
 * Define the administration settings form for the logger module
 */
function _logger_admin_settings() {
//TODO
}

