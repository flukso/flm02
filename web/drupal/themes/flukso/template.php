<?php
/**
 * @file template.php
 * Core functions for the Flukso theme.
 *
 * $Id$
 */


/**
 * Add a prefix to the terms list and insert a separateor between them.
 *
 * @param $terms The pre-rendered HTML string containing the term list
 *        elements.
 * @param $prefix The text to show before the list of terms. By default the
 *        output of t('Tags: ') is used.
 * @param $separator The character(s) to place between the terms. By default,
 * 		  the output of t(', ') is used.
 *
 * @return The modified HTML.
 */
function flukso_separate_terms($terms, $prefix = NULL, $separator = NULL) {
  $prefix    = ($prefix == NULL) ? t('Tags: ') : $prefix;
  $separator = ($separator == NULL) ? t(', ') : $separator;
  $output    = $prefix . preg_replace('!a></li>\s<li!',
                                      "a>{$separator}</li>\n<li", $terms);
  return $output;
}


/**
 * Insert a separator between items in the list of links for a node.
 *
 * @param $links The pre-rendered HTML string containing the link list
 *        elements.
 * @param $separator character(s) to place between the links. By default, the
 *        output of t(' | ') is used.
 *
 * @return The modified HTML.
 */
function flukso_separate_links($links, $separator = ' | ') {
  $separator = ($separator == NULL) ? t(' | ') : $separator;
  $output    = preg_replace('!a></li>\s<li!',
                            "a>{$separator}</li>\n<li", $links);
  return $output;
}


/**
 * Preprocess the nodes.
 *
 * @param &$vars The template variables array. After invoking this function,
 *        these keys will be added to $vars:
 *        - 'flukso_node_author' - The node's "posted by" text and author
 *          link.
 *        - 'flukso_node_class' - The CSS classes to apply to the node.
 *        - 'flukso_node_links' - The node links with a separator placed
 *          between each.
 *        - 'flukso_perma_title' - The localized permalink text for the node.
 *        - 'flukso_node_timestamp' - The timestamp for this type, if one
 *          should be rendered for this type.
 *        - 'flukso_term_links' - The taxonomy links with a separator placed
 *          between each.
 */
function flukso_preprocess_node(&$vars) {
  $node                       = $vars['node'];
  $vars['flukso_node_class']  = 'node ' . ($node->sticky ? 'sticky ' : '') .
                                ($node->status ? '' : ' node-unpublished') .
                                ' node-' . $node->type .
                                ($teaser ? ' teaser' : '') . ' clear-block';
  $vars['flukso_term_links']  = flukso_separate_terms($vars['terms']);
  $vars['flukso_node_links']  = flukso_separate_links($vars['links']);
  $vars['flukso_perma_title'] = t('Permanent Link to !title',
                                array('!title' => $vars['title']));

  // --------------------------------------------------------------------------
  // -- Node authorship.
  if (!empty($vars['submitted'])) {
    $vars['flukso_node_author'] = t('Posted by !author',
                                    array('!author' => $vars['name']));
  }

  // --------------------------------------------------------------------------
  // -- Timestamp for this type?
  if (!empty($vars['submitted']) && isset($node->created)) {
    $vars['flukso_node_timestamp'] = format_date($node->created, 'custom', t('d M Y'));
  }
}

/**
 * Preprocess the pages.
 *
 * @param &$vars The template variables array. After invoking this function,
 *        no page title will be displayed on /node/x pages.
 */
function flukso_preprocess_page(&$vars) {
  if (substr($_GET['q'], 0, 4) == 'node') {
    $vars['title'] = ''; 
  }

 // -- tab text sould always be Flukso
 $vars['head_title'] = 'Flukso';
}
