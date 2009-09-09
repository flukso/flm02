<?php
/**
 * @file node.tpl.php
 * The node rendering logic for Flukso.
 *
 * In addition to the standard variables Drupal makes available to node.tpl.php,
 * these variables are made available by the theme:
 *
 * - $flukso_node_author - The node's "posted by" text and author link.
 *
 * - $flukso_node_class - The CSS classes to apply to the node.
 *
 * - $flukso_node_links - The node links with a separator placed between each.
 *
 * - $flukso_perma_title - The localized permalink text for the node.
 *
 * - $flukso_term_links - The taxonomy links with a separator placed between
 *   each.
 *
 * - $flukso_node_timestamp - The timestamp for this type, if one should be
 *   rendered for this type.
 *
 * $Id$
 */
?>
<div id="node-<?php print $node->nid; ?>" class="<?php echo $flukso_node_class; ?>">
<?php //if ($page == 0): ?>
  <div class="node-headline clear-block">
    <h2><a href="<?php print $node_url; ?>" rel="bookmark" title="<?php print $flukso_perma_title; ?>"><?php print $title; ?></a></h2>
    <?php if (isset($flukso_node_timestamp)): ?>
        <span class="timestamp"><?php print $flukso_node_timestamp; ?></span>
    <?php endif; ?>
  </div>
<?php //endif; ?>
  <div class="content clear-block">
    <?php if (isset($flukso_node_author)): ?>
    	<div class="node-author"><?php print $flukso_node_author; ?></div>
    <?php endif; ?>
    <?php print $picture; ?>
    <?php print $content; ?>
  </div>
  <?php if (!empty($taxonomy) || !empty($links)): ?>
    <div class="meta">
      <?php
      if ($taxonomy) { print $flukso_term_links; }
      if (!empty($links)): ?>
          <div class="links"><?php print $flukso_node_links; ?></div>
      <?php endif; ?>
    </div>
  <?php endif; ?>
</div> <!-- node -->
