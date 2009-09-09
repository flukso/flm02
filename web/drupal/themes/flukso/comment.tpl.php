<?php
// $Id: comment.tpl.php,v 1.5 2008/11/23 22:16:19 shannonlucas Exp $

$comment_class = 'comment' . (($comment->new) ? ' comment-new' : '') . 
                 ' ' . $status . ' ' . $zebra;
?>
<div class="<?php print $comment_class; ?>">
  <div class="content">
    <?php if (!empty($picture)) { print $picture; } ?>
    <?php if ($comment->new): ?>
      <span class="new"><?php print $new ?></span>
    <?php endif; ?>
    <?php print $content ?>
    <div class="clear"></div>
    <?php if ($signature): ?>
      <div class="user-signature clear-block">
        <?php print $signature ?>
      </div>
    <?php endif; ?>
    <div class="clear"></div>
  </div>
  <div class="comment-meta">
    <span><strong><?php print $author; ?></strong> | <?php print format_date($comment->timestamp, 'custom', t('d M Y')); ?></span>
      <?php if ($links) { print $links; } ?>
  </div>
</div>
