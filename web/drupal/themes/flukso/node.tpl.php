<div class="node<?php print ($sticky) ? " sticky" : ""; ?>">

	<?php if ($page == 0): ?>
		<h2><a href="<?php print $node_url ?>" title="<?php print $title ?>"><?php print $title ?></a></h2>
	<?php else: ?>
		<?php print $picture ?>
	<?php endif; ?>

	<?php if ($submitted): ?>
		<span class="submitted"><?php print t('!date — !username', array('!username' => theme('username', $node), '!date' => format_date($node->created))); ?></span>
	<?php endif; ?>

	<div class="content"><?php print $content ?></div>

	<?php if ($links): ?>
		<em class="clear links">>> <?php print $links ?></em>
	<?php endif; ?>
	<?php if ($page == 1): ?>
		<em class="clear terms"><?php print $terms ?></em>
	<?php endif; ?>

</div>
