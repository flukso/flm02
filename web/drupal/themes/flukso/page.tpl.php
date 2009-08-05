<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="<?php print $language->language ?>" xml:lang="<?php print $language->language ?>">
<head>
  <title><?php print $head_title ?></title>
  <meta http-equiv="Content-Style-Type" content="text/css" />
  <?php print $head ?>
  <?php print $styles ?>
  <?php print $scripts ?>
</head>

<body <?php print theme("onload_attribute"); ?>>
<div id="container">
  <div id="top">
    <div id="home">
      <?php if ($logo) { ?><a id="logo" href="<?php print $base_path ?>" title="<?php print t('Home') ?>"><img src="<?php print $logo ?>" alt="<?php print t('Home') ?>"/></a><?php } ?>
       <?php if ($site_slogan) { ?><div class="site-slogan"><?php print $site_slogan ?></div><?php } ?>
    </div>
    <div id="navigation">
      <?php if (isset($primary_links)) { ?><div id="primary"><?php print theme('links', $primary_links) ?></div><?php } ?>
      <?php if (isset($secondary_links)) { ?><div id="secondary"><?php print theme('links', $secondary_links) ?></div><?php } ?>
    </div>
    <!-- <div> ?php print $search_box ? </div> -->
    <br />
  </div>
  <?php if ($left) { ?> <div id="leftnav"><?php print $left ?></div> <?php } ?>
  <?php if ($right) { ?> <div id="rightnav"><?php print $right ?></div> <?php } ?>
  <div id="content">
    <!-- print $breadcrumb ?> -->
    <?php if ($header != ""): ?><div id="header"><?php print $header ?></div><?php endif; ?>
    <?php if ($title != ""): ?><h2 class="content-title"><?php print $title ?></h2><?php endif; ?>
    <?php if ($tabs != ""): ?><?php print $tabs ?><?php endif; ?>
    <?php if ($mission != ""): ?><div id="mission"><?php print $mission ?></div><?php endif; ?>
    <?php if ($help != ""): ?><p id="help"><?php print $help ?></p><?php endif; ?>
    <?php if ($messages != ""): ?><div id="message"><?php print $messages ?></div><?php endif; ?>
    <!-- start main content --><?php print($content) ?><!-- end main content -->
  </div>
  <div id="footer"><?php print $footer_message;?><p>Flukso - community metering. Flukso is a project of the <a href="http://www.jokamajo.org">Jokamajo Institute</a>.</p></div>
</div>
<?php print $closure;?>
</body>
</html>
