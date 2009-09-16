Image Annotate allows users to attach notes or user references to areas of a picture. This is
what Flickr and Facebook already do.

This module is based on jQuery UI and only work with the Image and ImageField modules: please
feedback, issues and remarks on the project page (http://drupal.org/project/image_annotate).

Install
-------

1. Copy the image_annotate folder into your module folder.

2. Go to the modules page (admin/build/modules) and enable the Image Annotate module
   (dependency on jquery_ui and imagefield modules)

3. Go to the permissions page (admin/user/permissions) and activate the appropriate
   permissions: users need both permissions for comments AND image annotations.

4. Navigate to the admin page of the content type for which you want to have an
   annotative image (admin/content/node-type/YOUR_CONTENT_TYPE):

   a. Activate comments for that content type ("Comment settings")

   b. Go to the "Display fields" sub section (admin/content/node-type/picture/display) and
      select the widget for handling display of the body: select the "Image with annotations" one.

4. Next time you create a node of that type, when viewing the node you will see
   an "Add a note" link that lets you add comments on the picture (given that you have
   the permission to do so).

Todo:
-----

This module is still being actively develop, here are a few things that are still to be done:

* Clean and optimize jQuery

* Add a hook to enable other types of annotations: user reference, taxonomy...

Author:
-------
Ronan Berder <hunvreus@gmail.com>
http://drupal.org/user/49057
http://teddy.fr