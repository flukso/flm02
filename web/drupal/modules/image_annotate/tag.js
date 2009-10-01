Drupal.behaviors.imageAnnotate = function (context) {
  var annotativeImage = new Array();
  for (var i=0; i<Drupal.settings.imageAnnotate.length; i++) {
    annotativeImage[i] = new Drupal.annotativeImage(Drupal.settings.imageAnnotate[i]);
  }
  // If the URL contains a fragment starting with image-annotate we define the aid of the note to highlight/show
  var url = document.location.toString();
  var highlight = 0;
  if (url.match('#image-annotate-')) {
    highlight = url.split('#image-annotate-')[1];
    for (var i=0; i<annotativeImage.length; i++) {
      annotativeImage[i].showNote(highlight);
    }
  }
  // We replace the target of the comment links
  $('a.image-annotate-link').click(function() {
    for (var i=0; i<annotativeImage.length; i++) {
      annotativeImage[i].showNote($(this).attr('rel').split('image-annotate-')[1]);
    }
    return false;
  });
};

/**
 * An annotative image object
 */
Drupal.annotativeImage = function (image) {
  //BVDM 13/09/09: substitute image.field for image.nid
  this.image = $('img.image-annotate-nid-' + image.nid);
  this.nid = image.nid;
  this.field = image.field;
  this.mode = 'view';
  // Add the canvas (which has the image as a background) and the containers for the notes
  this.canvas = $('<div class="image-annotate-canvas"><div class="image-annotate-view"></div><div class="image-annotate-edit"><div class="image-annotate-edit-area"></div></div></div>');
  this.canvas.children('.image-annotate-edit').hide();
  this.canvas.children('.image-annotate-view').hide();
  this.image.after(this.canvas);
  // Give the canvas and the container their size and background
  this.canvas.height(this.image.height());
  this.canvas.width(this.image.width());
  this.canvas.css('background-image', 'url("'+ this.image.attr('src') +'")');  
  this.canvas.children('.image-annotate-view, .image-annotate-edit').height(this.image.height());
  this.canvas.children('.image-annotate-view, .image-annotate-edit').width(this.image.width());
  // Add the behavior: hide/show the notes when hovering the picture
  this.canvas.hover(
    function() {
      if ($(this).children('.image-annotate-edit').css('display') == 'none') {
        $(this).children('.image-annotate-view').show();
      }
    },
    function() {
      $(this).children('.image-annotate-view').hide();
    }
  );
  this.canvas.children('.image-annotate-view').hover(
    function() {
      $(this).show();
    },
    function() {
      $(this).hide();
    }
  );
  // Create the notes
  this.notes = new Array();
  for (var i=0; i<image.notes.length; i++) {
    this.notes[image.notes[i].aid] = new Drupal.imageAnnotation(this, image.notes[i]);
  }
  // Add the "Add a note" button
  if (image.editable) {
    this.button = $('<a class="image-annotate-add" id="image-annotate-add-'+ image.field +'">' + Drupal.t('Add a note') + '</a>');
    var image = this;
    this.button.click(function(){
      image.addNote();
    });
    this.image.before(this.button);
  }
  // Hide the original
  this.image.hide();
};

/**
 * Highlight and show one of the notes
 */
Drupal.annotativeImage.prototype.showNote = function(aid) {
  for (key in this.notes) {
    if (key == aid) {
      var highlight = this.notes[key];
    }
    this.notes[key].hide();
  }
  if (highlight) {
    this.canvas.children('.image-annotate-view').show();
    highlight.show();
    $('html, body').animate({scrollTop: highlight.area.offset().top}, 'slow'); // Hack with html & body scrolling so that it works in Safari
  }
};

/**
 * Add a note
 */
Drupal.annotativeImage.prototype.addNote = function () {
  if (this.mode == 'view') {
    this.mode = 'edit';
    var image = this;
    // Create/prepare the editable note elements
    var editable = new Drupal.imageAnnotationEditable(this);
    // Load the form and set the draggable/resizable area
    editable.note.load(Drupal.settings.basePath + 'content/image-annotate/create/' + this.nid, {}, function() {
      Drupal.behaviors.collapse(editable.note);
      var form = $('#image-annotate-edit-form form');
      // TODO: remove these *EVIL* fixes
      form.attr('action', Drupal.settings.basePath + 'comment/reply/' + image.nid); /* Evil! */
      $('#image-annotate-edit-form input').attr('for', ''); /* Kicking babies evil! */
      // Add the image note information to the form action on submission
      form.submit(function() {
        var areaFields = $('<input type="hidden" value="1" name="image-annotate"/>'+
          '<input type="hidden" value="'+ editable.area.height() +'" name="image-annotate-height"/>'+
          '<input type="hidden" value="'+ editable.area.width() +'" name="image-annotate-width"/>'+
          '<input type="hidden" value="'+ editable.area.position().top +'" name="image-annotate-top"/>'+
          '<input type="hidden" value="'+ editable.area.position().left +'" name="image-annotate-left"/>'+
          '<input type="hidden" value="'+ editable.image.field +'" name="image-annotate-field"/>');
        form.append(areaFields);
      });
      // We add the cancel/close button
      var cancel = $('<a class="image-annotate-edit-close">'+ Drupal.t('Cancel') +'</a>');
      cancel.click(function() {
        editable.destroy();
        image.mode = 'view';
      });
      editable.note.prepend(cancel);
    });
  }
};

/**
 * An image annotation
 */
Drupal.imageAnnotation = function (image, note) {
  this.image = image;
  this.aid = note.aid;
  this.cid = note.cid;
  this.height = note.height;
  this.width = note.width;
  this.left = note.left;
  this.top = note.top;
  this.editable = note.editable;
  // Add the area
  this.area = $('<div class="image-annotate-area'+ (this.editable ? ' image-annotate-area-editable' : '') +'"><div></div></div>');
  this.image.canvas.children('.image-annotate-view').prepend(this.area);
  // Add the note
  this.note = $('<div class="image-annotate-note">'+ note.text +'</div>');
  this.note.hide();
  this.image.canvas.children('.image-annotate-view').append(this.note);
  this.note.children('span.actions').hide();
  // Set the position and size of the note
  this.set();
  // Add the behavior: hide/display the note when hovering the area
  var annotation = this;
  this.area.hover(
    function() {
      annotation.show();
    },
    function() {
      annotation.hide();
    }
  );
  this.note.hover(
    function(){
      annotation.show();
      annotation.note.children('span.actions').show('slow');
    },
    function(){
      annotation.hide();
      annotation.note.children('span.actions').hide();
    }
  );
  // Edit a note feature
  if (this.editable) {
    var note = this;
    this.area.click(function () {
      note.edit();
    });
  }
};

/**
 * Set the position and size of the note
 */
Drupal.imageAnnotation.prototype.set = function() {
  this.area.children('div').height((this.height - 2) +'px');
  this.area.children('div').width((this.width - 2) +'px');
  this.area.css('left', (this.left) +'px');
  this.area.css('top', (this.top) +'px');
  this.note.css('left', (this.left) +'px');
  this.note.css('top', (parseInt(this.top) + parseInt(this.height) + 2) +'px');
};

/**
 * Highlight/show the note
 */
Drupal.imageAnnotation.prototype.show = function() {
  this.note.show();
  this.area.addClass('image-annotate-area-hover');
};

/**
 * Hide the note
 */
Drupal.imageAnnotation.prototype.hide = function() {
  this.note.hide();
  this.area.removeClass('image-annotate-area-hover');
};

/**
 * Show the note edit form
 */
Drupal.imageAnnotation.prototype.edit = function() {
  if (this.image.mode == 'view') {
    this.image.mode = 'edit';
    var note = this;
    // Create/prepare the editable note elements
    var editable = new Drupal.imageAnnotationEditable(this.image, this);
    // Load the form and set the draggable/resizable area
    editable.note.load(Drupal.settings.basePath + 'content/image-annotate/edit/' + this.aid, {}, function() {
      Drupal.behaviors.collapse(editable.note);
      var form = $('#image-annotate-edit-form form');
      // TODO: remove these *EVIL* fixes
      form.attr('action', Drupal.settings.basePath + 'comment/edit/'+ note.cid); /* Evil! */
      $('#image-annotate-edit-form input').attr('for', ''); /* Kicking babies evil! */
      // Add the image note information to the form action on submission
      form.submit(function() {
        var areaFields = $('<input type="hidden" value="1" name="image-annotate"/>'+
          '<input type="hidden" value="'+ editable.area.height() +'" name="image-annotate-height"/>'+
          '<input type="hidden" value="'+ editable.area.width() +'" name="image-annotate-width"/>'+
          '<input type="hidden" value="'+ editable.area.position().top +'" name="image-annotate-top"/>'+
          '<input type="hidden" value="'+ editable.area.position().left +'" name="image-annotate-left"/>'+
          '<input type="hidden" value="'+ editable.image.field +'" name="image-annotate-field"/>');
        form.append(areaFields);
        // var areaInfo = editable.area.position().top +'/'+ editable.area.position().left +'/'+ editable.area.height() +'/'+ editable.area.width();
        // 	  form.attr('action', form.attr('action') +'/image-annotate/'+ areaInfo);
      });
      // We add the cancel/close button
      var cancel = $('<a class="image-annotate-edit-close">'+ Drupal.t('Cancel') +'</a>');
      cancel.click(function() {
        editable.destroy();
        note.image.mode = 'view';
      });
      editable.note.prepend(cancel);
    });
  }
};

/**
 * The annotation form
 */
Drupal.imageAnnotationEditable = function (image, note) {
  this.image = image;
  // Set up the area
  this.area = this.image.canvas.children('.image-annotate-edit').children('.image-annotate-edit-area');
  if (note) {
    this.area.css('height', note.height +'px');
    this.area.css('width', note.width +'px');
    this.area.css('left', note.left +'px');
    this.area.css('top', note.top +'px');
  }
  // Show the edition canvas and hide the view canvas
  this.image.canvas.children('.image-annotate-view').hide();
  this.image.canvas.children('.image-annotate-edit').show();
  // Add the note (which we'll load with the form afterwards)
  this.note = $('<div id="image-annotate-edit-form"></div>');
  $('body').append(this.note);
  this.note.css('left', this.area.offset().left +'px');
  this.note.css('top', (parseInt(this.area.offset().top) + parseInt(this.area.height()) + 2) +'px');
  // Set the area as a draggable/resizable element contained in the image canvas.
  // Would be better to use the containment option for resizable but buggy
  var editable = this;
  var area = this.area;
  var note = this.note;
  this.area.resizable({
    handles: 'all',
	  resize: function(e, ui) {
      if (parseInt(area.position().top) + parseInt(area.height()) + 2 > parseInt(editable.image.canvas.height())) {
        area.height(parseInt(editable.image.canvas.height()) - parseInt(area.position().top) - 2);
      }
      if (parseInt(area.position().left) + parseInt(area.width()) + 2 > parseInt(editable.image.canvas.width())) {
        area.width(parseInt(editable.image.canvas.width()) - parseInt(area.position().left) - 2);
      }
      if (parseInt(area.position().top) < 0) {
        area.height(parseInt(editable.image.canvas.height())).css('top', 0);
      }
      if (parseInt(area.position().left) < 0) {
        area.width(parseInt(editable.image.canvas.width())).css('left', 0);
      }
      note.css('left', area.offset().left +'px');
	  note.css('top', (parseInt(area.offset().top) + parseInt(area.height()) + 2) +'px');
    },
    stop: function(e, ui) {
      note.css('left', area.offset().left +'px');
	  note.css('top', (parseInt(area.offset().top) + parseInt(area.height()) + 2) +'px');
    }
  })
  .draggable({
    containment: editable.image.canvas,
    drag: function(e, ui) {
      note.css('left', area.offset().left +'px');
	  note.css('top', (parseInt(area.offset().top) + parseInt(area.height()) + 2) +'px');
	},
    stop: function(e, ui) {
      note.css('left', area.offset().left +'px');
	  note.css('top', (parseInt(area.offset().top) + parseInt(area.height()) + 2) +'px');
    }
  });
};

/**
 * Destroy the annotation form
 */
Drupal.imageAnnotationEditable.prototype.destroy = function () {
  this.image.canvas.children('.image-annotate-edit').hide();
  this.area.resizable('destroy');
  this.area.draggable('destroy');
  this.area.css('height', '');
  this.area.css('width', '');
  this.area.css('left', '');
  this.area.css('top', '');
  this.note.remove();
};
