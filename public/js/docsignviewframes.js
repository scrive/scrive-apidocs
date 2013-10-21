/* Top and bottom of signview */


(function(window) {

window.DocumentSignViewHeader = Backbone.View.extend({
 initialize: function(args) {
     _.bindAll(this, 'render');
     this.model.bind('reset', this.render);
     this.model.bind('change', this.render);
     this.prerender();
     this.render();

  },
  prerender : function() {
        var maindiv = $(this.el);
        maindiv.addClass("pageheader");
        this.content = $("<div class='content' />");
        this.logowrapper = $("<div class='logowrapper' />");
        this.sender = $("<div class='sender' />");
        this.inner = $('<div class="inner" />');
        this.content.append(this.logowrapper).append(this.sender.append(this.inner)).append("<div class='clearfix'/>");
        maindiv.append(this.content);
  },
  refresh : function() {
      var el = $(this.el);
      var tbd = $("<span/>");
      $(this.el).append(tbd);
      setTimeout(function() {tbd.remove();},1);
      if (!$.browser.msie) {
        var width = Math.max($('body').width(),$(document).width());
        if (width > 1020)
          el.css("min-width",width + "px");
        var pti = $(".padTopIcon");
        if (pti.size() > 0)
            pti.css("left", width - pti.width() - 2);
      }
  },
  render: function() {
    var view = this;
    var model = this.model;
    var maindiv = $(this.el);
    if (!this.model.ready()) {
      maindiv.css("display", "none");
      return this;
    }

    if (BrowserInfo.isSmallScreen()) {
      maindiv.css("display", "none");
      return this;
    }

    maindiv.css("display","block");

    // Setting logo

    var img = $("<img class='logo'></img>");
    img.load(function(){  view.refresh();  });

    img.attr('src', (model.signviewlogo() != undefined && model.signviewlogo() != "") ? model.signviewlogo() : '/img/logo.png');
    this.logowrapper.empty().append(img);

    // Background color of top bar
    if( model.signviewbarscolour() == undefined) {
        maindiv.css('background-image', '');
        maindiv.css('background-color', '');
    }
    else {
        maindiv.css('background-image', 'none');
        maindiv.css('background-color', model.signviewbarscolour());
    }

    // Text color in header
    if(model.signviewbarstextcolour() == undefined) {
        maindiv.css("color", '');
        if (this.sender != undefined) this.sender.css("color", '');
    }
    else {
        maindiv.css("color", model.signviewbarstextcolour());
        if (this.sender != undefined) this.sender.css("color", model.signviewbarstextcolour());
    }

    // Text font in header
    if(model.signviewtextfont() == undefined) {
        maindiv.css('font-family', '');
        if (this.sender != undefined) this.sender.css('font-family', '');
    }
    else {
        maindiv.css('font-family', model.signviewtextfont());
        if (this.sender != undefined) this.sender.css('font-family', model.signviewtextfont());
    }

    // Text in header | Scrive or Author details
    var name = $("<div class='name' />").text(model.fullname());
    var phone = $("<div class='phone' />").text(model.phone());
    this.inner.empty().append(name).append(phone);

    return this;
  }
});



window.DocumentSignViewFooter = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render' );
    this.model.bind('reset', this.render);
    this.model.bind('change', this.render);
    this.prerender();
    this.render();
  },
  prerender : function() {
        var maindiv = $(this.el);
        maindiv.addClass("pagefooter");
        this.content = $("<div class='content' />");
        this.pbs = $("<div class='poweredbyscrive' />");
        this.content.append(this.pbs).append("<div class='clearfix'/>");
        maindiv.append(this.content);
  },
  render: function() {
    var view = this;
    var model = this.model;
    var maindiv = $(this.el);
    if (!model.ready()) {
      maindiv.css("display", "none");
      return this;
    }
    maindiv.css("display","block");

    if (BrowserInfo.isSmallScreen())
      maindiv.addClass("small-screen");

   // Background color of top bar
    if(model.signviewbarscolour() == undefined) {
        maindiv.css('background-image', '');
        maindiv.css('background-color', '');
    }
    else {
        maindiv.css('background-image', 'none');
        maindiv.css('background-color', model.signviewbarscolour());
    }

    // Text color in footer
    if(model.signviewbarstextcolour() == undefined) {
        maindiv.css("color", '');
    }
    else  {
        maindiv.css("color", model.signviewbarstextcolour());
    }

    // Text font in footer
    if(model.signviewtextfont() == undefined) {
        maindiv.css('font-family', '');
    }
    else  {
        maindiv.css('font-family', model.signviewtextfont());
    }

    if(model.signviewbarscolour() != undefined) {
        var pbstext = $("<span class='text' />").text("Powered by Scrive");
        this.pbs.empty().append(pbstext);
    }
    else {
        var pbstext = $("<div class='text' />").text("Powered by ");
        var pbslogo = $("<span class='logo' />");
        this.pbs.empty().append(pbstext).append(pbslogo);
    }

    return this;
  }
});

})(window);
