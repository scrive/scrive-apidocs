/* Signatory view of document
 */


(function(window) {

window.DocumentSignViewHeader = Backbone.View.extend({
 initialize: function(args) {
     this.mainview = args.mainview;
     _.bindAll(this, 'render' ,'refresh');
     this.model.document().bind('reset', this.render);
     this.model.document().bind('change', this.render);
     this.model.bind('change', this.render);
     this.mainview.bind('change:task', this.refresh);
     this.prerender();
     this.render();

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
  updateHeaderSenderPosition: function() {
    if ($(window).width() < 1150) {
        this.sender.addClass("shoved");
    } else {
        this.sender.removeClass("shoved");
    }
  },
  useStandardBranding : function() {
    return this.model.document().currentSignatory() != undefined && this.model.document().currentSignatory().hasSigned() &&  this.model.justSaved();
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
        this.usedStandardLogo = undefined;
        this.usedStandardColorsBars = undefined;
        this.usedStandardColorsTexts = undefined;
        this.usedStandardTextsFonts = undefined;
        this.usedStandardDescription = undefined;
  },
  render: function() {
    var view = this;
    var model = this.model;
    var document = this.model.document();
    var maindiv = $(this.el);
    if (!document.ready()) {
      maindiv.css("display", "none");
      return this;
    }
    maindiv.css("display","block");

    // Setting logo


    if((this.useStandardBranding() || document.signviewlogo() == undefined)) {
       if (this.usedStandardLogo != true) {
         maindiv.removeClass('withcustomlogo').addClass('withstandardlogo');
         this.logowrapper.empty().append("<a href='/'><div class='logo'></div></a>");
         this.usedStandardLogo = true;
       }
    }
    else {
      if (this.usedStandardLogo != false) {
        maindiv.removeClass('withstandardlogo').addClass('withcustomlogo');
        var img = $("<img class='logo'></img>");
        img.load(function(){  view.refresh();  });
        img.attr('src',document.signviewlogo());
        this.logowrapper.empty().append(img);
        this.usedStandardLogo = false;
      }
    }

    // Background color of top bar
    if((this.useStandardBranding() || document.signviewbarscolour() == undefined)) {
      if (this.usedStandardColorsBars != true) {
        maindiv.css('background-image', '');
        maindiv.css('background-color', '');
        this.usedStandardColorsBars = true;
      }
    }
    else {
      if (this.usedStandardColorsBars != false) {
        maindiv.css('background-image', 'none');
        maindiv.css('background-color', document.signviewbarscolour());
        this.usedStandardColorsBars = false;
      }
    }

    // Text color in header
    if((this.useStandardBranding() || document.signviewbarstextcolour() == undefined)) {
      if (this.usedStandardColorsTexts != true) {
        maindiv.css("color", '');
        if (this.sender != undefined) this.sender.css("color", '');
        this.usedStandardColorsTexts = true;
      }
    }
    else {
      if (this.usedStandardColorsTexts != false) {
        maindiv.css("color", document.signviewbarstextcolour());
        if (this.sender != undefined) this.sender.css("color", document.signviewbarstextcolour());
        this.usedStandardColorsTexts = false;
      }
    }

    // Text font in header
    if((this.useStandardBranding() || document.signviewtextfont() == undefined)) {
      if (this.usedStandardTextsFonts != true) {
        maindiv.css('font-family', '');
        if (this.sender != undefined) this.sender.css('font-family', '');
        this.usedStandardTextsFonts = true;
      }
    }
    else {
      if (this.usedStandardTextsFonts != false) {
        maindiv.css('font-family', document.signviewtextfont());
        if (this.sender != undefined) this.sender.css('font-family', document.signviewtextfont());
        this.usedStandardTextsFonts = false;
      }
    }

    // Text in header | Scrive or Author details
    if(this.useStandardBranding()) {
      if (this.usedStandardDescription != true) {
        var name = $("<div class='name' />").text("Scrive help desk");
        var phone = $("<div class='phone' />").text("+46 8 519 779 00");
        this.inner.empty().append(name).append(phone);
        this.usedStandardDescription = true;
      }
    }
    else {
      if (this.usedStandardDescription != false) {
        var name = $("<div class='name' />").text(document.authoruser().fullname());
        var phone = $("<div class='phone' />").text(document.authoruser().phone());
        this.inner.empty().append(name).append(phone);
        this.usedStandardDescription = false;
      }
    }

    this.updateHeaderSenderPosition();
    $(window).resize(function() { view.updateHeaderSenderPosition();});
    return this;
  }
});



window.DocumentSignViewFooter = Backbone.View.extend({
  initialize: function(args) {
    this.mainview = args.mainview;
    _.bindAll(this, 'render' , 'refresh');
    this.model.document().bind('reset', this.render);
    this.model.document().bind('change', this.render);
    this.mainview.bind('change:task', this.refresh);
    this.prerender();
    this.render();
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
      }
  },
  useStandardBranding : function() {
    return this.model.document().currentSignatory() != undefined && this.model.document().currentSignatory().hasSigned() &&  this.model.justSaved();
  },
  prerender : function() {
        var maindiv = $(this.el);
        maindiv.addClass("pagefooter");
        this.content = $("<div class='content' />");
        this.pbs = $("<div class='poweredbyscrive' />");
        this.content.append(this.pbs).append("<div class='clearfix'/>");
        maindiv.append(this.content);
        this.usedStandardColorsBars = undefined;
        this.usedStandardColorsTexts = undefined;
        this.usedStandardTextsFonts = undefined;
        this.usedStandardDescription = undefined;
  },
  render: function() {
    var view = this;
    var model = this.model;
    var document = this.model.document();
    var maindiv = $(this.el);
    if (!document.ready()) {
      maindiv.css("display", "none");
      return this;
    }
    maindiv.css("display","block");

   // Background color of top bar
    if((this.useStandardBranding() || document.signviewbarscolour() == undefined)) {
      if (this.useStandardBranding != true) {
        maindiv.css('background-image', '');
        maindiv.css('background-color', '');
        this.usedStandardColorsBars = true;
      }
    }
    else {
      if (this.useStandardBranding != false) {
        maindiv.css('background-image', 'none');
        maindiv.css('background-color', document.signviewbarscolour());
        this.usedStandardColorsBars = false;
      }
    }

    // Text color in footer
    if((this.useStandardBranding() || document.signviewbarstextcolour() == undefined)) {
      if (this.usedStandardColorsTexts != true) {
        maindiv.css("color", '');
        this.usedStandardColorsTexts = true;
      }
    }
    else  {
      if (this.usedStandardColorsTexts != false) {
        maindiv.css("color", document.signviewbarstextcolour());
        this.usedStandardColorsTexts = false;
      }
    }

    // Text font in footer
    if((this.useStandardBranding() || document.signviewtextfont() == undefined)) {
      if (this.usedStandardTextsFonts != true) {
        maindiv.css('font-family', '');
        this.usedStandardTextsFonts = true;
      }
    }
    else  {
      if (this.usedStandardTextsFonts != false) {
        maindiv.css('font-family', document.signviewtextfont());
        this.usedStandardTextsFonts = false;
      }
    }

    if(document.signviewbarscolour() != undefined) {
      if (this.usedStandardDescription != true ) {
        var pbstext = $("<span class='text' />").text("Powered by Scrive");
        this.pbs.empty().append(pbstext);
        this.usedStandardDescription = true;
      }
    }
    else {
      if (this.usedStandardDescription != false ) {
        var pbstext = $("<span class='text' />").text("Powered by ");
        var pbslogo = $("<span class='logo' />");
        this.pbs.empty().append(pbstext).append(pbslogo);
        this.usedStandardDescription = false;
      }
    }

    return this;
  }
});

})(window);
