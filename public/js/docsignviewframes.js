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
        this.content.append(this.logowrapper).append(this.sender.append(this.inner)).append("<div class='clearboth'/>");
        maindiv.append(this.content);
        this.usedStandardLogo = undefined;
        this.usedStandardColorsBars = undefined;
        this.usedStandardColorsTexts = undefined;
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

    
    if((this.useStandardBranding() || document.logo() == undefined)) {
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
        img.attr('src',document.logo());
        this.logowrapper.empty().append(img);
        this.usedStandardLogo = false;
      }  
    }
    
    // Background color of top bar
    if((this.useStandardBranding() || document.barsbackgroundcolor() == undefined)) {
      if (this.usedStandardColorsBars != true) {
        maindiv.css('background-image', '');
        maindiv.css('background-color', '');
        this.usedStandardColorsBars = true;
      }
    }
    else {
      if (this.usedStandardColorsBars != false) {
        maindiv.css('background-image', 'none');
        maindiv.css('background-color', document.barsbackgroundcolor());
        this.usedStandardColorsBars = false;
      }
    }

    // Text color in header
    if((this.useStandardBranding() || document.barsbackgroundtextcolor() == undefined)) {
      if (this.usedStandardColorsTexts != true) {
        maindiv.css("color", '');
        this.usedStandardColorsTexts = true;
      }
    }
    else {
      if (this.usedStandardColorsTexts != false) {
        maindiv.css("color", document.barsbackgroundtextcolor());
        this.usedStandardColorsTexts = false;
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
        var author = $("<div class='author' />").text((document.authoruser().fullname().trim()||document.authoruser().phone().trim())?localization.docsignview.contact:"");
        var name = $("<div class='name' />").text(document.authoruser().fullname());
        var phone = $("<div class='phone' />").text(document.authoruser().phone());
        this.inner.empty().append(author).append(name).append(phone);
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
        this.dogtooth = $("<div class='dogtooth' />");
        this.sender = $("<div class='sender' />");
        this.content.append(this.sender).append("<div class='clearboth'/>");
        maindiv.append(this.dogtooth.append(this.content));
        this.usedStandardColorsBars = undefined;
        this.usedStandardColorsTexts = undefined;
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
    if((this.useStandardBranding() || document.barsbackgroundcolor() == undefined)) {
      if (this.useStandardBranding != true) {
        maindiv.css('background-image', '');
        maindiv.css('background-color', '');
        this.usedStandardColorsBars = true;
      }
    }
    else {
      if (this.useStandardBranding != false) {
        maindiv.css('background-image', 'none');
        maindiv.css('background-color', document.barsbackgroundcolor());
        this.usedStandardColorsBars = false;
      }
    }

    // Text color in header
    if((this.useStandardBranding() || document.barsbackgroundtextcolor() == undefined)) {
      if (this.usedStandardColorsTexts != true) {
        maindiv.css("color", '');
        this.usedStandardColorsTexts = true;
      }
    }
    else  {
      if (this.usedStandardColorsTexts != false) {
        maindiv.css("color", document.barsbackgroundtextcolor());
        this.usedStandardColorsTexts = false;
      }  
    }

    // Text in header | Scrive or Author details
    if(this.useStandardBranding()) {
      if (this.usedStandardDescription != true ) {
        var name = $("<div class='name' />").text("Scrive help desk");
        var phone = $("<div class='phone' />").text("+46 8 519 779 00");
        this.sender.empty().append(name).append(phone);
        this.usedStandardDescription = true;
      }
    }
    else {
      if (this.usedStandardDescription != false ) {
        var name = $("<div class='name' />").text(document.authoruser().fullname());
        var position = $("<div class='position' />").text(document.authoruser().position());
        var company = $("<div class='company' />").text(document.authoruser().company());
        var phone = $("<div class='phone' />").text(document.authoruser().phone());
        var email = $("<div class='email' />").text(document.authoruser().email());
        this.sender.empty().append(name).append(position).append(company).append(phone).append(email);
        this.usedStandardDescription = false;
      }
    }

    return this;
  }
});

})(window);
