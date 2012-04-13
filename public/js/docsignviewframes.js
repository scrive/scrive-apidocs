/* Signatory view of document
 */


(function(window) {

window.DocumentSignViewHeader = Backbone.View.extend({
 initialize: function(args) {
     _.bindAll(this, 'render');
     this.model.document.bind('reset', this.render);
     this.model.document.bind('change', this.render);
     this.model.bind('change', this.render);
     this.render();

     /*$(window).resize(function() {
         var width = Math.max($('body').width(),$(document).width());
         if (width > $(this.el).width())
             $(this.el).width(width);
     })*/
  },
  tagName: "div",
  updateHeaderSenderPosition: function() {
    if ($(window).width() < 1150) {
        this.sender.addClass("shoved");
    } else {
        this.sender.removeClass("shoved");
    }
  },
  render: function() {
    var view = this;
    var model = this.model;
    var document = this.model.document;
    if (!document.ready()) return this;
    var maindiv = $(this.el);
    maindiv.empty();
      maindiv.removeClass();
      maindiv.attr("style","");
    maindiv.addClass("pageheader");
    if(model.hasSigned() && model.saved()) {
        maindiv.addClass('withstandardlogo');
/*
        if (document.barsbackgroundcolor() != undefined)
        {
            maindiv.css('background-image', 'none');
            maindiv.css('background-color', document.barsbackgroundcolor());
        }
        if (document.barsbackgroundtextcolor() != undefined)
            maindiv.css("color", document.barsbackgroundtextcolor());
*/
        var content = $("<div class='content' />");
        var logowrapper = $("<div class='logowrapper' />");
        logowrapper.append("<a href='/'><div class='logo'></div></a>");

    } else {
        maindiv.addClass(document.logo() == undefined ? 'withstandardlogo' : 'withcustomlogo');
        if (document.barsbackgroundcolor() != undefined)
        {
            maindiv.css('background-image', 'none');
            maindiv.css('background-color', document.barsbackgroundcolor());
        }
        if (document.barsbackgroundtextcolor() != undefined)
            maindiv.css("color", document.barsbackgroundtextcolor());

        var content = $("<div class='content' />");
        var logowrapper = $("<div class='logowrapper' />");
        if (document.logo() == undefined)
            logowrapper.append("<a href='/'><div class='logo'></div></a>");
        else
            logowrapper.append("<img class='logo' src='" + document.logo() + "'></img>");
    }
    
    this.sender = $("<div class='sender' />");
    var inner = $('<div class="inner" />');
      this.sender.append(inner);
    if(model.hasSigned() && model.saved()) {
      var name = $("<div class='name' />").text("Scrive help desk");
      var phone = $("<div class='phone' />").text("+46 8 519 779 00");
      inner.append(name).append(phone);
    } else {
      var author = $("<div class='author' />").text((document.authoruser().fullname().trim()||document.authoruser().phone().trim())?localization.docsignview.contact:"");
      var name = $("<div class='name' />").text(document.authoruser().fullname());
      var phone = $("<div class='phone' />").text(document.authoruser().phone());
      inner.append(author).append(name).append(phone);
    }
    

    this.updateHeaderSenderPosition();
    $(window).resize(function() { view.updateHeaderSenderPosition();});

    content.append(logowrapper).append(this.sender).append("<div class='clearboth'/>");
    maindiv.append(content);
    return this;
  }
});

window.DocumentSignViewFooter = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.document.bind('reset', this.render);
    this.model.document.bind('change', this.render);
    /*$(window).resize(function() {
         var width = Math.max($('body').width(),$(document).width());
         if (width > $(this.el).width())
             $(this.el).width(width);
    })*/
    this.render();
  },
  tagName: "div",
  render: function() {
    var model = this.model;
    var document = this.model.document;
    if (!document.ready()) return this;
    var maindiv = $(this.el);
    maindiv.empty();
    maindiv.addClass("pagefooter");
    if (document.barsbackgroundcolor() != undefined)
    {
        maindiv.css('background-image', 'none');
        maindiv.css('background-color', document.barsbackgroundcolor());
    }

    if (document.barsbackgroundtextcolor() != undefined)
        maindiv.css("color", document.barsbackgroundtextcolor());

    var content = $("<div class='content' />");
    var dogtooth = $("<div class='dogtooth' />");
    var powerdiv = $("<div class='poweredbyscrive'/>").append($("<a href='/'>").text(localization.poweredByScrive).css("color", document.barsbackgroundtextcolor()));
    var sender = $("<div class='sender' />");

    if(model.hasSigned() && model.saved()) {
      var name = $("<div class='name' />").text("Scrive help desk");
      var phone = $("<div class='phone' />").text("+46 8 519 779 00");
      sender.append(name).append(phone);
    } else {

      var name = $("<div class='name' />").text(document.authoruser().fullname());
      var position = $("<div class='position' />").text(document.authoruser().position());
      var company = $("<div class='company' />").text(document.authoruser().company());
      var phone = $("<div class='phone' />").text(document.authoruser().phone());
      var email = $("<div class='email' />").text(document.authoruser().email());
      sender.append(name).append(position).append(company).append(phone).append(email);
    }

    content.append(sender).append(powerdiv).append("<div class='clearboth'/>");
    maindiv.append(dogtooth.append(content));
    return this;
  }
});

})(window);
