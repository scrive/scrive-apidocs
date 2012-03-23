/* Signatory view of document
 */


(function(window) {

window.DocumentSignViewHeader = Backbone.View.extend({
 initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('reset', this.render);
    this.model.bind('change', this.render);
    this.render();
  },
  tagName : "div",
  updateHeaderSenderPosition : function() {
    if ($(window).width() < 1150) {
        this.sender.addClass("shoved");
    } else {
        this.sender.removeClass("shoved");
    }
  },
  render: function() {
    var view = this;
    var document = this.model;
    if (!document.ready()) return this;
    var maindiv = $(this.el)

    maindiv.addClass("pageheader");
    maindiv.addClass(document.logo() == undefined ? 'withstandardlogo': 'withcustomlogo');
    if (document.barsbackgroundcolor() != undefined)
    {
        maindiv.css('background-image','none');
        maindiv.css('background-color',document.barsbackgroundcolor());
    }
    if (document.barsbackgroundtextcolor() != undefined)
    {
        maindiv.css("color",document.barsbackgroundtextcolor());
    }

    var content = $("<div class='content' />")
    var logowrapper = $("<div class='logowrapper' />");
    if (document.logo() == undefined) {
        logowrapper.append("<a href='/'><div class='logo'></div></a>")
    }
    else {
        logowrapper.append("<img class='logo' src='"+document.logo()+"'></img>");
    }
        
    this.sender = $("<div class='sender' />");
    var name =  $("<div class='name' />").text(document.authoruser().fullname());
    var phone =  $("<div class='phone' />").text(document.authoruser().phone());
    this.sender.append(name).append(phone);
    this.updateHeaderSenderPosition();
    $(window).resize(function(){ view.updateHeaderSenderPosition();});
    
    content.append(logowrapper).append(this.sender).append("<div class='clearboth'/>");
    maindiv.append(content);
    return this;
  }
});

window.DocumentSignViewFooter = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('reset', this.render);
    this.model.bind('change', this.render);
    this.render();
  },
  tagName : "div",
  render: function() {
    var document = this.model;
    if (!document.ready()) return this;
    var maindiv = $(this.el)
    maindiv.addClass("pagefooter");
    if (document.barsbackgroundcolor() != undefined)
    {
        maindiv.css('background-image','none');
        maindiv.css('background-color',document.barsbackgroundcolor());
    }

    if (document.barsbackgroundtextcolor() != undefined)
    {
        maindiv.css("color",document.barsbackgroundtextcolor());
    }
    var content = $("<div class='content' />")
    var dogtooth = $("<div class='dogtooth' />");
    var sender = $("<div class='sender' />");
    var name =  $("<div class='name' />").text(document.authoruser().fullname());
    var position =  $("<div class='position' />").text(document.authoruser().position());
    var company =  $("<div class='company' />").text(document.authoruser().company());
    var phone =  $("<div class='phone' />").text(document.authoruser().phone());
    var email =  $("<div class='email' />").text(document.authoruser().email());

    var powerdiv = $("<div class='poweredbyscrive'/>").append($("<a href='/'>").text(localization.poweredByScrive));

    sender.append(name).append(position).append(company).append(phone).append(email);
    content.append(sender).append(powerdiv).append("<div class='clearboth'/>");
    maindiv.append(dogtooth.append(content));
    return this;
  }
});

})(window);
