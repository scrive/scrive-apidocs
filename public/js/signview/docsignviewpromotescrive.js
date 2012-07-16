/* All things that are shown after person had signed */


(function(window) {

window.PromoteScriveView = Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, 'render');
    this.model.bind('reset', this.render);
    this.model.bind('change', this.render);
    this.model.view = this;
    this.render();
  },
  facebookLikeButton: function() {
    var container = $("<div />");

    var button = $("<div class='facebook btn' />");
    button.append($("<div class='label' />").text(localization.docsignview.facebookButtonLabel));
    container.append(button);

    var dropdown = $("<div class='facebook dropdown' style='display:none'/>");
    dropdown.append($("<div class='content' />").append('<iframe src="//www.facebook.com/plugins/likebox.php?href=http%3A%2F%2Fwww.facebook.com%2Fpages%2FScrive%2F237391196280189&amp;width=292&amp;height=62&amp;colorscheme=light&amp;show_faces=false&amp;border_color&amp;stream=false&amp;header=false" scrolling="no" frameborder="0" style="border:none; overflow:hidden; width:292px; height:62px;" allowTransparency="true"></iframe>'));
    container.append(dropdown);

    button.mouseover(function() {
      dropdown.addClass("over");
    });

    button.mouseleave(function() {
      dropdown.removeClass("over");
    });

    button.click(function() {
      dropdown.toggle();
    });
    return container;
  },
  tweetButton: function() {
    var container = $("<a />");
    container.attr("href", "https://twitter.com/intent/tweet?screen_name=scrive");
    var twitterscript = $("<script type='text/javascript' />");
    twitterscript.attr("src", "//platform.twitter.com/widgets.js");
    console.log("adding twitter script to head");
    $("head").append(twitterscript);
    var button = $("<div class='twitter btn' />");
    button.append($("<div class='label' />").text(localization.docsignview.tweetButtonLabel));
    container.append(button);
    return container;
  },
  callMeButton: function() {
    var model = this.model;
    var container = $("<div />");
    var button = $("<div class='phone btn' />");
    button.append($("<div class='label' />").text(localization.docsignview.phoneButtonLabel));
    container.append(button);



    var dropdown = $("<div class='phone dropdown' style='display:none'/>");
    var form = $("<div />");
    form.append($("<div />").text(localization.docsignview.phoneFormDescription));
    var numberinput = $("<input type='text' />");
    form.append(numberinput);

    var callMeButton = Button.init({
      color: "green",
      size: "tiny",
      text: localization.docsignview.phoneSubmitButtonLabel,
      onClick: function() {
        var phone = numberinput.val();
        if (phone.trim().length == 0) return;
        new Submit({
            url: model.phoneurl,
            method: "POST",
            email: model.email(),
            phone: phone,
            ajax: true,
            onSend: function() {
                form.empty();
                form.append("<div class='loading'/>")
            },
            ajaxerror: function(d, a) {
                form.empty();
            },
            ajaxsuccess: function(d) {
                form.empty();
                form.append("<BR/>");
                form.append($("<div>").text(localization.docsignview.phoneConfirmationText));
            }
        }).send();
     }
    });
    form.append(callMeButton.input());

    numberinput.keypress(function(e) {
      if (e.which == 13) {
        callMeButton.clicked();
      }
    });

    form.append($("<div class='clearfix' />"));
    dropdown.append(form);
    container.append(dropdown);
    
    button.mouseover(function() {
      dropdown.addClass("over");
    });

    button.mouseleave(function() {
      dropdown.removeClass("over");
    });

    button.click(function() {
      dropdown.toggle();
    });
    return container;
  },
  toServiceButton: function() {
    var container = $("<a href='/upload'/>")
    var button = $("<div class='start btn' />");
    button.append($("<div class='label' />").text(localization.docsignview.startButtonLabel));
    container.append(button);
    return container;
  },
  render: function() {
    $(this.el).empty();

    var container = $("<div class='share' />");

    container.append($("<div class='title'/>").text(localization.docsignview.shareTitle));
    var panel = $("<div class='panel ' />");
    panel.append($("<div class='item' />").append(this.facebookLikeButton()));
    panel.append($("<div class='item' />").append(this.tweetButton()));
    panel.append($("<div class='item' />").append(this.callMeButton()));
    panel.append($("<div class='item' />").append(this.toServiceButton()));
    panel.append($("<div class='clearfix' />"));
    container.append(panel);
    var videobox = $('<div class="sbox"><div class="video">' 
                    +  '<iframe src="https://player.vimeo.com/video/41846881" width="620" height="330" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe>'
                    +'</div></div>');
    container.append(videobox);
    $(this.el).append(container);
    return this;
  }
});

})(window);
