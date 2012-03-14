/* Main and only flash messages module
 * Usage
 *   FlashMessages.add({ color : "red | green | blue" , content: "Text to be displayed"})
 *
 * Adding next flash messages when previous one is active will deactivate previous one.
 */

$(function(){

/* Flash mesage contains text and color.
 * It can be active, but after 10 sec it will trigger deactive event.
  */
var FlashMessage = Backbone.Model.extend({
  initialize: function(attr) {
       if (!(attr.color == "red" || attr.color == "blue" || attr.color == "green"))
            console.log("FlashMessage error: Bad color selected ( "  + attr.color +" )");
        if (!_.isString(attr.content) || attr.content.length === 0 )
            console.log("FlashMessage error: No content provided");
  },
  isActive : function() {
      return this.has("active");
  },
  activate : function(){
      if (!this.isActive()){
          this.set({"active": "true"});
          var current = this;
          setTimeout(function(){current.deactivate();},10000);
      }
  },
  deactivate: function() {
      this.trigger("deactivate");
  }
});

/* Collection of flash messages. There will be only one instance in the system
 * It tries to keep last flash message active if it is not empty.
 */
var FlashMessagesList = Backbone.Collection.extend({
    model: FlashMessage ,
    initialize: function (args) {
        this.bind('add', this.reactivate);
        this.bind('remove', this.reactivate);
    },
    reactivate: function(){
        if (!this.isEmpty())
        {
            for(i=0;i<this.length-1;i++)
                this.at(i).deactivate();
            this.last().activate();
        }

    },
    closeCurrent: function() {
        if (!this.isEmpty() && this.first().isActive())
            this.first().deactivate();

    }
});

/* Basic view of flash message.
 * It can render its look and feel
 * On deactivate event will try to gracefully hide
 * flash message and inform the model that it can be destroyed.
 */
var FlashMessageView = Backbone.View.extend({

    initialize: function (args) {
        _.bindAll(this, 'render', 'hide');
        this.model.bind('deactivate', this.hide);
        this.model.view = this;
        this.render();
    },
    render: function () {
        this.el = $("<div class='flash-container " + this.model.get("color") + "'/>")
            .append($("<div class='flash-content'> </div>")
                    .append("<div class='scrive-logo float-left'></div>")
                    .append("<div class='flash-icon " + this.model.get("color") + "'></div>" )
                    .append($("<div class='flash-body'></div>").append(this.model.get("content")))
                    .append("<div class='flash-close modal-close'></div>")
                   );
        return this;
    },
    hide: function(){
        var view = this;
        var model = this.model;
        $(this.el).parent().slideUp(800, function(){
            view.remove();
            model.destroy();
        });
    }
});

/* View for flash message list.
 * It makes sure that each flash message added to the list will have a view
 * and it can agragete rendered views of active flash messages
 */
var FlashMessagesView = Backbone.View.extend({
    events: {
        'click .flash-close': 'closeCurrent'
    },
    initialize: function (args) {
        _.bindAll(this, 'addFlashMessage', 'render');
        this.model.bind('add', this.addFlashMessage);
        this.model.bind('remove', this.render);
        this.model.view = this;
    },
    render: function () {
        var el = $(this.el);
        if (this.model.isEmpty())
            $(this.el).hide();
        else {
            $(this.el).empty();
            var el = $(this.el);
            this.model.forEach(function(e){
                if (e.isActive() && e.view != undefined)
                    el.append($(e.view.el));
            });
           if ($(this.el).css("display") == "none")
               $(this.el).slideDown(800, function() {
                   el.css("height","");
                   el.show();

            });
        }
    },
    addFlashMessage : function(fm){new FlashMessageView({model: fm}); this.render() ;},
    closeCurrent: function () { this.model.closeCurrent(); }
});


/* Globally visible list of flash messages
 * Its view is bind to .flashmsgbox dom element
 */
    window.FlashMessages = new FlashMessagesList;
    new FlashMessagesView({model: FlashMessages,  el:  $(".flashmsgbox")});
});
