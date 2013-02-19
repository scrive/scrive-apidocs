/*Module for basic system schema called tabs. Like three steps in design view or Conteacts | Offers | Orders | .. in Archive
 *
 * Tab contails of name and body,
 * Tabs is collection of Tabs.
 * By default first one is active.
 *
 *
 */
(function(window){


window.Tab = Backbone.Model.extend({
  defaults: {
    pagehash : undefined,
    active : false,
    disabled : false,
    clickable : true,
    iconClass : undefined,
    onActivate : function() {}
    }
  ,
  initialize : function(args){
       var self = this;
       if (this.pagehash() != undefined && window.location.hash == "#" + this.pagehash())
          this.set({active: true}, {silent: true});

       if (this.active())
          this.get("onActivate")();
  },
  name : function() {
      return this.get("name");
  },
  pagehash : function() {
      return this.get("pagehash");
  },
  iconClass : function() {
      return this.get("iconClass");
  },
  right : function() {
      return this.get("right") == true;
  },
  elems : function() {
      return this.get("elems");
  },
  setActive : function(bool) {
      this.set({active: bool});
      if (bool) {
          if (this.pagehash() != undefined)
            window.location.hash = this.pagehash();
          this.get("onActivate")();
      }
  },
  active : function() {
        return this.get("active");
  },
  disabled : function() {
        return this.get("disabled");
  },
  clickable : function() {
        return this.get("clickable");
  },
  number : function() {
        return this.get("number");
  },
  hasNumber : function() {
     return this.number() != undefined;
  },
  setNumber: function(number)
  {
       this.set({number:number});
  },
  initateAll: function() {
      var elems = this.elems();
      for(var i = 0; i< elems.length; i ++ )
          if (!(elems[i] instanceof jQuery))
              elems[i] = elems[i]();
  }
});



var Tabs = Backbone.Model.extend({
   defaults: {
       numbers : false
    },
   numbers : function() {
     return   this.get("numbers") == true;
   },
   initialize : function(args){
       if (_.all(args.tabs,function(t) {return !t.active(); }))
          this.activate(args.tabs[0]);
       if (this.numbers())
        this.addTabsNumbers();
   },
   activate: function(newtab)
   {
        var tabs = this.tabs();
        for(var i=0;i<tabs.length;i++)
            tabs[i].setActive(newtab === tabs[i]);
        this.trigger("change");
   },
   activateNext : function() {
        var tabs = this.tabs();
        var next = this.activeTab();
        for(var i=tabs.length-1;i>=0 && !tabs[i].active() ;i--)
            if (!tabs[i].disabled())
                next = tabs[i];
        this.activate(next);
   },
   hideAll: function()
   {    var tabs = this.tabs();
        for(var i=0;i<tabs.length;i++)
        {
            _.each(tabs[i].elems(), function(e) {
              if (e instanceof jQuery)
                e.hide();
            });
        }
   },
   activeTab : function()
   {    var tabs = this.tabs();
        for(var i=0;i<tabs.length;i++)
          if (tabs[i].active())
              return tabs[i];

   },
   tabs : function(){
        return this.get("tabs");
   },
   hasManyTabs : function() {
     var notdisabled = 0;
     var tabs = this.tabs();
        for(var i=0;i<tabs.length;i++)
            if (!tabs[i].disabled()) notdisabled++;
     return notdisabled > 1;
   },
   addTabsNumbers : function() {
     var number = 1;
     var tabs = this.tabs();
        for(var i=0;i<tabs.length;i++)
            if (!tabs[i].disabled()) {
                  tabs[i].setNumber(number);
                  number++;
            }
   },
   tabsTail: function(){
     return this.get("tabsTail");
   }
});


var TabsView = Backbone.View.extend({
    model: Tabs,
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.extrasInTabsRow = args.extrasInTabsRow;
        this.model.bind('change', this.render);
        this.model.view = this;
        this.prerender();
        this.render();
    },
    prerender: function(){
        var container = $(this.el);
        container.addClass("tab-viewer");
        this.toprow = $("<div class='tab-viewer-header'/>");
        container.append(this.toprow);
    },
    render: function () {
        var container = $(this.el);
        var tabsview = this;
        this.toprow.children().detach();
        var tabsrow = $("<ul class='tabs'/>");
        var model = this.model;
        var hasRight = false;
        _.each(this.model.tabs(), function(tab)
        {
            if (tab.disabled()) return;
            var li = $("<li/>");
            if (tab.right()) {
                li.addClass("float-right");
                hasRight = true;
            }
            else
                li.addClass("float-left");
            var icon = $("<h5/>");
            if (tab.hasNumber())
                icon.addClass("numbericon").addClass("numbericon" + tab.number());
            icon.text(tab.name());
            if (tab.iconClass() != undefined){
                icon.addClass(tab.iconClass());
                li.addClass('withIcon');
            }
            li.append(icon);

            if (tab.active())
                li.addClass("active");
            li.click(function() {
                    if (tab.clickable())
                        model.activate(tab);
                    return false;
                    });
            tabsrow.append(li);
        });
        if (!hasRight && model.tabsTail() == undefined)
            tabsrow.append("<li class='float-right empty'/>");
        if (model.hasManyTabs())
            this.toprow.append(tabsrow);
        if (model.tabsTail() != undefined)
           _.each(model.tabsTail(), function (elem) {
           var li = $("<li class='float-right' style='padding-left:0px;padding-right:20px;'/>").append(elem);
           tabsrow.append(li);
        });


        this.model.hideAll();
        this.model.activeTab().initateAll();
        _.each(this.model.activeTab().elems(), function(e) {
            container.append(e);
            e.show();
        });
        return this;
    }
});


window.KontraTabs = function(args){
        var self = this;
        this.model = new Tabs(args);
        this.view = new TabsView({
                        model: this.model,
                        el : $("<div/>")
                    });
        $(window).hashchange(function() {
         _.each(self.model.tabs(), function(tab) {
          if (tab.pagehash() != undefined && window.location.hash == "#" + tab.pagehash() && !tab.active())
            self.model.activate(tab);
          });
        });
        return {
              model    : this.model
            , view     : this.view
            , next     : function()    { this.model.activateNext();}
            , activate : function(tab) { this.model.activate(tab);}
         };
};


})(window);
