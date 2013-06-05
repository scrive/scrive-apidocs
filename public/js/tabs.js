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
    available : true,
    clickable : true,
    iconClass : undefined,
    onActivate : function() {},
    onShow : function() {},
    onHide : function() {}
    }
  ,
  initialize : function(args){
       var self = this;
       if (this.currentHashMatches())
          this.set({active: true}, {silent: true});

       if (this.active())
          this.onActivate();
  },
  onActivate : function(){
      return this.get("onActivate")();
  },
  name : function() {
      return this.get("name");
  },
  pagehash : function() {
      return this.get("pagehash");
  },
  currentHashMatches : function() {
    if (this.pagehash() == undefined)
        return false;
    else if (this.pagehash() instanceof Array) {
        for(var i = 0; i < this.pagehash().length; i++) {
          if (window.location.hash == "#" + this.pagehash()[i])
            return true;
        }
        return false;
    }
    else
      return window.location.hash == "#" + this.pagehash()   ;
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
            if (this.pagehash() instanceof Array)
              window.location.hash = this.pagehash()[0];
            else
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
  initateAll: function() {
      var elems = this.elems();
      for(var i = 0; i< elems.length; i ++ )
          if (!(elems[i] instanceof jQuery))
              elems[i] = elems[i]();
  },
  onShow : function() {
     return this.get("onShow")();
  },
  onHide : function() {
     return this.get("onHide")();
  },
  available : function() {
     return this.get('available');
  },
  setAvailable : function(b) {
     this.set({'available' : b});
  }
});



var Tabs = Backbone.Model.extend({
   defaults: {
       canHaveNoActiveTab : false,
       slideEffect : false
    },
   initialize : function(args){
       if (!this.canHaveNoActiveTab() && _.all(args.tabs,function(t) {return !t.active(); }))
          this.activate(args.tabs[0]);
   },
   activate: function(newtab)
   {
        if (newtab.active()) return;
        var tabs = this.tabs();
        for(var i=0;i<tabs.length;i++)
            tabs[i].setActive(newtab == tabs[i]);
        this.trigger("change");
   },
   deactive : function() {
        var tabs = this.tabs();
        for(var i=0;i<tabs.length;i++)
            tabs[i].setActive(false);
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
   tabsTail: function(){
     return this.get("tabsTail");
   },
   canHaveNoActiveTab : function() {
     return this.get('canHaveNoActiveTab');
   },
   slideEffect : function() {
     return this.get('slideEffect');
  }
});


var TabsView = Backbone.View.extend({
    model: Tabs,
    initialize: function (args) {
        _.bindAll(this, 'render', 'updateAvabilityOfTab', 'postRenderTabEvents');
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
    updateAvabilityOfTab : function(tab,li) {
       var model = this.model;
       if (tab.available()) {
         li.removeClass('inactive');
         li.click(function() {
           if (tab.clickable()) {
             if (!tab.active())
               model.activate(tab);
             else if (!model.canHaveNoActiveTab())
               tab.onActivate(); // Manual trigger of activate
             else
               model.deactive();
           }
           return false;
         });
       }
       else {
          li.unbind('click');
          li.addClass('inactive');
       }
    },
    postRenderTabEvents : function() {
      _.each(this.model.tabs(), function(t) {
                   if(t.active())
                     t.onShow();
                   else
                     t.onHide();
                });
    },
    render: function () {
        var self = this;
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

            if (tab.name() instanceof jQuery)
              icon.append(tab.name());
            else
              icon.text(tab.name());

            if (tab.iconClass() != undefined){
                icon.addClass(tab.iconClass());
                li.addClass('withIcon');
            }
            li.append(icon);

            if (tab.active())
                li.addClass("active");
            self.updateAvabilityOfTab(tab,li);
            tab.unbind('change:available');
            tab.bind('change:available', function() {self.updateAvabilityOfTab(tab,li);});

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


        var tabs = model.tabs();
        var visible = $();
        for(var i=0;i<tabs.length;i++)
        {
            _.each(tabs[i].elems(), function(e) {
              if (e instanceof jQuery && e.is(':visible'))
                visible = visible.add(e);
            });
        }

        var newvisible = $();
        if (this.model.activeTab() != undefined) {
          this.model.activeTab().initateAll();
          _.each(this.model.activeTab().elems(), function(e) {
              container.append(e.css('display','none'));
              newvisible = newvisible.add(e);
          });
        }
        var toHide = visible.not(newvisible);
        var toShow = newvisible.not(visible);
        var hideMethod =  (model.slideEffect() ? function(c) { toHide.slideUp(200,c);} : function(c) { toHide.hide(0,c);}) ;
        var showMethod =  (model.slideEffect() ? function(c) { toShow.slideDown(200,c);} : function(c) { toShow.show(0,c);}) ;

        if (toHide.size() != 0 || toShow.size() != 0) {

          var activeTab = this.model.activeTab();

          if (toHide.size() != 0 || toShow.size() != 0) {

            var activeTab = this.model.activeTab();
              if (toHide.size() == 0)
                showMethod(self.postRenderTabEvents);
              else if (toShow.size() == 0)
                hideMethod(self.postRenderTabEvents);
              else
                hideMethod(function() {showMethod(self.postRenderTabEvents);} );
                return this;
          }
        }
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
          if (tab.currentHashMatches() && !tab.active())
            self.model.activate(tab);
          });
        });

        return {el: function() {return $(self.view.el);},
                next: function() {self.model.activateNext();},
                activeTab : function() {return self.model.activeTab();},
                activate: function(tab) {self.model.activate(tab);},
                activateFirst: function() {self.model.activate(self.model.tabs()[0]);},
                deactive : function() { self.model.deactive();}
        };

};


})(window);
