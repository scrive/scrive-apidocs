/*Module for basic system schema called tabs. Like three steps in design view or Conteacts | Offers | Orders | .. in Archive
 *
 * Tab contails of name and body, 
 * Tabs is collection of Tabs with some title.
 * By default first one is active.
 *
 * 
 */
  
$(function(){


window.Tab = Backbone.Model.extend({
  defaults: {
    active : false,
    disabled : false,
    clickable : true
    }  
  ,
  name : function() {
      return this.get("name");
  },
  elems : function() {
      return this.get("elems");
  },
  setActive : function(bool) {
      return this.set({active: bool});
  },
  active : function() {
        return this.get("active");
  },
  disabled : function() {
        return this.get("disabled");
  },
  clickable : function() {
        return this.get("clickable");
  }
});



var Tabs = Backbone.Model.extend({
   title: function(){
     return this.get("title");
    },
   initialize : function(args){
       if (_.all(args.tabs,function(t) {return !t.active(); }))
          args.tabs[0].setActive(true);
   }, 
   activate: function(newtab)
   {
        var tabs = this.tabs();
        for(var i=0;i<tabs.length;i++)
            tabs[i].setActive(newtab === tabs[i]);
        this.trigger("change");
   },
   hideAll: function()
   {    var tabs = this.tabs();
        for(var i=0;i<tabs.length;i++)
        {
            _.each(tabs[i].elems(), function(e) {e.hide();})
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
        var container = this.el;
        container.attr("id","tab-viewer");
        this.toprow = $("<div id='signStepsContainer'/>");
        container.append(this.toprow);
        _.each(this.model.tabs(), function(t) {
            _.each(t.elems(), function(e) {container.append(e);})
        })
    },
    render: function () {
        this.toprow.children().detach();


        // Top part , with title and the tabs
        var titlepart = $("<div id='signStepsTitleRow'/>")
        titlepart.append(this.model.title());
        var tabsrow = $("<ul class='tabs'/>")
        var model = this.model
        _.each(this.model.tabs(), function(tab) 
        {
            if (tab.disabled()) return;
            var li = $("<li/>").append($("<a href='#'/>").html(tab.name()));
            if (tab.active())
                li.addClass("active");
            li.click(function() {
                    if (tab.clickable())
                        model.activate(tab);
                    return false;
                    });
            tabsrow.append(li);
        });
        this.toprow.append(titlepart);
        if (model.hasManyTabs())
            this.toprow.append(tabsrow);
        if (this.extrasInTabsRow != undefined)
           //DO SOMETHING extrasInTabsRow

        // Main part
        this.model.hideAll();
        _.each(this.model.activeTab().elems(), function(e) {e.show();})
        return this;
    }
});


window.KontraTabs = {
    init : function(args){
        this.model = new Tabs(args)
        this.view = new TabsView({
                        extrasInTabsRow : args.extrasInTabsRow,
                        model: this.model,
                        el : $("<div/>")
                    })
        return this;
    }
}



});
