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
    clickable : true,
    }  
  ,
  name : function() {
      return this.get("name");
  },
  elems : function() {
      return this.get("elems");
  },
  setActive : function(bool) {
      this.set({active: bool});
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
  }
});



var Tabs = Backbone.Model.extend({
   defaults: {
       numbers : true,
    },  
   title: function(){
     return this.get("title");
    },
   numbers : function() {
     return   this.get("numbers") == true;
   }, 
   initialize : function(args){
       if (_.all(args.tabs,function(t) {return !t.active(); }))
          args.tabs[0].setActive(true);
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
   },
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
        container.addClass("tab-viewer");
        this.toprow = $("<div id='signStepsContainer'/>");
        container.append(this.toprow);
        _.each(this.model.tabs(), function(t) {
            _.each(t.elems(), function(e) {container.append(e);})
        })
    },
    render: function () {
        var tabsview = this;
        this.toprow.children().detach();
        // Top part , with title and the tabs
        var titlepart = $("<div id='signStepsTitleRow'/>")
        titlepart.append(this.model.title());
        var tabsrow = $("<ul class='tabs'/>")
        var model = this.model
        _.each(this.model.tabs(), function(tab) 
        {
            if (tab.disabled()) return;
            var li = $("<li/>");
            if (tab.hasNumber())
                li.append(tabsview.numberIcon(tab.number()));
            li.append($("<a href='#'/>").html(tab.name()));
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
        if (model.tabsTail() != undefined) 
           _.each(model.tabsTail(), function (elem) {
           var li = $("<li style= 'float:right;padding-left:0px;padding-right:20px;'/>").append(elem);
           tabsrow.append(li);    
        }) 
           
        
        this.model.hideAll();
        _.each(this.model.activeTab().elems(), function(e) {e.show();})
        return this;
    },
    numberIcon : function(number) {
        var icon = $("<span/>");
        icon.addClass("numbericon" + number);
        return icon;
    }
});


window.KontraTabs = {
    init : function(args){
        this.model = new Tabs(args)
        this.view = new TabsView({
                        model: this.model,
                        el : $("<div/>")
                    })
        return this;
    }
}



});
