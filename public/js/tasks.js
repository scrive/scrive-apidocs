/* Tasks list  */


(function(window) {

window.PageTask = Backbone.Model.extend({
  defaults: {
    complete   : false, // Cache for isCompleate
    isComplete : function() {return false;},
    onActivate : function() {return false;},
    onDeactivate : function() {return false;},
    tipSide : "right",
    label:""
  },
  initialize: function(args) {
    _.bindAll(this, 'update');
    this.model = args.model;
    this.update();
  },
  el: function() {
    return this.get("el");
  },
  onActivate: function() {
    return this.get("onActivate")();
  },
  onDeactivate : function() {
    return this.get("onDeactivate")();
  },
  isComplete: function() {
    return this.get("complete");
  },
  label : function() {
    return this.get("label");
  },
  tipSide : function() {
    return this.get("tipSide");
  },
  update: function() {
    this.set({ complete: this.get("isComplete")()});
  }
});

window.PageTasks = Backbone.Model.extend({
  defaults: {
    tasks: [],
    active : undefined
  },
  initialize: function(args) {
    var model = this;  
    var tasks = args.tasks.sort(function(t1,t2) {return t1.el().offset().top > t2.el().offset().top} );
   _.each(tasks, function(t) {t.bind('change', function() { model.activateTask();})});
   this.activateTask();
  },
  active : function() {
    return this.get("active");
  },
  tasks: function() {
    return this.get("tasks");
  },
  activateTask: function() {
     for (var i=0;i< this.tasks().length ; i++ )
         if (!this.tasks()[i].isComplete()) {
             if (this.tasks()[i] == this.active()) break;
             if (this.active() != undefined) this.active().onDeactivate();
             this.set({"active" : this.tasks()[i]});
             this.tasks()[i].onActivate();
             this.trigger("change");
             break;
         }
  },
  notCompleatedTasks : function() {
         var tasks = []
         for (var i=0;i< this.tasks().length ; i++ )
               if (!this.tasks()[i].isComplete()) tasks.push(this.tasks()[i]);
         return tasks;                             
  }
});


window.PageTasksArrowView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    var view = this;
    this.model.bind("change", function() {view.render()});
    this.render();
  },
  blink : function() {
    if (this.arrow != undefined)
        this.arrow.blink(10);  
  },
  taskArrow : function(task) {
        var view = this;
        var scrolltop = $(window).scrollTop();
        var scrollbottom = scrolltop + $(window).height();
        var eltop = task.el().offset().top;
        var elbottom = eltop +task.el().height();
        var bottommargin = 0;
        var topmargin = 0;

        if ((scrolltop >= 0) && (elbottom <= eltop))
        {
           window.setTimeout(function() {view.updateArrow()} , 500);
           return;
        }
        else if (((elbottom + bottommargin) <= scrollbottom) && ((eltop - topmargin) >= scrolltop))
           return Arrow.init({       type: task.tipSide() != "right" ? 'point-left' : 'point-right'
                                   , point : $(task.el())
                                   , text : task.label()
                             });
        else if ((elbottom + bottommargin) > scrollbottom)
            return new Arrow.init({type: 'scroll-down',  point : $(task.el())});
        else
            return new Arrow.init({type: 'scroll-up',  point : $(task.el())});
  },
  updateArrow : function() {
     var view = this;
     if (view.arrow != undefined)
        this.arrow.clear();
     if (this.model.active() != undefined)
        {
            this.arrow = this.taskArrow(this.model.active());
            if (this.arrow != undefined)
                $(this.el).append(this.arrow.view().el);
            this.trigger("change:arrow");
        }
  },
  render: function() {
    var view = this;
    var document = this.document;
    $(this.el).addClass('arrows');
    $(window).resize(function() {view.updateArrow();});
    $(window).scroll(function() {view.updateArrow();});
    view.updateArrow()
  }
});


})(window);
