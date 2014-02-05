/*
 * General list of tasks (actions) that user should perform on page.
 *
 * Classes :
 *      PageTask - single task. There is some jQuery element connected to it.
 *      PageTasks - collection of tasks. It keeps one active. Takes care of updates and order.
 *      PageTasksArrowView - generates green arrow pointing at active task | Hidden
 *      PageTasksArrow - controler for  PageTasksArrowView & PageTasks
 *
 * PageTasksArrowView points at active task (or up/down if task is out of scope)
 *
 * Task is being active as long as it is not completed (isComplete function is used for test).
 * Tasks are ordered top to bottom, based on this jQuery object position.
 * On task update - first, not compleated task is activated.
 *
 * ! You need to manually call 'update' method. 'update' performes isComplete check on task.
 *   isComplete result is stored in task internal cache - it gets updated only on 'update' method call.
 *
 * ! If UI of element has been changed - call task method triggerUIChange. This will force arrow to refresh.
 *
   Sample task :
        var el = $("<input type='text'/>").appendTo('body');
        var task = new PageTask({
            el : el,
            isComplete   : function() {return el.val() == "abc";},          // Check if task is compleated
            onActivate   : function() {el.css("border", "1px solid red");}, // Code performed when task is being activated
            onDeactivate : function() {el.css("border", "");},              // Code performed when task stops being active
            label        : "Put 'abc' here",                                // Text on arrow that will be pointing to el
            tipSide      : "right"                                           // Side (left or right) of such arrow
            })
        el.change(function() {task.update();});

  Usage:

        var arrow = new PageTasksArrow({
            tasks : new PageTasks({tasks : [task]}),
        })

       $('body').append(arrow.view().el)
 */

define(['Backbone', 'legacy_code'], function() {

window.PageTask = Backbone.Model.extend({
  defaults: {
    complete   : false, // Cache for isCompleate
    isComplete : function() {return false;},
    onActivate : function() {return false;},
    onDeactivate : function() {return false;},
    tipSide : "right",
    label:"",
    labelCss: {},
    arrowColour: undefined,
    pointSelector : undefined
  },
  initialize: function(args) {
    _.bindAll(this, 'update');
    this.model = args.model;
    this.update();
  },
  el: function() {
    if (this.pointSelector() != undefined && $(this.pointSelector(),this.get("el")).size() > 0)
      return $(this.pointSelector(), this.get("el"));
    return this.get("el");
  },
  pointSelector : function() {
    return this.get("pointSelector");
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
  labelCss : function() {
    return this.get('labelCss');
  },
  arrowColour : function() {
    return this.get('arrowColour');
  },
  tipSide : function() {
    return this.get("tipSide");
  },
  update: function() {
    this.set({ complete: this.get("isComplete")()});
  },
  triggerUIChange : function() {
    this.trigger("change:ui");
  }
});

window.PageTasks = Backbone.Model.extend({
  defaults: {
    tasks: [],
    active : undefined
  },
  initialize: function(args) {
    var model = this;
    var tasks = args.tasks.sort(function(t1,t2) {return t1.el().offset().top - t2.el().offset().top} );
   _.each(tasks, function(t) {t.bind('change', function() { model.activateTask();})});
   _.each(tasks, function(t) {t.bind('change:ui', function() {model.trigger("change");})});
   this.activateTask();
  },
  active : function() {
    return this.get("active");
  },
  tasks: function() {
    return this.get("tasks");
  },
  activateTask: function() {
     var hasIncompleteTask = false;
     for (var i=0;i< this.tasks().length ; i++ )
         if (!this.tasks()[i].isComplete()) {
             hasIncompleteTask = true;
             if (this.tasks()[i] == this.active()) break;
             if (this.active() != undefined) this.active().onDeactivate();
             this.tasks()[i].onActivate();
             this.set({"active" : this.tasks()[i]});
             this.trigger("change");
             break;
         }

    if (!hasIncompleteTask)
    {
         if (this.active() != undefined) this.active().onDeactivate();
         this.set({"active" : this.tasks()[i]});
         this.trigger("change");
    }

  },
  notCompleatedTasks : function() {
         var tasks = [];
         for (var i=0;i< this.tasks().length ; i++ )
               if (!this.tasks()[i].isComplete()) tasks.push(this.tasks()[i]);
         return tasks;
  }
});

var PageTasksArrowView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    var view = this;
    this.arrowcolour = args.arrowcolour;
    this.model.bind("change", function() {view.render()});
    this.render();
  },
  blink : function() {
    if (this.arrow != undefined)
        this.arrow.blink(10);
  },
  taskArrow : function(task) {
        var view = this;
        this.task = task;
        var scrolltop = $(window).scrollTop();
        var scrollbottom = scrolltop + (window.innerHeight ? window.innerHeight : $(window).height());
        var eltop = task.el().offset().top;
        var elbottom = eltop +task.el().height();
        var bottommargin = 0;
        var topmargin = 0;

        if ((scrolltop >= 0) && (elbottom <= eltop))
        {
           window.setTimeout(function() {view.updateArrow()} , 500);
           return;
        }
        else if (((elbottom + bottommargin) <= scrollbottom) && ((eltop - topmargin) >= scrolltop)) {
            return Arrow.init({      type: task.tipSide() != "right" ? 'point-left' : 'point-right'
                                   , point : $(task.el())
                                   , text : task.label()
                                   , labelCss: task.labelCss()
                                   , arrowColour: this.arrowcolour
                              });
        }
        else if ((elbottom + bottommargin) > scrollbottom)
            return new Arrow.init({type: 'scroll-down', arrowColour: this.arrowcolour,  point : $(task.el()), scrollDone : function() {task.onActivate();} });
        else
            return new Arrow.init({type: 'scroll-up',  arrowColour: this.arrowcolour, point : $(task.el()), scrollDone : function() {task.onActivate();} });
  },
  arrowShouldChange : function(newtask) {
        var view = this;
        var task = view.task;
        var arrow = view.arrow;
        var scrolltop = $(window).scrollTop();
        var scrollbottom = scrolltop +  (window.innerHeight ? window.innerHeight : $(window).height());
        var eltop = task.el().offset().top;
        var elleft = task.el().offset().left;
        var elwidth = task.el().outerWidth();
        var elbottom = eltop +task.el().height();
        var bottommargin = 0;
        var topmargin = 0;
        if (task == undefined || arrow == undefined) return true;
        if (task != newtask) return true;
        if ((scrolltop >= 0) && (elbottom <= eltop)) return false;

        if (((elbottom + bottommargin) <= scrollbottom) && ((eltop - topmargin) >= scrolltop)) {
           if (arrow.model().type() == 'point-left')  return false;
           if (arrow.model().type() == 'point-right') return (Math.abs(elleft + elwidth - $(arrow.view().el).offset().left) > 10);
           return (arrow.model().type() != 'point-left' && arrow.model().type() != 'point-right');
        }
        if ((elbottom + bottommargin) > scrollbottom)
           return (arrow.model().type() != 'scroll-down');

        return (arrow.model().type() != 'scroll-up');
  },
  shouldBinkOnUpdate : function(oldarrow,newarrow) {
    var oldArrowIsUpOrDown = oldarrow != undefined && (oldarrow.model().type() == 'scroll-up' || oldarrow.model().type() == 'scroll-down');
    var newArrowIsPoint = newarrow.model().type() == 'point-left' || newarrow.model().type() == 'point-right';
    var newUpArrow = newarrow.model().type() == 'scroll-up' && (oldarrow == undefined || oldarrow.model().type() != 'scroll-up');
    return (oldArrowIsUpOrDown && newArrowIsPoint) || newUpArrow;
  },
  updateArrow : function() {
     var view = this;
     var oldArrow = view.arrow;
     if (view.arrow == undefined || view.arrowShouldChange(this.model.active()))
     {
      if (view.arrow != undefined)
          this.arrow.clear();
      if (this.model.active() != undefined)
          {
              this.arrow = this.taskArrow(this.model.active());
              if (this.arrow != undefined)
                  $(this.el).append(this.arrow.view().el);
              this.trigger("change:arrow");
              if (this.arrow != undefined) {
                  view.arrow.fixWidth();
                  setTimeout(function() {view.arrow.fixWidth();},100);
                  if (this.shouldBinkOnUpdate(oldArrow,view.arrow)) {
                    this.blink();
                  }
              }
          }
     }
  },
  updatePosition : function() {
    if (this.arrow != undefined)
      this.arrow.updatePosition();
  },
  render: function() {
    var view = this;
    var document = this.document;
    $(this.el).addClass('arrows');
    $(window).resize(function() {view.updateArrow();});
    $(window).scroll(function() {view.updateArrow();});
    view.updateArrow();
  }
});

window.PageTasksArrow = function(args){
        var model = args.tasks;
        var arrowcolour = args.arrowcolour;
        var view = new PageTasksArrowView({
                        model: model,
                        arrowcolour: arrowcolour,
                        el : $("<div/>")
                    });
        return {
              model    : function()    { return model;}
            , view     : function()    { return view;}
            , blink    : function()    { view.blink();}
            , updatePosition: function() { view.updatePosition();}
         };
};

});
