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
            tipSide      : "right"                                          // Side (left or right) of such arrow
            margin       : -22                                              // Margin to element it's pointing at.
            })
        el.change(function() {task.update();});

  Usage:

        var arrow = new PageTasksArrow({
            tasks : new PageTasks({tasks : [task]}),
        })

       $('body').append(arrow.el())
 */

define(['Backbone', 'legacy_code'], function() {

window.PageTask = Backbone.Model.extend({
  defaults: {
    active: false,
    complete   : false, // Cache for isCompleate
    isComplete : function() {return false;},
    onActivate : function() {return false;},
    onScrollWhenActive : function() {return false;},
    onArrowClick : function() { return false; },
    onDeactivate : function() {return false;},
    tipSide : "right",
    pointSelector : undefined,
    margin: -22,
    type: undefined,
    field: undefined
  },
  initialize: function(args) {
    _.bindAll(this, 'update');
    this.model = args.model;
  },
  el: function() {
    if (this.pointSelector() != undefined && $(this.pointSelector(),this.get("el")).size() > 0)
      return $(this.pointSelector(), this.get("el"));
    return this.get("el");
  },
  pointSelector : function() {
    return this.get("pointSelector");
  },
  active: function () {
    return this.get("active");
  },
  margin: function () {
    return this.get("margin");
  },
  onActivate: function() {
    if (!this.active()) {
      this.set("active", true, {silent: true});
      return this.get("onActivate")();
    }
  },
  forceOnActivate: function () {
    return this.get("onActivate")();
  },
  onScrollWhenActive: function() {
    return this.get("onScrollWhenActive")();
  },
  onDeactivate : function() {
    if (this.active()) {
      this.set("active", false, {silent: true});
      return this.get("onDeactivate")();
    }
  },
  onDelete : function() {
    this.stopListening();
  },
  isComplete: function() {
    return this.get("isComplete")();
  },
  isSignatoryAttachmentTask : function() {
    return this.get("type") == 'signatory-attachment';
  },
  isRequiredAuthorAttachmentTask : function() {
    return this.get("type") == 'author-attachment';
  },
  field : function() {
    return this.get("field");
  },
  isSignTask : function() {
    return this.get("type") == 'sign';
  },
  isFieldTask : function() {
    return this.get("type") == 'field';
  },
  isExtraDetailsTask : function() {
    return this.get("type") == 'extra-details';
  },
  tipSide : function() {
    return this.get("tipSide");
  },
  update: function() {
    this.trigger("change");
  },
  triggerUIChange : function() {
    this.trigger("change:ui");
  },
  onArrowClick : function() {
    return this.get("onArrowClick")();
  }
});

window.PageTasks = Backbone.Model.extend({
  defaults: {
    tasks: [],
    active : undefined
  },
  initialize: function(args) {
    var model = this;
    var tasks = args.tasks.sort(function(t1, t2) {
      var offset1 = t1.el().offset();
      var offset2 = t2.el().offset();
      var height1 = t1.el().outerHeight();
      var height2 = t2.el().outerHeight();
      var verticalDiff = (offset1.top + height1) - (offset2.top + height2);
      var horizontalDiff = offset1.left - offset2.left;
      return (verticalDiff === 0 ? horizontalDiff : verticalDiff);
    });
   _.each(tasks, function(t) {model.listenTo(t, 'change', model.activateTask)});
   _.each(tasks, function(t) {model.listenTo(t, 'change:ui', function() {this.trigger("change");});});
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

  forceActivate: function () {
    var active = this.get("active");

    if (active) {
      active.forceOnActivate();
    }
  },

  notCompletedTasks : function() {
         var tasks = [];
         for (var i=0;i< this.tasks().length ; i++ )
               if (!this.tasks()[i].isComplete()) tasks.push(this.tasks()[i]);
         return tasks;
  },
  deletePageTasks: function() {
    var model = this;
    model.stopListening();
    _.each(this.tasks(), function(t) {
      t.onDelete();
    });
  }
});

var PageTasksArrowView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render', 'updateArrow', 'onScroll');
    var view = this;
    view.listenTo(view.model, "change", view.render);
    $(window).on("resize", view.updateArrow);
    $(window).on("scroll", view.onScroll);
    this.render();
  },
  blink : function() {
    if (this.arrow != undefined)
        this.arrow.blink(10);
  },
  disable: function() {
    if (this.arrow != undefined)
        this.arrow.disable();
  },
  enable: function() {
    if (this.arrow != undefined)
        this.arrow.enable();
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
            return new Arrow({      type: task.tipSide() != "right" ? 'point-left' : 'point-right'
                                   , margin: task.margin()
                                   , point : $(task.el())
                                   , onClick : function () { task.onArrowClick(); }
                              });
        }
        else if ((elbottom + bottommargin) > scrollbottom)
            return new Arrow({type: 'scroll-down', point : $(task.el()), scrollDone : function() {task.onActivate();} });
        else
            return new Arrow({type: 'scroll-up', point : $(task.el()), scrollDone : function() {task.onActivate();} });
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
        if (arrow.isDisabled()) return false;
        if (task != newtask) return true;
        if ((scrolltop >= 0) && (elbottom <= eltop)) return false;

        if (((elbottom + bottommargin) <= scrollbottom) && ((eltop - topmargin) >= scrolltop)) {
           if (arrow.type() == 'point-left')  return false;
           if (arrow.type() == 'point-right') return (Math.abs(elleft + elwidth - arrow.el().offset().left) > 10);
           return (arrow.type() != 'point-left' && arrow.type() != 'point-right');
        }
        if ((elbottom + bottommargin) > scrollbottom)
           return (arrow.type() != 'scroll-down');

        return (arrow.type() != 'scroll-up');
  },
  shouldBlinkOnUpdate : function(oldarrow,newarrow) {
    if (oldarrow !== undefined && oldarrow.isBlinking()) {
      return true;
    }
    var oldArrowIsUpOrDown = oldarrow != undefined && (oldarrow.type() == 'scroll-up' || oldarrow.type() == 'scroll-down');
    var newArrowIsPoint = newarrow.type() == 'point-left' || newarrow.type() == 'point-right';
    var newUpArrow = newarrow.type() == 'scroll-up' && (oldarrow == undefined || oldarrow.type() != 'scroll-up');
    var newDownArrow = newarrow.type() == 'scroll-down' && (oldarrow == undefined || oldarrow.type() != 'scroll-down');
    return (oldArrowIsUpOrDown && newArrowIsPoint) || newUpArrow || newDownArrow;
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
                  $(this.el).append(this.arrow.el());
              this.trigger("change:arrow");
              if (this.arrow != undefined) {
                  view.arrow.fixWidth();
                  setTimeout(function() {view.arrow.fixWidth();},100);
                  if (this.shouldBlinkOnUpdate(oldArrow,view.arrow)) {
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
  goToCurrentTask: function() {
    if (this.arrow === undefined) {
      return;
    }
    this.arrow.activate();
  },
  onScroll: function() {
    this.updateArrow();
    if (this.model.active() != undefined) {
      this.model.active().onScrollWhenActive();
    }
  },
  render: function() {
    var view = this;
    var document = this.document;
    $(this.el).addClass('arrows');
    setTimeout(function () {
      view.updateArrow();
    }, 100);
  },
  deletePageTasksArrowView: function() {
    $(window).off("resize", this.updateArrow);
    $(window).off("scroll", this.onScroll);
    this.stopListening();
  },
  replaceModel: function (model) {
    this.stopListening(this.model, "change", this.render);
    this.model = model;
    this.listenTo(model, "change", this.render);
    this.updateArrow();
  }
});

window.PageTasksArrow = function(args){
        var model = args.tasks;
        var view = new PageTasksArrowView({
                        model: model,
                        el : $("<div/>")
                    });
        return {
              model    : function()    { return model;}
            , view     : function()    { return view;}
            , blink    : function()    { view.blink();}
            , enable   : function()    { view.enable(); }
            , disable  : function()    { view.disable(); }
            , updatePosition: function() { view.updatePosition();}
            , updateArrow: function() { view.updateArrow();}
            , replaceTasks: function (tasks) { view.replaceModel(tasks) }
            , click: function () { if (view.task) {view.task.onArrowClick();} }
            , goToCurrentTask: function() { view.goToCurrentTask();}
            , forceActivate: function() { model.forceActivate();}
            , deletePageTasksArrow: function() { view.deletePageTasksArrowView(); model.deletePageTasks(); }
            , notCompletedTasks: function() { return model.notCompletedTasks(); }
         };
};

});
