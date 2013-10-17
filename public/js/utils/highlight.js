/**
 * Highlights an element on the page, with margin.
 * 
 * Basic usage:
 * var guide = new Highlight({
 *   el: jQuery element,
 *   margins: {
 *       left: 10,
 *       right: 10,
 *       top: 10,
 *       bottom: 10
 *   },
 *   explanation: {
 *       placement: 'left | above',
 *       content: jQuery element,
 *       height: 90,
 *       width: 120
 *   }
 * })
 * and to update what to highlight/the explanation:
 * highlight.moveTo({...}) 
 *  
 * Explanation:
 * el The jQuery element that will be highlighted.
 * margins An object with the attributes top, bottom, right and left, 
 *         which will be the area around $el that will also be highlighted.
 * explanation An object with the attributes content and placement, 
 *             indicating how content will be placed in relation to 
 *             what we're highlighting. 
 */

(function(window) {

var HighlightModel = Backbone.Model.extend({
    margins: function() { return this.get("margins"); },
    el: function() { return this.get("el"); },
    boxes: function() { return this.get("boxes"); },
    explanation: function() { return this.get("explanation"); },
    explanationModel: function() { return this.get("explanationModel"); }
});

var HighlightView = Backbone.View.extend({
    boxClassName: "element-highlight", // applied to all boxes
    initialize: function(args) {
        _.bindAll(this, 'render', 'getBoundaryBox', 'getOrCreateBox', 'renderBoxes', 'renderExplanation', 'hide');
        this.model.bind('change:el', this.render);
        this.model.bind('destroy', this.hide);
        $(window).resize(this.render);
        $(window).scroll(this.render);

        this.render();
    },

    /**
     * Returns a boundary box that will contain the highlighted element, 
     * with specified margins.
     */
    getBoundaryBox : function() {
        var margins = this.model.margins() || {};
        var el = this.model.el();
        var box = el.offset();
        box.right = box.left + el.width() + margins.right;
        box.bottom = box.top + el.height() + margins.bottom - $(window).scrollTop();
        box.top -= margins.top + $(window).scrollTop();
        box.left -= margins.left;

        return box;
    },

    /**
     * Get or create the box element specified by its class
     */
    getOrCreateBox: function(className) {
        var box = $('.' + this.boxClassName + '.' + className);
        if (box.length == 1) {
            return box;
        } else {
            return $('<div></div>').addClass(this.boxClassName).addClass(className).appendTo('body');
        }
    },

    render: function() {
        var el = this.model.el();
        
        if (!el) { this.hide(); }

        this.renderBoxes();
        this.renderExplanation();
    },

    renderExplanation: function() {
        var explanation = this.model.explanation();

        // Add boxes to the explanation and we have a HighlightExplanationModel.
        _.extend(explanation, {boxes: this.model.boxes()});

        if (!this.model.has('explanationModel')) {
            // this feels ugly, to set something in a render method...?
            this.model.set({'explanationModel': new HighlightExplanationModel(explanation)});
            $('body').append((new HighlightExplanationView({model: this.model.explanationModel()})).el);
        } else {
            var explanationModel = this.model.explanationModel();
            explanationModel.set(explanation);
            explanationModel.trigger('change');
        }
    },

    renderBoxes: function() {
        var boundaryBox = this.getBoundaryBox();

        var boxOver = this.getOrCreateBox('over');
        var boxRightOf = this.getOrCreateBox('rightof');
        var boxLeftOf = this.getOrCreateBox('leftof');
        var boxUnder = this.getOrCreateBox('under');

        boxOver.css({
            'height': boundaryBox.top + 'px'
        }).show().css('min-height', '0px');

        boxUnder.css({
            'left': boundaryBox.left + 'px',
            'width': (boundaryBox.right-boundaryBox.left) + 'px',
            'height': $(window).height() - boundaryBox.bottom + 'px'
        }).css('top', 'auto').show();

        boxLeftOf.css({
            'top': boundaryBox.top + 'px',
            'width': boundaryBox.left  + 'px'
        }).show();

        boxRightOf.css({
            'top': boundaryBox.top + 'px',
            'left': boundaryBox.right + 'px'
        }).show();

        this.model.set({'boxes': {'over': boxOver, 'under': boxUnder, 'left': boxLeftOf, 'right': boxRightOf}});
    },

    /**
     * Hide all highlights
     */
    hide: function() {
        $('.' + this.boxClassName).hide();
        $('.highlight-explanation').hide();
        $(window).unbind('resize', this.render);
        $(window).unbind('scroll', this.render);
    }
});

var HighlightExplanationModel = Backbone.Model.extend({
    defaults: {
        placement: 'above',
        height: 0,
        width: 0,
        content: $('<div></div>'),
        boxes: undefined // the highlight boxes
    },
    placement: function() { return this.get("placement"); },
    height: function() { return this.get("height"); },
    width: function() { return this.get("width"); },
    content: function() { return this.get("content"); },
    boxes: function() { return this.get("boxes"); }
});

var HighlightExplanationView = Backbone.View.extend({
    className: 'highlight-explanation',
    initialize: function (args) {
        this.model = args.model;
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.bind('hide', this.hide);
        return this.render();
    },
    hide: function() {
        $(this.el).hide();
    },
    render: function() {
        var boxes = this.model.boxes();

        this.$el.css({
            // reset
            top: 'auto', bottom: 'auto', left: 'auto', right:'auto',
            // set size
            height: this.model.height() + 'px',
            width: this.model.width() + 'px'
        });

        this.$el.children().detach();
        this.$el.append(this.model.content());

        // set place
        if (this.model.placement() === 'above') {
            this.$el.css('top', boxes.over.height() - this.model.height());
            this.$el.css('left', boxes.left.width());
        } else if (this.model.placement() === 'left') {
            this.$el.css('bottom', boxes.under.height());
            this.$el.css('right', $(window).width() - boxes.left.width());
        }

        this.$el.show();

        return this.$el;
    }
});

window.Highlight = function(args) {
    var model = new HighlightModel(args);
    var view = new HighlightView({model: model});
    this.moveTo = function(args) { model.set(args); };
    this.hide = function(args) { model.destroy(); };
};

})(window);
