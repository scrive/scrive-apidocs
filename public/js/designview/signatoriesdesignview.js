/* Signatory view of document
 * Now unified with author and viewer views
 */


(function(window){

var SignatoriesDesignModel = Backbone.Model.extend({
  defaults : {
  },
  initialize: function (args) {
  },
  document : function() {
     return this.documentdesignview.document();
  },
  documentdesignview :function() {
     return this.get("documentdesignview");
  },
  extra : function() {
    this.get("extra");
  }
});

var SignatoriesDesignView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'signatoriesList');
        this.model.document().bind('reset', this.render);
        this.model.document().bind('change', this.render);
        this.model.document().bind('change:signatories', this.signatoriesList);

        this.render();
    },
    addRemoveSignatoryBox : function() {
        var document = this.model.document();
        var view = this;
        var box =  $("<div class='addremovesignatorybox'/>");
        var addLink = $("<a class='addSignatory' href='#'/>").text(localization.addSignatory);
        addLink.click(function(){
            var nsig = document.addSignatory();
            view.showSignatory(nsig);
            return false;
        });
        var removeLink = $("<a class='removeSignatory' href='#'/>").text(localization.removeSignatory);
        removeLink.click(function() {
            document.removeSignatory(view.current);
            return false;

        });
        box.append(addLink).append(removeLink).append(this.model.extra());
        return box;
    },
    fixCurrent : function() {
      if (this.current == undefined || _.indexOf(this.model.document().signatories(), this.current) == -1)
          this.current = this.model.document().signatories()[0];
    },
    signatoriesList : function() {
        var document = this.model.document();
        var view = this;
        if (view.signatoriesListBox == undefined)
            view.signatoriesListBox =  $("<div class='siglist'/>");
        else
            view.signatoriesListBox.empty();
        var header = $("<a href='#' class='header'/>");
        if (document.view.signOrderVisible())
            header.append($("<span class='float-right'/>").text(localization.signatoryListSignOrder));
        header.append($("<span>").text(localization.signatoryListPartner));
        view.signatoriesListBox.append(header);

        var sigs = document.signatories();
        _.each(document.signatories(),function(sig) {
            var sigline = $("<a href='#' class='signame'/>");
            var setSiglineContent= function() {
                var text = sig.isCsv() ?  localization.csv.title : sig.nameOrEmail();
                if (text == undefined || text == "")
                   text = "(" + localization.noNamePerson + ")";
                var number = sig.signs()? sig.signorder() : "-";
                sigline.empty();
                if (document.view.signOrderVisible())
                  sigline.append($("<span class='float-right'/>").text(number));
                sigline.append($("<span/>").text(text));
            }
            sig.bind('change', function() { setSiglineContent();} );
            setSiglineContent();
            sigline.click(function() {
                view.current= sig;
                view.render();
                return false;
            });
            view.signatoriesListBox.append(sigline);
        });
        return view.signatoriesListBox;
    },
    render: function(){
        this.fixCurrent();
        var box = $(this.el);
        box.children().detach();
        box.addClass('signatoriesbox');
        var document = this.model.document();
        if (document.signatories().length == 1)
        {
            var s1view = new SignatoryDesignView({model: document.signatories()[0], el: $("<div/>")});
            box.append(s1view.el);
            box.append($("<div class='sigview dummy'/>"));
        }
        else if (document.signatories().length == 2)
        {
            var s1view = new SignatoryDesignView({model: document.signatories()[0], el: $("<div/>")});
            var s2view = new SignatoryDesignView({model: document.signatories()[1], el: $("<div/>")});
            box.append(s1view.el);
            box.append(s2view.el);

        }
        else
        {
           box.append(this.signatoriesList());
           var sview = new SignatoryDesignView({model: this.current, el: $("<div/>")});
           box.append(sview.el);

        }
        box.append(this.addRemoveSignatoryBox());
        return this;
    },
    showSignatory : function(sig) {
        this.current= sig;
        this.render();
        return true;
    }
});

window.SignatoriesDesign = function(args) {
          var model = new SignatoriesDesignModel(args);
          var view =  new SignatoriesDesignView({model : model, el : $("<div/>")});
          this.el = function() {return $(view.el);};
};

})(window);
