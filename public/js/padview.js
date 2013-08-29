/* Signatory view of document
 * Now unified with author and viewer views
 */


(function(window){



window.PadQueue = Backbone.Model.extend({
    defaults: {
        needFullRefresh  : false,
        ready : false
    },
    initialize: function (args) {
        var padqueue = this;
        this.url = "/padqueue/state";
        padqueue.refresher();
    },
    refresher : function() {
        var padqueue = this;
        this.refresherstarted = true;
        padqueue.recall();
        setTimeout(function() {padqueue.refresher();} , 3000 );
    },
    ready: function(){
        return this.get("ready");
    },
    recall : function() {
       this.fetch({ processData:  true, cache : false});
    },
    documentid : function() {
        return this.get("documentid");
    },
    signatorylinkid : function() {
        return this.get("signatorylinkid");
    },
    hasDocument : function() {
        return this.documentid() != undefined && this.signatorylinkid() != undefined;
    },
    logged : function() {
        return this.loggedToSystem() || this.loggedToPad();
    },
    loggedToSystem : function() {
        return this.get("logged") == "system";
    },
    loggedToPad : function() {
        return this.get("logged") == "pad";
    },
    needFullRefresh : function() {
        return this.get("needFullRefresh");
    },
    logout : function() {
        return new Submit({method: "POST", url : "/padqueue/logout"});
    },
    parse: function(args) {
     return {
      documentid : args.documentid,
      signatorylinkid: args.signatorylinkid,
      logged : args.logged,
      needFullRefresh : this.documentid() != undefined &&
                        ((this.signatorylinkid() != args.signatorylinkid) || (this.documentid() != args.documentid)),
      ready: true
      };
    }

});


window.PadQueueView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.view = this;
        this.render();
    },
    documentView : function(svb) {
        var padqueue = this.model;
        var doc =  new DocumentSignView({
                    id: padqueue.documentid(),
                    signviewbranding : svb,
                    usebranding : true,
                    viewer : new DocumentViewer({
                        signatoryid : padqueue.signatorylinkid()
                      })
                   });
        return doc.view.el;
    },
    signViewBranding : function() {
      var svb = new BrandingForSignView({});
      svb.fetch({ processData:  true, cache : false});
      return svb;
    },
    signviewheader : function(svb) {
      if (this.signviewheader_ == undefined)
        this.signviewheader_ = new DocumentSignViewHeader({model : svb});
      return this.signviewheader_;
    },
    signviewfooter: function(svb) {
      if (this.signviewfooter_ == undefined)
        this.signviewfooter_ = new DocumentSignViewFooter({model : svb});
      return this.signviewfooter_;
    },
    noDocumentView : function() {
        var box = $("<div class='noDocumentAvaible'> </div>");
        var header = $("<div class='header'>Inget dokument tillgängligt</div>");
        var sheader = $("<div class='sheader'>Inväntar dokument</div>");
        box.append(header).append(sheader);
        return box;

    },
    logToPadDevice : function() {
        return new Login({
            pad : true,
            referer : "/padqueue"
            }).el();
    },
    padLogoutIcon : function() {
        var icon = $("<div class='padTopIcon logout-image' style='position: absolute;top: 0;right:0'>");
        var padqueue = this.model;
        icon.click(function() {padqueue.logout().send();});
        return icon;
    },
    backToSystemIcon : function() {
        var padqueue = this.model;
        var icon = $("<div class='padTopIcon go-back-image' style='position: absolute;top: 0;right:0'>");
        icon.click(function() { if (padqueue.hasDocument())
                                 window.location = '/d/' + padqueue.documentid();
                                else
                                 window.location = '/d';});
        return icon;
    },
    render: function () {
        var padqueue = this.model;
        var container = $(this.el);
        container.empty();
        if (padqueue.ready()) {
            if (padqueue.needFullRefresh())
               window.location.reload(); // We reload if content has changes so much that it is not good to keep it opened.
            else if (padqueue.logged()) {
                var svb = this.signViewBranding();
                svb.bind("change", function() {
                  if (svb.ready() && svb.signviewbackgroundcolour())
                    $('.signview').css('background-image','none').css('background-color', svb.signviewbackgroundcolour());
                });
                $('body').prepend(this.signviewheader(svb).el);
                $('body').append(this.signviewfooter(svb).el);
                if (padqueue.hasDocument()) {
                  container.append(this.documentView(svb));
                }
                else {
                  container.append(this.noDocumentView());
                }
            }
            else
                container.append(this.logToPadDevice());

            if (padqueue.loggedToPad())
               $('body').append(this.padLogoutIcon());
            if (padqueue.loggedToSystem())
               $('body').append(this.backToSystemIcon());

        }
        return this;

    }
});

window.KontraPad = function() {
     var model = new PadQueue({ });
     var view = new PadQueueView({
                        model: model,
                        el : $("<div/>")
                    });
     return {
         model : function()  {return model;},
         view :  function()  {return view;},
         recall : function() {model.recall();}
    };
};
})(window);
