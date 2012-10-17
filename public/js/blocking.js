(function(window) {

    /*
      Holds information relevant to blocking for a user.
      
    */
    window.BlockingInfoModel = Backbone.Model.extend({
        defaults: {
            block: false,
            dunning: false
        },
        url: function() {
            return "/blockinginfo";
        },
        docsTotal: function() {
            if(this.isEnterprise())
                return 500000; 
            if(!this.isFree() && this.isActive())
                return 500; // # of docs it says in the TOS
            else 
                return 3;
        },
        docsUsed: function() {
            return this.get('docsused');
        },
        docsLeft: function() {
            return this.docsTotal() - this.docsUsed();
        },
        plan: function() {
            return this.get('plan');
        },
        status: function() {
            return this.get('status');
        },
        isEnterprise: function() {
            return this.plan() === 'enterprise';
        },
        isFree: function() {
            return this.plan() === 'free';
        },
        isActive: function() {
            return this.status() === 'active';
        },
        isCanceled: function() {
            return this.status() === 'canceled';
        },
        isDeactivated: function() {
            return this.status() === 'deactivated';
        },
        isOverdue: function() {
            return this.status() === 'overdue';
        },
        isDunning: function() {
            return this.get('dunning');
        }
    });

    window.BlockingInfoView = Backbone.View.extend({
        className: 'blocking-info',
        initialize: function(args) {
            _.bindAll(this);
            this.model.bind('change reset fetch', this.render);
        },
        headline: function() {
            var view = this;
            var model = view.model;
            if(model.isFree())
                return localization.blocking.free.headline;
            else if(model.isOverdue())
                return localization.blocking.overdue.headline;
            else if(model.isDunning())
                return localization.blocking.dunning.headline;
            else if(model.isCanceled())
                return localization.blocking.canceled.headline;
            else if(model.isDeactivated())
                return localization.blocking.deactivated.headline;
        },
        subtext1: function() {
            var view = this;
            var model = view.model;
            if(model.isFree())
                return localization.blocking.free.subtext1;
            else if(model.isOverdue())
                return localization.blocking.overdue.subtext1;
            else if(model.isDunning())
                return localization.blocking.dunning.subtext1;
            else if(model.isCanceled())
                return localization.blocking.canceled.subtext1;
            else if(model.isDeactivated())
                return localization.blocking.deactivated.subtext1;
        },
        subtext2: function() {
            var view = this;
            var model = view.model;
            if(model.isFree())
                return localization.blocking.free.subtext2;
            else if(model.isOverdue())
                return localization.blocking.overdue.subtext2;
            else if(model.isDunning())
                return localization.blocking.dunning.subtext2;
            else if(model.isCanceled())
                return localization.blocking.canceled.subtext2;
            else if(model.isDeactivated())
                return localization.blocking.deactivated.subtext2;
        },
        makeBox: function() {
            var view = this;
            var model = view.model;
            
            var container = $("<div class='instructions' />");
            container.append($("<div class='headline' />").html(this.headline()));
            container.append($("<div class='subheadline' />").html(this.subtext1()));
            container.append($("<div class='subheadline' />").html(this.subtext2()));

            return container;
        },
        render: function() {
            var view = this;
            var model = view.model;
            if(model.isFree() || 
               model.isOverdue() ||
               model.isDunning() || 
               model.isCanceled() || 
               model.isDeactivated())
                $(view.el).html(view.makeBox());
        },
        createPopup: function() {
            var view = this;
            var model = view.model;

            if(model.isFree())
                view.freeCreatePopup();
            else if(model.isOverdue())
                view.overdueCreatePopup();
            else if(model.isCanceled())
                view.canceledCreatePopup();
            else if(model.isDeactivated())
                view.deactivatedCreatePopup();
            else
                view.payingCreatePopup();
        },
        csvPopup: function(n) {
            var view = this;
            var model = view.model;
           
            if(model.isFree())
                view.freeCSVPopup();
            else if(model.isOverdue())
                view.overdueCSVPopup();
            else if(model.isCanceled())
                view.canceledCSVPopup();
            else if(model.isDeactivated())
                view.deactivatedCSVPopup();
            else
                view.payingCSVPopup();
        },
        freeCreatePopup: function() {
            var div = $('<div />');
            Confirmation.popup({
                title: localization.blocking.free.create.title,
                content: div,
                acceptVisible: false
            });
            PricePage({header : localization.blocking.free.create.header}).show(div);
        },
        freeCSVPopup: function() {
            var div = $('<div />');
            Confirmation.popup({
                title: localization.blocking.free.csv.title,
                content: div,
                acceptVisible: false
            });
            PricePage({header : localization.blocking.free.csv.header,
                       showContact : false}).show(div);
        },
        overdueCreatePopup: function() {
            var p = $('<p />');
            p.text(localization.blocking.overdue.create.body);
            Confirmation.popup({
                title: localization.blocking.overdue.create.title,
                content: p,
                acceptText: localization.blocking.button.doublecheck,
                acceptColor: "green",
                onAccept: function() {
                    window.location = "/payments/dashboard";
                }
            });
        },
        overdueCSVPopup: function() {
            var p = $('<p />');
            p.text(localization.blocking.overdue.csv.body);
            Confirmation.popup({
                title: localization.blocking.overdue.csv.title,
                content: p,
                acceptText: localization.blocking.button.doublecheck,
                acceptColor: "green",
                onAccept: function() {
                    window.location = "/payments/dashboard";
                }
            });
        },
        canceledCreatePopup: function() {
            
        },
        canceledCSVPopup: function() {

        },
        deactivatedCreatePopup: function() {

        },
        deactivatedCSVPopup: function() {

        },
        payingCreatePopup: function() {

        },
        payingCSVPopup: function() {

        }
    });

    window.Blocking = function() {
        var model = new BlockingInfoModel({});
        var view = new BlockingInfoView({model:model});
        model.fetch({success:function() {
            model.trigger('fetch');
        }});
        return {
            model: model,
            show: function(selector) {
                $(selector).html(view.el);
            },
            shouldBlockDocs: function(n) {
                return n > model.docsLeft();
            },
            maybePopup: function(n) {
                if(this.shouldBlockDocs(n))
                    view.popup();
            }
        }
    };

}(window));
