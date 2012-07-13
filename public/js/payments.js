/*

  Payments 



*/

(function(window) {

    var planPrices = {
        basic :   199,
        branding: 279,
        advanced: 299
    };

    var maketable = function(header, rows, footer) {
        var table = $('<table />');
        var thead = $('<thead />');
        table.append(thead);
        var tfoot = $('<tfoot />');
        table.append(tfoot);
        var tbody = $('<tbody />');
        table.append(tbody);

        if(header) {
            var hrow = $('<tr />');
            thead.append(hrow);
            _.each(header, function(s) { hrow.append($('<th />').append(s)) });
        }
        if(footer) {
            var frow = $('<tr />');
            tfoot.append(frow);
            _.each(footer, function(s) { frow.append($('<td />').append(s)) });
        }
        _.each(rows, function(row) {
            var brow = $('<tr />');
            _.each(row, function(s) { brow.append($('<td />').append(s)) });
            tbody.append(brow);
        });
        return table;
    };

    var PaymentsContactModel = Backbone.Model.extend({
        firstName: function() {
            return this.get("first_name");
        },
        lastName: function() {
            return this.get("last_name");
        },
        email: function() {
            return this.get("email");
        },
        companyName: function() {
            return this.get("company_name");
        },
        country: function() {
            return this.get("country");
        }
    });

    // may be overkill?
    var PaymentsServerInfoModel = Backbone.Model.extend({
        subdomain: function() {
            return this.get("subdomain");
        }
    });

    var PaymentsAccountModel = Backbone.Model.extend({
        currency: function() {
            return this.get("currency");
        },
        accountCode: function() {
            return this.get("code");
        },
        quantity: function() {
            return this.get("quantity");
        },
        signature: function() {
            return this.get("signature");
        },
        status: function() {
            return this.get("status");
        },
        newSignup: function() {
            return this.status() === "none";
        },
        plan: function() {
            return this.get("plan");
        },
        userid: function() {
            return this.get("userid");
        },
        companyid: function() {
            return this.get("companyid");
        }
    });

    var PaymentsModel = Backbone.Model.extend({
        initialize: function(args) {
            this.set({contact: new PaymentsContactModel(args.contact),
                      server:  new PaymentsServerInfoModel(args.server),
                      account: new PaymentsAccountModel(args.account)
                     });
        },
        contact: function() {
            return this.get("contact");
        },
        server: function() {
            return this.get("server");
        },
        account: function() {
            return this.get("account");
        }
    });

    var PaymentsView = Backbone.View.extend({
        tagName: "div",
        initialize: function(args){
            _.bindAll(this);
            
        },
        showRecurlySubscriptionForm: function() {
            var view = this;
            var model = view.model;
            var $el = $(view.el);
            var form = $('<div id="js-recurly-form"></div>');
            $el.append(form);
            Recurly.config({
                subdomain: model.server().subdomain()
                , currency: model.account().currency()
                , country: model.contact().country()
            });
            Recurly.buildSubscriptionForm({
                target: '#js-recurly-form'
                , enableAddons: false
                , enableCoupons: false
                , planCode: 'advanced'
                , addressRequirement: 'full'
                //, successURL: 'http://requestb.in/1hxgzae1'
                , distinguishContactFromBillingInfo: false
                , collectCompany: true
                , accountCode: model.account().accountCode()
                //, termsOfServiceURL: 'http://example.com/tos'
                , account: {
                    firstName: model.contact().firstName()
                    , lastName: model.contact().lastName()
                    , email: model.contact().email()
                    , companyName: model.contact().companyName()
                }
                , billingInfo: {
                    firstName: model.contact().firstName()
                    , lastName: model.contact().lastName()
                    , country: model.contact().country()
                }
                , signature: model.account().signature()
                , afterInject: function(form) {
                    if(model.account().quantity() === 1)
                        $('.quantity').hide();
                    else {
                        var j = $('.quantity input');
                        j.val(model.account().quantity());
                        j.change();
                        j.hide();
                        var usersbox = $('<div class="users-count"></div>');
                        usersbox.append(model.account().quantity() + " " + localization.payments.user);
                        $('.quantity').append(usersbox);
                    }
                }
                , successHandler: function(stuff) {
                    $.ajax("/payments/subscription/result",
                           { type: "POST"
                             ,data: {account_code: model.account().accountCode(), 
                                     xtoken: readCookie("xtoken")}
                             ,dataType: "json"
                             ,success: function(data) {
                                 console.log(data);
                                 if(data.error)
                                     console.log(data.error);
                                 else {
                                     model.initialize(data);
                                     view.render();
                                 }
                             }
                           });
                }
            });
        },
        planName: function(p) {
            if(p in localization.payments.plans)
                return localization.payments.plans[p];
            return "Unknown";
        },
        planPrice: function(p) {
            if(p in planPrices)
                return planPrices[p];
            return -1;
        },
        showCurrentSubscription: function () {
            var view = this;
            var model = view.model;
            var $el = $(view.el);

            var plan = $('<div class="plan" />');
            var planheader = $('<div class="plan-header" />')
                .append(localization.payments.table.currentplan + ": ")
                .append($('<span class="plan-name" />').append(view.planName(model.account().plan())));
            
            var plantable = $('<div class="plan-table" />').append(
                maketable([localization.payments.table.item,
                           localization.payments.table.price,
                           localization.payments.table.quantity,
                           localization.payments.table.subtotal],
                          [[view.planName(model.account().plan()),
                            view.planPrice(model.account().plan()) + "kr",
                            model.account().quantity()  + " " + localization.payments.user,
                            view.planPrice(model.account().plan()) * model.account().quantity() + "kr"]],
                          ["", "", localization.payments.table.total, view.planPrice(model.account().plan()) * model.account().quantity() + "kr"]));

            plan.append(planheader).append(plantable);
            $el.append(plan);
        },
        render: function() {
            var view = this;
            var model = view.model;
            var $el = $(view.el);
            $el.empty();
            if(model.account().newSignup())
                view.showRecurlySubscriptionForm();
            else
                view.showCurrentSubscription();
        }
    });

    window.paymentsDashboardModel = null;
    window.paymentsDashboardView  = null;

    window.bootPaymentsDashboard = function(selector) {
        $("head").append('<link rel="stylesheet" href="/libs/recurly/recurly.css"></link>');

        $.ajax("/payments/info.json", 
               {
                   dataType: "json",
                   data: {plan:"advanced"},
                   success: function(data) {
                       window.paymentsDashboardModel = new PaymentsModel(data);
                       window.paymentsDashboardView  = new PaymentsView({model:window.paymentsDashboardModel});
                       window.paymentsDashboardView.render();
                       $(selector).append(window.paymentsDashboardView.el);
                   }
               });
    };

})(window);
