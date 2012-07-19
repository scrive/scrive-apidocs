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

    var invoicelist = function() {
        return KontraList().init({
            name : "Invoice Table",
            schema: new Schema({
                url: "/payments/invoices",
                sorting: new Sorting({ fields: ["date", "invoice_number", "total_in_cents", "currency", "state"]}),
                paging: new Paging({}),
                cells : [
                    new Cell({name: "Date",      width:"30px", field:"date",
                              rendering: function(value, _idx, _model) {
                                  var date = new Date(value);
                                  var curr_date  = date.getDate();
                                  var curr_month = date.getMonth() + 1; //Months are zero based
                                  var curr_year  = date.getFullYear();
                                  return curr_date + "-" + curr_month + "-" + curr_year;
                              }
                             }),
                    new Cell({name: "Invoice #", width:"30px", field:"invoice_number"}),
                    new Cell({name: "Total",     width:"30px", field:"total_in_cents"}),
                    new Cell({name: "Currency",  width:"30px", field:"currency"}),
                    new Cell({name: "Status",    width:"30px", field:"state"})
                ]
            })
        });
    };

    var PaymentsContactModel = Backbone.Model.extend({
        firstName: function(n) {
            if(n || n === '') {
                this.set({first_name:n});
                return this;
            }
            return this.get("first_name");
        },
        lastName: function(n) {
            if(n || n === '') {
                this.set({last_name:n});
                return this;
            }
            return this.get("last_name");
        },
        email: function(e) {
            if(e || e === '') {
                this.set({email:e});
                return this;
            }
            return this.get("email");
        },
        companyName: function() {
            return this.get("company_name");
        },
        country: function() {
            return this.get("country");
        },
        // the number of users in the company
        quantity: function() {
            return this.get("quantity");
        }
    });

    // may be overkill?
    var PaymentsServerInfoModel = Backbone.Model.extend({
        subdomain: function() {
            return this.get("subdomain");
        }
    });

    var PaymentsSignupModel = Backbone.Model.extend({
        defaults: {
            plan_code: 'advanced'
        },
        planCode: function(p) {
            if(p) {
                this.set({plan_code: p});
                return this;
            }
            return this.get("plan_code");
        },
        // code for account stored on both Scrive db and Recurly
        code: function(p) {
            return this.get('code');
        },
        signatures: function() {
            return this.get("signatures");
        }
    });

    var PaymentsPendingModel = Backbone.Model.extend({
        name: function() {
            return this.get("plan_name");
        },
        code: function() {
            return this.get("plan_code");
        },
        quantity: function() {
            return this.get("quantity");
        },
        unitAmountInCents: function() {
            return this.get("unit_amount_in_cents");
        }
    });

    var PaymentsSubscriptionModel = Backbone.Model.extend({
        initialize: function(args) {
            if(args.pending)
                this.set({pending: new PaymentsPendingModel(args.pending)});
            else
                this.set({pending: undefined});
        },
        name: function() {
            return this.get("plan_name");
        },
        code: function() {
            return this.get("plan_code");
        },
        quantity: function() {
            return this.get("quantity");
        },
        unitAmountInCents: function() {
            return this.get("unit_amount_in_cents");
        },
        currency: function() {
            return this.get('currency');
        },
        activated: function() {
            return this.get('activated');
        },
        cancelled: function() {
            return this.get('cancelled');
        },
        billingStarted: function() {
            return this.get('billing_started');
        },
        billingEnds: function() {
            return this.get('billing_ends');
        },
        pending: function() {
            return this.get('pending');
        }
    });

    var PaymentsPlanModel = Backbone.Model.extend({
        initialize: function(args) {
            if(args.subscription)
                this.set({subscription: new PaymentsSubscriptionModel(args.subscription)});
            else
                this.set({subscription: undefined});
        },
        accountCode: function() {
            return this.get("code");
        },
        signatures: function() {
            return this.get("signatures");
        },
        status: function() {
            return this.get("status");
        },
        plan: function(p) {
            if(p) {
                this.set({plan:p});
                return this;
            }
            return this.get("plan");
        },
        userid: function() {
            return this.get("userid");
        },
        companyid: function() {
            return this.get("companyid");
        },
        subscription: function() {
            return this.get("subscription");
        }
    });

    var PaymentsModel = Backbone.Model.extend({
        initialize: function(args) {
            this.set({contact: new PaymentsContactModel(args.contact),
                      server:  new PaymentsServerInfoModel(args.server)
                     });
            if(args.plan)
                this.set({plan: new PaymentsPlanModel(args.plan)});
            else
                this.set({plan: undefined});
            if(args.signup)
                this.set({signup: new PaymentsSignupModel(args.signup)});
            else
                this.set({signup: undefined});
            if(this.plan() && !this.invoicelist())
                this.set({invoicelist: invoicelist()});
            else
                this.set({invoicelist: undefined});
        },
        contact: function() {
            return this.get("contact");
        },
        server: function() {
            return this.get("server");
        },
        plan: function() {
            return this.get("plan");
        },
        signup: function() {
            return this.get("signup");
        },
        invoicelist: function() {
            return this.get("invoicelist");
        },
        creditcard: function(c) {
            if(c || c === "") {
                this.set({creditcard:c});
                return this;
            }
            return this.get("creditcard");
        },
        cvv: function(c) {
            if(c || c === "") {
                this.set({cvv:c});
                return this;
            }
            return this.get("cvv");
        },
        month: function(c) {
            if(c || c === "") {
                this.set({month:c});
                return this;
            }
            return this.get("month");
        },
        year: function(c) {
            if(c || c === "") {
                this.set({year:c});
                return this;
            }
            return this.get("year");
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
            
            var plans = $('<div />').addClass('plans');

            plans.append(Button.init({color: 'blue', size: 'small', text: 'Basic',
                                      onClick: function() {
                                          model.signup().planCode('basic');
                                          view.render();
                                          return false;
                                      }}).input().addClass('float-left'))
                .append(Button.init({color: 'blue', size: 'small', text: 'Branding',
                                      onClick: function() {
                                          model.signup().planCode('branding');
                                          view.render();
                                          return false;
                                      }}).input().addClass('float-left'))
                .append(Button.init({color: 'blue', size: 'big', text: 'Advanced',
                                      onClick: function() {
                                          model.signup().planCode('advanced');
                                          view.render();
                                          return false;
                                      }}).input().addClass('float-left'))
                .height(50);
            
            $el.append(plans);

            var form = $('<div id="js-recurly-form"></div>');
            $el.append(form);
            $el.append($('<div class="clear-fix" />'));
            Recurly.config({
                subdomain: model.server().subdomain()
                , currency: 'SEK'
                , country: model.contact().country()
            });
            Recurly.buildSubscriptionForm({
                target: '#js-recurly-form'
                , enableAddons: false
                , enableCoupons: false
                , planCode: model.signup().planCode()
                , addressRequirement: 'none'
                , distinguishContactFromBillingInfo: false
                , collectCompany: true
                , accountCode: model.signup().code()
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
                , signature: model.signup().signatures()[model.signup().planCode()]
                , afterInject: function(form) {
                    if(model.contact().quantity() === 1)
                        $('.quantity').hide();
                    else {
                        var j = $('.quantity input');
                        j.val(model.contact().quantity());
                        j.change();
                        j.hide();
                        var usersbox = $('<div class="users-count"></div>');
                        usersbox.append(model.contact().quantity() + " " + localization.payments.user);
                        $('.quantity').append(usersbox);
                    }
                    // we can store the field values so they won't have to type them again
                    var c = model.creditcard();
                    var cc = $('.card_number input');
                    cc.val(model.creditcard());
                    cc.change();
                    cc.change(function(e) { 
                        model.creditcard($(e.target).val()); 
                    });
                    var cvv = $('.cvv input');
                    cvv.val(model.cvv());
                    cvv.change();
                    cvv.change(function(e) {
                        model.cvv($(e.target).val());
                    });
                    var fn = $('.first_name input');
                    fn.val(model.contact().firstName());
                    fn.change();
                    fn.change(function(e) {
                        model.contact().firstName($(e.target).val());
                    });
                    var ln = $('.last_name input');
                    ln.val(model.contact().lastName());
                    ln.change();
                    ln.change(function(e) {
                        model.contact().lastName($(e.target).val());
                    });
                    var en = $('.email input');
                    en.val(model.contact().email());
                    en.change();
                    en.change(function(e) {
                        model.contact().email($(e.target).val());
                    });
                    var m = $('.month select');
                    m.val(model.month());
                    m.change();
                    m.change(function(e) {
                        model.month($(e.target).val());
                    });
                    var y = $('.year select');
                    y.val(model.year());
                    y.change();
                    y.change(function(e) {
                        model.year($(e.target).val());
                    });
                }
                , successHandler: function(stuff) {
                    LoadingDialog.open("Saving subscription.");
                    $.ajax("/payments/newsubscription",
                           { type: "POST"
                             ,data: {account_code: model.signup().code(), 
                                     xtoken: readCookie("xtoken")}
                             ,success: function() {
                                 $.ajax("/payments/info.json",
                                        {dataType: 'json',
                                         success: function(data) {
                                             model.initialize(data);
                                             view.render();
                                         }});
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
        addMark: function(p) {
            return (""+p).slice(0,-2) + "," + (""+p).slice(-2);
        },
        pricePlanTable: function() {
            return maketable(
                [$("<span />").append($('<h3 />').append("Basic")).append($('<p />').append("För distanssignering.")),
                 $("<span />").append($('<h3 />').append("Branding")).append($('<p />').append("För distanssignering.")),
                 $("<span />").append($('<h3 />').append("Advanced")).append($('<p />').append("För distanssignering."))
                ],
                [["Fria signaturer *", "Fria signaturer *", "Fria signaturer *"]

                ]
            );
        },
        showCurrentSubscription: function () {
            var view = this;
            var model = view.model;
            var $el = $(view.el);

            var plan = $('<div class="plan" />');
            var planheader = $('<div class="plan-header" />')
                .append(localization.payments.table.currentplan + ": ")
                .append($('<span class="plan-name" />').append(model.plan().subscription().name()));
            
            var plantable = $('<div class="plan-table" />').append(
                maketable([localization.payments.table.item,
                           localization.payments.table.price,
                           localization.payments.table.quantity,
                           localization.payments.table.subtotal],
                          [[model.plan().subscription().name(),
                            view.addMark(model.plan().subscription().unitAmountInCents()) + "kr",
                            model.plan().subscription().quantity()  + " " + localization.payments.user,
                            view.addMark(model.plan().subscription().unitAmountInCents() * model.plan().subscription().quantity()) + "kr"]],
                          ["", "", localization.payments.table.total, view.addMark(model.plan().subscription().unitAmountInCents() * model.plan().subscription().quantity()) + "kr"]));
            
            plan.append(planheader).append(plantable);
            $el.append(plan);

            $el.append(model.invoicelist().view.el);

            var plans = $('<div />').addClass('plans');
            plans.append($('<h3>').append('Change account type:'));

            plans.append(Button.init({color: 'blue', size: 'small', text: 'Basic',
                                      onClick: function() {
                                          if(model.plan().subscription().code() !== 'basic')
                                              var conf = Confirmation.popup({
                                                  title: "Change account",
                                                  acceptText: "Change account",
                                                  content: $('<p />').append("Your card will be credited for the remaining time this month and charged the new amount."),
                                                  submit: new Submit({url: "/payments/changeplan",
                                                                      method: "POST",
                                                                      plan: 'basic',
                                                                      ajax: true,
                                                                      ajaxsuccess: function() {
                                                                          $.ajax("/payments/info.json",
                                                                                 {dataType:'json',
                                                                                  success: function(data) {
                                                                                      model.initialize(data);
                                                                                      view.render();
                                                                                  }
                                                                                 });
                                                                      },
                                                                      onSend: function() {
                                                                          conf.view.clear();
                                                                      }
                                                                     })
                                              });
                                          return false;
                                      }}).input().addClass('float-left'))
                .append(Button.init({color: 'blue', size: 'small', text: 'Branding',
                                      onClick: function() {
                                          if(model.plan().subscription().code() !== 'branding')
                                          var conf = Confirmation.popup({
                                              title: "Change account",
                                              acceptText: "Change account",
                                              content: $('<p />').append("Your card will be credited for the remaining time this month and charged the new amount."),
                                              submit: new Submit({url: "/payments/changeplan",
                                                                  method: "POST",
                                                                  plan: 'branding',
                                                                  ajax: true,
                                                                  ajaxsuccess: function() {
                                                                          $.ajax("/payments/info.json",
                                                                                 {dataType:'json',
                                                                                  success: function(data) {
                                                                                      model.initialize(data);
                                                                                      view.render();
                                                                                  }
                                                                                 });
                                                                      },
                                                                      onSend: function() {
                                                                          conf.view.clear();
                                                                      }
                                                                 })
                                          });
                                          return false;
                                      }}).input().addClass('float-left'))
                .append(Button.init({color: 'blue', size: 'big', text: 'Advanced',
                                      onClick: function() {
                                          if(model.plan().subscription().code() !== 'advanced')
                                          var conf = Confirmation.popup({
                                              title: "Change account",
                                              acceptText: "Change account",
                                              content: $('<p />').append("Your card will be credited for the remaining time this month and charged the new amount."),
                                              submit: new Submit({url: "/payments/changeplan",
                                                                  method: "POST",
                                                                  plan: 'advanced',
                                                                  ajax: true,
                                                                  ajaxsuccess: function() {
                                                                      $.ajax("/payments/info.json",
                                                                             {dataType:'json',
                                                                              success: function(data) {
                                                                                  model.initialize(data);
                                                                                  view.render();
                                                                              }
                                                                             });
                                                                  },
                                                                  onSend: function() {
                                                                      conf.view.clear();
                                                                  }
                                                                 })
                                          });
                                          return false;
                                      }}).input().addClass('float-left'))
                .height(90);
            
            $el.append(plans);


            $el.append(view.cancelButton());
            $el.append($('<div />').addClass('js-billing-button').append(view.changeBillingButton()));
        },
        cancelButton: function() {
            var view = this;
            var model = view.model;
            var button = Button.init({color: "red"
                                     ,size: "small"
                                     ,text: "Cancel account"
                                     ,onClick: function() {
                                         $.ajax("/payments/changeplan",
                                                {dataType: "json",
                                                 type: "POST",
                                                 data: {xtoken: readCookie("xtoken"),
                                                        plan: 'free'},
                                                 success: function() {
                                                     $.ajax("/payments/info.json",
                                                            {
                                                                dataType: "json",
                                                                success: function(data) {
                                                                    model.initialize(data);
                                                                    view.render();
                                                                }
                                                            });
                                                 }
                                                });
                                         return false;
                                     }});
            return button.input();
        },
        changeBillingButton: function() {
            var view = this;
            var model = view.model;
            var button = Button.init({color: "green"
                                     ,size: "small"
                                     ,text: "Change billing info"
                                     ,onClick: function() {
                                         Recurly.config({
                                             subdomain: model.server().subdomain()
                                             , currency: 'SEK'
                                             , country: model.contact().country()
                                         });
                                         Recurly.buildBillingInfoUpdateForm(
                                             {target: ".js-billing-button", 
                                              signature: model.plan().signatures().billing, 
                                              accountCode: model.plan().accountCode(),
                                              account: {
                                                  firstName: model.contact().firstName()
                                                  , lastName: model.contact().lastName()
                                                  , email: model.contact().email()
                                                  , companyName: model.contact().companyName()
                                              },
                                              billingInfo: {
                                                  firstName: model.contact().firstName()
                                                  , lastName: model.contact().lastName()
                                                  , country: model.contact().country()
                                              },
                                              addressRequirement: 'none',
                                              distinguishContactFromBillingInfo: false,
                                              collectCompany: true,
                                              successHandler: function(stuff) {
                                                  $.ajax("/payments/info.json",
                                                         {
                                                             dataType: "json",
                                                             success: function(data) {
                                                                 model.initialize(data);
                                                                 view.render();
                                                             }
                                                         });
                                                  
                                              }
                                             }
                                         );
                                     }});
            return button.input();
        },
        render: function() {
            var view = this;
            var model = view.model;
            var $el = $(view.el);
            $el.empty();
            if(model.signup())
                view.showRecurlySubscriptionForm();
            else
                view.showCurrentSubscription();
            LoadingDialog.close();
        }
    });

    window.paymentsDashboardModel = null;
    window.paymentsDashboardView  = null;

    window.bootPaymentsDashboard = function(selector) {
        LoadingDialog.open("Loading");
        $("head").append('<link rel="stylesheet" href="/libs/recurly/recurly.css"></link>');

        $.ajax("/payments/info.json", 
               {
                   dataType: "json",
                   success: function(data) {
                       window.paymentsDashboardModel = new PaymentsModel(data);
                       window.paymentsDashboardView  = new PaymentsView({model:window.paymentsDashboardModel});
                       window.paymentsDashboardView.render();
                       $(selector).append(window.paymentsDashboardView.el);
                   }
               });
    };

})(window);
