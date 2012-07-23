/*

  Payments 



*/

(function(window) {

    var planPrices = {
        basic :   19900,
        branding: 27900,
        advanced: 29900
    };

    var planPrice = function(p) {
        if(p in planPrices)
            return planPrices[p];
        return -1;
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

    var PaymentsInvoiceModel = Backbone.Model.extend({
        number: function() {
            return this.get('invoice_number');
        },
        totalInCents: function() {
            return this.get('total_in_cents');
        },
        currency: function() {
            return this.get('currency');
        },
        state: function() {
            return this.get('state');
        },
        date: function() {
            return new Date(this.get('date'));
        }
    });

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
            if(args.invoices)
                this.set({invoices: _.map(args.invoices, function(i) { return new PaymentsInvoiceModel(i) })});
            else
                this.set({invoices: undefined});
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
        },
        invoices: function() {
            return this.get("invoices");
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
        className: "payments",
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
                    LoadingDialog.open("Saving subscription");
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
        planName: function() {
            var pOrS = this.model.plan().subscription().pending() || this.model.plan().subscription();
            return $('<div class="plan-name" />').text(pOrS.name());
        },
        planPrice: function() {
            var view = this;
            var pOrS = this.model.plan().subscription().pending() || this.model.plan().subscription();
            var amount   = pOrS.unitAmountInCents();
            var amountf  = view.addMark(amount);
            var currency = this.model.plan().subscription().currency();
            var quantity = pOrS.quantity();
            var txt = 
                amountf + " " + currency + " x " + quantity + " " + localization.payments.user;
            return $('<div class="plan-price" />').text(txt);
        },
        planTotal: function() {
            var view = this;
            var pOrS = this.model.plan().subscription().pending() || this.model.plan().subscription();
            var amount   = pOrS.unitAmountInCents();
            var quantity = pOrS.quantity();
            var total    = amount * quantity;
            var totalf   = view.addMark(total);
            var currency = this.model.plan().subscription().currency();
            var txt      = "Total: " + totalf + " " + currency + " " + "billed monthly." ;
            return $('<div class="plan-total" />').text(txt);
        },
        pendingLine: function() {
            var plan = this.model.plan().subscription();
            var txt = "You have " + plan.name() + " access until " + plan.billingEnds() + ".";
            return $('<div class="plan-pending" />').text(txt);
        },
        currentSubscriptionTable: function() {
            var view = this;
            var model = view.model;
            
            var plan = $('<div class="plan" />');
            var planheader = $('<div class="header" />')
                .text(localization.payments.table.currentplan);
            
            var plantable = $('<div class="table" />')
                .append(view.planName())
                .append(view.planPrice())
                .append(view.planTotal());

            if(model.plan().subscription().pending())
                plantable.append(view.pendingLine());

            return plan.append(planheader).append(plantable);
        },
        nextPayment: function() {
            var view = this;
            var model = view.model;

            var pOrS = model.plan().subscription().pending() 
                || model.plan().subscription();
            var billingEnds  = model.plan().subscription().billingEnds();
            var billingEndsf = billingEnds.toString(); // todo: change to something better
            var price        = pOrS.unitAmountInCents();
            var quantity     = pOrS.quantity();
            var total        = price * quantity;
            var currency     = model.plan().subscription().currency();
            var totalf       = view.addMark(total) + " " + currency;

            var div = $('<div class="next-payment" />');
            var header = $('<div class="subheader" />')
                .text(localization.payments.table.nextpayment);
            var line = $('<div class="line" />')
                .append($('<span class="invoice-date" />').text(billingEndsf))
                .append($('<span class="total" />').text(totalf));

            return div.append(header).append(line);
        },
        previousPayments: function() {
            var view = this;
            var model = view.model;

            var div = $('<div class="previous-payments" />');
            var header = $('<div class="subheader" />')
                .text(localization.payments.table.previouspayments);
            var lines = $([]);
            _.each(model.plan().invoices(), function(invoice) {
                var date   = invoice.date();
                var datef  = date.toString(); // todo: change to something better
                var total  = invoice.totalInCents();
                var currency     = model.plan().subscription().currency();
                var totalf = view.addMark(total) + " " + currency;
                var line   = $('<div class="line" />')
                    .append($('<span class="invoice-date" />').text(datef))
                    .append($('<span class="total" />').text(totalf));
                lines.add(line);
            });

            if(lines.length === 0)
                lines = $('<div class="line" />').text(localization.payments.table.none);

            return div
                .append(header)
                .append(lines);
        },
        paymentsTable: function() {
            var view = this;
            var model = view.model;
            
            var payments = $('<div class="payments" />');
            var paymentsheader = $('<div class="header" />')
                .text(localization.payments.table.payments);
            var paymentstable = $('<div class="table" />')
                .append(view.nextPayment())
                .append(view.previousPayments());

            return payments.append(paymentsheader).append(paymentstable);
        },
        changeSubscription: function() {
            var view = this;
            var model = view.model;
            //var pOrS = model.plan().subscription().pending() || model.plan().subscription();

            var changesub = $('<div class="changesubscription" />');
            var changesubheader = $('<div class="header" />')
                .text(localization.payments.table.changesubscription);
            var changesubtable = $('<div class="table" />');

            var select = $('<select name="plan" id="js-select-subscription" />');
            
            _.each(['basic', 'branding', 'advanced'], function(value) {
                var name = localization.payments.plans[value];
                var price = planPrice(value);
                var pricef = view.addMark(price);
                var currency = model.plan().subscription().currency();
                var txt = 
                    name + " " + pricef + " " + currency;
                select.append($("<option />").attr("value", value).text(txt));
            });

            // todo: make advanced selected

            var totaltable = $('<div class="totaltable" />');

            select.change(function() {
                var plan     = select.val();
                var quantity = model.plan().subscription().quantity();
                var price    = planPrice(plan);
                var pricef   = view.addMark(price);
                var total    = price * quantity;
                var totalf   = view.addMark(total);
                var currency = model.plan().subscription().currency();
                
                totaltable.empty();
                totaltable
                    .append($('<div class="plan-price" />')
                            .text(pricef + " " + currency +
                                  " / month x " + quantity  + 
                                  " " + localization.payments.user))
                    .append($('<div class="plan-total" />')
                            .text("Total: " + totalf + " " + currency + " " + "billed monthly"));
            });

            select.change();

            var button = Button.init({color: 'green',
                                      size: 'small',
                                      cssClass: 'savechanges',
                                      text: 'Save Changes',
                                      onClick: function() {
                                          var message = "Your card will be credited for the remaining time this month and charged the new amount.";
                                          
                                          var conf = Confirmation.popup({
                                              title: "Change account",
                                              acceptText: "Change account",
                                              content: $('<p />').text(message),
                                              submit: new Submit({url: "/payments/changeplan",
                                                                  method: "POST",
                                                                  inputs: select,
                                                                  ajax: true,
                                                                  ajaxsuccess: function() {
                                                                      // Put a popup here?
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
                                                                      LoadingDialog.open("Saving");
                                                                  }
                                                                 })
                                          });
                                          return false;
                                          
                                      }
                                     });

            return changesub
                .append(changesubheader)
                .append(changesubtable
                        .append($('<span />').text("Select Plan: ").append(select))
                        .append(totaltable))
                .append(button.input())
                .append($('<div class="clearfix" />'));
        },
        changeBillingForm: function() {
            var view = this;
            var model = view.model;
            
            var billing = $('<div class="changebilling" />');
            var billingheader = $('<div class="header" />')
                .text(localization.payments.table.changebilling);
            var billingform = $('<div class="changebilling-form" />');
            Recurly.config({
                subdomain: model.server().subdomain()
                , currency: 'SEK'
                , country: model.contact().country()
            });
            Recurly.buildBillingInfoUpdateForm(
                {target: billingform, // will this work?
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
                            { dataType: "json",
                              success: function(data) {
                                  model.initialize(data);
                                  view.render();
                              }
                            });
                 }
                }
            );
            return billing.append(billingheader).append(billingform);
        },
        showCurrentSubscription: function () {
            var view = this;
            var model = view.model;
            var $el = $(view.el);

            $el.append(view.currentSubscriptionTable())
                .append(view.paymentsTable())
                .append(view.cancelButton())
                .append($('<div class="clearfix" />'))
                .append(view.changeSubscription())
                .append(view.changeBillingForm());

        },
        cancelButton: function() {
            var view   = this;
            var model  = view.model;
            var action = function() {
                $.ajax("/payments/changeplan",
                       {dataType: "json",
                        type: "POST",
                        data: {xtoken: readCookie("xtoken"),
                               plan: 'free'},
                        success: function() {
                            $.ajax("/payments/info.json",
                                   {dataType: "json",
                                    success: function(data) {
                                        model.initialize(data);
                                        view.render();
                                    }
                                   });
                        }
                       });
                return false;
            };
            var button = Button.init({color: "red"
                                      ,size: "small"
                                      ,text: "Cancel account"
                                      ,onClick: action
                                      ,cssClass: "cancel"});
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

// spare code

/*
                maketable([localization.payments.table.item,
                           localization.payments.table.price,
                           localization.payments.table.quantity,
                           localization.payments.table.subtotal],
                          [[model.plan().subscription().name(),
                            view.addMark(model.plan().subscription().unitAmountInCents()) + "kr",
                            model.plan().subscription().quantity()  + " " + localization.payments.user,
                            view.addMark(model.plan().subscription().unitAmountInCents() * model.plan().subscription().quantity()) + "kr"]],
                          ["", "", localization.payments.table.total, view.addMark(model.plan().subscription().unitAmountInCents() * model.plan().subscription().quantity()) + "kr"]));
*/

/*

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

*/
/*
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
*/
/*
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
*/
