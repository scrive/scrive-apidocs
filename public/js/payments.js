/*

  Payments

  Author and Maintainer: Eric Normand

*/

(function(window) {

    var PaymentsPendingModel = Backbone.Model.extend({
        name: function() {
            return this.get("planName");
        },
        code: function() {
            return this.get("planCode");
        },
        quantity: function() {
            return this.get("quantity");
        },
        unitAmountInCents: function() {
            return this.get("unitAmountInCents");
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
            return this.get("planName");
        },
        code: function() {
            return this.get("planCode");
        },
        quantity: function() {
            return this.get("quantity");
        },
        unitAmountInCents: function() {
            return this.get("unitAmountInCents");
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
            return new Date(this.get('billingStarted'));
        },
        billingEnds: function() {
            return new Date(this.get('billingEnds'));
        },
        pending: function() {
            return this.get('pending');
        }
    });

    var PaymentsInvoiceModel = Backbone.Model.extend({
        number: function() {
            return this.get('invoiceNumber');
        },
        totalInCents: function() {
            return this.get('totalInCents');
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

    var PricePageModel = Backbone.Model.extend({
        defaults: {
            firstName: '',
            lastName: '',
            email: '',
            creditCard: '',
            ccv: '',
            year: '',
            month: '',
            createdUser: false,
            accountCreated: false,
            currentPlan: 'team',
            header: localization.payments.subscribeheader
        },
        reset: function() {
            if(this.get('firstName'))
                this.set({hasFirstName:true});
            if(this.get('lastName'))
                this.set({hasLastName:true});
            if(this.get('email'))
                this.set({hasEmail:true});
        },
        initialize: function(args) {
            _.bindAll(this);
            this.bind('fetch', this.reset);
            this.reset();
        },
        firstName: function() {
            return this.get("firstName");
        },
        setFirstName: function(n) {
            this.set({firstName:n});
            return this;
        },
        hasFirstName: function() {
            return this.get('hasFirstName');
        },
        lastName: function() {
            return this.get("lastName");
        },
        setLastName: function(n) {
            this.set({lastName:n});
            return this;
        },
        hasLastName: function() {
            return this.get('hasLastName');
        },
        email: function() {
            return this.get("email");
        },
        setEmail: function(e) {
            this.set({email:e});
            return this;
        },
        hasEmail: function() {
            return this.get('hasEmail');
        },
        companyName: function() {
            return this.get('companyName');
        },
        setCompanyName: function(cn) {
            this.set({companyName:cn});
            return this;
        },
        accountCode: function() {
            return this.get('accountCode');
        },
        setAccountCode: function(ac) {
            this.set({accountCode:ac});
            return this;
        },
        plans: function() {
            return this.get('plans');
        },
        subdomain: function() {
            return this.get('subdomain');
        },
        currentPlan: function() {
            return this.get('currentPlan');
        },
        setCurrentPlan: function(plan) {
            if(plan !== this.get('currentPlan'))
                this.set({currentPlan:plan});
            return this;
        },
        creditCard: function() {
            return this.get('creditCard');
        },
        setCreditCard: function(cc) {
            this.set({creditCard:cc});
            return this;
        },
        cvv: function() {
            return this.get('ccv');
        },
        setCvv: function(ccv) {
            this.set({ccv:ccv});
            return this;
        },
        month: function() {
            return this.get('month');
        },
        setMonth: function(m) {
            this.set({m:m});
            return this;
        },
        year: function() {
            return this.get('year');
        },
        setYear: function(y) {
            this.set({year:y});
            return this;
        },
        createdUser: function() {
            return this.get('createdUser');            
        },
        setCreatedUser: function(b) {
            this.set({createdUser:b});
            return this;
        },
        accountCreated: function(){
            return this.get('accountCreated');
        },
        setAccountCreated: function(b) {
            this.set({accountCreated:b});
            return this;
        },
        paidPlan: function() {
            return this.get('paidPlan');
        },
        status: function() {
            return this.get('status');
        },
        subscription: function() {
            return new PaymentsSubscriptionModel(this.get('subscription'));
        },
        invoices: function() {
            return _.map(this.get('invoices'), function(i) { return new PaymentsInvoiceModel(i) });
        },
        billingSig: function() {
            return this.get('billingSig');
        },
        header: function() {
            return this.get('header');
        },
        type: function() {
            return this.get('type');
        },
        quantity: function() {
            return this.get('quantity');
        },
        url: function() {
            return "/payments/pricepageinfo";
        },
        createaccount: function(email, firstname, lastname, callback) {
            var model = this;
            $.ajax('/payments/createuser',
                   {type: 'POST',
                    data: {email: email,
                           firstName: firstname,
                           lastName: lastname,
                           xtoken: readCookie("xtoken")},
                    timeout: 1000,
                    dataType: 'json',
                    error: function() {
                        // what to do?
                    },
                    success: function(data) {
                        model.setCreatedUser(true);
                        model.setAccountCreated(true);
                        callback(data);
                    }
                   });
        },
        checkuserexists: function(email, callback) {
            $.ajax('/payments/userexists',
                   {data: {email:email},
                    timeout: 500,
                    dataType: 'json',
                    success: callback,
                    error: function() {
                        // what to do with an error?
                    }});
        },
        submitSubscription: function(callback) {
            var model = this;
            $.ajax("/payments/newsubscriptionoutside",
                   { type: "POST"
                     ,data: {accountCode : model.accountCode(), 
                             email        : model.email()}
                     ,success: callback
                   });
        }
    });

    var PlanBoxView = Backbone.View.extend({
        className: "planbox",
        initialize: function(args) {
            _.bindAll(this);
            args.model.bind('change:currentPlan', this.render);
            args.model.bind('fetch', this.render);
            this.plan = args.plan;
            this.onClick = args.onClick;
            var view = this;
            this.$el.click(function() {
                view.onClick();
                return false;
            });
            this.$el.addClass(this.plan);
        },
        render: function() {
            var view = this;
            var model = view.model;
            // placeholder div
            var div = $('<div class="team planbox" />');

            var header = $('<div class="header" />');
            var header1 = $('<div class="header1" />');
            header1.text(localization.payments.plans[view.plan].name);
            var header2 = $('<div class="header2" />');
            header2.html(localization.payments.plans[view.plan].tag);
            header.append(header1).append(header2);
            div.append(header);

            var features = $('<div class="features" />');
            _.each(localization.payments.plans[view.plan].features, function(t) {
                features.append($('<div class="feature" />').html(t));
            });

            features.append($('<a />').attr('href', localization.payments.featuresurl)
                            .addClass('readmorelink')
                            .text(localization.payments.readmore)
                            .click(function(e) { 
                                e.stopPropagation();
                            }));

            div.append(features);

            var price = $('<div class="price" />');
            var price1 = $('<div class="price1" />');
            price1.text(localization.payments.plans[view.plan].price1);
            var price2 = $('<div class="price2" />');
            price2.text(localization.payments.plans[view.plan].price);
            var price3 = $('<div class="price3" />');
            price3.text(localization.payments.plans[view.plan].price3);
            price.append(price1).append(price2).append(price3);
            var price21 = $('<div class="price2 s" />');
            price21.text(localization.payments.plans[view.plan].price21);
            var price31 = $('<div class="price3" />');
            price31.text(localization.payments.plans[view.plan].price31);
            price.append(price21).append(price31);

            div.append(price);

            var color = 'black';
            var text = localization.payments.plans[view.plan].select;
            
            if(model.currentPlan() == view.plan) {
                color = 'green';
                text = localization.payments.plans[view.plan].selected;
                view.$el.addClass('selected');
            } else {
                view.$el.removeClass('selected');
            }

            var buttonbox = $('<div class="buttonbox" />');
            var button = Button.init({color: color,
                                      text: text,
                                      size: 'small',
                                      width: 150,
                                      onClick: function() {
                                          return false;
                                      }});
            
            buttonbox.append(button.input());
            div.append(buttonbox);
            view.$el.html(div.contents());
        }
    });

    var RecurlyView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this);
            this.element = $('<div />');
            this.$el.append(this.element);
            args.model.bind('change:currentPlan', this.render);
            args.model.bind('change:accountCreated', this.render);
            args.model.bind('fetch', this.render);
            this.hideContacts = args.hideContacts;
        },
        scrambleForm: function(form) {
            var view = this;
            var model = view.model;
            form = $(form);
            var div = $('<div class="borderbox" />');
            div.append(form.contents());
            form.append(div);
            
            form.find('.server_errors').prependTo(form);

            // set the quantity that we found from the db
            var quantbox = form.find('.quantity input');

            var guarantee = $('<div class="guarantee" />');
            guarantee.text(localization.payments.guarantee);

            if((model.currentPlan() || 'team') == 'team') {
                var q = model.quantity() || 1;
                quantbox.val(q);
                var usertext = q === 1 ? localization.payments.user : localization.payments.users;
                quantbox.after($('<span class="quantity" />').text(q + " " + usertext));
                form.find('.quantity .label').hide();
            } else {
                quantbox.val(1);
                form.find('.quantity .label').text("You next invoice will include: ");
                quantbox.after($('<span class="quantity" />').html("&nbsp;"));
                guarantee.text(localization.and + " " + localization.payments.plans.docprice + " " + localization.payments.perdocument + " " + localization.payments.signed);
            }
            quantbox.change();                    
            quantbox.hide();

            var permonth = $('<span />');
            permonth.text(" " + localization.payments.permonth);

            form.find('.due_now').append(permonth).append(guarantee);
            form.find('.due_now .title').hide();

            var work = true;
            var handlechargeaccount = function(data) {
                // three cases
                if(!data.user_exists) {
                    // create the user and charge the account
                    if(work) {
                        work = false;
                        model.createaccount(model.email(),
                                            model.firstName(), 
                                            model.lastName(), 
                                            function(data) {
                                                LoadingDialog.close();
                                                if(data.success)
                                                    form.submit();
                                                // what else?
                                            });
                        work = true;
                    }
                } else if(data.user_exists && !data.has_plan) {
                    // charge the existing account
                    LoadingDialog.close();
                    if(work) {
                        work = false;
                        form.submit();
                        work = true;
                    }
                } else {
                    // show message (they should log in)
                    LoadingDialog.close();
                    var text = localization.payments.outside.sorryExistingUser;
                    var header = localization.payments.outside.sorryExistingUserHeader;
                    var popup = Confirmation.popup({
                        title: header,
                        content: $('<p />').text(text),
                        onAccept: function(){
                            popup.view.clear();
                        }
                    });
                }
            };
            
            // replace button with our own
            var button = Button.init({color:'green',
                                      size:'big',
                                      cssClass:'s-subscribe',
                                      text:localization.payments.subscribe,
                                      onClick: function() {
                                          view.validator.validate(function() {
                                              LoadingDialog.open(localization.payments.loading);
                                              model.checkuserexists(model.email(),
                                                                    handlechargeaccount);
                                          });
                                      }});
            div.find('button').replaceWith(button.input());
            
            var expires = form.find('.field.expires');
            form.find('.field.cvv').insertAfter(expires);
            expires.find('.title').text(localization.payments.expires);
            form.find('.card_number .placeholder').text(localization.payments.creditcardnumber);
            form.find('.field .month').after($('<span />').text(" / "));

            // we can store the field values so they won't have to type them again
            var cc = form.find('.card_number input');
            cc.val(model.creditCard());
            cc.change();
            cc.change(function(e) { 
                model.setCreditCard($(e.target).val()); 
            });
            var cvv = form.find('.cvv input');
            cvv.val(model.cvv());
            cvv.change();
            cvv.change(function(e) {
                model.setCvv($(e.target).val());
            });
            var fn = form.find('.first_name input');
            fn.val(model.firstName());
            fn.change();
            fn.change(function(e) {
                model.setFirstName($(e.target).val());
            });
            var ln = form.find('.last_name input');
            ln.val(model.lastName());
            ln.change();
            ln.change(function(e) {
                model.setLastName($(e.target).val());
            });

            var en = form.find('.email input');
            en.val(model.email());
            en.change();
            en.change(function(e) {
                model.setEmail($(e.target).val());
            });
            var m = form.find('.month select');
            m.val(model.month());
            m.change();
            m.change(function(e) {
                model.setMonth($(e.target).val());
            });
            var y = form.find('.year select');
            y.val(model.year());
            y.change();
            y.change(function(e) {
                model.setYear($(e.target).val());
            });
            if(view.hideContacts) {
                if(model.hasFirstName() && model.hasLastName() && model.hasEmail()) {
                    fn.hide();
                    ln.hide();
                    en.hide();
                    form.find('.contact_info').hide();
                }
            }
        },
        render: function() {
            var view = this;
            var model = view.model;

            if(model.accountCreated()) {
                view.$el.children().detach();
                return false;
            }

            Recurly.config({
                subdomain: model.subdomain()
                , currency: 'SEK'
                , country: 'SE'
            });

            this.validator = Recurly.buildSubscriptionForm({
                target: view.element
                , enableAddons: false
                , enableCoupons: false
                , planCode: model.currentPlan() || 'team'
                , addressRequirement: 'none'
                , distinguishContactFromBillingInfo: false
                , collectCompany: false
                , acceptedCards : ['mastercard', 'visa']
                , accountCode: model.accountCode()
                , account: {
                    firstName: model.firstName()
                    , lastName: model.lastName()
                    , email: model.email()
                }
                , billingInfo: {
                    firstName: model.firstName()
                    , lastName: model.lastName()
                    , country: 'SE'
                }
                , signature: model.plans()[model.currentPlan() || 'team'].signature
                , beforeInject: this.scrambleForm
                , successHandler: function(stuff) {
                    LoadingDialog.open(localization.payments.savingsubscription);
                    model.submitSubscription(function() {
                        var text;
                        var header;
                        if(model.type() === 'user') {
                            window.location = '/account#subscription';
                            return true;
                        }

                        LoadingDialog.close();
                        if(model.createdUser()) {
                            text = localization.payments.outside.confirmAccountCreatedUser;
                            header = localization.payments.outside.confirmAccountCreatedUserHeader;
                        } else {
                            text = localization.payments.outside.confirmAccountExistingUser;
                            header = localization.payments.outside.confirmAccountExistingUserHeader;
                        }
                        var popup = Confirmation.popup({
                            title: header,
                            content: $('<p />').text(text),
                            onAccept: function() {
                                if(model.type() === 'user') {
                                    window.location = '/account#subscription';
                                } else if(model.createdUser()) {
                                    popup.view.clear();
                                    Login({referer:'/upload'});
                                } else {
                                    Login({referer:'/d'});
                                }
                            }
                        });
                    });
                }
            });
        }
    });

    var PricePageView = Backbone.View.extend({
        className: "payments",
        initialize: function(args){
            var view = this;
            _.bindAll(this);
            this.teamBox = new PlanBoxView({model: args.model,
                                            plan:'team',
                                            onClick: function() { 
                                                if(view.model.currentPlan() !== 'team')
                                                    view.model.setCurrentPlan('team');
                                            }});
            this.formBox = new PlanBoxView({model: args.model,
                                            plan:'form', 
                                            onClick: function() { 
                                                view.getInTouch();
                                            }});
            this.enterpriseBox = new PlanBoxView({model: args.model,
                                                  plan:'enterprise', 
                                                  onClick: function() {
                                                      view.getInTouch();
                                                  }});
            this.recurlyForm = new RecurlyView({model: args.model, hideContacts: args.hideContacts});
            view.model.bind('fetch', this.render);
        },
        render: function() {
            var view = this;
            var model = view.model;
            var div = $('<div />');

            var header = $('<div class="header" />')
                .append($('<h2 />').text(model.header()));

            div.append(header);
            div.append($('<h3 />').text(localization.payments.chooseplan));

            div.append(view.teamBox.el)
                .append(view.formBox.el)
                .append(view.enterpriseBox.el);

            div.append($('<div class="clearfix" />'));
            div.append($('<h3 />').text(localization.payments.paywithcard));

            div.append(view.recurlyForm.el);
            
            view.$el.html(div.contents());
        },
        getInTouch: function() {
            var view = this;
            var model = view.model;
            var form = $("<div />");
            var popup;
            var numberinput = InfoTextInput.init({infotext: localization.payments.phonenumber, 
                                                  onEnter: function() {
                                                      popup.model.accept();
                                                      return false;
                                                  }
                                                 }).input().blur().change();
            form.append(numberinput);

            var content = $('<div />');
            content.append($('<p />').text(localization.payments.pleaseleavenumber));
            content.append(form);
            var done = false;
            popup = Confirmation.popup({
                //acceptText: localization.payments.ok,
                title: localization.payments.pleaseleavenumbertitle,
                content: content,
                onAccept: function() {
                    if(done)
                        popup.view.clear();
                    var phone = numberinput.val();
                    if (phone.trim().length == 0) return;
                    new Submit({
                        url: "/account/phoneme",
                        method: "POST",
                        email: model.email(),
                        phone: phone,
                        ajax: true,
                        onSend: function() {
                            content.empty();
                            content.append("<div class='loading payments'/>");
                        },
                        ajaxerror: function(d, a) {
                            content.empty();
                            content.append($("<div/>").text(localization.docsignview.phoneConfirmationText));
                            done = true;
                        },
                        ajaxsuccess: function(d) {
                            content.empty();
                            content.append($("<div/>").text(localization.docsignview.phoneConfirmationText));
                            done = true;
                        }
                    }).send();
                }
            });
        }
    });

    var PaymentsDashboardNoneView = Backbone.View.extend({
        className: 'noprovider',
        initialize: function(args) {
            var view = this;
            view.model = args.model;
            _.bindAll(this);
            view.model.bind('fetch', this.render);
        },
        render: function() {
            var view = this;
            var model = view.model;
            var $el = $(view.el);

            var div = $('<div class="col" />');
            var header = $('<div class="account-header" />')
                .append($('<h2 />')
                        .text(localization.payments.table.currentplan));
            var table = $('<div class="account-body" />')
                .append($('<div class="plan-name" />')
                        .text(localization.payments.plans[model.paidPlan()].name))
                .append($('<p />')
                       .text(model.quantity() + " " + localization.payments.users))
                .append($('<p class="askviktor" />')
                        .html(localization.payments.askviktor));

            $el.html(div.append(header).append(table));
        }
    });

    var InvoicePaymentsView = Backbone.View.extend({
        className: 'subscription-payments',
        initialize: function(args) {
            var view = this;
            view.model = args.model;
            _.bindAll(this);
            view.model.bind('fetch', this.render);
        },
        render: function() {
            var view = this;
            var model = view.model;
            
            var payments = $('<div class="col" />');
            var paymentsheader = $('<div class="account-header" />')
                .append($('<h2 />')
                        .text(localization.payments.table.payments));
            var paymentstable = $('<div class="account-body" />')
                .append(view.nextPayment())
                .append(view.previousPayments());

            payments.append(paymentsheader).append(paymentstable);
            view.$el.html(payments);
        },
        dateString: function(d) {
            return moment(d).format("YYYY-MM-DD");
        },
        addMark: function(p) {
            if(p === 0)
                return "0,00";
            return (""+p).slice(0,-2) + "," + (""+p).slice(-2);
        },
        nextPayment: function() {
            var view = this;
            var model = view.model;

            var pOrS = model.subscription().pending() 
                || model.subscription();
            var billingEnds  = model.subscription().billingEnds();
            var billingEndsf = view.dateString(billingEnds);
            var price        = pOrS.unitAmountInCents();
            var quantity     = pOrS.quantity();
            var total        = price * quantity;
            var currency     = model.subscription().currency();
            var totalf       = view.addMark(total) + " " + currency;

            var div = $('<div class="next-payment" />');
            var header = $('<div class="subheader" />')
                .text(localization.payments.table.nextpayment);
            var line = $('<div class="line" />')
                .append($('<span class="invoice-date" />').text(billingEndsf))
                .append($('<span class="total" />').text(totalf));

            if(total === 0)
                line = $('<div class="line" />').text(localization.payments.table.none);

            return div.append(header).append(line);
        },
        previousPayments: function() {
            var view = this;
            var model = view.model;

            var div = $('<div class="previous-payments" />');
            var header = $('<div class="subheader" />')
                .text(localization.payments.table.previouspayments);
            var lines = $();
            _.each(model.invoices(), function(invoice) {
                var date   = invoice.date();
                var datef  = view.dateString(date);
                var total  = invoice.totalInCents();
                var currency     = model.subscription().currency();
                var totalf = view.addMark(total) + " " + currency;
                var status = invoice.state();
                var line   = $('<div class="line" />')
                    .append($('<span class="invoice-date" />').text(datef))
                    .append($('<span class="invoice-status" />').text(status))
                    .append($('<span class="total" />').text(totalf));
                lines = lines.add(line);
            });

            if(lines.length === 0)
                lines = $('<div class="line" />').text(localization.payments.table.none);

            return div
                .append(header)
                .append(lines);
        }
    });

    var ChangeBillingFormView = Backbone.View.extend({
        className: 'col',
        initialize: function(args) {
            var view = this;
            view.model = args.model;
            _.bindAll(this);
            view.model.bind('fetch', this.render);
        },
        render: function() {
            var view = this;
            var model = view.model;
            var $el = $(view.el);
            
            var billing = $('<div class="col" />');
            var billingheader = $('<div class="account-header" />')
                .append($('<h2 />')
                        .text(localization.payments.table.changebilling));
            var billingform = $('<div class="account-body changebilling-form" />');
            Recurly.config({
                subdomain: model.subdomain()
                , currency: 'SEK'
                , country: 'SE'
            });
            Recurly.buildBillingInfoUpdateForm(
                {target: billingform,
                 signature: model.billingSig(),
                 accountCode: model.accountCode(),
                 acceptedCards : ['mastercard', 'visa'],
                 account: {
                     firstName: model.firstName()
                     , lastName: model.lastName()
                     , email: model.email()
                     , companyName: model.companyName()
                 },
                 billingInfo: {
                     firstName: model.firstName()
                     , lastName: model.lastName()
                     , country: 'SE'
                 },
                 beforeInject: function(el) {
                     el = $(el);
                     var work = true;
                     var button = Button.init({color:'green',
                                               size:'small',
                                               text:localization.payments.savechanges,
                                               onClick: function() {
                                                   if(work) {
                                                       work = false;
                                                       $('.changebilling-form form').submit();
                                                       work = true;
                                                       return false;
                                                   }
                                               }
                                              });
                     el.find('button').replaceWith(button.input());
                 },
                 addressRequirement: 'none',
                 successHandler: function(stuff) {
                     model.fetch({success: function() {
                         model.trigger('fetch');
                     }});
                 }
                }
            );
            billingform.append($('<div class="cancel" />').append(view.cancelButton()));
            $el.html($().add(billingheader).add(billingform));
        },
        cancelButton: function() {
            var view = this;
            var model = view.model;

            var button = Button.init({color:'red',
                                      size: 'small',
                                      cssClass: 'cancel-button',
                                      text: localization.payments.cancelsubscription,
                                      onClick: function() {
                                          var message = localization.payments.changeconfirm;
                                          
                                          var conf = Confirmation.popup({
                                              title: localization.payments.changeaccount,
                                              acceptText: localization.payments.changeaccount,
                                              content: $('<p />').text(message),
                                              submit: new Submit({url: "/payments/changeplan",
                                                                  method: "POST",
                                                                  plan: "free",
                                                                  ajax: true,
                                                                  ajaxsuccess: function() {
                                                                      model.fetch({success:function() {
                                                                          model.trigger('fetch');
                                                                      }});
                                                                  },
                                                                  onSend: function() {
                                                                      conf.view.clear();
                                                                      LoadingDialog.open(localization.payments.savingsubscription);
                                                                  }
                                                                 })
                                          });
                                          return false;
                                      }
                                     });
            return button.input();
        }
    });

    var PaymentsDashboardRecurlyView = Backbone.View.extend({
        className: 'payments subscribed',
        initialize: function(args) {
            var view = this;
            view.model = args.model;
            _.bindAll(this);
            view.model.bind('fetch', this.render);
            view.paymentsTable = new InvoicePaymentsView({model:view.model});
            view.changeBillingForm = new ChangeBillingFormView({model:view.model});
        },
        render: function() {
            var view = this;
            var model = view.model;
            var $el = $(view.el);

            $el.addClass("subscribed").removeClass("subscribing");

            var col1 = $('<div class="col1" />')
                .append(view.paymentsTable.el);
            var col2 = $('<div class="col2" />')
                .append(view.changeBillingForm.el);

            var c = $().add(col1).add(col2);
            $el.html(c);
        }
    });

    var chooseView = function(model) {
        if(model.type()      === 'nouser')
            return new PricePageView({model:model});
        else if(model.type() === 'user')
            return new PricePageView({model:model,
                                      hideContacts: true});
        else if(model.type() === 'plannone')
            return new PaymentsDashboardNoneView({model:model});
        else if(model.type() === 'planrecurly')
            return new PaymentsDashboardRecurlyView({model:model});
    };
    window.PaymentsDashboard = function(opts) {
        $("head").append('<link rel="stylesheet" href="/libs/recurly/recurly.css"></link>');
        var model = new PricePageModel(opts);
        var el = $("<div class='tab-container'/>");
        var view = null;
        var subel = $("<div class='tab-content account js-paymentsdashboard payments-dashboard price-plan'/>");
        el.append(subel);
        var sel = null;

        model.fetch({success: function() {
            view = chooseView(model);
            subel.append(view.el);
            if(model.type() == 'planrecurly')
                    subel.removeClass('price-plan');
            model.reset();
            model.trigger('fetch');
        }});


        return {
          refresh : function() { },
          el : function() {return el;}
          
        };
                if(model.type() == 'planrecurly')
                    sel.removeClass('price-plan');

    };

    window.PricePage = function(opts) { 
        var model = new PricePageModel(opts);
        var view  = new PricePageView($.extend({model:model}, opts));

        model.fetch({success: function() {
            model.trigger('fetch');
        }});

        return { show : function(selector) {
            $("head").append('<link rel="stylesheet" href="/libs/recurly/recurly.css"></link>');
            $(selector).html(view.el);
        }};
    };

}(window));
