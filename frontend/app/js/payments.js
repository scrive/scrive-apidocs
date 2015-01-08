/*

  Payments

  Author and Maintainer: Eric Normand

*/
define(['Backbone', 'moment', 'legacy_code'], function(Backbone, moment) {

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
            yearlyprices: true,
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
        setYearlyPrices: function(bool) {
          this.set({yearlyprices: bool});
          return this;
        },
        yearlyprices: function() {
          return this.get('yearlyprices');
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
        setDone: function() {
            this.set({done:true});
        },
        done: function() {
            this.get('done');
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
        isAdmin : function() {
            return this.get('is_admin');
        },
        canPurchase : function() {
            return this.isAdmin();
        },
        url: function() {
            return "/payments/pricepageinfo";
        },
        createaccount: function(email, firstname, lastname, callback) {
            var model = this;
            $.ajax('/api/frontend/signup',
                   {type: 'POST',
                    data: {email: email,
                           firstName: firstname,
                           lastName: lastname,
                           lang : Language.current()
                    },
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
                    cache: false,
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

    var TopTabsView = Backbone.View.extend({
        className: "top-tabs",
        initialize: function(args) {
            _.bindAll(this);
            args.model.bind('change:yearlyprices', this.render);
            var view = this;
            this.render();
        },
        render: function() {
            var view = this;
            var model = view.model;

            var div = this.$el;

            var monthly = $("<div class='monthly'>MONTHLY</div>");
            var yearly = $("<div class='yearly'>YEARLY</div>");

            if (model.yearlyprices()) {
              yearly.css('font-size', '22px');
            } else {
              monthly.css('font-size', '22px');
            }

            monthly.click(function() {
              model.setYearlyPrices(false);
            });

            yearly.click(function() {
              model.setYearlyPrices(true);
            });

            div.empty().append(monthly).append(yearly);
        }
    });

    var FeaturesView = Backbone.View.extend({
      renderFeatures: function() {
        var view = this;
        var model = view.model;

        var features = $('<div class="features" />');

        var titleheader = $('<h4 />').text(localization.payments.plans[view.plan].name);
        if (model.headercolour()) {
          titleheader.css('color', model.headercolour());
        }
        var title = $('<div class="title" />').append(titleheader);
        features.append(title);

        var plandescription = $('<h4 class="description" />').text(localization.payments.plans[view.plan].description);
        var fineprint = $('<h4 class="fineprint" />').text(localization.payments.plans[view.plan].fineprint);
        if (model.headercolour()) {
          plandescription.css('color', model.headercolour());
          fineprint.css('color', model.headercolour());
        }
        var explanation = $('<div class="explanation" />').append(plandescription).append(fineprint);
        features.append(explanation);

        var price = $('<span class="price" />').html(localization.payments.plans[view.plan].price);
        var priceUnit = $('<span class="unit" />').html(localization.payments.priceUnit);
        if (model.pricecolour()) {
          price.css('color', model.pricecolour());
          priceUnit.css('color', model.pricecolour());
        }

        var cost = $('<div class="cost" />')
            .append(price).append(priceUnit);

        features.append(cost);

        return features;
      }
    });


    var ContactBoxView = FeaturesView.extend({
        className: "plan-container",
        initialize: function(args) {
            _.bindAll(this);
            args.model.bind('change:currentPlan', this.render);
            args.model.bind('fetch', this.render);
            this.plan = args.plan;
            this.onClick = args.onClick;
            var view = this;
            this.$el.addClass(this.plan);
        },
        render: function() {
            var view = this;
            var model = view.model;

            var div = view.$el;
            var div2 = $('<div class="plan" />');

            var features = this.renderFeatures();

            var button = $('<a class="button action action-sign-up" />')
                .append($('<span class="blue" />')
                        .text(localization.payments.contact))
                .append($('<span class="gray" />')
                        .text(localization.cancel));

            var action = $('<div class="action" />').append(button);

            features.toggle(function() {
                $('.plan-container').removeClass('on-top');
                button.addClass('button-gray');
                div2.addClass("active");
                div.addClass('on-top');
                div2.find('input.initial-focus').focus();
            }, function() {
                button.removeClass('button-gray');
                div2.removeClass("active");
                div2.find('input.initial-focus').blur();
            });

            features.append(action);
            // end features construction

            div2.append(features);

            var form = $('<form method="POST" action="/payments/contact" />').addClass(view.plan);
            var ul = $('<ul class="fields" />');

            var fn = $('<li class="field" />');
            fn.append($('<label for="input-first-name" />').text(localization.fstname));
            fn.append($('<div class="input" />')
                      .append($('<input type="text" id="input-first-name" name="firstname" />')
                              .attr('placeholder', localization.fstname)
                              .val(model.firstName() || '')));
            ul.append(fn);

            var ln = $('<li class="field" />');
            ln.append($('<label for="input-last-name" />').text(localization.sndname));
            ln.append($('<div class="input" />')
                      .append($('<input type="text" id="input-last-name" name="lastname" />')
                              .attr('placeholder', localization.sndname)
                              .val(model.lastName() || '')));
            ul.append(ln);

            var email = $('<li class="field" />');
            email.append($('<label for="input-email" />').text(localization.email));
            email.append($('<div class="input" />')
                         .append($('<input type="text" id="input-email" name="email" />')
                                 .attr('placeholder', localization.email)
                                 .val(model.email() || '')));
            ul.append(email);

            var message = $('<li class="field" />');
            message.append($('<label for="input-message" />').text(localization.email));
            message.append($('<div class="input" />')
                           .append($('<textarea type="text" id="input-message" name="message" />')
                                   .attr('placeholder', localization.payments.placeholder)));
            ul.append(message);

            var submit = $('<li class="field submit" />');
            submit.append($('<div class="input" />')
                          .append($('<input type="submit" id="input-submit" class="button action" />')
                                  .val(localization.payments.sendmsg)));
            ul.append(submit);

            form.append(ul);
            form.append($('<input type="hidden" name="plan" />').val(view.plan));
            div2.append(form);

            div.append(div2);
        }
    });

    var FreeBoxView = FeaturesView.extend({
        className: "plan-container",
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
            this.recurly = new RecurlyView(args);
        },
        render: function() {
            var view = this;
            var model = view.model;

            var div = view.$el;
            var div2 = $('<div class="plan" />');

            var features = this.renderFeatures();

            var button = $('<a class="button button-green action-sign-up" />')
                .append($('<span class="blue" />')
                        .text(localization.payments.purchase))
                .append($('<span class="gray" />')
                        .text(localization.cancel));

            var action = $('<div class="action" />')
                .append(button);

            features.append(action);

            div2.append(features);

            div.append(div2);

        }
    });

    var TeamBoxView = FeaturesView.extend({
        className: "plan-container",
        initialize: function(args) {
            _.bindAll(this);
            args.model.bind('change:currentPlan', this.render);
            args.model.bind('fetch', this.render);
            this.plan = args.plan;
            this.onClick = args.onClick;
            var view = this;
            this.$el.click(function() {
                mixpanel.track('Team Box Purchase click');
                view.onClick();
                return false;
            });
            this.$el.addClass(this.plan);
            this.recurly = new RecurlyView(args);
        },
        render: function() {
            var view = this;
            var model = view.model;

            var div = view.$el;
            var div2 = $('<div class="plan" />');

            var features = this.renderFeatures();

            var button = $('<a class="button action action-sign-up" />')
                .append($('<span class="blue" />')
                        .text(localization.payments.purchase))
                .append($('<span class="gray" />')
                        .text(localization.cancel));

            var action = $('<div class="action" />')
                .append(button);

            features.append(action);
            features.toggle(function() {
                $('.plan-container').removeClass('on-top');
                button.addClass('button-gray');
                div.addClass('on-top');
                div2.addClass("active");
                div2.find('input.initial-focus').focus();
            }, function() {
                button.removeClass('button-gray');
                div2.removeClass("active");
                div2.find('input.initial-focus').blur();
            });
            // end features construction

            div2.append(features);
            div2.append(view.recurly.el);

            div.append(div2);

        }
    });

    var RecurlyView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this);
            this.element = $('<div />');
            this.$el.append(this.element);
            args.model.bind('change:currentPlan', this.render);
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
            }
            quantbox.change();
            quantbox.hide();

            var permonth = $('<span />');
            permonth.text(localization.payments.table.totalpermonth);

            form.find('.due_now .title').html(permonth);

            // move errors above contact_info

            form.find('div.contact_info').before(form.find('div.server_errors'));


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
                                                if(data.sent)
                                                    form.submit();
                                                // what else?
                                            });
                        work = true;
                    }
                } else if(data.user_exists && !data.has_plan && !data.is_admin) {
                    loadingicon.hide();
                    var popup = new Confirmation({
                        title: localization.payments.mustBeAdmin,
                        content: $('<p />').text(localization.payments.contactAdmin),
                        onAccept: function() {
                            popup.clear();
                        }
                    });
                } else if(data.user_exists && !data.has_plan &&  data.is_admin) {
                    if(work) {
                        work = false;
                        form.submit();
                        work = true;
                    }
                } else {
                    // show message (they should log in)
                    loadingicon.hide();
                    var text = localization.payments.outside.sorryExistingUser;
                    var header = localization.payments.outside.sorryExistingUserHeader;
                    var popup = new Confirmation({
                        title: header,
                        content: $('<p />').text(text),
                        onAccept: function(){
                            popup.clear();
                        }
                    });
                }
            };
            var loadingicon = $('<img src="/libs/recurly/images/submitting.gif" class="loading-icon" style="display:none" />');
            // replace button with our own
            var button = new Button({ type : 'action',
                                      cssClass:'s-subscribe',
                                      text:localization.payments.subscribe,
                                      icon: loadingicon,
                                      onClick: function() {
                                          form.find('.coupon .check').click();
                                          view.validator.validate(function success() {
                                              loadingicon.css({display:'inline'});
                                              if(model.type() === 'user') {
                                                  handlechargeaccount({
                                                      'user_exists' : true,
                                                      'has_plan' : false,
                                                      'is_admin' : model.isAdmin()
                                                  });
                                              } else if(model.type() === 'plan' || model.type() == 'plannone') {
                                                  handlechargeaccount({
                                                      'user_exists' : true,
                                                      'has_plan' : true,
                                                      'is_admin' : model.isAdmin()
                                                  });
                                              } else { // not logged in, so we have to check
                                                  model.checkuserexists(model.email(),
                                                                        handlechargeaccount);
                                              }
                                          }, function failure() {
                                              loadingicon.hide();
                                          });
                                      }});
            div.find('button').replaceWith(button.el());

            var expires = form.find('.field.expires');
            form.find('.field.cvv').insertAfter(expires);
            expires.find('.title').text(localization.payments.expires);
            form.find('.card_number .placeholder').text(localization.payments.creditcardnumber);
            form.find('.field .month').after($('<span />').text(" / "));

            // we can store the field values so they won't have to type them again
            var cc = form.find('.card_number input');
            cc.val(model.creditCard());
            cc.attr('placeholder', cc.parent().find('.placeholder').text());
            cc.change();
            cc.change(function(e) {
                model.setCreditCard($(e.target).val());
            });
            var cvv = form.find('.cvv input');
            cvv.val(model.cvv());
            cvv.attr('placeholder', cvv.parent().find('.placeholder').text());
            cvv.change();
            cvv.change(function(e) {
                model.setCvv($(e.target).val());
            });
            var fn = form.find('.first_name input');
            fn.val(model.firstName());
            fn.attr('placeholder', fn.parent().find('.placeholder').text());
            fn.change();
            fn.change(function(e) {
                model.setFirstName($(e.target).val());
            });
            var ln = form.find('.last_name input');
            ln.val(model.lastName());
            ln.attr('placeholder', ln.parent().find('.placeholder').text());
            ln.change();
            ln.change(function(e) {
                model.setLastName($(e.target).val());
            });

            var en = form.find('.email input');
            en.val(model.email());
            en.attr('placeholder', en.parent().find('.placeholder').text());
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
                    form.addClass('nocontact');
                    form.find('.contact_info').hide();
                }
            }
            form.find('.vat .title').text(localization.payments.vat25);
            form.find('.placeholder').remove();
            var params = parseQueryString();
            if(params.coupon) {
                form.find('.coupon input').val(params.coupon);
                form.find('.coupon .check').click();
            }

        },
        render: function() {
            var view = this;
            var model = view.model;

            if(model.type() === 'plannone' || model.type() === 'planrecurly') {
                var f = $('<form />');

                f.append($('<h3 />')
                         .html(localization.payments.already));

                var goSpan = $('<span />').html(localization.payments.alreadyGo);
                goSpan.find('.put-link-to-payments-here').attr('href', '/account#subscription');

                f.append($('<h4 />')
                         .html(goSpan)
                         .click(function() {
                             location = '/account#subscription';
                         }));

                view.element.append(f);
                return false;
            }

            if(model.done() && model.accountCreated()) {
                view.$el.children().detach();
                return false;
            }

            if(model.type() === 'user' && !model.canPurchase()) {
                view.element.text(localization.payments.contactAdmin);
                view.element.addClass('contact-admin');
                return false;
            }

            var loadingicon;

            Recurly.config({
                subdomain: model.subdomain()
                , currency: 'SEK'
                , country: 'SE'
            });

            view.validator = Recurly.buildSubscriptionForm({
                target: view.element
                , enableAddons: false
                , enableCoupons: true
                , planCode: model.currentPlan() || 'team'
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
                    , address1: ' '
                    , city: ' '
                    , state: ' '
                    , zip: ' '
                }
                , signature: model.plans()[model.currentPlan() || 'team'].signature
                , beforeInject: function(form) {
                    view.scrambleForm(form);
                    loadingicon = $(form).find('img.loading-icon');
                }
                , onError: function() {
                    console.log("Recurly error: Something with Recurly setup went wrong. Correct Recurly config set up?");
                    console.log(loadingicon);
                    loadingicon.hide();
                }
                , successHandler: function(stuff) {

                    LoadingDialog.open({header: localization.payments.savingsubscription});
                    model.submitSubscription(function() {
                        var text;
                        var header;
                        if(model.type() === 'user') {
                            window.location.reload();
                            return true;
                        }

                        model.setDone();

                        loadingicon.hide();
                        LoadingDialog.close();
                        if(model.createdUser()) {
                            text = localization.payments.outside.confirmAccountCreatedUser;
                            header = localization.payments.outside.confirmAccountCreatedUserHeader;
                        } else {
                            text = localization.payments.outside.confirmAccountExistingUser;
                            header = localization.payments.outside.confirmAccountExistingUserHeader;
                        }
                        new Confirmation({
                            title: header,
                            content: $('<p />').text(text),
                            onAccept: function() {
                                window.location = '/login';
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
            this.freeBox = new FreeBoxView({model: args.model,
                                            plan:'free',
                                            onClick: function() {
                                              window.location.href = '/signup';
                                            }});
            this.teamBox = new TeamBoxView({model: args.model,
                                            hideContacts: args.hideContacts,
                                            plan:'team',
                                            onClick: function() {
                                                mixpanel.track('Click team plan');
                                                if(view.model.currentPlan() !== 'team')
                                                    view.model.setCurrentPlan('team');
                                            }});
            this.storeBox = new ContactBoxView({model: args.model,
                                               plan:'store',
                                               onClick: function() {
                                                   mixpanel.track('Click online form plan');
                                               }});
            this.formBox = new ContactBoxView({model: args.model,
                                               plan:'form',
                                               onClick: function() {
                                                   mixpanel.track('Click online form plan');
                                               }});

            this.topTabs = new TopTabsView({model: args.model});

            this.noheaders = args.noheaders;
            //this.recurlyForm = new RecurlyView({model: args.model, hideContacts: args.hideContacts});
            view.model.bind('fetch', this.render);
        },
        render: function() {
            var view = this;
            var model = view.model;
            var div = $('<div />'); // container div


            var header = $('<header />')
                .append($('<h1 />').text(model.header()))
                .append($('<h4 />').text(''));


            div.append(header);

            div.append(view.topTabs.el);

            div.append(view.freeBox.el)
                .append(view.teamBox.el)
                .append(view.storeBox.el)
                .append(view.formBox.el);

            div.append($('<div class="clearfix" />'));
            div.append($('<div class="vat-box" />').text(localization.payments.vat));

            view.$el.html(div.contents());
        }
    });

    var PaymentsCurrentSubscriptionView = Backbone.View.extend({
        /* This view is used both indepenendently and by PaymentsDashboardRecurlyView */
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

          var div = $('<div class="col subscription-plan" />');
            var header = $('<div class="account-header" />')
                .append($('<h4></h4>').text(localization.payments.table.currentplan));
          var table = $('<div class="account-body standard-input-table" />');

	  var planInformation = $('<div class="plan-information" />');
          var planName = $('<p class="plan-name"></p>').text(localization.payments.plans[model.paidPlan()].name);
          var numberOfUsers = $('<p></p>').text(localization.payments.numberOfUsers + " : " + model.quantity());
	  planInformation.append(planName);
	  planInformation.append(numberOfUsers);
	  table.append(planInformation);

	  var askviktor = $('<p class="askviktor"></p>').html(localization.payments.askviktor);
          askviktor.find('.put-link-to-mail-sales-here').attr('href', 'mailto:viktor@scrive.com').attr('target', '_blank');
          table.append(askviktor);

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
            var paymentsheader = $('<div class="account-header" />').append($('<h4 />')
									    .text(localization.payments.table.payments));

            var paymentstable = $('<div class="account-body standard-input-table" />')
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
            var total        = price * quantity * 1.25;
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
                    .append($('<span class="total" />').text(totalf))
                    .append($('<span class="invoice-status" />').text(status));
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
        initialize: function(args) {
            var view = this;
            view.model = args.model;
            _.bindAll(this);
            view.model.bind('fetch', this.render);
	    view.model.bind('subscriptionCancelled', this.subscriptionCancelled);
        },
	subscriptionCancelled: function() {
	    this.remove();
	    this.unbind();
	},
        render: function() {
            var view = this;
            var model = view.model;
            var $el = $(view.el);

            var billing = $('<div class="col" />');
            var billingheader = $('<div class="account-header" />')
                .append($('<h4 />')
                        .text(localization.payments.table.changebilling));
            var billingform = $('<div class="account-body standard-input-table changebilling-form" />');
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
                     var button = new Button({ type : 'action',
                                               size:'small',
                                               text:localization.payments.savechanges,
                                               onClick: function() {
                                                   if(work) {
                                                       LoadingDialog.open({header: localization.payments.savingsubscription});
                                                       work = false;
                                                       $('.changebilling-form form').submit();
                                                       work = true;
                                                       return false;
                                                   }
                                               }
                                              });
                     el.find('button').replaceWith(button.el());
                     el.find('div.field').each(function(i, f) {
                         f = $(f);
                         var p = f.find('.placeholder').text();
                         if(p)
                             f.find('input').attr('placeholder', p);
                     });
                 },
                 addressRequirement: 'none',
                 successHandler: function(stuff) {
                     mixpanel.track('Change billing information');
                     model.fetch({success: function() {
                         LoadingDialog.close();
                         model.trigger('fetch');
                         new FlashMessage({ content: localization.payments.billingInfoSaved, type: 'success'});
                     }});
                 },
                 onError: function() {
                     LoadingDialog.close();
                 },
                 onValidationError: function() {
                     LoadingDialog.close();
                 }
                }
            );


	    var cancel = $('<div class="col cancel-subscription" />');
            var cancelheader = $('<div class="account-header" />')
                .append($('<h4 />')
                        .text(localization.payments.cancelsubscription));
            var cancelbody = $('<div class="account-body standard-input-table" />');
	    cancelbody.append(view.cancelButton());
	    cancel.append(cancelheader);
	    cancel.append(cancelbody);

	    var viewContainer = $('<div class="view-container" />');
	    viewContainer.append($('<div class="col" />').append(billingheader).append(billingform));
	    viewContainer.append(cancel);
            $el.html(viewContainer);
        },
        cancelButton: function() {
            var view = this;
            var model = view.model;

	    var cancelSubscriptionButton = $('<a href="">' + localization.payments.cancelsubscription + '</a>');
	    cancelSubscriptionButton.click(function() {
                mixpanel.track('Click cancel subscription button');
                var message = localization.payments.cancelDialog;

                var popup = new Confirmation({
                    title: localization.payments.cancelsubscription,
                    acceptText: localization.payments.cancelsubscription,
                    content: $('<p />').text(message),
                    submit: new Submit({url: "/payments/changeplan",
                                        method: "POST",
                                        plan: "free",
                                        ajax: true,
                                        ajaxsuccess: function() {
                                            model.fetch({success:function() {
                                                LoadingDialog.close();
                                                model.trigger('fetch');
                                                var message_content = $('<span>' + localization.blocking.willcancel.headline + '</span>');
                                                $('.put-days-left-here', message_content).text(Math.ceil(moment.duration(model.subscription().billingEnds() - moment()).asDays()));
                                                new FlashMessage({type: 'success', content: message_content});
						model.trigger('subscriptionCancelled');
                                            }});
                                        },
                                        onSend: function() {
                                            mixpanel.track('Accept',
                                                           {'Accept' : 'Cancel subscription'});
                                            popup.clear();
                                            LoadingDialog.open({header: localization.payments.cancelingSubscription});
                                        }
                                       })
                });
                return false;
            });

            return cancelSubscriptionButton;
        }
    });

    var RewnewSubscriptionView = Backbone.View.extend({
        className: 'col renew-subscription',
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
                .append($('<h4 />')
                        .text(localization.payments.renewSubscription));
            var billingform = $('<div class="account-body standard-input-table renew-form" />');

            var canceledDate = model.subscription() && model.subscription().cancelled() && new Date(model.subscription().cancelled());

            var message;
            if(canceledDate) {
                message = $("<span>" + localization.payments.canceledDate + "</span>");
                $(".put-date-here",message).text(moment(canceledDate).format("YYYY-MM-DD"));
            }
            else
                message = $("<span>" + localization.payments.wasCanceled + "</span>");
            billingform.append(message);
            //billingform.append(view.renewButton());
            $el.html($().add(billingheader).add(billingform).add(view.renewButton()));
        },
        renewButton: function() {
            var view = this;
            var model = view.model;

            var price = model.quantity() * 299 + "kr";

            var button = new Button({ type : 'action',
                                      size: 'small',
                                      cssClass: 'renew-button',
                                      text: localization.payments.renewSubscription,
                                      onClick: function() {
                                          mixpanel.track('Click renew subscription');
                                          var message = $("<p>" + localization.payments.renewDialog+"</p>");
                                          $(".put-price-here",message).text(price);
                                          var popup = new Confirmation({
                                              title: localization.payments.renewSubscription,
                                              acceptText: localization.payments.renewSubscription,
                                              content: message,
                                              submit: new Submit({url: "/payments/changeplan",
                                                                  method: "POST",
                                                                  plan: "team",
                                                                  ajax: true,
                                                                  ajaxsuccess: function() {
                                                                      model.fetch({success:function() {
                                                                          LoadingDialog.close();
                                                                          model.trigger('fetch');
                                                                          new FlashMessage({type: 'success', content: localization.payments.subscriptionRenewed });
                                                                          view.unbind();
                                                                          view.remove();
                                                                      }});
                                                                  },
                                                                  onSend: function() {
                                                                      mixpanel.track('Accept',
                                                                                     {'Accept' : 'Renew subscription'});
                                                                      popup.clear();
                                                                      LoadingDialog.open({header: localization.payments.renewingSubscription});
                                                                  }
                                                                 })
                                          });
                                          return false;
                                      }
                                     });
            return button.el();
        }
    });


    var PaymentsDashboardRecurlyView = Backbone.View.extend({
        className: 'payments subscribed',
        initialize: function(args) {
            var view = this;
            view.model = args.model;
            _.bindAll(this);
            view.model.bind('fetch', this.render);
	    view.paymentsSubscription = new PaymentsCurrentSubscriptionView({model:view.model});
            view.paymentsTable = new InvoicePaymentsView({model:view.model});
            view.changeBillingForm = new ChangeBillingFormView({model:view.model});
            view.renewSubscription = new RewnewSubscriptionView({model:view.model});
        },
        render: function() {
            var view = this;
            var model = view.model;
            var $el = $(view.el);

            $el.addClass("subscribed").removeClass("subscribing");
	    $el.append(view.paymentsSubscription.el);
            $el.append(view.paymentsTable.el);

            if(model.subscription().code() === 'free' || (model.subscription().pending() && model.subscription().pending().code() === 'free'))
                $el.append(view.renewSubscription.el);
            else
                $el.append(view.changeBillingForm.el);

        }
    });

    var chooseView = function(model) {
        /* Choose main view to display on the subscribe page */
        if(model.type()      === 'nouser')
            return new PricePageView({model:model});
        else if(model.type() === 'user')
            return new PricePageView({model:model,
                                      hideContacts: true});
        else if(model.type() === 'plannone')
            return new PaymentsCurrentSubscriptionView({model:model});
        else if(model.type() === 'planrecurly')
            return new PaymentsDashboardRecurlyView({model:model});
    };
    window.PaymentsDashboard = function(opts) {
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
    };

    window.PricePage = function(opts) {
        var model = new PricePageModel(opts);
        var view  = new PricePageView($.extend({model:model}, opts));

        model.fetch({success: function() {
            model.trigger('fetch');
        }});

        return { show : function(selector) {
            $(selector).html(view.el);
        }};
    };

});
