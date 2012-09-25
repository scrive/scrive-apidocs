/*

  Payments 



*/

(function(window) {

    var planPrices = {
        free         :     0,
        pay          : 29900,
        team         : 29900,
        form         :100000
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
            plan_code: 'pay'
        },
        code: function(p) {
            if(p) {
                this.set({plan_code: p});
                return this;
            }
            return this.get("plan_code");
        },
        // code for account stored on both Scrive db and Recurly
        accountCode: function(p) {
            return this.get('code');
        },
        signatures: function() {
            return this.get("signatures");
        },
        currency: function() {
            return this.get('currency');
        },
        // the number of users in the company
        quantity: function(q) {
            if(q) {
                this.set({quantity:q});
                return this;
            }
            return this.get("quantity");
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
        },
        provider: function() {
            return this.get("provider");
        },
        quantity: function() {
            return this.get("quantity");
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
        dateString: function(d) {
            return moment(d).format("YYYY-MM-DD");
        },
        teamBox: function() {
            var view = this;
            var model = view.model;

            var div = $('<div class="team planbox" />');

            var header = $('<div class="header" />');
            var header1 = $('<div class="header1" />');
            header1.text(localization.payments.plans.team.name);
            var header2 = $('<div class="header2" />');
            header2.text(localization.payments.plans.team.tag);
            header.append(header1).append(header2);
            div.append(header);

            var features = $('<div class="features" />');
            _.each(localization.payments.plans.team.features, function(t) {
                features.append($('<div class="feature" />').text(t));
            });

            div.append(features);

            var price = $('<div class="price" />');
            var price1 = $('<div class="price1" />');
            price1.text();
            var price2 = $('<div class="price2" />');
            price2.text(localization.payments.plans.team.price);
            var price3 = $('<div class="price3" />');
            price3.text(localization.payments.plans.team.price3);
            price.append(price1).append(price2).append(price3);
            div.append(price);

            var buttonbox = $('<div class="buttonbox" />');
            var button = Button.init({color: 'black',
                                      text: localization.payments.select,
                                      size: 'small',
                                      width: 150,
                                      onClick: function() {
                                          model.signup().code('team');
                                          view.showRecurlySubscriptionForm();
                                          return false;
                                      }});
            
            buttonbox.append(button.input());
            div.append(buttonbox);
            div.click(function() {
                model.signup().code('team');
                view.showRecurlySubscriptionForm();
                return false;
            });
            return div;
        },
        formBox: function() {
            var view = this;
            var model = view.model;

            var div = $('<div class="form planbox" />');

            var header = $('<div class="header" />');
            var header1 = $('<div class="header1" />');
            header1.text(localization.payments.plans.form.name);
            var header2 = $('<div class="header2" />');
            header2.text(localization.payments.plans.form.tag);
            //var img = $('<img src="/img/bestvalue.png" alt="Best Value" class="bestvalue" />');
            header.append(header1).append(header2); //.append(img);
            div.append(header);

            var features = $('<div class="features" />');
            _.each(localization.payments.plans.form.features, function(t) {
                features.append($('<div class="feature" />').text(t));
            });

            div.append(features);

            var price = $('<div class="price" />');
            var price1 = $('<div class="price1" />');
            price1.text("");
            var price2 = $('<div class="price2" />');
            price2.text(localization.payments.plans.form.price);
            var price3 = $('<div class="price3" />');
            price3.text(localization.payments.plans.form.price3);
            price.append(price1).append(price2).append(price3);

            var price21 = $('<div class="price2 s" />');
            price21.text(localization.payments.plans.form.price21);
            var price31 = $('<div class="price3 s" />');
            price31.text(localization.payments.plans.form.price31);
            price.append(price21).append(price31);

            div.append(price);

            var buttonbox = $('<div class="buttonbox" />');
            var button = Button.init({color: 'black',
                                      text: localization.payments.select,
                                     size: 'small',
                                     width: 150,
                                     onClick: function() {
                                         model.signup().code('form');
                                         view.showRecurlySubscriptionForm();
                                         return false;
                                     }});
            
            buttonbox.append(button.input());
            div.append(buttonbox);
            div.click(function() {
                model.signup().code('form');
                view.showRecurlySubscriptionForm();
                return false;
            });
            return div;
        },
        enterpriseBox: function() {
            var view = this;
            var model = view.model;

            var div = $('<div class="enterprise planbox" />');
            var header = $('<div class="header" />');
            var header1 = $('<div class="header1" />');
            header1.text(localization.payments.plans.enterprise.name);
            var header2 = $('<div class="header2" />');
            header2.html(localization.payments.plans.enterprise.tag);
            header.append(header1).append(header2);
            div.append(header);

            var features = $('<div class="features" />');
            _.each(localization.payments.plans.enterprise.features, function(t) {
                features.append($('<div class="feature" />').html(t));
            });
            div.append(features);

            var price = $('<div class="price" />');
            var price1 = $('<div class="price1" />');
            price1.text(localization.payments.plans.enterprise.price1);
            var price2 = $('<div class="price2" />');
            price2.text(localization.payments.plans.enterprise.price);
            var price3 = $('<div class="price3" />');

            price.append(price1).append(price2).append(price3);
            div.append(price);

            var buttonbox = $('<div class="buttonbox" />');
            var button = Button.init({color: 'black',
                                      text: localization.payments.getintouch,
                                      size: 'small',
                                      width: 150,
                                      onClick: function() {
                                          view.getInTouch();
                                          return false;
                                      }});
            buttonbox.append(button.input());
            div.append(buttonbox);
            div.click(function() {
                view.getInTouch();
                return false;
            });
            return div;
        },
        showSubscribeForm: function() {
            var view = this;
            var model = view.model;
            var $el = $(view.el);
            $el.empty();
            $el.removeClass("subscribed").addClass("subscribing");
            
            var header = $('<div class="header" />')
                .append($('<h2 />').text(localization.payments.subscribeheader));

            $el.append(header);
            $el.append($('<h3 />').text(localization.payments.chooseplan));

            var subs = $('<div class="subscription-chooser" />');

            var team = this.teamBox();

            //team.addClass('selected');

            subs.append(team);
            subs.append(this.formBox());
            subs.append(this.enterpriseBox());

            model.signup().code('team');

            $el.append(subs);
            $el.append($('<div class="clearfix" />'));
            $el.append($('<h3 />').text(localization.payments.paywithcard));

            var recurlyform = $('<div id="js-recurly-form"></div>');

            $el.append(recurlyform);

            view.showRecurlySubscriptionForm();

        },
        showRecurlySubscriptionForm: function() {
            var view = this;
            var model = view.model;
            var $el = $(view.el);

            var form = $('.planbox.form');
            var team = $('.planbox.team');

            if(model.signup().code() === 'team') {
                team.addClass('selected');
                team.find('.btn-small').removeClass('black').addClass('green')
                    .find('.label').text(localization.payments.selected);
                form.removeClass('selected');
                form.find('.btn-small').removeClass('green').addClass('black')
                    .find('.label').text(localization.payments.select);
            } else if(model.signup().code() === 'form') {
                form.addClass('selected');
                form.find('.btn-small').removeClass('black').addClass('green')
                    .find('.label').text(localization.payments.selected);
                team.removeClass('selected');
                team.find('.btn-small').removeClass('green').addClass('black')
                    .find('.label').text(localization.payments.select);
            } else {
                form.removeClass('selected');
                form.find('.btn-small').addClass('black').removeClass('green')
                    .find('.label').text(localization.payments.select);
                team.removeClass('selected');
                team.find('.btn-small').removeClass('green').addClass('black')
                    .find('.label').text(localization.payments.select);
            }

            Recurly.config({
                subdomain: model.server().subdomain()
                , currency: 'SEK'
                , country: model.contact().country()
            });
            Recurly.buildSubscriptionForm({
                target: '#js-recurly-form'
                , enableAddons: false
                , enableCoupons: false
                , planCode: model.signup().code()
                , addressRequirement: 'none'
                , distinguishContactFromBillingInfo: false
                , collectCompany: true
                , acceptedCards : ['mastercard', 'visa']
                , accountCode: model.signup().accountCode()
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
                , signature: model.signup().signatures()[model.signup().code()]
                , beforeInject: function(form) {
                    form = $(form);
                    /*
                    form.find('.field').prepend(function(){
                        return $('<div class="label" />').text($(this).find('.placeholder').text().trim() + ": ");
                    });
                    */
                    var div = $('<div class="borderbox" />');
                    div.append(form.contents());
                    form.append(div);

                    form.find('.server_errors').prependTo(form);

                    // set the quantity that we found from the db
                    var quantbox = form.find('.quantity input');

                    var guarantee = $('<div class="guarantee" />');
                    guarantee.text(localization.payments.guarantee);

                    if(model.signup().code() == 'team') {
                        quantbox.val(model.signup().quantity());
                        quantbox.after($('<span class="quantity" />').text(model.signup().quantity() + " " + localization.payments.users));
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
                    if(model.signup().code() == 'form') {
                        //permonth.text(" " + localization.payments.permonth + " " + localization.and + " " + localization.payments.plans.docprice + " " + localization.payments.perdocument);
                    }
                    form.find('.due_now').append(permonth).append(guarantee);
                    form.find('.due_now .title').hide();

                    //quantbox.attr('type', 'number').attr('min', 3);
                    // replace button with our own
                    var work = true;
                    var button = Button.init({color:'green',
                                              size:'big',
                                              text:localization.payments.subscribe,
                                              onClick: function() {
                                                  if(work) {
                                                      work = false;
                                                      form.submit();
                                                      work = true;
                                                  }
                                              }});
                    div.find('button').replaceWith(button.input());
                    
                    //form.append(button.input());
                    var expires = form.find('.field.expires');
                    form.find('.field.cvv').insertAfter(expires);
                    expires.find('.title').text(localization.payments.expires);
                    form.find('.card_number .placeholder').text(localization.payments.creditcardnumber);
                    form.find('.field .month').after($('<span />').text(" / "));

                    // we can store the field values so they won't have to type them again
                    var c = model.creditcard();
                    var cc = form.find('.card_number input');
                    cc.val(model.creditcard());
                    cc.change();
                    cc.change(function(e) { 
                        model.creditcard($(e.target).val()); 
                    });
                    var cvv = form.find('.cvv input');
                    cvv.val(model.cvv());
                    cvv.change();
                    cvv.change(function(e) {
                        model.cvv($(e.target).val());
                    });
                    var fn = form.find('.first_name input');
                    fn.val(model.contact().firstName());
                    fn.change();
                    fn.change(function(e) {
                        model.contact().firstName($(e.target).val());
                    });
                    var ln = form.find('.last_name input');
                    ln.val(model.contact().lastName());
                    ln.change();
                    ln.change(function(e) {
                        model.contact().lastName($(e.target).val());
                    });

                    if(model.contact().firstName() === '' || model.contact().lastName() === '') {
                        form.find('.field.first_name, .field.last_name').show();
                    }

                    var en = form.find('.email input');
                    en.val(model.contact().email());
                    en.change();
                    en.change(function(e) {
                        model.contact().email($(e.target).val());
                    });
                    var m = form.find('.month select');
                    m.val(model.month());
                    m.change();
                    m.change(function(e) {
                        model.month($(e.target).val());
                    });
                    var y = form.find('.year select');
                    y.val(model.year());
                    y.change();
                    y.change(function(e) {
                        model.year($(e.target).val());
                    });
                }
                , successHandler: function(stuff) {
                    LoadingDialog.open(localization.payments.savingsubscription);
                    $.ajax("/payments/newsubscription",
                           { type: "POST"
                             ,data: {account_code: model.signup().accountCode(), 
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
        getInTouch: function() {
            var view = this;
            var model = view.model;
            var form = $("<div />");
            form.append($("<span />").text(localization.docsignview.phoneFormDescription + " "));
            var numberinput = $("<input type='text' />");
            form.append(numberinput);

            var content = $('<div />');
            content.append($('<p />').text(localization.payments.pleaseleavenumber));
            content.append(form);
            var done = false;
            var popup = Confirmation.popup({
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
                        email: model.contact().email(),
                        phone: phone,
                        ajax: true,
                        onSend: function() {
                            content.empty();
                            content.append("<div class='loading payments'/>");
                        },
                        ajaxerror: function(d, a) {
                            content.empty();
                            content.append($("<div>").text(localization.docsignview.phoneConfirmationText));
                            done = true;
                        },
                        ajaxsuccess: function(d) {
                            content.empty();
                            content.append($("<div>").text(localization.docsignview.phoneConfirmationText));
                            done = true;
                        }
                    }).send();
                }
            });
        },
        getPlanName: function(p) {
            if(p in localization.payments.plans)
                return localization.payments.plans[p];
            return "Unknown";
        },
        addMark: function(p) {
            if(p === 0)
                return "0,00";
            return (""+p).slice(0,-2) + "," + (""+p).slice(-2);
        },
        pOrS: function() {
            return this.model.plan().subscription().pending() || this.model.plan().subscription();
        },
        planName: function() {
            return $('<div class="plan-name" />').text(this.pOrS().name());
        },
        planPrice: function() {
            var view = this;
            var pOrS = this.model.plan().subscription().pending() || this.model.plan().subscription();
            var amount   = pOrS.unitAmountInCents();
            var amountf  = view.addMark(amount);
            var currency = this.model.plan().subscription().currency();
            var quantity = pOrS.quantity();
            var txt = 
                amountf + " " + currency + " x " + quantity + " " + localization.payments.users;
            return $('<div class="plan-price" />').text(txt);
        },
        planTotal: function() { // not used?
            var view = this;
            var pOrS = this.model.plan().subscription().pending() || this.model.plan().subscription();
            var amount   = pOrS.unitAmountInCents();
            var quantity = pOrS.quantity();
            var total    = amount * quantity;
            var totalf   = view.addMark(total);
            var currency = this.model.plan().subscription().currency();
            var txt      = 
                localization.payments.table.total + ": " + totalf + " " + currency + " " + localization.payments.permonth + "." ;
            return $('<div class="plan-total" />').text(txt);
        },
        pendingLine: function() {
            var plan = this.model.plan().subscription();
            var txt = 
                localization.payments.pendingline.part1 + " " + plan.name() + " " + 
                localization.payments.pendingline.part2 + " " + this.dateString(plan.billingEnds()) + ".";
            return $('<div class="plan-pending" />').text(txt);
        },
        nextPayment: function() {
            var view = this;
            var model = view.model;

            var pOrS = model.plan().subscription().pending() 
                || model.plan().subscription();
            var billingEnds  = model.plan().subscription().billingEnds();
            var billingEndsf = view.dateString(billingEnds);
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
            _.each(model.plan().invoices(), function(invoice) {
                var date   = invoice.date();
                var datef  = view.dateString(date);
                var total  = invoice.totalInCents();
                var currency     = model.plan().subscription().currency();
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
        },
        paymentsTable: function() {
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
            return $('<div class="subscription-payments" />')
                .append(payments);
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
                                                                      LoadingDialog.open(localization.payments.savingsubscription);
                                                                  }
                                                                 })
                                          });
                                          return false;
                                      }
                                     });
            return button.input();
        },
        subscriptionChooser: function() {
            var view = this;
            var model = view.model;
            var sOrS = model.signup() || model.plan().subscription();

            var changesubtable = $('<div />');

            var select = $('<select name="plan" id="js-select-subscription" />');
            
            _.each(['professional', 'team'], function(value) {
                var name     = localization.payments.plans[value];
                var price    = planPrice(value);
                var pricef   = view.addMark(price);
                var currency = sOrS.currency();
                var txt = 
                    name + " " + pricef + " " + currency;
                select.append($("<option />").attr("value", value).text(txt));
            });

            var subchooser = $('<span class="subscription-chooser" />');

            var totaltable = $('<div class="totaltable" />');

            var num = $('<input type="number" name="quantity" min="3" />');
            num.val(view.pOrS().quantity());

            var userchooser = $('<div class="plan-price" />');

            select.change(function() {
                console.log("hello");
                var plan     = select.val();

                if(plan === 'team') {
                    if(num.val() < 3) {
                        num.val(3);
                        num.change();
                    }
                    userchooser.empty();
                    userchooser.append($('<span class="select-users" />')
                                       .text(localization.payments.users + ": "))
                        .append(num);
                    num.change(changefun).bind('input', changefun).blur(function() { if(num.val() < 3) num.val(3); changefun(); });
                } else {
                    userchooser.empty();
                    userchooser.append($('<span class="select-users" />')
                                       .text(localization.payments.users + ": 1"));
                }

                var quantity = plan === 'team' ? num.val() : 1;
                var price    = planPrice(plan);
                var pricef   = view.addMark(price);
                var total    = price * quantity;
                var totalf   = view.addMark(total);
                var currency = sOrS.currency();

                
                totaltable.empty();
                totaltable.append($('<div class="plan-total" />')
                                  .text(localization.payments.table.total + ": " + totalf + " " + currency + " " + localization.payments.permonth + "."));
                
            });

            var changefun = function() {
                console.log("hello2");
                var plan     = select.val();
                var quantity = plan === 'team' ? num.val() : 1;
                if(quantity < 3) quantity = 3;
                var price    = planPrice(plan);
                var pricef   = view.addMark(price);
                var total    = price * quantity;
                var totalf   = view.addMark(total);
                var currency = sOrS.currency();

                totaltable.empty();
                totaltable.append($('<div class="plan-total" />')
                                  .text(localization.payments.table.total + ": " + totalf + " " + currency + " " + localization.payments.permonth + "."));
            };


            select.val(view.pOrS().code());
            select.change();
            subchooser
                .append($('<span class="select-plan" />').text(localization.payments.selectplan + ": "))
                .append(select);
 

            
            return changesubtable
                .append(subchooser)
                .append(userchooser)
                .append(totaltable);
        },
        changeSubscription: function() {
            var view = this;
            var model = view.model;

            var changesub = $('<div class="col" />');
            var changesubheader = $('<div class="account-header" />')
                .append($('<h2 />')
                        .text(localization.payments.table.currentplan));

            var button = Button.init({color: 'green',
                                      size: 'small',
                                      cssClass: 'savechanges',
                                      text: localization.payments.savechanges,
                                      onClick: function() {
                                          var message = localization.payments.changeconfirm;
                                          
                                          var conf = Confirmation.popup({
                                              title: localization.payments.changeaccount,
                                              acceptText: localization.payments.changeaccount,
                                              content: $('<p />').text(message),
                                              submit: new Submit({url: "/payments/changeplan",
                                                                  method: "POST",
                                                                  inputs: $('.change-subscription select, .change-subscription input'),
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
                                                                      LoadingDialog.open(localization.payments.savingsubscription);
                                                                  }
                                                                 })
                                          });
                                          return false;
                                          
                                      }
                                     });

            var accountbody = $('<div class="account-body" />')
                        .append(view.subscriptionChooser());
            
            changesub
                .append(changesubheader)
                .append(accountbody);

            if(model.plan().subscription().pending() && 
               model.plan().subscription().pending().code() !== model.plan().subscription().code())
                accountbody.append(view.pendingLine());

            if(!(model.plan().subscription().code() === 'free' ||
                 (model.plan().subscription().pending() && 
                  model.plan().subscription().pending().code() === 'free')))
                changesub.append(view.cancelButton());

            changesub
                .append(button.input())
                .append($('<div class="clearfix" />'));

            return $('<div class="change-subscription" />').append(changesub);
        },
        changeBillingForm: function() {
            var view = this;
            var model = view.model;
            
            var billing = $('<div class="col" />');
            var billingheader = $('<div class="account-header" />')
                .append($('<h2 />')
                        .text(localization.payments.table.changebilling));
            var billingform = $('<div class="account-body changebilling-form" />');
            Recurly.config({
                subdomain: model.server().subdomain()
                , currency: 'SEK'
                , country: model.contact().country()
            });
            Recurly.buildBillingInfoUpdateForm(
                {target: billingform,
                 signature: model.plan().signatures().billing, 
                 accountCode: model.plan().accountCode(),
                 acceptedCards : ['mastercard', 'visa'],
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
            billingform.append($('<div class="cancel" />').append(view.cancelButton()));
            return billing.append(billingheader).append(billingform);
        },
        showCurrentSubscription: function () {
            var view = this;
            var model = view.model;
            var $el = $(view.el);
            $el.addClass("subscribed").removeClass("subscribing");

            var col1 = $('<div class="col1" />')
                .append(view.paymentsTable());


            var col2 = $('<div class="col2" />')
                .append(view.changeBillingForm());
            
            $el.append(col1).append(col2);
        },
        currentPlan: function() {
            var view = this;
            var model = view.model;

            return $('<div class="plan-name" />')
                .text(view.getPlanName(model.plan().plan()));
        },
        showCurrentPlan: function() {
            var view = this;
            var model = view.model;
            var $el = $(view.el);

            $el.addClass("noprovider");

            var div = $('<div class="col" />');
            var header = $('<div class="account-header" />')
                .append($('<h2 />')
                        .text(localization.payments.table.currentplan));
            var table = $('<div class="account-body" />')
                .append(view.currentPlan())
                .append($('<p />')
                       .text(model.plan().quantity() + " " + localization.payments.users))
                .append($('<p class="askviktor" />')
                        .html(localization.payments.askviktor));

            $el.append(div.append(header).append(table));

        },
        render: function() {
            var view = this;
            var model = view.model;
            var $el = $(view.el);
            $el.empty();
            console.log(model.signup());
            if(model.signup())
                view.showSubscribeForm();
            else if(model.plan() && model.plan().provider() === "recurly")
                view.showCurrentSubscription();
            else if(model.plan() && model.plan().provider() === "none")
                view.showCurrentPlan();
            LoadingDialog.close();
        }
    });

    window.paymentsDashboardModel = null;
    window.paymentsDashboardView  = null;

    window.bootPaymentsDashboard = function(selector) {
        LoadingDialog.open(localization.payments.loading);
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

