/*

  Payments - outside the logged-in area



*/

(function(window) {

    var PricePageModel = Backbone.Model.extend({
        defaults: {
            first_name: '',
            last_name: '',
            email: '',
            credit_card: '',
            ccv: '',
            year: '',
            month: ''
        },
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
        accountCode: function(ac) {
            if(ac) {
                this.set({account_code:ac});
                return this;
            }
            return this.get('account_code');
        },
        plans: function() {
            return this.get('plans');
        },
        subdomain: function() {
            return this.get('subdomain');
        },
        currentPlan: function(plan) {
            if(plan) {
                this.set({current_plan:plan});
                return this;
            }
            return this.get('current_plan');
        },
        creditCard: function(cc) {
            if(cc) {
                this.set({credit_card:cc});
                return this;
            }
            return this.get('credit_card');
        },
        cvv: function(ccv) {
            if(ccv) {
                this.set({ccv:ccv});
                return this;
            }
            return this.get('ccv');
        },
        month: function(m) {
            if(m) {
                this.set({m:m});
                return this;
            }
            return this.get('month');
        },
        year: function(y) {
            if(y) {
                this.set({year:y});
                return this;
            }
            return this.get('year');
        },

    });

    var PricePageView = Backbone.View.extend({
        tagName: "div",
        className: "payments",
        initialize: function(args){
            _.bindAll(this);
        },
        teamBox: function() {
            var view = this;
            var model = view.model;

            var div = $('<div class="team planbox" />');

            var header = $('<div class="header" />');
            var header1 = $('<div class="header1" />');
            header1.text(localization.payments.plans.team);
            var header2 = $('<div class="header2" />');
            header2.text(localization.payments.plans.teamtag);
            header.append(header1).append(header2);
            div.append(header);

            var features = $('<div class="features" />');
            _.each(localization.payments.features.team, function(t) {
                features.append($('<div class="feature" />').text(t));
            });

            div.append(features);

            var price = $('<div class="price" />');
            var price1 = $('<div class="price1" />');
            price1.text();
            var price2 = $('<div class="price2" />');
            price2.text(localization.payments.plans.teamprice);
            var price3 = $('<div class="price3" />');
            price3.text(localization.payments.peruserpermonth);
            price.append(price1).append(price2).append(price3);
            div.append(price);

            var buttonbox = $('<div class="buttonbox" />');
            var button = Button.init({color: 'black',
                                      text: localization.payments.select,
                                      size: 'small',
                                      width: 150,
                                      onClick: function() {
                                          model.currentPlan('team');
                                          view.render();
                                          return false;
                                      }});
            
            buttonbox.append(button.input());
            div.append(buttonbox);
            div.click(function() {
                model.currentPlan('team');
                view.render();
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
            header1.text(localization.payments.plans.form);
            var header2 = $('<div class="header2" />');
            header2.text(localization.payments.plans.formtag);
            //var img = $('<img src="/img/bestvalue.png" alt="Best Value" class="bestvalue" />');
            header.append(header1).append(header2); //.append(img);
            div.append(header);

            var features = $('<div class="features" />');
            _.each(localization.payments.features.form, function(t) {
                features.append($('<div class="feature" />').text(t));
            });

            div.append(features);

            var price = $('<div class="price" />');
            var price1 = $('<div class="price1" />');
            price1.text("");
            var price2 = $('<div class="price2" />');
            price2.text(localization.payments.plans.formprice);
            var price3 = $('<div class="price3" />');
            price3.text(localization.payments.permonth + " " + localization.and);
            price.append(price1).append(price2).append(price3);

            var price21 = $('<div class="price2 s" />');
            price21.text(localization.payments.plans.docprice);
            var price31 = $('<div class="price3" />');
            price31.text(localization.payments.perdocument);
            price.append(price21).append(price31);

            div.append(price);

            var buttonbox = $('<div class="buttonbox" />');
            var button = Button.init({color: 'black',
                                      text: localization.payments.select,
                                     size: 'small',
                                     width: 150,
                                     onClick: function() {
                                         model.currentPlan('form');
                                         view.render();
                                         return false;
                                     }});
            
            buttonbox.append(button.input());
            div.append(buttonbox);
            div.click(function() {
                model.currentPlan('form');
                view.render();
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
            header1.text(localization.payments.plans.enterprise);
            var header2 = $('<div class="header2" />');
            header2.html(localization.payments.plans.enterprisetag);
            header.append(header1).append(header2);
            div.append(header);

            var features = $('<div class="features" />');
            _.each(localization.payments.features.enterprise0, function(t) {
                features.append($('<div class="feature" />').html(t));
            });
            features.append($('<div class="feature" />').html("&nbsp;"));
            _.each(localization.payments.features.enterprise, function(t) {
                features.append($('<div class="feature" />').text(t));
            });
            div.append(features);

            var price = $('<div class="price" />');
            var price1 = $('<div class="price1" />');
            price1.text(localization.payments.askfora);
            var price2 = $('<div class="price2" />');
            price2.text(localization.payments.quote);
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
        showRecurlyForm: function(el) {
            var view  = this;
            var model = view.model;
            var form = el.find('.planbox.form');
            var team = el.find('.planbox.team');

            if(model.currentPlan() === 'team') {
                team.addClass('selected');
                team.find('.btn-small').removeClass('black').addClass('green')
                    .find('.label').text(localization.payments.selected);
                form.removeClass('selected');
                form.find('.btn-small').removeClass('green').addClass('black')
                    .find('.label').text(localization.payments.select);
            } else if(model.currentPlan() === 'form') {
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
                subdomain: model.subdomain()
                , currency: 'SEK'
                , country: 'SE'
            });
            var recurlyForm = $(el).find('.js-recurly-form');
            
            Recurly.buildSubscriptionForm({
                target: recurlyForm
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
                , signature: model.plans()[model.currentPlan() || 'team']
                , beforeInject: function(form) {
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
                        quantbox.val(1);
                        quantbox.after($('<span class="quantity" />').text(1 + " " + localization.payments.user));
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
                    var cc = form.find('.card_number input');
                    cc.val(model.creditCard());
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
                    fn.val(model.firstName());
                    fn.change();
                    fn.change(function(e) {
                        model.firstName($(e.target).val());
                    });
                    var ln = form.find('.last_name input');
                    ln.val(model.lastName());
                    ln.change();
                    ln.change(function(e) {
                        model.lastName($(e.target).val());
                    });

                    var en = form.find('.email input');
                    en.val(model.email());
                    en.change();
                    en.change(function(e) {
                        model.email($(e.target).val());
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

            });
        },
        showPage: function() {
            var view = this;
            var model = view.model;
            var $el = $(view.el);

            var div = $('<div></div>');
            
            var header = $('<div class="header" />')
                .append($('<h2 />').text(localization.payments.subscribeheader));

            div.append(header);
            div.append($('<h3 />').text(localization.payments.chooseplan));

            var subs = $('<div class="subscription-chooser" />');

            var team = this.teamBox();

            team.addClass('selected');

            subs.append(team);
            subs.append(this.formBox());
            subs.append(this.enterpriseBox());

            div.append(subs);
            div.append($('<div class="clearfix" />'));
            div.append($('<h3 />').text(localization.payments.paywithcard));

            var recurlyform = $('<div class="js-recurly-form"></div>');

            div.append(recurlyform);

            view.showRecurlyForm(div);
            $el.html(div);
        },
        render: function() {
            var view = this;
            
            view.showPage();
            LoadingDialog.close();
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
        }
    });

    window.pricePageModel = null;
    window.pricePageView  = null;

    var fetchAndShow = function(selector) {
        $.ajax("/payments/pricepage.json", 
               {
                   dataType: "json",
                   timeout: 1000,
                   success: function(data) {
                       window.pricePageModel = new PricePageModel(data);
                       window.pricePageView  = new PricePageView({model:window.pricePageModel});
                       window.pricePageView.render();
                       $(selector).append(window.pricePageView.el);
                   },
                   error: function() {
                       fetchAndShow(selector);
                   }
               });
        
    };

    window.bootPricePage = function(selector) {
        LoadingDialog.open(localization.payments.loading);
        $("head").append('<link rel="stylesheet" href="/libs/recurly/recurly.css"></link>');
        fetchAndShow(selector);
    };

})(window);
