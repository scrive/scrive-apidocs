/*
 * API demo main model + view
 */


(function(window) {

window.GetProfileApiCall = ApiCall.extend({
        defaults: {
             name : "Get user profile. Only for current user"
        },
        initialize: function (args) {
        },
        isGetProfile : function() {return true;},
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"getprofile", {
                type: 'GET',
                cache: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});

window.GetPaymentInfoApiCall = ApiCall.extend({
        defaults: {
             name : "Get payment if for current user"
        },
        initialize: function (args) {
        },
        isGetPaymentInfo : function() {return true;},
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"paymentinfo", {
                type: 'GET',
                cache: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(JSON.stringify(JSON.parse(res),undefined," "));
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});



window.SetLanguageApiCall = ApiCall.extend({
        defaults: {
             name : "Set user language",
             language : LocalStorage.get("api","language")

        },
        initialize: function (args) {
        },
        isSetLanguage : function() {return true;},
        language : function() {return this.get("language");},
        setLanguage : function(language) {
            LocalStorage.set("api","language",language);
            return this.set({"language" : language});
        },

        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"changelanguage", {
                type: 'POST',
                cache: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(res);
                },
                data : { lang : model.language()},
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});


window.UpdateProfileApiCall = ApiCall.extend({
        defaults: {
             name : "Update data in user profile",
             fstname : "",
             sndname : "",
             personalnumber : "",
             companyname :  "",
             companynumber :  "",
             companyposition : ""

        },
        initialize: function (args) {
        },
        isUpdateProfile : function() {return true;},

        fstname : function() {return this.get("fstname");},
        setFstname : function(fstname) {
            return this.set({"fstname" : fstname});
        },

        sndname : function() {return this.get("sndname");},
        setSndname : function(sndname) {
            return this.set({"sndname" : sndname});
        },

        personalnumber : function() {return this.get("personalnumber");},
        setPersonalnumber : function(personalnumber) {
            return this.set({"personalnumber" : personalnumber});
        },

        companyname : function() {return this.get("companyname");},
        setCompanyname : function(companyname) {
            return this.set({"companyname" : companyname});
        },

        companynumber : function() {return this.get("companynumber");},
        setCompanynumber : function(companynumber) {
            return this.set({"companynumber" : companynumber});
        },

        companyposition : function() {return this.get("companyposition");},
        setCompanyposition : function(companyposition) {
            return this.set({"companyposition" : companyposition});
        },

        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"updateprofile", {
                type: 'POST',
                cache: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(res);
                },
                data : {
                    fstname : model.fstname(),
                    sndname : model.sndname(),
                    personalnumber : model.personalnumber(),
                    companyname :  model.companyname(),
                    companynumber :  model.companynumber(),
                    companyposition : model.companyposition()
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});


window.SignupApiCall = ApiCall.extend({
        defaults: {
             name : "Create new user",
             email : LocalStorage.get("api","email"),
             language : LocalStorage.get("api","language")


        },
        initialize: function (args) {
        },
        isSignup : function() {return true;},
        email : function() {return this.get("email");},
        setEmail : function(email) {
            LocalStorage.set("api","email",email);
            return this.set({"email" : email});
        },

        language : function() {return this.get("language");},
        setLanguage : function(language) {
            LocalStorage.set("api","language",language);
            return this.set({"language" : language});
        },
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"signup", {
                type: 'POST',
                cache: false,
                headers : { authorization : model.authorization() },
                success : function(res) {
                    model.setResult(res);
                },
                data : { lang : model.language() , email : model.email()},
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});



window.SetPasswordApiCall = ApiCall.extend({
        defaults: {
             name : "Add to padqueue API call",
             oldpassword : LocalStorage.get("api","oldpassword"),
             newpassword : LocalStorage.get("api","newpassword"),

        },
        initialize: function (args) {
        },
        isSetPassword : function() {return true;},
        oldpassword : function() {return this.get("oldpassword");},
        setOldpassword: function(oldpassword) {
            LocalStorage.set("api","oldpassword",oldpassword);
            return this.set({"oldpassword" : oldpassword});
        },
        newpassword : function() {return this.get("newpassword");},
        setNewpassword: function(newpassword) {
            LocalStorage.set("api","newpassword",newpassword);
            return this.set({"newpassword" : newpassword});
        },
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"changepassword" , {
                type: 'POST',
                cache: false,
                headers : { authorization : model.authorization() },
                data : { oldpassword   : model.oldpassword(),
                         password  : model.newpassword() },
                success : function(res) {
                    model.setResult(res);
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});


window.GetProfileApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();

        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});

window.GetPaymentInfoApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();
        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});


window.SetLanguageApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();

        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var langInput = $("<input type='text'/>").val(model.language());
            langInput.change(function() {model.setLanguage(langInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Language (en or sv): <BR/></div>").append(langInput))
                   .append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});

window.SignupApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();

        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var emailInput = $("<input type='text'/>").val(model.email());
            emailInput.change(function() {model.setEmail(emailInput.val()); return false;})

            var langInput = $("<input type='text'/>").val(model.language());
            langInput.change(function() {model.setLanguage(langInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Email: <BR/></div>").append(emailInput)).append("<BR/>")
                   .append($("<div>Language (en or sv, optional): <BR/></div>").append(langInput))
                   .append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});


window.UpdateProfileApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();

        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);

            var fstnameInput = $("<input type='text'/>").val(model.fstname());
            fstnameInput.change(function() {model.setFstname(fstnameInput.val()); return false;})

            var sndnameInput = $("<input type='text'/>").val(model.sndname());
            sndnameInput.change(function() {model.setSndname(sndnameInput.val()); return false;})

            var personalnumberInput = $("<input type='text'/>").val(model.personalnumber());
            personalnumberInput.change(function() {model.setPersonalnumber(personalnumberInput.val()); return false;})

            var companynameInput = $("<input type='text'/>").val(model.companyname());
            companynameInput.change(function() {model.setCompanyname(companynameInput.val()); return false;})

            var companynumberInput = $("<input type='text'/>").val(model.companynumber());
            companynumberInput.change(function() {model.setCompanynumber(companynumberInput.val()); return false;})

            var companypositionInput = $("<input type='text'/>").val(model.companyposition());
            companypositionInput.change(function() {model.setCompanyposition(companypositionInput.val()); return false;})

            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Fstname: <BR/></div>").append(fstnameInput)).append("<BR/>")
                   .append($("<div>Sndname: <BR/></div>").append(sndnameInput)).append("<BR/>")
                   .append($("<div>Personalnumber: <BR/></div>").append(personalnumberInput)).append("<BR/>")
                   .append($("<div>Companyname: <BR/></div>").append(companynameInput)).append("<BR/>")
                   .append($("<div>Companynumber: <BR/></div>").append(companynumberInput)).append("<BR/>")
                   .append($("<div>Companyposition: <BR/></div>").append(companypositionInput)).append("<BR/>")
                     .append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});


window.SetPasswordApiCallView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.prerender();

        },
        prerender : function() {
            var model = this.model;
            var box = $(this.el);
            box.children().detach();
            var boxLeft  = $("<div class='left-box'>");
            this.boxRight = $("<div class='right-box'>");
            box.append(this.boxRight).append(boxLeft);
            var oldpasswordInput = $("<input type='text'/>").val(model.oldpassword());
            oldpasswordInput.change(function() {model.setOldpassword(oldpasswordInput.val()); return false;})
            var passwordInput = $("<input type='text'/>").val(model.newpassword());
            passwordInput.change(function() {model.setNewpassword(passwordInput.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Old password : <BR/></div>").append(oldpasswordInput))
                   .append($("<div>New password : <BR/></div>").append(passwordInput))
                   .append($("<div/>").append(button));
            this.render();
        },
        render : function() {
            this.boxRight.empty();
            var model = this.model;
            if (model.result() != undefined)
                this.boxRight.append($("<div>Result : <BR/></div>").append($("<textarea class='json-text-area'>").val(model.result() )))
        }
});

})(window);
