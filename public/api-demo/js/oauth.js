
(function(window) {

var OAuthModel = Backbone.Model.extend({
        defaults: {
            shortcut : false,
            consumer_key : LocalStorage.get("oauth","consumer_key") != undefined ? LocalStorage.get("oauth","consumer_key") : "",
            client_shared_secret : LocalStorage.get("oauth","client_shared_secret") != undefined ? LocalStorage.get("oauth","client_shared_secret") : "",
            callback :  LocalStorage.get("oauth","callback") != undefined ? LocalStorage.get("oauth","callback") : window.location.href,
            token : LocalStorage.get("oauth","token") != undefined ? LocalStorage.get("oauth","token") : "",
            token_secret : LocalStorage.get("oauth","token_secret") != undefined ? LocalStorage.get("oauth","token_secret") : "",
            verifier : LocalStorage.get("oauth","verifier") != undefined ? LocalStorage.get("oauth","verifier") : "",
            final_token :   LocalStorage.get("oauth","final_token") != undefined ? LocalStorage.get("oauth","final_token") : "",
            final_token_secret :   LocalStorage.get("oauth","final_token_secret") != undefined ? LocalStorage.get("oauth","final_token_secret") : "",
            priviliges :  LocalStorage.get("oauth","priviliges") != undefined ? LocalStorage.get("oauth","priviliges") : "DOC_CREATE",
        },
        save : function() {
            LocalStorage.set("oauth","consumer_key", this.consumer_key());
            LocalStorage.set("oauth","client_shared_secret",this.client_shared_secret());
            LocalStorage.set("oauth","callback",this.callback());
            LocalStorage.set("oauth","token",this.token());
            LocalStorage.set("oauth","verifier",this.verifier());
            LocalStorage.set("oauth","token_secret",this.token_secret());
            LocalStorage.set("oauth","final_token",this.final_token());
            LocalStorage.set("oauth","final_token_secret",this.final_token_secret());
            LocalStorage.set("oauth","priviliges",this.priviliges());
            
        },
        clear : function() {
            LocalStorage.set("oauth","verifier","");
            LocalStorage.set("oauth","token","");
            LocalStorage.set("oauth","token_secret","");
            LocalStorage.set("oauth","final_token","");
            LocalStorage.set("oauth","final_token_secret","");
            this.set({
                  "token" : ""
                , "token_secret" : ""
                , "final_token" : ""
                , "final_token_secret" : ""
                , "verifier" : ""
                , "priviliges" : "DOC_CREATE"
            }, {silent: true});
            this.trigger("change");
            this.trigger("clear");
        },   
        shortcut : function() {
            return this.get("shortcut");
        },
        set_shortcut : function(v) {
            this.set({"shortcut" : v}, {silent: true});
            this.save();
        },
        /* TCR PARAMS*/
        consumer_key : function() {
            return this.get("consumer_key");
        },
        set_consumer_key : function(v) {
            this.set({"consumer_key" : v}, {silent: true});
            this.save();
        },
        client_shared_secret : function() {
            return this.get("client_shared_secret");
        },
        set_client_shared_secret : function(v) {
            this.set({"client_shared_secret" : v}, {silent: true});
            this.save();
        },
        callback : function() {
            return this.get("callback");
        },
        set_callback : function(v) {
            this.set({"callback" : v}, {silent: true});
            this.save();
        },
        priviliges : function() {
            return this.get("priviliges");
        },
        set_priviliges : function(v) {
            this.set({"priviliges" : v}, {silent: true});
            this.save();
        },
        /* ROA PARAMS*/
        token : function() {
            return this.get("token");
        },
        set_token : function(v) {
            this.set({"token" : v}, {silent: true});
            this.save();
        },

        token_secret : function() {
            return this.get("token_secret");
        },
        set_token_secret : function(v) {
            this.set({"token_secret" : v}, {silent: true});
            this.save();
        },

        /* TR PARAMS*/
        verifier : function() {
            return this.get("verifier");
        },
        set_verifier : function(v) {
            this.set({"verifier" : v}, {silent: true});
            this.save();
        },

        /* Final tokens*/

        final_token : function() {
            return this.get("final_token");
        },
        set_final_token : function(v) {
            this.set({"final_token" : v}, {silent: true});
            this.save();
        },
        final_token_secret : function() {
            return this.get("final_token_secret");
        },
        set_final_token_secret : function(v) {
            this.set({"final_token_secret" : v}, {silent: true});
            this.save();
        },

        
        sendeTCR : function() {
                var model = this;
                new OAuthTemporaryCredentialRequest({
                      oauth_consumer_key : this.consumer_key(),
                      oauth_callback:  this.callback(),
                      oauth_client_shared_secret : this.client_shared_secret(),
                      priviliges : this.priviliges()                              
                }).send(function(res) {
                    model.set_token(res.oauth_token);
                    model.set_token_secret(res.oauth_token_secret);
                    model.save();
                    model.trigger("change");
                });
        },
        sendeTR : function() {
                var model = this;
                new OAuthTokenRequest({
                      oauth_consumer_key : this.consumer_key(),
                      oauth_client_shared_secret : this.client_shared_secret(),
                      oauth_token : this.token(),
                      oauth_token_secret : this.token_secret(),
                      oauth_verifier : this.verifier()                      
                }).send(function(res) {
                    model.set_final_token(res.oauth_token);
                    model.set_final_token_secret(res.oauth_token_secret);
                    model.save();
                    model.trigger("change");
                    model.trigger("ready");
                    
                });
        },
        authorizationForRequests: function() {
            return 'oauth_signature_method="PLAINTEXT",' +
                   'oauth_consumer_key="' + this.consumer_key() + '",' +
                   'oauth_token="'        + this.final_token()     + '",' +
                   'oauth_signature="'    + this.client_shared_secret() + "&" + this.final_token_secret()    + '"'
            
        },
        ready: function() {
            return    this.final_token() != undefined && this.final_token() != ""
                   && this.final_token_secret() != undefined && this.final_token_secret() != ""
                   && this.consumer_key() != undefined && this.consumer_key() != ""
                   && this.client_shared_secret() != undefined && this.client_shared_secret() != "";
                   
        },
        initialize : function() {
            if ($.getUrlVar("oauth_verifier") != undefined) {
                this.set_verifier($.getUrlVar("oauth_verifier"));
                window.location.href = window.location.href.substring(0,window.location.href.indexOf("?"))
            }
        }
          
});


var OAuthView = Backbone.View.extend({
        model: OAuthModel,
        initialize: function(args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.render();
        },
        personalAccessSection : function() {
            var model = this.model;
            var box = $("<div class='section'>");
            box.append("<div class='header'>Shortcut: Enter personal access credentials."+
                       "<BR/><small><small>Bypass OAuth handshake by providing personal access credentials obtained at <a href='"+Scrive.serverUrl()+"/oauth/dashboard'>"+Scrive.serverUrl()+"/oauth/dashboard</a> </small></small></div>");

            var consumer_key_input = $("<input type='text'>").val(model.consumer_key());
            consumer_key_input.change(function() {model.set_consumer_key(consumer_key_input.val()); return false;});
            box.append($("<div><div class='label'>Client credentials identifier (<span class='code'>oauth_consumer_key</span>): </div></div>").append(consumer_key_input));
            
            var client_shared_secret_input = $("<input type='text'>").val(model.client_shared_secret());
            client_shared_secret_input.change(function() {model.set_client_shared_secret(client_shared_secret_input.val()); return false;})
            box.append($("<div><div class='label'>Client credentials secret (<span class='code'>oauth_signature</span>): </div></div>").append(client_shared_secret_input));

            var token_input = $("<input type='text'>").val(model.final_token());
            token_input.change(function() {model.set_final_token(token_input.val()); return false;});
            box.append($("<div><div class='label'>Token credentials identifier (<span class='code'>oauth_token</span>): </div></div>").append(token_input));
            
            var token_secret_input = $("<input type='text'>").val(model.final_token_secret());
            token_secret_input.change(function() {model.set_final_token_secret(token_secret_input.val()); return false;});
            box.append($("<div><div class='label'>Token credentials secret (<span class='code'>oauth_token_secret</span>): </div></div>").append(token_secret_input));

            var button = $("<input type='button' value='Set personal credentials'/ >");
            button.click(function() {model.trigger("change"); model.trigger("ready"); return false;});
            box.append(button);
            
            return box;
        },
        TCRSection : function() {
            var model = this.model;
            var box = $("<div class='section'>");
            box.append("<div class='header'>Step 1. Obtain temporary credentials from Scrive."+
                        "<BR/> <small><small> You can generate client credentials at <a href='"+Scrive.serverUrl()+"/oauth/dashboard'>"+Scrive.serverUrl()+"/oauth/dashboard</a> </small></small></div>");

            var consumer_key_input = $("<input type='text'>").val(model.consumer_key());
            box.append($("<div><div class='label'>Client credentials identifier (<span class='code'>oauth_consumer_key</span>): </div></div>").append(consumer_key_input))
            consumer_key_input.change(function() {model.set_consumer_key(consumer_key_input.val()); return false;})
            
            var client_shared_secret_input = $("<input type='text'>").val(model.client_shared_secret());
            client_shared_secret_input.change(function() {model.set_client_shared_secret(client_shared_secret_input.val()); return false;})
            box.append($("<div><div class='label'>Client credentials secret (<span class='code'>oauth_signature</span>): </div></div>").append(client_shared_secret_input))
            box.append($("<div><div class='label'>Callback: </div></div>").append($("<b>").text(model.callback())))

            var priviliges = model.priviliges();
            var priviliges_input = $("<select/>");

            var o1 = $("<option value='DOC_CREATE'>DOC_CREATE</option>")
            priviliges_input.append(o1);
            if (priviliges == "DOC_CREATE") o1.attr("selected","TRUE");

            var o2 = $("<option value='DOC_SEND'>DOC_SEND</option>")
            priviliges_input.append(o2);
            if (priviliges == "DOC_SEND") o1.attr("selected","TRUE");

            var o3 = $("<option value='DOC_CHECK'>DOC_CHECK</option>")
            priviliges_input.append(o3);
            if (priviliges == "DOC_CHECK") o3.attr("selected","TRUE");

            var o4 = $("<option value='DOC_CREATE+DOC_SEND'>DOC_CREATE+DOC_SEND</option>")
            priviliges_input.append(o4);
            if (priviliges == "DOC_CREATE+DOC_SEND") o4.attr("selected","TRUE");

            var o5 = $("<option value='DOC_CREATE+DOC_SEND+DOC_CHECK'>DOC_CREATE+DOC_SEND+DOC_CHECK</option>")
            priviliges_input.append(o5);
            if (priviliges == "DOC_CREATE+DOC_SEND+DOC_CHECK") o5.attr("selected","TRUE");
                                     
            priviliges_input.change(function() {model.set_priviliges(priviliges_input.val()); return false;})
                                     
            box.append($("<div><div class='label'>Priviliges: </div></div>").append(priviliges_input));
            
            var sendeTCRButton = $("<input type='button' value='Send Temporary Credential Request'/ >");
            sendeTCRButton.click(function() {model.sendeTCR(); return false;});
            box.append(sendeTCRButton);

            return box;
        },
        ROASection : function() {
            var model = this.model;
            var box = $("<div class='section'>");
            box.append("<div class='header'>Step 2. Let a user (resource owner) grant access to his Scrive account (Resource Owner Authorization)</div>");
            
            box.append($("<div><div class='label'>Temporary credentials identifier (<span class='code'>oauth_token</span>): </div></div>").append($("<b>").text(model.token())))
            box.append($("<div><div class='label'>Temporary credentials secret (<span class='code'>oauth_token_secret</span>): </div></div>").append($("<b>").text(model.token_secret())))

            var rao = new OAuthResourceOwnerAuthorization({oauth_token : model.token()});
            var confirm_link = $("<a href='"+rao.requestUrl()+"' >"+rao.requestUrl()+"</a>");
            box.append($("<div><div class='label'>Confirmation link: </div></div>").append(confirm_link))

            return box;
        },
        TRSection : function() {
            var model = this.model;
            var box = $("<div class='section'>");
            box.append("<div class='header'>Step 3. Get token credentials to act on behalf of Scrive user (Token Request)</div>");

            box.append($("<div><div class='label'>Verifier (<span class='code'>oauth_verifier</span>): </div></div>").append($("<b>").text(model.verifier())))

            var sendeTRButton = $("<input type='button' value='Send Token Request'/ >");
            sendeTRButton.click(function() {model.sendeTR(); return false;});
            box.append(sendeTRButton);
            return box;
        },
        FinalTokenSection : function() {
            var model = this.model;
            var box = $("<div class='section'>");
            box.append("<div class='header'>Done. We have everything needed to make API calls.</div>");
            box.append($("<div><div class='label'>Client credentials identifier (<span class='code'>oauth_consumer_key</span>): </div></div>").append($("<b>").text(model.consumer_key())));
            box.append($("<div><div class='label'>Client credentials secret (<span class='code'>oauth_signature</span>): </div></div>").append($("<b>").text(model.client_shared_secret())));
            box.append($("<div><div class='label'>Token credentials identifier (<span class='code'>oauth_token</span>): </div></div>").append($("<b>").text(model.final_token())))
            box.append($("<div><div class='label'>Token credentials secret (<span class='code'>oauth_token_secret</span>): </div></div>").append($("<b>").text(model.final_token_secret())))
            return box;
        },
        render: function() {
            $(this.el).children().detach();
            var view = this;
            var model = this.model;
            var container = $(this.el);
            container.children().detach();
            container.append($("<a href='#' style='float:right'>Clear all OAuth data</a>").click(function() {model.clear();}))
            container.append("<div class='mainheader'>OAuth</div>");
            if (model.ready())
                container.append(this.FinalTokenSection());
            else {
                if (model.shortcut()) {
                    switchbutton = $("<input type='button' value='Use full OAuth'/ >");
                    switchbutton.click(function() {model.set_shortcut(false); model.trigger("change"); return false;});
                    container.append(switchbutton);
                } else {
                    switchbutton = $("<input type='button' value='Bypass OAuth and use Personal Credentials'/ >");
                    switchbutton.click(function() {model.set_shortcut(true); model.trigger("change"); return false;});
                    container.append(switchbutton);
                }
                if (model.shortcut()) {
                    container.append(this.personalAccessSection());
                } else {
                    container.append(this.TCRSection());
                    if (model.token() != undefined && model.token() != "") {
                        container.append(this.ROASection());
                        if (model.verifier() != undefined && model.verifier() != "")
                            container.append(this.TRSection());
                    }

                }

            }    
            return this;
        }
});


window.OAuth = function() {
        var model = new OAuthModel({ });
        var view  = new OAuthView({
                            model: model,
                            el: $("<div class='oauth'/>")
                    });
        return {
                model : function() {return model;},
                view : function() {return view;},
                ready : function() {return model.ready();},
                authorizationForRequests : function() {return model.authorizationForRequests();}
        };
}

$.extend({
  getUrlVars: function(){
    var vars = [], hash;
    var hashes = window.location.href.slice(window.location.href.indexOf('?') + 1).split('&');
    for(var i = 0; i < hashes.length; i++)
    {
      hash = hashes[i].split('=');
      vars.push(hash[0]);
      vars[hash[0]] = hash[1];
    }
    return vars;
  },
  getUrlVar: function(name){
    return $.getUrlVars()[name];
  }
});


window.OAuthModel = OAuthModel;

})(window);
