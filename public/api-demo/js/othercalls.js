/*
 * API demo main model + view
 */


(function(window) {

window.CheckClientApiCall = ApiCall.extend({
        defaults: {
             name : "Check if given client needs update"
        },
        initialize: function (args) {
            this.set({"client" : "{ \n \"platform\": \"iOS\", \n \"platform_version\":\"5.0\", \n \"client\":\"Scrive iPad App\",\n \"client_version\":\"0.1\" \n}"});
        },
        client : function() {return this.get("client");},
        setClient : function(client) {
            LocalStorage.set("api","client",client);
            return this.set({"client" : client});
        },
        isCheckClient : function() {return true;},
        send : function() {
            var model = this;
            $.ajax(Scrive.apiUrl()+"checkclient", {
                type: 'POST',
                 data : {client : this.client()},
                cache: false,
                success : function(res) {
                    model.setResult(res);
                },
                error : function(res) {
                    model.setResult(JSON.stringify(res.responseText,undefined," "));
                }
            });
        }
});



window.CheckClientApiCallView = Backbone.View.extend({
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
            var clienttextarea = $("<textarea class='json-text-area'>"+model.client()+"</textarea>");
            clienttextarea.change(function() {model.setClient(clienttextarea.val()); return false;})
            var button = $("<input type='button' value='Send request'/>");
            button.click(function() {model.send(); return false;});
            boxLeft.append($("<div>Client JSON : <BR/></div>").append(clienttextarea)).append($("<div/>").append(button));
            this.render();
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
