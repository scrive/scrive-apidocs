// Polling for Mobile BankID

(function(window){

    // keeps track of the polling state
    window.MobileBankIDPolling = Backbone.Model.extend({
        defaults: {
            status: "outstanding",
            message: localization.startingSaveSigning,
            magichash: "",
            fetching: false,
            callback: function() {},
            remaining: [10, // wait 10s before first poll
                        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3, // then we can poll 20 times with 3s intervals
                        10,10,10,10,10,10,10,10,10] // then we finish with 10s intervals; docs say we get Fault before the end
        },
        magichash: function() {
            return this.get("magichash");
        },
        status: function(s) {
            if(s) {
                if(this.get("status") !== s)
                    this.set({status: s});
                return this;
            } else
                return this.get("status");
        },
        message: function(msg) {
            if(msg) {
                this.set({message:msg});
                return this;
            } else
                return this.get("message");
        },
        next: function() {
            return this.get("remaining").shift();
        },
        keepPolling: function() {
            return this.get("status") === "outstanding" || this.get("status") === "usersign";
        },
        docid: function() {
            return this.get("docid");
        },
        slid: function() {
            return this.get("slid");
        },
        trid: function() {
            return this.get("trid");
        },
        collectUrl: function() {
            return this.get("collecturl");
        },
        callback: function() {
            return this.get("callback")();
        },
        poll: function() {
            var polling = this;
            if(polling.keepPolling()) {
                var next = polling.next();
                console.log("Setting up timer: " + next);
                if(next)
                    setTimeout(function() { 
                        if(polling.trid() && polling.collectUrl())
                            console.log("yo!");
                        $.ajax(polling.collectUrl(),
                               {
                                   "data" : {
                                       "transactionid" : polling.trid(),
                                       "magichash" : polling.magichash()
                                   },
                                   "dataType": "json",
                                   "success": function(d) {
                                       if(d.error) {
                                           polling.status("error");
                                       } else {
                                           polling.status(d.status);
                                           polling.message(d.message);
                                           if(polling.status() === "complete") {
                                               polling.callback();
                                           }
                                       }
                                   }});
                        polling.poll();
                    }, next * 1000);
            }
        }
    });

    window.MobileBankIDPollingView = Backbone.View.extend({
        initialize: function (args) {
            _.bindAll(this, 'render');
            this.model.bind('change:message', this.render);
        },
        render: function() {
            var polling = this.model;
            LoadingDialog.open(polling.message());
        }
    });
})(window);
