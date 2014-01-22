// Polling for Mobile BankID

(function(window){

    // keeps track of the polling state
    window.MobileBankIDPolling = Backbone.Model.extend({
        defaults: function() {
          return {
            done : false,
            status: "outstanding",
            message: localization.startingSaveSigning,
            callback: function() {},
            errorcallback: function() {},
            remaining: [10, // wait 10s before first poll
                        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3, // then we can poll 20 times with 3s intervals
                        10,10,10,10,10,10,10,10,10] // then we finish with 10s intervals; docs say we get Fault before the end
          };
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
        done: function() {
          return this.get("done");
        },
        setDone : function() {
          this.set({"done":true});
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
        errorcallback: function() {
            return this.get("errorcallback")();
        },
        poll: function() {
            var polling = this;
            if(polling.keepPolling()) {
                var next = polling.next();
                console.log("Setting up timer: " + next);
                if(next)
                    setTimeout(function() {
                        $.ajax(polling.collectUrl(),
                               {
                                   "data" : {
                                       "transactionid" : polling.trid()
                                   },
                                   "dataType": "json",
                                   "success": function(d) {
                                       if(d.error) {
                                           polling.status("error");
                                           if (!polling.done()) {
                                             polling.setDone();
                                             polling.errorcallback();
                                           }
                                       } else {
                                           polling.status(d.status);
                                           polling.message(d.message);
                                           if(polling.status() === "complete" && !polling.done()) {
                                               polling.callback();
                                               polling.setDone();
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
            this.model.bind('change:message change:done', this.render);
        },
        render: function() {
            var polling = this.model;
            if (!polling.done())
              LoadingDialog.open(polling.message());
            else
              LoadingDialog.close();
        }
    });
})(window);
