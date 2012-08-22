(function(window) {
    var teamplans = {'team': true, 'enterprise': true};

    /*
      Holds information relevant to blocking for a user.

    */
    window.BlockingInfoModel = Backbone.Model.extend({
        defaults: {
            block: false
        },
        docsTotal: function() {
            if(this.plan() === 'free')
                return 3;
            else
                return 40; // # of docs it says in the TOS
        },
        docsUsed: function() {
            return this.get('docsused');
        },
        docsLeft: function() {
            return this.docsTotal() - this.docsUsed();
        },
        plan: function() {
            return this.get('plan');
        },
        free: function() {
            return this.plan() === 'free';
        },
        teamPlan: function() {
            return teamplans[this.plan()];
        },
        blockAny: function() {
            return this.get('block');
        },
        blockCreate: function() {
            return this.blockAny() && this.docsLeft() < 1;
        },
        blockCSV: function(n) {
            return this.blockAny() && this.docsLeft() < n;
        },
        blockBranding: function() {
            return this.blockAny() && !this.teamPlan();
        },
        blockUsers: function() {
            return this.blockAny() && (!this.teamPlan()
                                       || this.usersUsed() >= this.usersPaid());
        },
        usersUsed: function() {
            return this.get('usersused') || 0;
        },
        usersPaid: function() {
            return this.get('userspaid') || 0;
        }
    });

    $.ajax("/blockinginfo", {dataType: 'json',
                            timeout: 3000,
                            success: function(data) {
                                data.block = true;
                                window.BlockingInfo = new window.BlockingInfoModel(data);
                            },
                            error: function() {
                                window.BlockingInfo = new window.BlockingInfoModel({});
                            }});

    window.blocking = {show: function(text) {

        Confirmation.popup({
            title: "Please purchase an account to continue",
            content: $("<p />").text(text),
            color: "Green",
            acceptText: "Purchase",
            onAccept: function() {
                // just send them to the payments page!
                window.location = "/payments/dashboard";
                return false;
            }
        });

    }};

})(window);
