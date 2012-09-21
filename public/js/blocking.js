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
                return 500; // # of docs it says in the TOS
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
        status: function() {
            return this.get('status');
        },
        free: function() {
            return this.plan() === 'free';
        },
        enterprise: function() {
            return this.plan() === 'enterprise';
        },
        teamPlan: function() {
            return teamplans[this.plan()];
        },
        overdue: function() {
            return this.status() === 'overdue';
        },
        blockAny: function() {
            return this.get('block') && !this.enterprise();
        },
        blockCreate: function() {
            return this.blockAny() && this.docsLeft() < 1;
        },
        blockCreateMessage: function() {
            if(this.free())
                return localization.blocking.create.noneLeftFree;
            else
                return localization.blocking.create.noneLeftBlock;
        },
        blockCSV: function(n) {
            return this.blockAny() && this.docsLeft() < n;
        },
        blockCSVMessage: function(n) {
            if(this.free())
                return localization.blocking.create.csvfree;
            else
                return localization.blocking.create.csvblock;
        },
        blockBranding: function() {
            return this.blockAny() && !this.teamPlan();
        },
        blockBrandingMessage: function() {
            if(this.free()) 
                return localization.blocking.branding.free;
            else if(this.overdue()) 
                return localization.blocking.branding.overdue;
            else
                return localization.blocking.branding.notTeam;
        },
        blockCompanyMessage: function() {
            if(this.free())
                return localization.blocking.branding.companyfree;
            else if(this.overdue())
                return localization.blocking.branding.companyoverdue;
            else
                return localization.blocking.branding.companyNotTeam;
        },
        blockUsers: function() {
            return this.blockAny() 
                && (!this.teamPlan()
                    || this.usersUsed() >= this.usersPaid());
        },
        blockUsersMessage: function() {
            if(this.free())
                return localization.blocking.users.free;
            if(!this.teamPlan())
                return localization.blocking.users.notTeam;
            if(this.overdue())
                return localization.blocking.users.overdue;
            else if(this.usersUsed() >= this.usersPaid())
                return localization.blocking.users.noneLeft;
            else 
                return "This should not be displayed.";
        },
        usersUsed: function() {
            return this.get('usersused') || 0;
        },
        usersPaid: function() {
            return this.get('userspaid') || 0;
        },
        modalTitle: function() {
            if(this.free())
                return localization.blocking.title.free;
            else if(this.overdue())
                return localization.blocking.title.update;
            else
                return localization.blocking.title.upgrade;
        },
        buttonText: function() {
            if(this.free())
                return localization.blocking.button.purchase;
            else if(this.overdue())
                return localization.blocking.button.doublecheck;
            else 
                return localization.blocking.button.upgrade;
        }
    });

/*
    $.ajax("/blockinginfo", {dataType: 'json',
                             timeout: 3000,
                             success: function(data) {
                                 data.block = true;
                                 window.BlockingInfo = new window.BlockingInfoModel(data);
                             },
                             error: function() {
                                 window.BlockingInfo = new window.BlockingInfoModel({});
                             }});
*/
    window.blocking = {show: function(text) {
        
        Confirmation.popup({
            title: BlockingInfo.modalTitle(),
            content: $("<p />").html(text),
            color: "Green",
            acceptText: BlockingInfo.buttonText(),
            onAccept: function() {
                // just send them to the payments page!
                window.location = "/payments/dashboard";
                return false;
            }
        });

    }};

})(window);
