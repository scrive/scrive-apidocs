/* Definition of document list seen by admins/sales */

define(['Backbone', 'legacy_code'], function() {

window.BrandedDomainAdminListDefinition = function() {
    var cells = [
        new Cell({name: "URL", width:"80px", field: "url", special: "rendered",
                    rendering: function(_, _, doc) {
                        return jQuery("<span/>").text(doc.field("url"));
                    }
        }),
        new Cell({name: "Contact Email", width:"80px", field: "contact_email", special: "rendered",
                    rendering: function(_, _, doc) {
                        return jQuery("<span/>").text(doc.field("contact_email"));
                    }
        }),
        new Cell({name: "Email Originator", width:"80px", field: "email_originator", special: "rendered",
                    rendering: function(_, _, doc) {
                        return jQuery("<span/>").text(doc.field("email_originator"));
                    }
        }),
        new Cell({name: "SMS Originator", width:"80px", field: "sms_originator", special: "rendered",
                    rendering: function(_, _, doc) {
                        return jQuery("<span/>").text(doc.field("sms_originator"));
                    }
        }),
        new Cell({name: "Logo", width:"80px", field: "url", special: "rendered",
                    rendering: function(_, _, doc) {
                        return jQuery("<img/>").attr("src",doc.field("logolink"));
                    }
        })
    ];

    var list = {
	loadOnInit: false,
        name : "Branded Domains",
        schema: new Schema({
            url: "/adminonly/brandeddomainslist",
            sorting: new Sorting({fields: []}),
            paging: new Paging({}),
            textfiltering: new TextFiltering({infotext: "Filter", disabled : true }),
            cells : cells
        })
    };

    return list;
};

});
