/* Definition of document list seen by admins/sales */

define(['Backbone', 'legacy_code'], function() {

window.DomainBrandingAdminListDefinition = function() {
    var cells = [
        new Cell({name: "Prefix", width:"80px", field: "url", special: "rendered",
                    rendering: function(_, _, doc) {
                        return jQuery("<span/>").text(doc.field("url"));
                    }
        })
    ];

    var list = {
	loadOnInit: false,
        name : "Documents Table",
        schema: new Schema({
            url: "/adminonly/domainbrandingslist",
            sorting: new Sorting({fields: []}),
            paging: new Paging({}),
            textfiltering: new TextFiltering({infotext: "Filter", disabled : true }),
            cells : cells
        })
    };

    return list;
};

});
