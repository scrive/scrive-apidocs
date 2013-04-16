/* Definition of document list seen by admins/sales */
(function(window){
window.DocumentAdminListDefinition = function(admin) { 
    var cells = [
        new Cell({name: "Dates", width:"80px", field: "ctime", special: "rendered",
                    rendering: function(_, _, doc) {
                        return jQuery("<small/>")
                            .append(jQuery("<span/>").text(doc.field("ctime")))
                            .append("<BR/>")
                            .append(jQuery("<span/>").text(doc.field("mtime")));
                    }
        }),
        new Cell({name: "Author", width:"120px", field:"author", special: "rendered",
                    rendering: function(value, idx, doc) {
                        if (value != undefined)
                            return jQuery("<small/>")
                                .append(jQuery("<span/>").text(value.name))
                                .append("<BR/>")
                                .append(jQuery("<span/>").text(value.email))
                                .append("<BR/>")
                                .append(jQuery("<span/>").text(value.company));
                    }
        }),
        new Cell({name: "Title", width:"120px", field:"title",  special: "rendered",
                    rendering: function(value, idx, doc) {
                        return jQuery("<small/>").text(value);
                    }
        }),
        new Cell({name: "Service", width:"40px", field:"service", special: "rendered",
                    rendering: function(value, idx, doc) {
                        if (value == undefined)
                            return jQuery("<small/>").text(value);

                        return jQuery("<small>Scrive</small>");
                    }
        }),
        new Cell({name: "Status", width:"40px", field:"status",  special: "rendered",
                    rendering: function(value, idx, doc) {
                        return jQuery("<small/>").text(value);
                    }
        }),
        new Cell({name: "Type", width:"40px", field:"type",  special: "rendered",
                    rendering: function(value, idx, doc) {
                        return jQuery("<small/>").text(value);
                    }
        }),
        new Cell({name: "Signatories", width:"120px", field:"signs", special: "rendered",
                    rendering: function(value, idx, doc) {
                        if (value == undefined)
                            return ;
                        var res = jQuery("<small/>");
                        // XXX is signatory a word? what's the singular of signatories?
                        _.each(value, function(signatory) {
                            res.append(jQuery("<span/>").text(signatory)).append("<BR/>");
                        });
                        return res;
                    }
        })
    ];

    // Only admins are allowed to access the DAVE pages.
    if (admin.isAdmin()) {
        cells.push(
            new Cell({name: "DAVE", width:"30px", field:"id", special:"rendered",
                        rendering: function(value, idx, doc) {
                            return jQuery("<a class='gotodave'>").attr('href','/dave/document/' + value + '/');
                        }
            })
        );
    }

    var list = {
        name : "Documents Table",
        schema: new Schema({
            url: "/adminonly/documentslist",
            sorting: new Sorting({
                fields: [
                      "ctime"
                    , "mtime"
                    , "id"
                    , "author"
                    , "title"
                    , "service"
                    , "status"
                    , "type"
                    , "signs"
                    ]}),
            paging: new Paging({}),
            textfiltering: new TextFiltering({infotext: "Filter documents"}),
            cells : cells
        })
    };

    return list;
};
})(window);
