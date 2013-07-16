/* Definition of user list seen by sales */
(function(window){
    var DownloadCSVButton = Button.init({
        color: "green",
        size: "tiny",
        text: "Download list as CSV",
        onClick: function() {
	    window.location.href = '/adminonly/paymentsstats.csv';
	},
    });

    window.UserAdminSalesListDefinition = function() {

        return {
	    loadOnInit: false,
            name : "Users Table",
            schema: new Schema({
                url: "/adminonly/userslist",
                sorting: new Sorting({
                    fields: ["username", "email", "company", "phone", "tos"]
                }),
                paging: new Paging({}),
                textfiltering: new TextFiltering({text: "", infotext: "Username, email or company name."}),
                cells : [
                    new Cell({name: "Username", width:"130px", field:"username", special:"link"}),
                    new Cell({name: "Email", width:"130px", field:"email", special:"link"}),
                    new Cell({name: "Company", width:"100px", field:"company", special:"link"}),
                    new Cell({name: "Position", width:"100px", field:"companyposition", special:"link"}),
                    new Cell({name: "Phone", width:"100px", field:"phone", special:"link"}),
                    new Cell({name: "TOS date", width:"100px", field:"tos", special:"link",
                                    rendering: function(time, idx, doc) {
                                            if (time != undefined && time != "")
                                              return $("<small/>").text(new Date(Date.parse(time)).toTimeAbrev());
                                            else return $("<small/>");
                                    }
			    })
                ],
            }),
            headerExtras: function() {
                var buttons = $('<div></div>');
                buttons.append(CreateUserButton.input());
                buttons.append(DownloadCSVButton.input());
                return buttons;
            },
        };
    };
})(window);
