/* Definition of user list seen by sales */
(function(window){
window.UserAdminSalesListDefinition = function() { 
    return {
        name : "Users Table",
        schema: new Schema({
            url: "/adminonly/userslist",
            sorting: new Sorting({ 
                fields: ["username", "email", "company", "phone", "tos", "signed_docs"]
            }),
            paging: new Paging({}),
            textfiltering: new TextFiltering({text: "", infotext: "Username, email or company name."}),
            cells : [
                new Cell({name: "Username", width:"130px", field:"username", special:"link"}),
                new Cell({name: "Email", width:"130px", field:"email", special:"link"}),
                new Cell({name: "Company", width:"100px", field:"company", special:"link"}),
                new Cell({name: "Position", width:"100px", field:"companyposition", special:"link"}),
                new Cell({name: "Phone", width:"100px", field:"phone", special:"link"}),
                new Cell({name: "TOS date", width:"100px", field:"tos", special:"link"}),
                new Cell({name: "Signed Docs", width:"100px", field:"signed_docs", special:"link"})
            ]
        }),
        headerExtras: CreateUserButton.input()
    }
};
})(window);
