/* Definition of user list seen by admins */
(function(window){
window.UserAdminListDefinition = function() { return {
    name : "Admin Users Table",
    schema: new Schema({
        url: "/adminonly/userslist",
        sorting: new Sorting({
            fields: [
                "username",
                "email",
                "company",
                "uploaded_docs",
                "signed_docs",
                "signed_1m",
                "signed_2m",
                "signed_3m",
                "signed_6m",
                "signed_12m"
            ]
        }),
        paging: new Paging({}),
        textfiltering: new TextFiltering({text: "", infotext: "Username, email or company name."}),
        cells : [
            new Cell({name: "Username"     , width:"100px", field:"username", special:"link"}),
            new Cell({name: "Email"        , width:"140px", field:"email", special:"link"}),
            new Cell({name: "Company"      , width:"120px", field:"company", special:"link"}),
            new Cell({name: "Viral Invites", width:"40px",  field:"viral_invites", special:"bool"}),
            new Cell({name: "Admin Invites", width:"40px",  field:"admin_invites", special:"bool"})
        ]
    })
};
};
})(window);
