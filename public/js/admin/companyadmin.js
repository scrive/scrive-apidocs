/* Definition of how the company list should be presented to admins */

(function(window){
window.CompanyAdminListDefinition = function() { return {
    name : "CompaniesTable",
    schema: new Schema({
        url: "/adminonly/companies",
        sorting: new Sorting({ fields: [
            "companyname",
            "companynumber",
            "companyaddress",
            "companyzip",
            "companycity",
            "companycountry"  
        ] }),
        paging: new Paging({}),
        textfiltering: new TextFiltering({ text: "", infotext: "Search companies" }),
        cells : [
            new Cell({name: "Name",   width:"120px", field:"companyname"   , special:"link"}),
            new Cell({name: "Number", width:"100px", field:"companynumber" , special:"link"}),
            new Cell({name: "Address",width:"200px", field:"companyaddress", special:"link"}),
            new Cell({name: "Zip",    width:"50px",  field:"companyzip"    , special:"link"}),
            new Cell({name: "City",   width:"100px", field:"companycity"   , special:"link"}),
            new Cell({name: "Country",width:"100px", field:"companycountry", special:"link"}),
            new Cell({name: "ID",     width:"120px", field:"id"            , special:"link"})
        ]
    })
}};
})(window);
