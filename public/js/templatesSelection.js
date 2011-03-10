safeReady(function(){
    $(".selectTemplateButton").click(function(){
        var location = $($(this).attr("rel"));
        var wheretoputdata = $(".waiting4data",location);
        location.show();
        jQuery.get("/templates" ,function(data){
                 wheretoputdata.replaceWith(data);
                
            
        })
    })   
})