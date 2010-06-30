

function getUniqueId()
{
    var rnd = Math.round(Math.random() * 1000000000);
    while($("#" + rnd).length  >0) {
        rnd = Math.round(Math.random() * 1000000000);
    }
    return rnd;
}

$(document).ready( function () {
    $('.flashmsgbox').delay(5000).fadeOut();
    $('.flashmsgbox').click( function() { 
         $(this).fadeOut() 
    });
    $.ajax({ url: "/pagesofdoc/" + documentid,
            success: function(data) {
                $('#documentBox').html(data);
            },
            error: function () {
                var that = this;
                $(document).delay(1000).queue(function() {
                        $(this).dequeue();
                        $.ajax(that);
                    });
            }
        });
});



