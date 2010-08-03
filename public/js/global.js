

function getUniqueId()
{
    var rnd = Math.round(Math.random() * 1000000000);
    while($("#" + rnd).length  >0) {
        rnd = Math.round(Math.random() * 1000000000);
    }
    return rnd;
}

function enableInfoText(where)
{
    if( where==null ) {
        where = $(document);
    }
    var inputs = where.find('input[type="text"][infotext!=""]');
    
    inputs.focus(function() { 
            if( $(this).hasClass("grayed")) {
                $(this).val("");
                $(this).removeClass("grayed");
            }
            });
    inputs.blur(function() {
            if( $(this).val()=="" || $(this).val()==$(this).attr("infotext") ) {
                $(this).addClass("grayed");
                $(this).val($(this).attr("infotext"));
            }
        });
    inputs.blur();
    $("form").submit(function () {
            // this is so wrong on so many different levels!
            $('input[type="text"][infotext!=""].grayed').val("");
        });
}

function disableInfoText(where)
{
    if( where==null ) {
        where = $(document);
    }
    inputs = where.filter('input[type="text"][infotext!=""]');
    inputs.focus();
}

function resizeToWindow()
{
    $("#mainContainer").height( $(this).height() - 130 );
}

$(document).ready( function () {
    $('.flashmsgbox').delay(5000).fadeOut();
    $('.flashmsgbox').click( function() { 
         $(this).fadeOut() 
    });
    $('#all').click(function() {
            var c = $('input:checkbox[name="doccheck"]');
            c.attr("checked", !c.attr("checked"));
    });
    enableInfoText();
    if(typeof(window.documentid)!= "undefined" ) {
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
    }
    $(window).resize(resizeToWindow);
    $(window).resize();
});
