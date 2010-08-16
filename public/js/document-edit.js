function addDraggableOverChildren(elem)
{
    var db = $(elem).find(".draggableBox");

    db.draggable({
     helper: "clone",
     opacity: 0.8,
     revert: "invalid"
    });
}

$(document).ready( function () {

 /*
 $("#footerContainer").ajaxStart( function() { alert("Ajax start");});
 $("#footerContainer").ajaxStop( function() { alert("Ajax stop");});
 $("#footerContainer").ajaxSuccess( function() { alert("Ajax success");});
 $("#footerContainer").ajaxError( function() { alert("Ajax error");});
 $("#footerContainer").ajaxSend( function() { alert("Ajax send");});
 $("#footerContainer").ajaxComplete( function() { alert("Ajax complete");});
 */

});

