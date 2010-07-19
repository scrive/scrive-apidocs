function addDraggableOverChildren(elem)
{
    var db = $(elem).find(".draggableBox");

    db.draggable({
     helper: "clone",
     opacity: 0.8,
     revert: "invalid"
    });
}

function signatoryadd()
{
    var signatorylist = $( "#signatorylist" );
    var li = $("#signatory_template").clone();
    signatorylist.append(li);
    enableInfoText(li);
    li.hide();
    li.slideDown("slow");
    return false;
}

function signatoryremove(node)
{
  var li = $(node).parent();
  var cls = li.data("draggableBoxClass");
  var db = $("." + cls);
  li.slideUp('slow',function() { $(this).remove(); });
  db.fadeOut('slow',function() { $(this).remove(); });
  return false;
}

$(document).ready( function () {
    $("#dialog-confirm-signinvite").hide();
    $("#dialog-confirm-sign").hide();
    $("#signinvite").click(function() {
         var button = $(this);
         var mrxs = $("form input[name='signatoryname']");
         var tot = "";
         mrxs.each(function(index) {
                 if( tot!="" ) tot += ", ";
                 tot += $(this).val();
             });
         $("#mrx").text(tot);

         $("#dialog-confirm-signinvite").dialog({
                 resizable: false,
                     height: 340,
                     width: 350,
                     modal: true,
                     buttons: {
                     'Underteckna': function() {
                         var form = $("#form");
                         var name = button.attr("name");
                         form.append("<input type='hidden' name='" + name + "' value='automatic'>");
                         form.submit();
                     },
                         'Avbryt': function() {
                             $(this).dialog('close');
                         }
                 }
             });

         return false;});

    $("#sign").click(function() {
         var button = $(this);
         $("#dialog-confirm-sign").dialog({
                 resizable: false,
                     height: 280,
                     width: 350,
                     modal: true,
                     buttons: {
                     'Underteckna': function() {
                         var form = $("#form");
                         var name = button.attr("name");
                         form.append("<input type='hidden' name='" + name + "' value='automatic'>");
                         form.submit();
                     },
                         'Avbryt': function() {
                             $(this).dialog('close');
                         }
                 }
             });
         
         return false;
    });

 /*
 $("#footerContainer").ajaxStart( function() { alert("Ajax start");});
 $("#footerContainer").ajaxStop( function() { alert("Ajax stop");});
 $("#footerContainer").ajaxSuccess( function() { alert("Ajax success");});
 $("#footerContainer").ajaxError( function() { alert("Ajax error");});
 $("#footerContainer").ajaxSend( function() { alert("Ajax send");});
 $("#footerContainer").ajaxComplete( function() { alert("Ajax complete");});
 */

});

