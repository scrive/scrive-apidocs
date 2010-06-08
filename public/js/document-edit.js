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
                         // FIXME: should have 'final' here but I don't know how to put in
                         // in succesful set of the form
                         document.form['final2'].value = 'something';
                         $("#form").submit();
                     },
                         'Avbryt': function() {
                             $(this).dialog('close');
                         }
                 }
             });

         return false;});

    $("#sign").click(function() {
         $("#dialog-confirm-sign").dialog({
                 resizable: false,
                     height: 280,
                     width: 350,
                     modal: true,
                     buttons: {
                     'Underteckna': function() {
                         // FIXME: should have 'final' here but I don't know how to put in
                         // in succesful set of the form
                         document.form['sign2'].value = 'something';
                         $("#form").submit();
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

    if( issuedone ) {
       $("#dialog-confirm-signinvite-done").dialog({
                 resizable: false,
                     height: 240,
                     width: 350,
                     modal: true,
                     buttons: {
                     'Skapa ett nytt avtal': function() {
                             document.location.href = "/"; 
                         },
                     'Avbryt': function() {
                             $(this).dialog('close');
                         }
                     }
                 });
    }
});

