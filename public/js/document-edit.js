
function getUniqueId()
{
    var rnd = Math.round(Math.random() * 1000000000);
    while($("#" + rnd).length  >0) {
        rnd = Math.round(Math.random() * 1000000000);
    }
    return rnd;
}

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
 
function makeDocumentAjax()
{
   $.ajax(
                       { url: "/pagesofdoc/" + documentid,
                               success: function(data) {
                               $('#documentBox').html(data);
                           },
                               error: function () {
                               // FIXME: add small delay here
                               //$.delay(1000)makeDocumentAjax();
                               $(document).delay(1000).queue(function() {
                                       //alert('Animation complete.');
                                       $(this).dequeue();
                                       makeDocumentAjax();
                                   });
                           }});
}

$(document).ready( function () {

 $("#dialog-confirm-signinvite").hide();
 $("#dialog-confirm-sign").hide();
 $("#signinvite").click(function() {
         $("#dialog-confirm-signinvite").dialog({
                 resizable: false,
                     height: 340,
                     width: 350,
                     modal: true,
                     buttons: {
                     'Bekräfta': function() {
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
                     height: 140,
                     width: 350,
                     modal: true,
                     buttons: {
                     'Bekräfta': function() {
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
         
         return false;});
 /*
 $("#footerContainer").ajaxStart( function() { alert("Ajax start");});
 $("#footerContainer").ajaxStop( function() { alert("Ajax stop");});
 $("#footerContainer").ajaxSuccess( function() { alert("Ajax success");});
 $("#footerContainer").ajaxError( function() { alert("Ajax error");});
 $("#footerContainer").ajaxSend( function() { alert("Ajax send");});
 $("#footerContainer").ajaxComplete( function() { alert("Ajax complete");});
 */

 //$("#documentBox").load("/pages/" + fileid + "/xml");
 makeDocumentAjax();
    });



