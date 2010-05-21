
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
    /*
      '<li>' +
      '<label>Full name:</label><br>' + 
      '<input name="signatoryname" type="text" value=""><br>' +
      '<label>Company:</label><br>' +
      '<input name="signatorycompany" type="text" value=""><br>' +
      '<label>Email:</label><br>' +
      '<input name="signatoryemail" type="text" value=""><br>' +
      '<a onclick="signatoryremove(this)" href="#">Remove</a>' +
      '</li>');
    */
    /*
    var li = signatorylist.children("li:last");
    var newid = getUniqueId();
    li.attr("id",newid);
    var cls = "uuu-" + newid;
    li.find(".draggableBox").addClass(cls);
    li.data("draggableBoxClass",cls);
    var nameinput = li.find("input.signatoryname");
    nameinput.bind( "change keyup",
                    function() {
                         var db = $("." + cls);
                         db.html($(this).val());
                         });

    addDraggableOverChildren(li);
    */
    return false;
}

function signatoryremove(node)
{
  var li = $(node).parent();
  var cls = li.data("draggableBoxClass");
  var db = $("." + cls);
  li.fadeOut('slow',function() { $(this).remove(); });
  db.fadeOut('slow',function() { $(this).remove(); });
  return false;
}

addDraggableOverChildren("body");

$("#dropBox").droppable({
        tolerance: "fit",
            drop: function (event,ui) {
            var x = ui.draggable.clone(false);
            $(this).append(x);
            x.css("position","absolute");
            x.offset(ui.offset);
            // to make it not copy the object many times
            x.draggable({scope: "somethingelse"});
            x.append("<input type='hidden' value='1,1' name='position'>");
            return true;
        }
    });
