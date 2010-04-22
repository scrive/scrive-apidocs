
function addDraggableOverChildren(elem)
{
    var db = $(elem).find(".dragableBox");

    db.draggable({
     helper: "clone",
     opacity: 0.8,
     revert: "invalid"
    });
}

function signatoryadd()
{
    var signatorylist = $( "#signatorylist" );
    signatorylist.append(
        "<li><input name='signatoryname' class='signatoryname' type='text'><br>" +
        "<input name='signatoryemail' class='signatoryemail' type='text'><br>" +
        "<div class='dragableBox'>SIGNATURE</div>" +
        "<a onclick='signatoryremove(this)' href='#'>Remove</a>" +
        "</li>");
    var li = signatorylist.children("li:last");
    var rnd = Math.round(Math.random() * 10000000);
    var cls = "uuu-" + rnd;
    li.find(".dragableBox").addClass(cls);
    li.data("draggableBoxClass",cls);
    var nameinput = li.find("input.signatoryname");
    nameinput.bind( "change keyup",
                    function() {
                         var db = $("." + cls);
                         db.html($(this).val());
                         });

    addDraggableOverChildren(li);
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
