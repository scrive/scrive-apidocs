
// make IE work also...
if (document.getElementsByClassName == undefined) {
    document.getElementsByClassName = function(className)
	{
            var hasClassName = new RegExp("(?:^|\\s)" + className + "(?:$|\\s)");
            var allElements = document.getElementsByTagName("*");
            var results = [];

            var element;
            for (var i = 0; (element = allElements[i]) != null; i++) {
                var elementClass = element.className;
                if (elementClass && elementClass.indexOf(className) != -1 && hasClassName.test(elementClass))
                    results.push(element);
            }

            return results;
	}
}

function addDraggableOverChildren(elem)
{
    var db = $(elem).find(".dragableBox");

    db.draggable({
     helper: "clone",
     opacity: 0.5,
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
    alert(cls);
    li.find(".dragableBox").addClass(cls);
    var nameinput = li.find("input.signatoryname");
    nameinput.bind( "change keyup",
                    function() {
                         var db = $("." + cls);
                         db.html($(this).val());
                         });

    addDraggableOverChildren(li);
}

function getNodeIndex(node)
{
    var prev = null;
    var index = 0;
    while(true) {
        prev = node.previousElementSibling;
        if( prev==null ) break;
        index = index + 1;
        node = prev;
    }
    return index;
}

function signatoryremove(node)
{
    var li = node.parentNode;
    var index = getNodeIndex(li);
    li.parentNode.removeChild(li);

    var signatureBox = document.getElementById("signatureBox");
    var sgn = signatureBox.children[index];
    sgn.parent.removeChild(sgn);
}

addDraggableOverChildren("body");

$("#dropBox").droppable({
  tolerance: "fit",
  drop: function (event,ui) {
     var x = ui.draggable.clone(false);
     $(this).append(x);
     x.css("position","absolute");
     x.offset(ui.offset);
     return true;
     }
});
