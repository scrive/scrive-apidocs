
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

var dragDropObj = new DHTMLgoodies_dragDrop();

function signatoryadd()
{
    var signatorylist = document.getElementById( "signatorylist" );
    var li = document.createElement('li');
    // FIXME: synchronize this with the Haskell version inside
    li.innerHTML = "<input name='signatoryname' class='signatoryname' type='text'><br>" +
        "<input name='signatoryemail' class='signatoryemail' type='text'><br>" +
        "<div class='dragableBox'>SIGNATURE</div>" +
        "<a onclick='signatoryremove(this)' href='#'>Remove</a>";
    signatorylist.appendChild(li);

    var db = $(li).find(".dragableBox");
    db.each( function(idx,elem) { dragDropObj.addSourceNode(elem,true); });
    var nameinput = $(li).find("input.signatoryname");

    // var emailinput = $(li).find("input.signatoryemail");
    nameinput.bind( "change keyup", function() { db.html($(this).val()); } );
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

function dropItems(sourceObj,targetObj,x,y)
{
    alert(sourceObj + targetObj);
}
var elems = document.getElementsByClassName("dragableBox");
for (var i = 0; elems[i] != null; i++) {
    dragDropObj.addSourceNode(elems[i],true);
}

dragDropObj.addTarget('dropBox','dropItems');
dragDropObj.init();
