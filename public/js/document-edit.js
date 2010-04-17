
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
    li.innerHTML = "<input name='signatoryname' type='text'><br>" +
                   "<input name='signatoryemail' type='text'><br>" +
                   "<div class='dragableBox'>SIGNATURE</div>" +
                   "<a onclick='signatoryremove(this)' href='#'>Remove</a>";
    signatorylist.appendChild(li);

    var db = li.getElementsByTagName("div")[0];
    dragDropObj.addSourceNode(db,true);
    var nameinput = li.getElementsByTagName("input")[0];
    var emailinput = li.getElementsByTagName("input")[1];
    nameinput.onchange = function() { db.innerHTML = nameinput.value; }
    nameinput.onkeyup = function() { db.innerHTML = nameinput.value; }
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
