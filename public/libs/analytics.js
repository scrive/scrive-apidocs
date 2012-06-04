var _gaq = _gaq || [];
_gaq.push(['_setAccount', 'UA-6387711-9']);
_gaq.push(['_addOrganic','maps.google','q']);
  
  
    var ref = document.referrer;
    if (ref.search(/(images|www)\.google\.([^\/]+)\/(images|imghp|imgres|imglanding)/) != -1 && ref.search(/prev/) != -1) {
    var regex = new RegExp("google\.([^\/]+)/.*&prev=([^&]+)&");
    var match = regex.exec(ref);

   _gaq.push(['_addOrganic','images.google.'+match[1],'q',true]);
   _gaq.push(['_setReferrerOverride', 'http://images.google.'+match[1]+unescape(match[2])]);
} 
_gaq.push(['_trackPageview']);
_gaq.push(['ei._setAccount', 'UA-6387711-9']); 
_gaq.push(['ei._trackPageview']); 


document.onclick = function(event) {
   event = event || window.event;
   var target = event.target || event.srcElement,
       targetElement = target.tagName.toLowerCase();

   if (targetElement == "a") {
       var href = target.getAttribute("href"),
           urlHost = document.domain.replace(/^www\./i,"");
       var urlPattern = "^(?:https?:)?\/\/(?:(?:www)\.)?" + urlHost + "\/?";
   }

   function eventCheck(href,urlPattern){
       if (href == undefined) return;
       if ((href.match(/^https?\:/i)) && (!href.match(urlPattern))){
           if (href.match(/^.*\.(pdf|jpg|png|gif|zip|mp3|txt|doc|rar|js|py)$/i))
           {
               _gaq.push(["_trackPageview","/mal/nedladdning/" + href]);
           } else {
               _gaq.push(["_trackPageview","/mal/utlank/" + href]);
           }
       } else if (href.match(/^mailto\:/i)) {
           _gaq.push(["_trackPageview","/mal/epost/" + href.substr(7)]);
       } else if(href.match(/^.*\.(pdf|jpg|png|gif|zip|mp3|txt|doc|rar|js|py)$/i)) {
           _gaq.push(["_trackPageview","/mal/nedladdning/" + href]);
       }
   }
};

(function() {
var ga = document.createElement('script'); ga.type =
'text/javascript'; ga.async = true;
ga.src = ('https:' == document.location.protocol ? 'https://ssl' :
'http://www') + '.google-analytics.com/ga.js';
(document.getElementsByTagName('head')[0] ||
document.getElementsByTagName('body')[0]).appendChild(ga);
})();

