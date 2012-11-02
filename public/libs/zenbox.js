

(function() {
   var zenbox = document.createElement('script');
   zenbox.type = 'text/javascript';
   zenbox.async = true;
   zenbox.src = "//asset0.zendesk.com/external/zenbox/v2.1/zenbox.js";

   zenbox.onload = function() {
       Zenbox.init({
           dropboxID:   "20010532",
           url:         "https://scrive.zendesk.com",
           tabID:       "support",
           tabColor:    "black",
           tabPosition: "Right"
       });
   };

   var zenboxcss = document.createElement('link');
   zenboxcss.rel = 'stylesheet';
   zenboxcss.type = 'text/css';
   zenboxcss.media = "screen, projection";
   zenboxcss.href = "//asset0.zendesk.com/external/zenbox/v2.1/zenbox.css";

   (document.getElementsByTagName('head')[0] ||
    document.getElementsByTagName('body')[0]).appendChild(zenbox);

   (document.getElementsByTagName('head')[0] ||
    document.getElementsByTagName('body')[0]).appendChild(zenboxcss);
})();
