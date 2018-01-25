####
#### Error page for Nets is always embedded in iframe that will signal main page that it should be reloaded
####
netsError=
<html>
<body>
<script>
  var qs = (function(a) {
      if (a == "") return {};
      var b = {};
      for (var i = 0; i < a.length; ++i)
      {
          var p=a[i].split('=', 2);
          if (p.length == 1)
              b[p[0]] = "";
          else
              b[p[0]] = decodeURIComponent(p[1].replace(/\\+/g, " "));
      }
      return b;
  })(window.location.search.substr(1).split('&'));

 var status = "identify_" + (qs["status"] || "none").toLowerCase();

 var triggerError = function() {
  window.parent.postMessage(status,"*");
  setTimeout(triggerError,100);
 };
 triggerError();
</script>
</body>
</html>
####
#### Page for Nets Sign is always embedded in iframe, so we need to signal the main page.
####
netsSignError=
<html>
<body>
<script>
  var qs = (function(a) {
      if (a == "") return {};
      var b = {};
      for (var i = 0; i < a.length; ++i)
      {
          var p=a[i].split('=', 2);
          if (p.length == 1)
              b[p[0]] = "";
          else
              b[p[0]] = decodeURIComponent(p[1].replace(/\\+/g, " "));
      }
      return b;
  })(window.location.search.substr(1).split('&'));

  var triggerError = function() {
    window.parent.postMessage({type: "error", err: qs["err"]},"*");
    setTimeout(triggerError,100);
  };
  triggerError();
</script>
</body>
</html>
####
#### Page for Nets Sign is always embedded in iframe, so we need to signal the main page.
####
netsSignExit=
<html>
<body>
<script>
  var triggerSigned = function() {
    window.parent.postMessage({type: "signed"},"*");
    setTimeout(triggerSigned,100);
  };
  triggerSigned();
</script>
</body>
</html>
####
#### Page for Nets Sign is always embedded in iframe, so we need to signal the main page.
####
netsSignAbort=
<html>
<body>
<script>
  var triggerAbort = function() {
    window.parent.postMessage({type: "abort"},"*");
    setTimeout(triggerAbort,100);
  };
  triggerAbort();
</script>
</body>
</html>
