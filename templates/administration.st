###
adminsmain=
<div class="mainContainer" >
<div class="body-container">
<div class='blocking-box'></div>
<div class='admin'></div>
<script>
  var fromTemplate = {
    forAdmin: $if(admin)$true$else$false$endif$
  };
</script>
<script src="$cdnbaseurl$/compiled/all/admin-$versioncode$.js" crossorigin="anonymous"></script>
</div>
</div>
###
adminuser=
<div class="mainContainer" >
<div class="body-container">
<div class='blocking-box'></div>
<div class='admin'></div>
<script>
var fromTemplate = {
  userId: "$userid$",
  forAdmin: $if(admin)$true$else$false$endif$
};
</script>
<script src="$cdnbaseurl$/compiled/all/useradmin-$versioncode$.js" crossorigin="anonymous"></script>
</div>
</div>
####
admincompany=
<div class="mainContainer" >
<div class="body-container">
<div class='blocking-box'></div>
<div class='admin'></div>
<script>
var fromTemplate = {
  companyId: "$companyid$",
  forAdmin: $if(admin)$true$else$false$endif$
};
</script>
<script src="$cdnbaseurl$/compiled/all/companyadmin-$versioncode$.js" crossorigin="anonymous"></script>
</div>
</div>
####
daveDocument=
<script>
  var fromTemplate = {
    documentid: "$id$",
    istemplate: $if(istemplate)$true$else$false$endif$
  };
</script>
<script src="$cdnbaseurl$/compiled/all/dave-$versioncode$.js" crossorigin="anonymous"></script>
<div class="mainContainer" >
  <div class="body-container">
$daveBody:noescape()$
$extraDaveBody:noescape()$

<div style="border:10px solid red;padding:10px">
<center> <h1>This option is extreme. Use this only if really needed </h1></center>
<BR/>
<BR/>
<BR/>
$if(couldBeClosed)$
  <form method="post" action="/dave/postpending/$id$">
    <input type="submit" value="Postpending"> </input>
  </form>
$elseif(couldBeResealed)$
  <form method="post" action="/dave/reseal/$id$">
    <input type="submit" value="Reseal"> </input>
  </form>
$endif$
<BR/>
</div>
<div class="dave-transfer"></div>
</div>
</div>
####
daveSignatoryLink=
<div class="mainContainer" >
  <div class="body-container">
$daveBody:noescape()$

<div style="border:10px solid red;padding:10px">
<center> <h1>This option is extreme. Use this only if really needed </h1></center>
This will record the change in the evidence log.
<BR/>
<BR/>
</div>
</div>
</div>
####
