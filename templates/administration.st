###
adminsmain=
<div class="mainContainer" >
<div class="body-container">
<div class='blocking-box'></div>
<div class='admin'></div>
<script>
 require(['jquery', 'legacy_code'], function(jQuery) {
    jQuery(".admin").append(new Admin({isAdmin: $if(admin)$true$else$false$endif$}).el());
 });
</script>
</div>
</div>
###
adminuser=
<div class="mainContainer" >
<div class="body-container">
<div class='blocking-box'></div>
<div class='admin'></div>
<script>
 require(['jquery', 'legacy_code'], function(jQuery) {
    jQuery(".admin").append(new UserAdmin({userid: "$userid$"}).el());
 });
</script>
</div>
</div>
####
admincompany=
<div class="mainContainer" >
<div class="body-container">
<div class='blocking-box'></div>
<div class='admin'></div>
<script>
 require(['jquery', 'legacy_code'], function(jQuery) {
    jQuery(".admin").append(new CompanyAdmin({companyid: "$companyid$"}).el());
 });
</script>
</div>
</div>
####
domainbranding=
<div class="mainContainer" >
<div class="body-container">
<div class='blocking-box'></div>
<div class='admin'></div>
<script>
 require(['jquery', 'legacy_code'], function(jQuery) {
    jQuery(".admin").append(new AdminBrandedDomain({bdid: "$bdid$"}).el());
 });
</script>
</div>
</div>
###
daveDocument=
<div class="mainContainer" >
  <div class="body-container">
$daveBody:noescape()$

<div style="border:10px solid red;padding:10px">
<center> <h1>This option is extreme. Use this only if really needed </h1></center>
<BR/>
<BR/>
<BR/>
$if(couldBeclosed)$
  <form method="post" action="/dave/reseal/$id$">
    <input type="submit" value="Reseal"> </input>
  </form>
$elseif(closed)$
  <form method="post" action="/dave/reseal/$id$">
    <input type="submit" value="Reseal"> </input>
  </form>
$endif$
<BR/>
</div>
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
