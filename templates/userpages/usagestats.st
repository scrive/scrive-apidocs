####
showUsageStats =
<form action="$linksecurity$" method="post">
  <div class="tab-viewer">
    <div class="tab-header">
    </div>
    <ul class="tabs">
      <li class='float-left'><a href="/account">$_HeadUserInfo()$</a></li>
      <li class='float-left'><a href="/account/security">$_HeadAccountSettings()$</a></li>
        $if(iscompanyadmin)$<li class='float-left'><a href="/account/company">$_CompanySettings()$</a></li>$endif$
      $if(iscompanyadmin)$<li class='float-left'><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>$endif$
      <li class='float-left'><a href="/account/mailapi">$_HeadMailApi()$</a></li>
      <li class="active float-left"><a href="/account/usagestats">$_HeadUsageStats()$</a></li>
       <!-- <li><a href="/oauth/dashboard">API</a></li> -->
      <li class='float-right'/>
    </ul>

<script>
var dayTable = KontraList().init({
    name : "Past Month Table",
    schema: new Schema({
    url: "/account/usagestats/days/json",
    cells : [
        new Cell({name: "$_usageStatsDate()$",   width:"100px", field:"date"}),
  $if(iscompanyadmin)$new Cell({name: "$_statsSender()$", width:"100px", field:"name", special:"expandable"}),$endif$
        new Cell({name: "$_closedDocuments()$",  width:"70px",  field:"closed", tdclass: 'num'}),
        new Cell({name: "$_sentDocuments()$",    width:"70px",  field:"sent", tdclass: 'num'}),
        new Cell({name: "$_closedSignatures()$", width:"70px",  field:"signatures", tdclass: 'num'})
    ]})});

jQuery(function(){
    jQuery("#jsdaytable").append(dayTable.view.el);
});

var monthTable = KontraList().init({
    name : "Past 6 Month Table",
    schema: new Schema({
    url: "/account/usagestats/months/json",
    cells : [
        new Cell({name: "$_usageStatsMonth()$",   width:"100px", field:"date"}),
  $if(iscompanyadmin)$new Cell({name: "$_statsSender()$", width:"100px", field:"name", special:"expandable"}),$endif$
        new Cell({name: "$_closedDocuments()$",  width:"70px",  field:"closed", tdclass: 'num'}),
        new Cell({name: "$_sentDocuments()$",    width:"70px",  field:"sent", tdclass: 'num'}),
        new Cell({name: "$_closedSignatures()$", width:"70px",  field:"signatures", tdclass: 'num'})
    ]})});

jQuery(function(){
    jQuery("#jsmonthtable").append(monthTable.view.el);
});

</script>
    <div class="tab-container">
      <div class="tab-content account usagestats">

        <h2>$_last30Days()$</h2>

        <div id="jsdaytable"> </div>
        <h2>$_last6Months()$</h2>

        <div id="jsmonthtable"> </div>

        <div class="clearfix"></div>

      </div>
    </div>
  </div>
</form>
####
statsOrgTotal=$_statsOrgTotal()$
###
