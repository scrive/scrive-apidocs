####
showUsageStats =
<form action="$linksecurity$" method="post">
  <div id="tab-viewer">
    <div class="tab-header">
    </div>
    <ul class="tabs">
      <li><a href="/account">$_HeadUserInfo()$</a></li>
      <li><a href="/account/security">$_HeadAccountSettings()$</a></li>
      $if(iscompanyadmin)$<li><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>$endif$
      <li><a href="/account/sharing">$_sharing()$</a></li>
      <li><a href="/account/mailapi">$_HeadMailApi()$</a></li>
      <li class="active"><a href="/account/usagestats">$_HeadUsageStats()$</a></li>
    </ul>

<script>
var DayTable = KontraList().init({
    name : "Past Month Table",
    schema: new Schema({
    url: "/account/usagestats/days/json",
    cells : [
        new Cell({name: "$_usageStatsDate()$",   width:"100px", field:"date"}),
        new Cell({name: "$_closedDocuments()$",  width:"70px",  field:"closed", class: 'num'}),
        new Cell({name: "$_sentDocuments()$",    width:"70px",  field:"sent", class: 'num'}),
        new Cell({name: "$_closedSignatures()$", width:"70px",  field:"signatures", class: 'num'})
    ]})});

jQuery(function(){
    jQuery("#jsdaytable").append(DayTable.view.el);
});

var MonthTable = KontraList().init({
    name : "Past 6 Month Table",
    schema: new Schema({
    url: "/account/usagestats/months/json",
    cells : [
        new Cell({name: "$_usageStatsDate()$",   width:"100px", field:"date"}),
        new Cell({name: "$_closedDocuments()$",  width:"70px",  field:"closed", class: 'num'}),
        new Cell({name: "$_sentDocuments()$",    width:"70px",  field:"sent", class: 'num'}),
        new Cell({name: "$_closedSignatures()$", width:"70px",  field:"signatures", class: 'num'})
    ]})});

jQuery(function(){
    jQuery("#jsmonthtable").append(MonthTable.view.el);
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
