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
    <div class="tab-container">
      <div class="tab-content account usagestats">

        <h2>$_last30Days()$</h2>
	    <table>
          <thead>
            <td >$_usageStatsDate()$</td>
            <td >$_closedDocuments()$</td>
            <td >$_sentDocuments()$</td>
            <td >$_closedSignatures()$</td>
	      </thead>
          $statisticsbyday:{stat|<tr>
            <td class="date">$stat.date$</td>
            <td class="num">$stat.closed$ </td>
            <td class="num">$stat.sent$ </td>
            <td class="num">$stat.signatures$ </td>
	      </tr>}$
	    </table>
        <h2>$_last6Months()$</h2>
	    <table>
          <thead>
            <td >$_usageStatsMonth()$</td>
            <td >$_closedDocuments()$</td>
            <td >$_sentDocuments()$</td>
            <td >$_closedSignatures()$</td>
	      </thead>
          $statisticsbymonth:{stat|<tr>
            <td class="date">$stat.date$</td>
            <td class="num">$stat.closed$ </td>
            <td class="num">$stat.sent$ </td>
            <td class="num">$stat.signatures$ </td>
	      </tr>}$
	    </table>

        <div class="clearfix"></div>

      </div>
    </div>
  </div>
</form>
####
