###
EvidenceLogEvidenceTable=<table id="event-table">
  <tr>
    <th id="time"><em>Time</em></th>
    <th id="ces"><em>CES</em></th>
    <th id="ipaddr"><em>IP address</em></th>
    <th id="event"><em>Event</em></th>
  </tr>
  $entries:{entry|
  <tr>
    <td>$entry.time$</td>
    <td>$entry.ces_time$</td>
    <td>$entry.ip$</td>
    <td>
      <div class="capitalize-first-letter event-content">
        $entry.text:noescape()$
      </div>
    </td>
  </tr>
  }$
</table><br/>
###
EvidenceOfTimeGraph=$graph_image:noescape()$
###
EvidenceOfTimeSamplesTable=<table>
<tr>
  <th>Time collected</th>
  <th>Clock offset</th>
</tr>
$entries:{entry|
  <tr>
    <td>$entry.offset_time$</td>
    <td>$entry.offset$</td>
  </tr>
}$
</table>
###
EvidenceLogClockErrorHistory=<table>
  <tr>
    <td class="time-stats-column">Maximum offset</td>
    <td>$ce_max$</td>
  </tr>
  <tr>
    <td>Sample mean</td>
    <td>$ce_mean$</td>
  </tr>
  <tr>
    <td>Sample standard deviation</td>
    <td>$ce_std_dev$</td>
  </tr>
  <tr>
    <td>Number of collected offset samples</td>
    <td>$ce_collected$</td>
  </tr>
  <tr>
    <td>Number of failed collection attempts</td>
    <td>$ce_missed$</td>
  </tr>
</table>
###
EvidenceOfIntentTable=<table>
  <tr>
    <th id="time"><em>Time</em></th>
    <th id="ipaddr"><em>IP</em></th>
    <th id="event"><em>Event</em></th>
  </tr>
  $entries:{entry|$if(entry.first.time)$
  <tr>
    <td>$entry.first.time$</td>
    <td>$entry.ip$</td>
    <td>$entry.signatory$ opened the document in the signing view. <a href="$entry.first.image$">Click here</a> to see an attached screen capture of the top of their signing view.</td>
  </tr>
  $endif$
  <tr>
    <td>$entry.signing.time$</td>
    <td>$entry.ip$</td>
    <td>$entry.signatory$ signed the document.$if(entry.signing.image)$ <a href="$entry.signing.image$">Click here</a> to see attached screen capture of their signing view.$else$ A screen capture was not done for this event.$endif$$if(entry.reference.image)$ <a href="$entry.reference.image$">Click here</a> to see attached screen capture of the reference signing view created at $entry.reference.time$.$endif$</td>
  </tr>}$
</table>
###
