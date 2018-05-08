###
EvidenceLogEvidenceTable=<table id="event-table">
  <tr>
    <th id="time"><em>Time</em></th>
    <th id="ces"><em>CES</em></th>
    <th id="ipaddr" style="width: 10%;"><em>IP address and User-Agent</em></th>
    <th id="event"><em>Event</em></th>
  </tr>
  $entries:{entry|
  <tr>
    <td>$entry.time$</td>
    <td>$entry.ces_time$</td>
    <td>
      $entry.ip$
      <span style="display: block; font-size: small; margin-top: 1em;">
        $entry.ua$
      </span>
    </td>
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
    <td>$entry.signatory$ opened the document in the signing view. <a class="screenshot entry-screenshot" href-length="$entry.first.length$" href="$entry.first.image$" href-backup="$entry.first.image$">Click here</a> to see an attached screen capture of the top of their signing view.</td>
  </tr>
  $endif$
  <tr>
    <td>$entry.signing.time$</td>
    <td>$entry.ip$</td>
    <td>$entry.signatory$ signed the document.$if(entry.signing.image)$ <a class="screenshot signing-screenshot" href-length="$entry.signing.length$" href="$entry.signing.image$" href-backup="$entry.signing.image$">Click here</a> to see attached screen capture of their signing view.$else$ A screen capture was not done for this event.$endif$$if(entry.reference.image)$ <a class="screenshot reference-screenshot" href-length="$entry.reference.length$" href="$entry.reference.image$" href-backup="$entry.reference.image$">Click here</a> to see attached screen capture of the reference signing view created at $entry.reference.time$.$endif$</td>
  </tr>}$
</table>
###
