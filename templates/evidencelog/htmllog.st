#####
htmlevidencelog=<!DOCTYPE html>
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />

  <title>Evidence Log</title>
  <style type="text/css">
    body, div, p, em, ul, li, table, tr, th, td {
      border: 0;
      font-size: 100%;
      margin: 0;
      outline: 0;
      padding: 0;
      vertical-align: baseline;
    }

     body {
      background: #ffffff;
      border: 0;
      display: block;
      font: 16px/1.5 Helvetica Neue, Arial, Helvetica, sans-serif;
      line-height: 1;
      margin: 0 auto;
      outline: 0;
      padding: 100px 0 0 0;
      position: relative;
      width: 800px;
      zoom: 1;
    }

    tr {
      margin-bottom: 70px;
    }

    h1, h4 {
      color: #364963;    
      font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
    }

    h1 {
      font-size: 3em;
      font-weight: bold;
      letter-spacing: normal;
      line-height: .7em;
      margin: 0 0 20px;
    }

    h4 {
      font-size: 1.1em;
      font-weight: normal;
      line-height: 1em;
      margin: 0 0 10px;
      text-align: justify;
    }

    p {
      color: #333333;
      font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
      font-size: 1em;
      line-height: 1.3em;
      margin-bottom: 1.2em;
      text-align: justify;
    }

    ul {
      margin-bottom: 20px;
      margin-left: 50px;
    }

    li {
      font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
      font-size: 1em;
      line-height: 1.5;
    }

    * {
      -webkit-box-sizing: border-box;
      -moz-box-sizing: border-box;
      box-sizing: border-box;
    }

    table {
      padding: 0;
      width: 100%;
    }

    td, th {
      color: #333333;
      font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
      font-size: .8em;
      font-weight: normal;
      line-height: 1.4em;
      padding-top: 18px;
    }

    th {
      border-bottom: solid 1px #666666;
      color: #364963;
      font-size: 1.1em;
      font-style: italic;
      font-weight: normal;
      line-height: 1em;
      margin: 0;
      text-align: justify;
    }

    .capitalize-first-letter:first-letter {
       text-transform: uppercase;
    }

    .event-content {
      max-width: 230px;
      word-wrap: break-word;
    }

    .time-stats-column {
      width: 220px;
    }

    #event-table {
      border-collapse: separate; 
      border-spacing: 20px 0px;
      margin: 0 -10px;
    }

    #time {
      width: 200px;
    }

    #ces, #event {
      width: 160px;
    }

    #ipaddr {
      width: 110px;
    }
  </style>
</head>

<body>
  <h1>Evidence Log</h1>

  <p>This evidence log is a complete collection of all actions and events that occurred during the document transaction, which have been logged by Scrive E-sign. The information included in each log event is the following:</p>

  <div>
    <ul>
      <li>What occurred</li>

      <li>Who executed the action</li>

      <li>At what time this action or event occurred</li>

      <li>An estimate of clock error with regards to the time of the action or event</li>

      <li>The time at which the previous clock error estimate was collected</li>
    </ul>
  </div>

  <h4>About the clock error estimate</h4>

  <p>As accurate time can be of crucial importance for some types of transactions, this
  evidence log includes abundant information about time and estimated time error. The
  purpose of the time error estimate is to give the holder of this document a tool to be
  able to evaluate the accuracy of Scrive E-sign’s clock at the point of each logged
  event. We estimate the clock error each hour and measure the estimated error in
  milliseconds (the Clock Error Sample or CES). The first column in the table tells us
  the time of the event and the estimated error at the time of the last Clock Error
  Sample. The second column in the table tells us the time of the latest successful Clock
  Error Sample (CES).</p>

  <p>Additionally, below this evidence log, are statistics about clock error history over
  an extended period of time. This information is to serve as a reference of Scrive
  E-sign’s time accuracy over time.</p><br>

  <table id="event-table">
    <tr>
      <th id="time"><em>Time</em></td>

      <th id="ces"><em>CES</em></td>

      <th id="ipaddr"><em>IP address</em></td>

      <th id="event"><em>Event</em></td>
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
    </tr>}$
  </table><br>
  <br>

  <h4>Clock error history statistics</h4>

  <p>For a time period including the lifetime of the document and at least 24 hours long,
  Scrive recorded the following statistics for the offset between Scrive system clocks
  and reference time:</p>

  <table>
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
</body>
</html>
###
