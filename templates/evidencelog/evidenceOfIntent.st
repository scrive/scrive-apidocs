#####
evidenceOfIntent=<!DOCTYPE html>
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />

  <title>Evidence of Intent</title>

  <style type="text/css">
    body, p, em, table, tr, th, td {
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
      padding: 100px 0 0;
      position: relative;
      width: 800px;
      zoom: 1;
    }

    tr {
      margin-bottom: 70px;
    }

    h1 {
      color: #364963;
      display: block;
      font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
      font-size: 3em;
      font-weight: bold;
      letter-spacing: normal;
      line-height: .7em;
      margin: 0 0 20px;
      padding: 24px 0 0;
    }

    p {
      color: #333333;
      font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
      font-size: 1em;
      line-height: 1.3em;
      margin-bottom: 1.2em;
      text-align: justify;
    }

    * {
      -webkit-box-sizing: border-box;
      -moz-box-sizing: border-box;
      box-sizing: border-box;
    }

    table {
      margin-bottom: 20px;
      padding: 0;
      width: 800px;
    }

    td, th {
      color: #333333;
      font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
      font-size: .8em;
      font-weight: normal;
      line-height: 1.4em;
      padding-top: 18px;
      text-align: justify;
    }

    th {
      border-bottom: solid 1px #666666;
      color: #364963;
      font-size: 1.1em;
      font-style: italic;
      margin: 0;
    }

    #event {
      width: 474px;
    }

    #ipaddr {
      width: 80px;
    }

    #time {
      width: 170px;
    }
  </style>
</head>

<body>
  <h1>Evidence of Intent</h1>

  <p>When a person signs a document using Scrive E-sign a screen capture is executed on the person’s screen and saved by Scrive E-sign. The purpose of making the screen capture is to collect evidence of the signing environment at the time of signing the document. When sealing the signed document Scrive E-sign includes the screen captures from all signing parties into this document.</p>

  <p>The screen capture technology used does not function with some older browsers. Therefore, in case the screen capture technology would fail with executing the screen capture, a screen shot with a reference signing view is prepared in Scrive E-sign at each production upgrade. Upon sealing the document, Scrive E-sign includes the latest reference signing view in this Evidence of Intent (one per party that signed). The reference signing view is included as a reference even if the screen capture succeeded.</p>

  <p>Important to note is also that, in the event log below each time stamp reflects the clock of the client, and may be different from the server clock.</p>

  <table>
    <tr>
      <th id="time"><em>Time</em></td>

      <th id="ipaddr"><em>IP</em></td>

      <th id="event"><em>Event</em></td>
    </tr>$entries:{entry|$if(entry.first.time)$

    <tr>
      <td>$entry.first.time$</td>

      <td>$entry.ip$</td>

      <td>$entry.signatory$ opened the document in the signing view. <a href="$entry.first.image$">Click here</a> to see an attached screen capture of the top of their signing view.</td>
    </tr>$endif$

    <tr>
      <td>$entry.signing.time$</td>

      <td>$entry.ip$</td>

      <td>$entry.signatory$ signed the document.$if(entry.signing.image)$ <a href="$entry.signing.image$">Click here</a> to see attached screen capture of their signing view.$else$ A screen capture was not done for this event.$endif$$if(entry.reference.image)$ <a href="$entry.reference.image$">Click here</a> to see attached screen capture of the reference signing view created at $entry.reference.time$.$endif$</td>
    </tr>}$
  </table>
</body>
</html>
###
