#####
evidenceOfIntent=
<html>

<head>

    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />

    <title>Evidence of Intent</title>

    <style>
      body,
      div,
      p,
      em,
      ol,
      ul,
      li,
      table,
      tbody,
      thead,
      tr,
      th,
      td {
        margin: 0;
        padding: 0;
        border: 0;
        outline: 0;
        font-size: 100%;
        vertical-align: baseline;
      /**  border:1px solid red;**/

      }
      body {
        line-height: 1;
        color: #000;
        background: #f8f8f8;
      }


      body {
        margin: 0;
        padding: 0;
        border: 0;
        outline: 0;
        display: block;
        background: #fff;
        padding-top: 100px;
        position: relative;
      }



      tr {
      margin-bottom: 70px;
      }

      body {
        font: 16px/1.5 Helvetica Neue, Arial, Helvetica, sans-serif;
      }

      h1,
      h2,
      h3 {
        display: block;
          letter-spacing: normal;
        padding: 24px 0 0;
        margin: 0px;

      /** border: 1px solid blue;**/
      }
      h1 {
        font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
        font-weight: bold;
        letter-spacing: normal;
        font-size: 3.0em;
        color: #364963;
        letter-spacing: 0px;
        line-height: .7em;
        margin: 0;
        margin-bottom: 20px;
        /**border: 1px solid red;**/
      }
      h2 {
        font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
        color: #364963;
        font-weight: normal;
        font-size: 2.0em;
        line-height: 1.1em;
        margin: 0;
        margin-top: 0.6em;
        margin-bottom: 0.3em;
        /**border: 1px solid green;**/
      }

      h3 {
        font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
        font-weight: normal;
        font-size: 1.5em;
        font-style: italic;
        line-height: 0.8em;
        color: #364963;
        margin: 0;
        margin-bottom: 0.4em;
        /**border: 1px solid red;**/
      }



      p {
        margin-bottom: 1.2em;
        font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
        font-size: 1.0em;
        line-height: 1.3em;
        text-align: justify;
        color: #333333;
        /**border: 1px solid red;**/
      }

      * {
        -webkit-box-sizing: border-box;
        -moz-box-sizing: border-box;
        box-sizing: border-box;
      }

      .ct {
        width: 800px;
        margin: 0 auto;
        zoom: 1;
      }


      table {
      width: 800px;
      margin-bottom: 20px;
      padding: 0;

      }

      table td{
        width:
        font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
        font-weight: normal;
        font-size: .8em;
        line-height: 1.4em;
        text-align: justify;
        color: #333333;
        padding-top: 18px;

      }

      td.top{
        font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
        font-size: 1.1em;
        line-height: 1.4em;
        text-align: justify;
        color: #364963;
        font-style: italic;
        margin: 0px;
      }

    </style>

</head>


<body class="en legal article ct full">
<h1>Evidence of Intent </h1>

<p>
  When a person signs a document using the Scrive e-signing system a screen capture is executed on the person´s screen and saved by the Scrive e-signing system. The purpose of making the screen capture is to collect evidence of the signing environment at the time of signing the document. When sealing the signed document the Scrive e-signing system includes the screen captures from all signing parties into this document.
</p>
<p>
  The screen capture technology used does not function with some older browsers. Therefore, in case the screen capture technology would fail with executing the screen capture, a screen shot with a reference signing view is prepared in the Scrive e-signing system at each production upgrade. Upon sealing the document, the Scrive e-signing system includes the latest reference signing view in this Evidence of Intent (one per party that signed). The reference signing view is included as a reference even if the screen capture succeeded.
</p>
<p>
  Important to note is also that, in the event log below each time stamp reflects the clock of the client, and may be different from the server clock.
</p>

<table>
    <tr>
        <td class="top" style="border-bottom: solid 1px #666; width:170px;"><em>Time</em></td>
        <td class="top" style="border-bottom: solid 1px #666; width:80px;"><em>IP</em></td>
        <td class="top" style="border-bottom: solid 1px #666; width:474px;"><em>Event</em></td>
    </tr>
             $entries:{entry|
                $if(entry.first.time)$
                <tr>
                  <td>$entry.first.time$</td>
                  <td>$entry.ip$</td>
                  <td class="column3">
                    $entry.signatory$ opened the document in the signing view.
                    <a href="$entry.first.image$">Click here</a> to see an attached screen capture of the top of their signing view.
                  </td>
                </tr>
                $endif$
                <tr>
                  <td>$entry.signing.time$</td>
                  <td>$entry.ip$</td>
                  <td class="column3">
                    $entry.signatory$ signed the document.
                    $if(entry.signing.image)$
                      <a href="$entry.signing.image$">
                        Click here
                      </a>
                      to see attached screen capture of their signing view.
                    $else$
                      A screen capture was not done for this event.
                    $endif$
                    <a href="$entry.reference.image$">Click here</a> to see attached screen capture of the reference signing view created at $entry.reference.time$.
                  </td>
                </tr>
              }$
</table>

</body>
</html>
###
