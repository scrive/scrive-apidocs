#####
evidenceOfIntent=
<html>

<head>

    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />

    <title>Scrive Evidence of Intent Log</title>

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
<h1>Contract</h1>
<h2>Evidence of Intent Log</h2>
<p>The following screen shots show the signing environments of
   the respective signing parties. Each time stamp reflects
   the clock of the client, and may be different from the
   server clock. </p>
<p> At any point in time, the signing service maintains a
    screen shot of a <em>reference sign view</em> showing a
   standard document at the moment it is being signed.  The
    reference sign view shot illustrates what the signing
    environment is supposed to look like for clients at the
    moment of signing, which is helpful for cases when
    clients' signing environments were unable to provide
    accurate screen shots.</p>

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
                    <a href="$entry.first.image$">$entry.signatory$ opened the sign view</a>.
                  </td>
                </tr>
                $endif$
                <tr>
                  <td>$entry.signing.time$</td>
                  <td>$entry.ip$</td>
                  <td class="column3">
                    $if(entry.signing.image)$<a href="$entry.signing.image$">$entry.signatory$ signed the document</a>.
                    $else$                                                   $entry.signatory$ signed the document.
                    $endif$
                    Corresponding <a href="$entry.reference.image$">reference sign view</a>, created $entry.reference.time$.
                  </td>
                </tr>
              }$
</table>

</body>
</html>
###
