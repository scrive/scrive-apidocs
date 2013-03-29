#####
htmlevidencelog=
<html>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title>Evidence Log</title>
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

      h4 {
        font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
        font-size: 1.1em;
        line-height: 1em;
        text-align: justify;
        color: #364963;
        margin: 0px;
        font-weight: normal;
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

      ul li {
        font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
        font-size: 1.0em;
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
      width: 100%;
      margin-bottom: 60px;
      padding: 0;

      }

      table td{
        font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
        font-weight: normal;
        font-size: .8em;
        line-height: 1.4em;
        color: #333333;
        padding-top: 18px;

      }

      td.top{
        font-family: "Source Sans Pro", Helvetica Neue, Arial, Helvetica, sans-serif;
        font-size: 1.1em;
        line-height: 1em;
        text-align: justify;
        color: #364963;
        font-style: italic;
        margin: 0px;
      }

      .capitalize-first-letter:first-letter {
         text-transform: uppercase;
      }

    </style>

</head>


<body class="en legal article ct full">
<h1>Evidence Log</h1>
<p>
  This evidence log is a complete collection of all actions and events that occurred during the document transaction, which have been logged by the Scrive e-signing service. The information included in each log event is the following:
</p>
<div style="margin-left: 50px;">
  <ul>
    <li>What occurred</li>
    <li>Who executed the action</li>
    <li>At what time this action or event occurred</li>
    <li>An estimate of clock error with regards to the time of the action or event</li>
    <li>The time at which the previous clock error estimate was collected</li>
  </ul>
</div>
<h4 style="margin-top: 20px; margin-bottom: 10px;">About the clock error estimate</h4>
<p>
  As accurate time can be of crucial importance for some types of transactions, this evidence log includes abundant information about time and estimated time error. The purpose of the time error estimate is to give the holder of this document a tool to be able to evaluate the accuracy of the Scrive e-signing service´s clock at the point of each logged event. We estimate the clock error each hour and measure the estimated error in milliseconds (the Clock Error Sample or CES). The first column in the table tells us the time of the event and the estimated error at the time of the last Clock Error Sample. The second column in the table tells us the time of the latest successful Clock Error Sample (CES).  
</p>
<p>
  Additionally, below this evidence log, are statistics about clock error history over an extended period of time. This information is to serve as a reference of the Scrive e-signing system´s time accuracy over time.  
</p>
<br/>
<table style="border-collapse: separate; border-spacing: 20px 0px; margin: 0 -20px;">
    <tr>
        <td class="top" style="border-bottom: solid 1px #666; width:120px;"><em>Time</em></td>
        <td class="top" style="border-bottom: solid 1px #666; width:100px;"><em>CES</em></td>
        <td class="top" style="border-bottom: solid 1px #666; width:90px;"><em>IP address</em></td>
        <td class="top" style="border-bottom: solid 1px #666; width:160px;"><em>Event</em></td>
    </tr>

    $entries:{entry|
     <tr>
       <td>$entry.time$</td>
       <td>$entry.hce_age$</td>
       <td>$entry.ip$</td>
       <td><div class='capitalize-first-letter'>$entry.text:noescape()$</div></td>
     </tr>
    }$

</table>
<br/>
<br/>
<h4>Clock error history statistics</h4>
<p>
For a time period including the lifetime of the document and at least 24 hours long, Scrive recorded the following statistics for the offset between Scrive system clocks and reference time:
<table>
   <tr><td style="width: 220px;">Maximum offset</td><td>$ce_max$</td></tr>
   <tr><td>Sample mean</td><td>$ce_mean$</td></tr>
   <tr><td>Sample standard deviation</td><td>$ce_std_dev$</td></tr>
   <tr><td>Number of collected offset samples</td><td>$ce_collected$</td></tr>
   <tr><td>Number of failed collection attempts</td><td>$ce_missed$</td></tr>
</table>
</body>
</html>
###
