####
dumpAllEvidenceTexts=
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title>Evidence texts ($lang$)</title>
    <style>
      tr:nth-child(even) { background-color: #f0f0f0; }
    </style>
  </head>
  <body>
    <div class="mainContainer" >
      <div class="body-container">
        <h1>Evidence texts ($lang$)</h1>

        <table>
          <tr><td>Build version:</td><td>$versionID$</td></tr>
          <tr><td>Time:</td><td>$timestamp$</td></tr>
        </table>
        <h2>Author’s view</h2>
        <table>
          $evidences:{evidence|
          $if(evidence.authorview)$<tr><td>$evidence.name$</td><td>$evidence.authorview:noescape()$</td></tr>$endif$}$
        </table>
        <h2>Verification page</h2>
        <table>
          $evidences:{evidence|
          $if(evidence.verificationpage)$<tr><td>$evidence.name$</td><td>$evidence.verificationpage:noescape()$</td></tr>$endif$}$
        </table>
        <h2>Evidence Log</h2>
        <table>
          $evidences:{evidence|
          <tr><td>$evidence.name$</td><td>$evidence.evidencelog:noescape()$</td></tr>}$
        </table>
      </div>
    </div>
  </body>
</html>
####
