########
screenshotReview=<!DOCTYPE html>
<html>
  <body>
    <table>
      <thead>
      </thead>
      <tbody>
        $if(firstimage)$
        <tr>
          <td>
            <img src="$firstimage:noescape()$"></img>
          </td>
          <td>
            $userAgent$
          </td>
          <td>
            First for signatory $signatoryid$
          </td>
        </tr>
        $endif$

        $if(signingimage)$
        <tr>
          <td>
            <img src="$signingimage:noescape()$"></img>
          </td>
          <td>
            $userAgent$
          </td>
          <td>
            Signing for signatory $signatoryid$
          </td>
        </tr>
        $endif$
      </tbody>
    </table>
  </body>
</html>
######