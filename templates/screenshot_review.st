screenshotReviewElem=
<tr>
  <td>
    <img src="$image:noescape()$">
  </td>
  <td>
    $userAgent$
  </td>
  <td>
    $if(first)$
      First
    $else$
      Signing
    $endif$
  </td>
</tr>
########
screenshotReview=
<!DOCTYPE html>
<html>
  <body>
    <table>
      <thead>
        <tr>
          <th>Screenshot</th>
          <th>User-Agent</th>
          <th>Screenshot type</th>
        </tr>
      </thead>
      <tbody>
        $content:noescape()$
      </tbody>
    </table>
  </body>
</html>
######