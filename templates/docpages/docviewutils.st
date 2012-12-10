####
documentsToFixView=
<table>
<tbody>
 $documents:{document | 
<tr>
  <td style="width:300px">
   $document.title$
  </td>
  <td style="width:300px">
   $document.involved$
  </td>
  <td>
   $document.cdate$
  </td>
  <td>
   <form method="POST" action="/adminonly/fix/$document.id$">
        <input type="submit" value="Fix"/>
   </form>
  </td>
</tr>
  }$
</tbody>
</table>
###
