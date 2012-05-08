####
showFileImagesReady= $images:{image| 

<div id="page$image.number$" class="pagediv">
  <div class="pagejpg" style="background-image: url($pageurl$/$image.number$);">
    <input type="hidden" name="width" value="$image.width$"/> 
    <input type="hidden" name="height" value="$image.height$"/>
  </div>
</div> 
}$
####
showFileImagesPending= 
<div class="pagejpga4 pagejpg">
  <img class="waiting" src="/img/wait30trans.gif"/>
</div>
####
showFileImagesError=
<div class="errormsg">
 $_showFileImagesError()$
</div>
###
customfield=
<span class="customfield dragfield inputWrapper">
  <input type="text" class="customfieldbox" autocomplete="off" value="$otherFieldValue$" name="fieldvalue" infotext="$otherFieldName$" />
  <a href="#minus2" class="minus"></a>
  <img src="/img/iconHand.png" class="draghandle" alt="drag" />
  <input type="hidden" value="$otherFieldID$" name="fieldid"/>
  <input type="hidden" value="$otherFieldOwner$" name="fieldsigid"/>
  <input type="hidden" value="$otherFieldName$" name="fieldname"/>
  <span class="type" style="display: none;">$otherFieldOwner$</span>
</span>
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
