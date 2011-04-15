####
showFileImagesReady= $images:{image| 

<div id="page$image.number$" class="pagediv">
  <div class="pagejpg" style="background-image: url($pageurl$/$image.number$);">
    <input type="hidden" name="width" value="$image.width$"> 
    <input type="hidden" name="height" value="$image.height$">
  </div>
</div> 
}$
####
showFileImagesPending= 
<div class="pagejpga4 pagejpg">
  <img class="waiting" src="/theme/images/wait30trans.gif">
</div>
####
showFileImagesError=
<div class="errormsg">
 Vi beklagar, det var något problem med formateringen på din PDF. Vänligen kontrollera dokumentet och försök igen.
</div>
####
###
signatoryScriptIfRequired=   
$if(addSignatoryScript)$
      <script type="text/javascript" src="/js/signatory.js"></script>
$endif$
####
customfield=
<span class="customfield dragfield inputWrapper">
  <input type="text" class="customfieldbox" autocomplete="off" value="$otherFieldValue$" name="fieldvalue" infotext="$otherFieldName$" />
  <a href="#minus2" class="minus"></a>
  <img src="/theme/images/iconHand.png" class="draghandle" alt="drag" />
  <input type="hidden" value="$otherFieldID$" name="fieldid">
  <input type="hidden" value="$otherFieldOwner$" name="fieldsigid">
  <input type="hidden" value="$otherFieldName$" name="fieldname">
  <span class="type" style="display: none;">$otherFieldOwner$</span>
</span>
####
