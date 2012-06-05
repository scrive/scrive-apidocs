####
gtVerificationPage=
<div class="left-column">
           $queryColumn()$
         </div>
         <div class="right-column">
           <div class="mainContainer">
               <div class='verificationBox wide box smaller white'>
                <span class='verificationDescription'>
                   <h1>$_verificationTitle()$</h1>
                   <BR/>
                   <h4>$_verificationDescription()$</h4>
                   <BR/>
                   <BR/>
                <span>
                <span class='verificationContainer'>
                </span>
               </div>
           </div>
         </div>
</div>
<script>
    jQuery('.verificationContainer').append(FileVerifier.init().view.el);
</script>
###
