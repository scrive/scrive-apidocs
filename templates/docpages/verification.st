####
gtVerificationPage=
<div class='box smaller white verificationBox'>
  <span class='verificationDescription'>
    <h1>$_verificationTitle()$</h1>
    <BR/>
    <h4>$_verificationDescription()$</h4>
    <BR/>
    <BR/>
  </span>
  <span class='verificationContainer'>  </span>
</div>

<script>
    jQuery('.verificationContainer').append(FileVerifier.init().view.el);
</script>
###
