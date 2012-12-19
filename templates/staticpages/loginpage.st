###
loginPage=
<div class='mainContainer big'>
<img width="1920" height="800" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.mainContainer').append(new Login({referer: "$referer$"}).el());
  });
</script> 
</div>
###
changePasswordPage=
<div class='mainContainer big'>
<img width="1920" height="800" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.mainContainer').append(new NewPassword({linkchangepassword: "$linkchangepassword$"}).el());
  });
</script>
</div>
###
signupPage=
<div class='mainContainer big'>
<img width="1920" height="800" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.mainContainer').append(new Signup({email: "$email:noescape()$"}).el());
  });
</script>
</div>
###
accountSetupPage=
<div class='mainContainer big'>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.mainContainer').append( new AccountSetup({
                                          fstname: "$fstname$",
                                          sndname: "$sndname$"})
                                      .el());
  });
</script>
</div>
###
pageAcceptTOS =
<div class='mainContainer big'>
<img width="1920" height="800" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.mainContainer').append(new AcceptTOS({}).el());
  });
</script>
</div>
####
whatever= 
</div>

    <form method="post" action="/accepttos" id="toscontainer" class="overlay">
    <div class="modal-container">
        <div class="modal-header">
            <span class="modal-icon message"></span>
            <span class="modal-title">
                <h2>$_PleaseAcceptTOS()$</h2>
            </span>
            <a class="modal-close close"> </a>
        </div>
        <div class="modal-body">
            <div class="modal-content">
                $termsPageContents()$
            </div>
        </div>
        <div class="modal-footer">
            <div style="float:left"><input type="checkbox" name="tos" id="tos" /> <label for="tos">$_ReadUnderstoodTOS()$</label><br/> </div>
            <a href="#" class="btn-small green float-right submit" id="editing-invite-text-finished">
                <div class="left"></div>
                <div class="label">
                    <p>$_send()$</p>
                    <div class="btn-symbol green arrow-left"></div>
                </div>
                <div class="right"></div>
            </a>
        </div>
    </div>
    </form>

###
