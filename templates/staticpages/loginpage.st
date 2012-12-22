###
loginPage=
<div class='mainContainer big'>
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.mainContainer').append(new Login({referer: "$referer$"}).el());
  });
</script> 
</div>
###
changePasswordPage=
<div class='mainContainer big'>
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.mainContainer').append(new NewPassword({linkchangepassword: "$linkchangepassword$"}).el());
  });
</script>
</div>
###
signupPage=
<div class='mainContainer big'>
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
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
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.mainContainer').append(new AcceptTOS({}).el());
  });
</script>
</div>
####