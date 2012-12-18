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
    jQuery('.mainContainer').append(new Signup({email: "$email:noescape()$", signuplink: "$signuplink$"}).el());
  });
</script>
</div>
###
accountSetupPage=
<div class='mainContainer big'>
<img width="1920" height="800" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    new AccountSetup({signuplink: "$signuplink$",
                      fstname: "$fstname$",
                      sndname: "$sndname$"});
  });
</script>
</div>
###
