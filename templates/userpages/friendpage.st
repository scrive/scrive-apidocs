####    Pages for friends
viewFriends =
<script>
  var friendsTable = KontraList.init({
  name : "FriendsTable",
  schema: new Schema({
  url: "/friends",
  sorting: new Sorting({ fields: ["email"]}),
  paging: new Paging({}),
  cells : [
  new Cell({name: "ID", width:"30px", field:"id", special: "select"}),
  new Cell({name: "$_SortEmail()$", width:"125px", field:"email" })
  ]})
  });
    
  jQuery(function(){
  jQuery(".tab-viewer").append(friendsTable.view.el);
  });
</script>


<div>
  <form method="post" action="$currentlink$" class="listForm">
	<div class="tab-viewer">
	  <div class="tab-header">
	  </div>
	  <ul class="tabs">
		<li><a href="/account">$_HeadUserInfo()$</a></li>
        <li><a href="/account/security">$_HeadAccountSettings()$</a></li>
        $if(iscompanyadmin)$<li><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>$endif$
        <li class="active"><a href="/account/sharing">$_sharing()$</a></li>
        <li><a href="/account/mailapi">$_HeadMailApi()$</a></li>
	  </ul>
      <div class="listtype" style="display:none;">$_friends()$</div>
	  <div class="tab-container">
		<div class="tab-content">
		  <div class="col float-left">
			<div class="tab-dd">
              <div class="tab-dd-button">
				<a href="javascript:;"><p>$_options()$</p></a>
              </div>
			  <ul class="tab-dd-opts">
				<li><a href="javascript:;" rel="#dialog-list-delete-confirm" class="listDelete">$_delete()$</a></li>
			  </ul>
			</div>
		  </div>
          <div class="col float-right">
            <div class="float-right">
              <input type="email" name="email" class="tab-input float-left" infotext="$_InviteBox()$" />
              <a class="btn-tiny blue submit float-right" href="#">
                <div class="left"></div>
                <div class="label">
                  <span>$_ok()$</span>
                </div>
                <div class="right"></div>
              </a>
            </div>
          </div>
		  <div class="clearfix"></div>
		</div>

      </div>
    </div>
  </form>
  $listLocalDialogs()$
</div>
###
