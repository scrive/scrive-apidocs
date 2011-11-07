####    Pages for users - usual from my account
viewCompanyAccounts =
<script>
  var companyAccountsTable = KontraList.init({
  name : "CompanyAccountsTable",
  schema: new Schema({
  url: "/companyaccounts",
  sorting: new Sorting({ fields: ["firstname",
                                  "lastname",
                                  "position",
                                  "phone",
                                  "email"]}),
  paging: new Paging({}),
  filtering: new Filtering({text: "", infotext: "$_SearchCompanyAccount()$"}),
  cells : [
  new Cell({name: "ID", width:"20px", field:"id", special: "select"}),
  new Cell({name: "$_firstname()$", width:"65px", field:"firstname"}),
  new Cell({name: "$_lastname()$", width:"80px", field:"lastname" }),
  new Cell({name: "$_companyposition()$", width:"50px", field:"position" }),
  new Cell({name: "$_phone()$", width:"105px", field:"phone" }),
  new Cell({name: "$_email()$", width:"100px", field:"email" })
  ]})
  });

  jQuery(function(){
  //uncomment this to add it
  // but it's still not done!
  //jQuery("#tab-viewer").append(companyAccountsTable.view.el);
  });
</script>

<div>
  <div id="tab-viewer">
    <form method="post" action="$currentlink$" class="listForm">
	  <div class="tab-header"></div>
	  <ul class="tabs">
	    <li><a href="/account">$_HeadUserInfo()$</a></li>
        <li><a href="/account/security">$_HeadAccountSettings()$</a></li>
        <li class="active"><a href="/account/companyaccounts" class="listtype">$_CompanyAccounts()$</a></li>
        <li><a href="/account/sharing">$_sharing()$</a></li>
        <li><a href="/account/mailapi">$_HeadMailApi()$</a></li>
	  </ul>
	  <div class="tab-container">
	    <div class="tab-content">
	      <div class="col float-left">
	        <div class="tab-dd float-left">
                  <div class="tab-dd-button">
                    <a href="javascript:;"><p>$_options()$</p></a>
                  </div>
                  <ul class="tab-dd-opts">
                    <li><a href="javascript:;" class="subaccount-add-sub">$_AddCompanyAccount()$</a></li>
                    <li><a href="javascript:;" rel="#dialog-list-delete-confirm" class="listDelete">$_delete()$</a></li>
                  </ul>
	        </div>
                <a class="btn-tiny green float-left" href="#" rel="#dialog-add-company-user" id="addcompanyuserlink">
                  <div class="left"></div>
                  <div class="label"><p>New Account</p></div>
                  <div class="right"></div>
                </a>
	      </div>
	      <div class="col float-right">
	        $searchBox(infotext=_SearchCompanyAccount())$
	      </div>
	      <div class="clearfix"></div>
	    </div>
	    <div class="tab-table">
	      <table>
	        <thead>
		      <tr>
		        <th style="width: 20px;"><input type="checkbox" name="" id="all" /></th>
		        <th style="width: 75px;">
                  $if(params.sorting.fstnameREV)$
                  <a href="$currentlink$&amp;sorting=fstname">$_firstname()$<em> ▲</em></a>
                  $elseif(params.sorting.fstname)$
                  <a href="$currentlink$&amp;sorting=fstnameREV">$_firstname()$<em> ▼</em></a>
                  $else$
                  <a href="$currentlink$&amp;sorting=fstname">$_firstname()$</a>
                  $endif$
                </th>
		        <th style="width: 90px;">
                  $if(params.sorting.sndnameREV)$
                  <a href="$currentlink$&amp;sorting=sndname">$_lastname()$<em> ▲</em></a>
                  $elseif(params.sorting.sndname)$
                  <a href="$currentlink$&amp;sorting=sndnameREV">$_lastname()$<em> ▼</em></a>
                  $else$
                  <a href="$currentlink$&amp;sorting=sndname">$_lastname()$</a>
                  $endif$
                </th>
		        <th style="width: 60px;">
                  $if(params.sorting.companypositionREV)$
                  <a href="$currentlink$&amp;sorting=companyposition">$_companyposition()$<em> ▲</em></a>
                  $elseif(params.sorting.companyposition)$
                  <a href="$currentlink$&amp;sorting=companypositionREV">$_companyposition()$<em> ▼</em></a>
                  $else$
                  <a href="$currentlink$&amp;sorting=companyposition">$_companyposition()$</a>
                  $endif$
                </th>
		        <th style="width: 115px;">
                  $if(params.sorting.phoneREV)$
                  <a href="$currentlink$&amp;sorting=phone">$_phone()$<em> ▲</em></a>
                  $elseif(params.sorting.phone)$
                  <a href="$currentlink$&amp;sorting=phoneREV">$_phone()$<em> ▼</em></a>
                  $else$
                  <a href="$currentlink$&amp;sorting=phone">$_phone()$</a>
                  $endif$
                </th>
		        <th style="width: 110px;">
                  $if(params.sorting.emailREV)$
                  <a href="$currentlink$&amp;sorting=email">$_email()$<em> ▲</em></a>
                  $elseif(params.sorting.email)$
                  <a href="$currentlink$&amp;sorting=emailREV">$_email()$<em> ▼</em></a>
                  $else$
                  <a href="$currentlink$&amp;sorting=email">$_email()$</a>
                  $endif$
                </th>
		        <th style="width: 52px;text-align:center"> <a class="icon small add subaccount-add-sub"> </a></th>
		      </tr>
	        </thead>
	        <tbody id="selectable">
		      $companyaccounts:{companyaccount |
		      <!-- Every other row should have class="odd" (Begin with first row) -->
		      <tr $if(companyaccount.odd)$class="odd"$endif$>
		        <td><input type="checkbox" name="acccheck" value="$companyaccount.id$" class="check" />
                  <span class="listname" style="display:none;">
                    $companyaccount.fstname$ $companyaccount.sndname$ &lt;$companyaccount.email$&gt;
                  </span>
                </td>
		        <td>$companyaccount.fstname$</td>
		        <td>$companyaccount.sndname$</td>
		        <td>$companyaccount.companyposition$</td>
		        <td>$companyaccount.phone$</td>
		        <td>$companyaccount.email$</td>
		        <td class="right">
		          <!-- <em>▼</em> -->
		        </td>
		      </tr>
		      }$
	        </tbody>
	      </table>
	      $pagingFooter()$
	    </div>
      </div>
    </form>
    $modalAddCompanyAccount()$
  </div>
  $listLocalDialogs()$
  <script> var useremail = "$user.email$"; </script>
</div>
####
modalAddCompanyAccount=
  <form method="post" name="form" action="$currentlink$" class="overlay redirectsubmitform" id="dialog-add-company-user" rel=".listForm">
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>New Account</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <div>
            <p>
            Invite a new or existing user to join your company.
            </p>
          </div>
          <input type="hidden" name="add" value="true" />
          <div class="account-body">
            <table>
              <tr>
                  <td>$_firstname()$:</td>
                  <td><input type="text" name="fstname" autocomplete="off" /></td>
              </tr>
              <tr>
                  <td>$_lastname()$:</td>
                  <td><input type="text" name="sndname" autocomplete="off" /></td>
              </tr>
              <tr>
                  <td>$_email()$:</td>
                  <td><input type="email" name="email" autocomplete="off"/></td>
              </tr>
            </table>
          </div>
        </div>
        <input type="submit" class="hidden" />
      </div>
      <div class="modal-footer">
        <a class="cancel close float-left" style="height:40px;">
          <div class="label">$_cancel()$</div>
        </a>
        <a class="btn-small green float-right submit" href="#">
          <div class="left"></div>
          <div class="label"><p>Invite</p></div>
          <div class="right"></div>
        </a>
      </div>
    </div>
  </form>
#####
