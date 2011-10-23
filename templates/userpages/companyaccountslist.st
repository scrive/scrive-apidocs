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
,
  new Cell({name: "adder", width:"40px", field:"isunsaved", special: "adder"})
  ]})
  });
    
  jQuery(function(){
  //uncomment this to add it
  // but it's still not done!
  //jQuery("#tab-viewer").append(companyAccountsTable.view.el);
  });
</script>

<div>
  <form method="post" action="$currentlink$" class="listForm">
    <div class="tab-viewer">
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
	        <div class="tab-dd">
              <div class="tab-dd-button">
		        <a href="javascript:;"><p>$_options()$</p></a>
              </div>
		      <ul class="tab-dd-opts">
		        <li><a href="javascript:;" class="subaccount-add-sub">$_AddCompanyAccount()$</a></li>
		        <li><a href="javascript:;" rel="#dialog-list-delete-confirm" class="listDelete">$_delete()$</a></li>
		      </ul>
	        </div>
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
    </div>
  </form>
  $listLocalDialogs()$
  <script> var useremail = "$user.email$"; </script>
</div>
###
