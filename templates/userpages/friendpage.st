####    Pages for friends
viewFriends =
<div>
  <form method="post" action="$currentlink$" class="listForm">
	<div id="tab-viewer">
		<div class="tab-header">
		</div>
		<ul class="tabs">
		   <li><a href="/account">$_HeadUserInfo()$</a></li>
                   <li><a href="/account/security">$_HeadAccountSettings()$</a></li>
                   $if(issubaccounts)$<li><a href="/account/subaccount">$_Subaccounts()$</a></li>$endif$
                   <li class="active"><a href="/account/sharing">$_sharing()$</a></li>
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
                                    <input type="email" name="email" class="tab-input float-left" infotext="Skriv e-post för att lägga till fler" />
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
			<div class="tab-table">
				<table>
					<thead>
						<tr>
							<th style="width: 20px;"><input type="checkbox" name="" id="all" /></th>
							<th style="width: 110px;">
                              $if(params.sorting.emailREV)$
                                <a href="$currentlink$&amp;sorting=email">$_SortEmail()$<em> ▲</em></a>
                               $elseif(params.sorting.email)$
                                <a href="$currentlink$&amp;sorting=emailREV">$_SortEmail()$<em> ▼</em></a>
                               $else$
                                <a href="$currentlink$&amp;sorting=email">$_SortEmail()$</a>
                               $endif$ 
                            </th>
							<th style="width: 52px;text-align:center"></th>
						</tr>
					</thead>
					<tbody id="selectable">
						$friends:{friend |
						<!-- Every other row should have class="odd" (Begin with first row) -->
						<tr $if(friend.odd)$class="odd"$endif$>
                                                        <td><input type="checkbox" name="subcheck" value="$friend.id$" class="check" /></td>
							<td class="listname">$friend.email$</td>
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
</div>
###
