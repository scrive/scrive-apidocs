####    Pages for users - usual from my account
viewSubaccounts =
<form method="post" action="$currentlink$">
	<div id="tab-viewer">
		<div class="tab-header">

		</div>
		<ul class="tabs">
			<li><a href="/d">Dokument</a></li>
			<li><a href="/t">Mallar</a></li>
			<!-- <li><a href="/contacts">Kontakter</a></li> -->
			<li class="active"><a href="/account/subaccount">Underkonton</a></li>
		</ul>
		<div class="tab-container">
			<div class="tab-content">
				<div class="col float-left">
					<div class="tab-dd">
						<a href="javascript:;"><p>Alternativ</p></a>
						<ul class="tab-dd-opts">
							<li><a href="javascript:;" id="subaccount-add-sub">Lägg till</a></li>
							<li><a href="javascript:;" class="submit" rel="remove">Radera</a></li>
						</ul>
					</div>
				</div>
				<div class="col float-right">
				  $searchBox(infotext='Sök underkonto')$
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
                                <a href="$currentlink$&sorting=fstname">Förnamn  <em> ▲</em></a>
                               $elseif(params.sorting.fstname)$
                                <a href="$currentlink$&sorting=fstnameREV">Förnamn  <em> ▼</em></a>
                               $else$
                                <a href="$currentlink$&sorting=fstname">Förnamn  </a>
                               $endif$
							<th style="width: 90px;">
                              $if(params.sorting.sndnameREV)$
                                <a href="$currentlink$&sorting=sndname">Efternamn  <em> ▲</em></a>
                               $elseif(params.sorting.sndname)$
                                <a href="$currentlink$&sorting=sndnameREV">Efternamn  <em> ▼</em></a>
                               $else$
                                <a href="$currentlink$&sorting=sndname">Efternamn  </a>
                               $endif$   
                            </th>
							<th style="width: 60px;">
                                 $if(params.sorting.companypositionREV)$
                                <a href="$currentlink$&sorting=companyposition">Befattning  <em> ▲</em></a>
                               $elseif(params.sorting.companyposition)$
                                <a href="$currentlink$&sorting=companypositionREV">Befattning  <em> ▼</em></a>
                               $else$
                                <a href="$currentlink$&sorting=companyposition">Befattning  </a>
                               $endif$ 
                            </th>
							<th style="width: 115px;">
                                $if(params.sorting.phoneREV)$
                                <a href="$currentlink$&sorting=phone">Personnummer  <em> ▲</em></a>
                               $elseif(params.sorting.phone)$
                                <a href="$currentlink$&sorting=phoneREV">Personnummer  <em> ▼</em></a>
                               $else$
                                <a href="$currentlink$&sorting=phone">Personnummer </a>
                               $endif$ 
                            </th>
							<th style="width: 110px;">
                              $if(params.sorting.emailREV)$
                                <a href="$currentlink$&sorting=email">Mailadress <em> ▲</em></a>
                               $elseif(params.sorting.email)$
                                <a href="$currentlink$&sorting=emailREV">Mailadress <em> ▼</em></a>
                               $else$
                                <a href="$currentlink$&sorting=email">Mailadress </a>
                               $endif$ 
                            </th>
							<th style="width: 40px;"></th>
						</tr>
					</thead>
					<tbody id="selectable">
						$subaccounts:{subaccount |
						<!-- Every other row should have class="odd" (Begin with first row) -->
						<tr $if(subaccount.odd)$class="odd"$endif$>
							<td><input type="checkbox" name="subcheck" value="$subaccount.id$" class="check" /></td>
							<td>$subaccount.fstname$</td>
							<td>$subaccount.sndname$</td>
							<td>$subaccount.companyposition$</td>
							<td>$subaccount.phone$</td>
							<td>$subaccount.email$</td>
							<td class="right">
								<!-- <em>▼</em> -->
							</td>
						</tr>
						}$
					</tbody>
				</table>
				$pagingFooter()$
			</div>
    </form>
###