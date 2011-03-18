####    Pages for users - usual from my account
viewSubaccounts =
<form method="post" action="$subaccountslink$">
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
							<li><a href="javascript:;" class="submit" rel="archive">Radera</a></li>
						</ul>
					</div>
				</div>
				<div class="col float-right">
					<input type="text" class="tab-search infotext" name="searchValue" value="Sök underkonto" />
					<a href="#" class="btn-tiny black float-right submit" rel="search"><div class="left"></div><div class="label">Sök</div><div class="right"></div></a>
				</div>
				<div class="clearfix"></div>
			</div>
			<div class="tab-table">
				<table>
					<thead>
						<tr>
							<th style="width: 20px;"><input type="checkbox" name="" id="all" /></th>
							<th style="width: 75px;"><a href="javascript:;">Förnamn <em>▼</em></a></th>
							<th style="width: 90px;"><a href="javascript:;">Efternamn <em>▼</em></a></th>
							<th style="width: 60px; text-align: right"><a href="javascript:;">Befattning <em>▼</em></a></th>
							<th style="width: 160px;"><a href="javascript:;">Avdelning <em>▼</em></a></th>
							<th style="width: 115px;"><a href="javascript:;">Personnummer <em>▼</em></a></th>
							<th style="width: 110px;"><a href="javascript:;">Mailadress <em>▼</em></a></th>
							<th style="width: 20px;"></th>
							<th style="width: 20px;"></th>
						</tr>
					</thead>
					<tbody id="selectable">
						$subusers:{subuser |
						<!-- Every other row should have class="odd" (Begin with first row) -->
						<tr $if(subusers.oddRow)$class="odd"$endif$>
							<td><input type="checkbox" name="subcheck" value="$subuser.usvId$" class="check" /></td>
							<td>$subuser.usvFirstName$</td>
							<td>$subuser.usvLastName$</td>
							<td>$subuser.usvPosition$</td>
							<td>$subuser.usvDept$</td>
							<td>$subuser.usvSsNr$</td>
							<td>$subuser.usvEmail$</td>
							<td></td>
							<td class="right">
								<em>▼</em>
							</td>
						</tr>
						}$
					</tbody>
				</table>
				<div class="table-paginate">
					<div class="pages">
						<strong>1-20 av 123</strong>
						<<
						<
						1
						<a href="#">2</a>
						<a href="#">3</a>
						<a href="#">4</a>
						<a href="#">5</a>
						<a href="#">6</a>
						<a href="#">7</a>
						...
						<a href="#">31</a>
						<a href="#">></a>
						<a href="#">>></a>
					</div>
					<a href="#" class="btn-tiny black float-left">
						<div class="left"></div>
						<div class="label">
							<div class="btn-symbol black arrow-right"></div>
							<p>Bakåt</p>
						</div>
						<div class="right"></div>
					</a>
					<a href="#" class="btn-tiny black float-right">
						<div class="left"></div>
						<div class="label">
							<p>Nästa</p>
							<div class="btn-symbol black arrow-left"></div>
						</div>
						<div class="right"></div>
					</a>
					<div class="clearfix"></div>
				</div>
			</div>
    </form>
###