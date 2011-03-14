###
contactsView=
<form method="post">
	<div id="tab-viewer">
		<div class="tab-header">
			
		</div>
		<ul class="tabs">
			<li><a href="/d">Dokument</a></li>
			<li><a href="/t">Mallar</a></li>
			<li class="active"><a href="/contacts">Kontakter</a></li>
			<li><a href="/account/subaccount">Underkonton</a></li>
		</ul>
		<div class="tab-container">
			<div class="tab-content">
				<div class="col float-left">
					<div class="tab-dd">
						<a href="javascript:;"><p>Alternativ</p></a>
						<ul class="tab-dd-opts">
							<li><a href="javascript:;">Lägg till</a></li>
							<li><a href="javascript:;" class="submit" rel="contacts">Radera</a></li>
						</ul>
					</div>
				</div>
				<div class="col float-right">
					<input type="text" class="tab-search infotext" name="" value="Sök kontakt" />
					<a href="#" class="btn-tiny black float-right"><div class="left"></div><div class="label">Sök</div><div class="right"></div></a>
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
							<th style="width: 160px;"><a href="javascript:;">Företag <em>▼</em></a></th>
							<th style="width: 115px;"><a href="javascript:;">Personnummer <em>▼</em></a></th>
							<th style="width: 110px;"><a href="javascript:;">Mailadress <em>▼</em></a></th>
							<th style="width: 20px;"></th>
							<th style="width: 20px;"></td>
						</tr>
					</thead>
					<tbody id="selectable">
						<!-- Every other row should have class="odd" (Begin with first row) -->
						$contacts:{contact |
						<tr >
							<td><input type="checkbox" name="contactcheck" id="" class="check" /></td>
							<td>$contact.firstName$</div></td>
							<td>$contact.lastName$</td>
							<td class="right">$contact.position$</td>
							<td>$contact.company$</td>
							<td>$contact.ssNr$</td>
							<td>$contact.email$</td>
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
		</div>
	</div>
</form>
###