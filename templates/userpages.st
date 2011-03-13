####    Pages for users - usual from my account
viewSubaccounts =
<form method="post" action="$subaccountslink$">
	<div id="tab-viewer">
		<div class="tab-header">

		</div>
		<ul class="tabs">
			<li><a href="/d">Dokument</a></li>
			<li><a href="/t">Mallar</a></li>
			<li><a href="/contacts">Kontakter</a></li>
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
							<th style="width: 100px;"><a href="javascript:;">Förnamn <em>▼</em></a></th>
							<th style="width: 120px;"><a href="javascript:;">Efternamn <em>▼</em></a></th>
							<th style="width: 120px;"><a href="javascript:;">Befattning <em>▼</em></a></th>
							<th style="width: 100px;"><a href="javascript:;">Avdelning <em>▼</em></a></th>
							<th style="width: 100px;"><a href="javascript:;">Telefonnummer <em>▼</em></a></th>
							<th style="width: 100px;"><a href="javascript:;">Mailadress <em>▼</em></a></th>
							<th style="width: 30px;"></th>
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
							<td>$subuser.usvPhoneNr$</td>
							<td>$subuser.usvEmail$</td>
							<td></td>
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
					<a href="#" class="btn-tiny black float-left"><div class="left"></div><div class="label">Bakåt<div class="btn-symbol black arrow-right"></div></div><div class="right"></div></a>
					<a href="#" class="btn-tiny black float-right"><div class="left"></div><div class="label">Nästa<div class="btn-symbol black arrow-left"></div></div><div class="right"></div></a>
					<div class="clearfix"></div>
				</div>
			</div>
    </form>
###
pageAcceptTOS =
    <form method="post" action="/accepttos" id="toscontainer" class="overlay">
      <a class="close"> </a>
      <h2>Vänligen acceptera användarvillkoren</h2>
      $tostext:noescape()$
      <br>
      <input type="checkbox" name="tos" id="tos" />Jag har läst och accepterar SkrivaPå Allmänna Villkor<br>
      <div class="buttonbox">
        <input class="submiter button" type="submit" value="Skicka" />
      </div>
    </form>
####
showUser =
    <div class="accounttable">
      <h1>$fname$ $lname$</h1>
      <div>
        <form action="$linkaccount$" method="post">
          <table>
            <tr>
              <td>Förnamn:</td>
              <td><input type="text" name="fname" autocomplete="off" value="$fname$"></td>
            </tr>
            <tr>
              <td>Efternamn:</td>
              <td><input type="text" name="lname" autocomplete="off" value="$lname$"></td>
            </tr>
            <tr>
            <tr>
              <td>E-post:</td>
              <td>$email$</td>
            </tr>
            <tr><td>Företagsnamn:</td>
              <td><input type="text" name="companyname" value="$companyname$" /></td>
            </tr>
            <tr>
              <td>Position:</td>
              <td><input type="text" name="companyposition" autocomplete="off" value="$companyposition$"></td>
            </tr>
            <tr><td>Organisationsnummer:</td>
              <td><input type="text" name="companynumber" value="$companynumber$" /></td>
            </tr>
            <tr><td>Faktureringsadress:</td>
              <td><input type="text" name="invoiceaddress" value="$invoiceaddress$ " /></td>
            </tr>
          </table>
          <p>Du delar dina dokument med följande användare:</p>
          <ul>
            $viewers:{viewer|<li> $viewer$</li> }$
            <li><input type="text" name="newvieweremail" infotext="Skriv e-post för att lägga till fler" size="28" /></li>
          </ul>
          <input class="button" type="submit" value="Spara ändringar" />
        </form>
      </div>
      <div>
        <form action="$linkaccountpassword$" method="post">
          <table>
            <tr><td>Nuvarande lösenord:</td>
              <td><input type="password" name="oldpassword" autocomplete="off" /></td>
            </tr>
            <tr><td>Nytt lösenord:</td>
              <td><input type="password" name="password" autocomplete="off" /></td>
            </tr>
            <tr><td>Upprepa nytt lösenord:</td>
              <td><input type="password" name="password2" autocomplete="off" /></td>
            </tr>
          </table>
          <input class="button" type="submit" value="Ändra lösenord" />
        </form>
      </div>
      <div>
        <a href="$linksubaccount$">Underkonton</a>
      </div>
    </div>
####
