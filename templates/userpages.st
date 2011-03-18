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
pageAcceptTOS =
    <form method="post" action="/accepttos" id="toscontainer" class="overlay">
    <div class="modal-container">
        <div class="modal-header">
            <div class="modal-icon message"></div>
            <div class="modal-title">
                <h2>Vänligen acceptera användarvillkoren</h2>
            </div>
            <a class="modal-close close"> </a>
        </div>
        <div class="modal-body">
            <div class="modal-content">     
                $tostext:noescape()$
            </div>
        </div>
        <div class="modal-footer">
            <div style="float:left"><input type="checkbox" name="tos" id="tos" />Jag har läst och accepterar SkrivaPå Allmänna Villkor<br> </div>
            <a href="#" class="btn-small green float-right submit submiter" id="editing-invite-text-finished">
                <div class="left"></div>
                <div class="label">
                    <p>Skicka</p>
                    <div class="btn-symbol green arrow-left"></div>
                </div>
                <div class="right"></div>
            </a>
        </div>
    </div>
    </form>
####
showUser =
<form action="$linkaccount$" method="post">
<div id="tab-viewer">
   <div class="tab-header">
   </div>
   <ul class="tabs">
       <li class="active">Konto</li>
   </ul>
   <div class="tab-container">
       <div class="tab-content account">
           <div class="col">
               <div class="account-header">
                   <h2>$fname$ $lname$</h2>
                   <a href="javascript:;">Ladda upp ny bild...</a>
                   <div class="image">
                       <img src="assets/img/portrait-lukas-d.png" height="68" width="58" />
                   </div>
               </div>
               <div class="account-body">
                   <h2>Personliga detaljer</h2>
                   <table>
                       <tr>
                           <td>Förnamn:</td>
                           <td><input type="text" name="fname" value="$fname$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Efternamn:</td>
                           <td><input type="text" name="lname" value="$lname$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Personnummer</td>
                           <td><input type="text" name="socialsecuritynr" value="$socialsecuritynr$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>E-post:</td>
                           <td><input type="email" name="email" value="$email$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Telennummer:</td>
                           <td><input type="text" name="phonenumber" value="$phonenumber$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Befattning</td>
                           <td><input type="text" name="companyposition" value="$companyposition$" autocomplete="off" /></td>
                       </tr>
                   </table>
               </div>
               <div class="account-body">
                   <h2>Byt lösenord</h2>
                   <table>
                       <tr>
                           <td>Nuvarande lösen.:</td>
                           <td><input type="password" name="oldpassword" /></td>
                       <tr>
                           <td>Nytt lösenord:</td>
                           <td><input type="password" name="password" /></td>
                       </tr>
                       <tr>
                           <td>Upprepa nytt lösen.:</td>
                           <td><input type="password" name="password2" /></td>
                       </tr>
                   </table>
               </div>
           </div>
           <div class="col">
               <div class="account-header">
                   <h2>Eventbook AB</h2>
                   <a href="javascript:;">Ladda upp ny logotyp...</a>
                   <div class="image">
                       <img src="assets/img/portrait-lukas-d.png" height="68" width="58" />
                   </div>
               </div>
               <div class="account-body">
                   <h2>Företagsdetaljer</h2>
                   <table>
                       <tr>
                           <td>Företag:</td>
                           <td><input type="text" name="companyname" value="$companyname$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Org. nr:</td>
                           <td><input type="text" name="companynumber" value="$companynumber$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Adress:</td>
                           <td><input type="text" name="invoiceaddress" value="$invoiceaddress$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Postnummer:</td>
                           <td><input type="email" name="zipcode" value="$zipcode$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Postort:</td>
                           <td><input type="text" name="city" value="$city$" autocomplete="off" /></td>
                       </tr>
                       <tr>
                           <td>Land:</td>
                           <td><input type="text" name="country" value="$country$" autocomplete="off" /></td>
                       </tr>
                   </table>
               </div>
           </div>
           <div class="clearfix"></div>
           <div class="account-footer">
               <a href="#" class="btn-small green submit">
                   <div class="left"></div>
                   <div class="label">
                       <p>Spara</p>
                       <div class="btn-symbol arrow-right"></div>
                   </div>
                   <div class="right"></div>
               </a>
           </div>
       </div>
   </div>
</div>
####
