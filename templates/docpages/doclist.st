####
mainpage=
         $upload:noescape()$
         <BR/>
         <div class="box">
         $doclist:noescape()$
         </div>

####

####		
pageDocumentList= 
<form method="post" action="">
	<input type="hidden" name="archive" value="1" />
	<div id="tab-viewer">
		<div class="tab-header">

		</div>
		<ul class="tabs">
			<li class="active"><a href="/d">Dokument</a></li>
			<li><a href="/t">Mallar</a></li>
			<li><a href="/contacts">Kontakter</a></li>
			<li><a href="/account/subaccount">Underkonton</a></li>
		</ul>
		<div class="tab-container">
			<div class="tab-content">
				<div class="col float-left">
					<div class="tab-dd">
						<a href="javascript:;"><p>Alternativ</p></a>
						<ul class="tab-dd-opts">
							<li><a href="javascript:;" class="submit" rel="archive">Radera</a></li>
						</ul>
					</div>
				</div>
				<div class="col float-right">
					<input type="text" class="tab-search infotext" name="searchValue" value="Sök dokument" />
					<a href="#" class="btn-tiny black float-right submit" rel="search"><div class="left"></div><div class="label">Sök</div><div class="right"></div></a>
				</div>
				<div class="clearfix"></div>
			</div>
			<div class="tab-table">
				<table>
					<thead>
						<tr>
							<th style="width: px;"><input type="checkbox" name="" id="all" /></th>
							<th style="width: px;"><a href="javascript:;">Status <em>▼</em></a></th>
							<th style="width: px;"><a href="javascript:;">Senaste händelse <em>▼</em></a></th>
							<th style="width: px;"><a href="javascript:;">Parter <em>▼</em></a></th>
							<th style="width: px;"><a href="javascript:;">Dokument <em>▼</em></a></th>
						</tr>
					</thead>
					
    				<tbody id="selectable">
				      $documents:{document |
						<!-- Every other row should have class="odd" (Begin with first row) -->
      					<tr $if(document.oddRow)$class="odd"$endif$>
							<td>$if(document.isauthor)$ <input type="checkbox" name="doccheck" value="$document.documentid$" class="check"> $endif$</td>
							<td><div src="/" class="icon status cancelled" rel="Status: avbruten"></div>$if(document.anyinvitationundelivered)$  <span style="color:#000000;position:relative;top:-3px">!</span>$endif$</td>
							<td>
								<span title="$document.mtime$">$document.mtime$</span>
								$if(document.timeoutdaysleft)$  
						          <span title="Förfallodatum: $document.timeoutdate$">
						            <a href="$document.doclink$">
						              ($document.timeoutdaysleft$)
						            </a>
						          </span> 
						          $endif$
							</td>
							<td><a href="$document.doclink$">$document.signatories:{sig | $sig.name$}; separator=", "$</a></td>
							<td>
								<a href="$document.doclink$">$document.documenttitle$</a>
								$if(document.davelink)$ 
					          	<a href="$document.davelink$">*</a>     
					          	$endif$</td>
						</tr>
				      }$
				    </tbody>
  				</table>
				<div class="table-statuses">
					<div class="icon status template"></div>Mall&nbsp;
					<div class="icon status draft"></div>Utkast&nbsp;
					<div class="icon status cancelled"></div>Avbrutet&nbsp;
					<div class="icon status expired"></div>Förfallet&nbsp;
					<div class="icon status sent"></div>Skickat&nbsp;
					<!--
					<div class="icon status reviewed"></div>Granskat&nbsp;
					-->
					<div class="icon status opened"></div>Öppnat&nbsp;
					<div class="icon status signed"></div>Signerat&nbsp;
				</div>
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
		</div>
	</div>
</form>
     