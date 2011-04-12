####		
pageContractsList=
$loadingdialog()$
<div>
  <form method="post" action="$currentlink$" class="listForm"  enctype="multipart/form-data" >
	<div id="tab-viewer">
		<div class="tab-header">

		</div>
		<ul class="tabs">
			<li class="active"><a href="/d" class="listtype">Dokument</a></li>
			<li><a href="/t">Mallar</a></li>
			<!-- <li><a href="/contacts">Kontakter</a></li> -->
		</ul>
		<div class="tab-container">
			<div class="tab-content">
				<div class="col float-left">
					<div class="tab-dd">
                                                <div class="tab-dd-button">
						        <a href="javascript:;"><p>Alternativ</p></a>
						</div>
                                                <ul class="tab-dd-opts">
                                                        <li><a href="javascript:;" rel="#dialog-list-remind-confirm" class="listRemind">Skicka påminnelse</a></li>
							<li><a href="javascript:;" rel="#dialog-list-delete-confirm" class="listDelete">Radera</a></li>
						</ul>
					</div>
				</div>
				<div class="col float-right">
                      $searchBox(infotext='Sök dokument')$
                </div>

     

				<div class="clearfix"></div>
			</div>
			<div class="tab-table">
				<table>
					<thead>
						<tr>
							<th style="width: 30px;"><input type="checkbox" name="" id="all" /></th>
							<th style="width: 50px;">
                               $if(params.sorting.statusREV)$
                                <a href="$currentlink$&sorting=status">Status  <em> ▲</em></a>
                               $elseif(params.sorting.status)$
                                <a href="$currentlink$&sorting=statusREV">Status  <em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&sorting=status">Status </a>
                               $endif$
                             </th>
							<th style="width: 140px;">
                               $if(params.sorting.timeREV)$
                                <a href="$currentlink$&sorting=time">Senaste händelse  <em> ▲</em></a>
                               $elseif(params.sorting.time)$
                                <a href="$currentlink$&sorting=timeREV">Senaste händelse  <em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&sorting=time">Senaste händelse </a>
                               $endif$
                            </th>
							<th style="width: 300px;">
                               $if(params.sorting.partnerREV)$
                                <a href="$currentlink$&sorting=partner">Parter <em> ▲</em></a>
                               $elseif(params.sorting.partner)$
                                <a href="$currentlink$&sorting=partnerREV">Parter  <em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&sorting=partner">Parter </a>
                               $endif$
                            </th>
							<th style="width: 300px;">
                               $if(params.sorting.titleREV)$
                                <a href="$currentlink$&sorting=title">Dokument <em> ▲</em></a>
                               $elseif(params.sorting.title)$
                                <a href="$currentlink$&sorting=titleREV">Dokument <em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&sorting=title">Dokument </a>
                               $endif$
                            </th>
							<th></th>
						</tr>
					</thead>
					
    				<tbody id="selectable">
				      $documents:{document |
      					<tr $if(document.odd)$class="odd"$endif$>
							<td>$if(document.isauthor)$ <input type="checkbox" name="doccheck" value="$document.documentid$" class="check"> $endif$</td>
							<td>
                              <div src="/" class="icon status $document.status$ tooltip" rel="#tooltip-$document.status$"></div>
                              $if(document.anyinvitationundelivered)$  <span style="color:#000000;">!</span>$endif$
                           </td>
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
								<a href="$document.doclink$" class="listname">$document.documenttitle$</a>
								$if(document.davelink)$ 
					          	<a href="$document.davelink$">*</a>     
					          	$endif$
							</td>
							<td class="right">
								<!-- <em>▼</em> -->
							</td>
						</tr>
				      }$
				    </tbody>
  				</table>
				<div class="table-statuses">
                    <!--
					<div class="icon status template"></div>Mall&nbsp;</div>
                    -->
					<div class="icon status draft float-left"></div><div class="float-left">Utkast&nbsp;</div>
					<div class="icon status cancelled float-left"></div><div class="float-left">Avbrutet&nbsp;</div>
					<div class="icon status expired float-left"></div><div class="float-left">Förfallet&nbsp;</div>
					<div class="icon status sent float-left"></div><div class="float-left">Skickat&nbsp;</div>
					<!--
					<div class="icon status reviewed float-left"></div><div class="float-left">Granskat&nbsp;</div>
					-->
					<div class="icon status opened float-left"></div><div class="float-left">Öppnat&nbsp;</div>
					<div class="icon status signed float-left"></div><div class="float-left">Signerat&nbsp;</div>
				</div>
                $pagingFooter()$
			</div>
		</div>
	</div>
	<div id="tooltip-draft" class="hidden">
		<div class="icon status draft"></div><p>Status: Utkast</p>
	</div>
    <div id="tooltip-cancelled" class="hidden">
        <div class="icon status cancelled"></div><p>Status: Avbrutet</p>
    </div>
    <div id="tooltip-expired" class="hidden">
        <div class="icon status expired"></div><p>Status: Förfallet</p>
    </div>
    <div id="tooltip-sent" class="hidden">
        <div class="icon status sent"></div><p>Status: Skickat</p>
    </div>
    <div id="tooltip-reviewed" class="hidden">
        <div class="icon status reviewed"></div><p>Status: Granskat</p>
    </div>
    <div id="tooltip-opened" class="hidden">
        <div class="icon status opened"></div><p>Status: Öppnat</p>
    </div>
    <div id="tooltip-signed" class="hidden">
        <div class="icon status signed"></div><p>Status: Signerat</p>
    </div>
  </form>
$listLocalDialogs()$
<form method="post" name="form" action="$currentlink$" class="overlay redirectsubmitform" id="dialog-list-remind-confirm" rel=".listForm" style="display:none;">
    <input type="hidden" name="remind" value="true" />
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>Skicka påminnelse</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <div class="singleremindmsg">
            Är du säker att du vill skicka en påminnelse til berörda parter i dokumentet <span class="docname"></span>.
          </div>
          <div class="multipleremindmsg">
            Är du säker att du vill skicka en påminnelse til berörda parter för <span class="doccount"></span> dokument.
          </div>
        </div>
      </div>
      <div class="modal-footer">        
        <a class="cancel close float-left" style="height:40px;">  
          <div class="label">Avbryt</div> 
        </a>
        <a class="btn-small green float-right submiter" href="#">
          <div class="left"></div>
          <div class="label"><p>OK</p></div>
          <div class="right"></div>
        </a>
      </div>
    </div>
  </form>

</div>
####
