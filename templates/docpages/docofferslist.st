####		
pageOffersList=
$loadingdialog()$
<div>
  <form method="post" action="$currentlink$" class="listForm"  enctype="multipart/form-data" >
	<div id="tab-viewer">
		<div class="tab-header">

		</div>
		$docListTabs()$
		<div class="tab-container">
			<div class="tab-content">
				<div class="col float-left">
					<div class="tab-dd">
                                                <div class="tab-dd-button">
						        <a href="javascript:;"><p>$_options()$</p></a>
						</div>
                                                <ul class="tab-dd-opts">
                                                        <li><a href="javascript:;" rel="#dialog-list-remind-confirm" class="listRemind">$_SendReminder()$</a></li>
							<li><a href="javascript:;" rel="#dialog-list-delete-confirm" class="listDelete">$_delete()$</a>
				          <div class="cantdeleteopenflashmsgtxt" style="display:none;">$_CantRemoveActiveOffers()$</div></li>
						</ul>
					</div>
				</div>
				<div class="col float-right">
                      $searchBox(infotext=_SearchDoc())$
                </div>

     

				<div class="clearfix"></div>
			</div>
			<div class="tab-table">
				<table>
					<thead>
						<tr>
							<th style="width: 30px;"><input type="checkbox" name="" id="all" /></th>
							<th style="width: 65px;">
                               $if(params.sorting.statusREV)$
                                <a href="$currentlink$&amp;sorting=status">$_SortStatus()$<em> ▲</em></a>
                               $elseif(params.sorting.status)$
                                <a href="$currentlink$&amp;sorting=statusREV">$_SortStatus()$<em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&amp;sorting=status">$_SortStatus()$</a>
                               $endif$
                             </th>
							<th style="width: 130px;">
                               $if(params.sorting.timeREV)$
                                <a href="$currentlink$&amp;sorting=time">$_SortTime()$<em> ▲</em></a>
                               $elseif(params.sorting.time)$
                                <a href="$currentlink$&amp;sorting=timeREV">$_SortTime()$<em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&amp;sorting=time">$_SortTime()$</a>
                               $endif$
                            </th>
							<th style="width: 130px;">
                               $if(params.sorting.partnerREV)$
                                <a href="$currentlink$&amp;sorting=partner">$_SortReciever()$<em> ▲</em></a>
                               $elseif(params.sorting.partner)$
                                <a href="$currentlink$&amp;sorting=partnerREV">$_SortReciever()$<em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&amp;sorting=partner">$_SortReciever()$</a>
                               $endif$
                            </th>
							<th style="width: 130px;">
                               $if(params.sorting.partnercompREV)$
                                <a href="$currentlink$&amp;sorting=partnercomp">$_SortCompany()$<em> ▲</em></a>
                               $elseif(params.sorting.partnercomp)$
                                <a href="$currentlink$&amp;sorting=partnercompREV">$_SortCompany()$<em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&amp;sorting=partnercomp">$_SortCompany()$</a>
                               $endif$
                            </th>
							<th style="width: 130px;">
                               $if(params.sorting.authorREV)$
                                <a href="$currentlink$&amp;sorting=author">$_SortSender()$<em> ▲</em></a>
                               $elseif(params.sorting.author)$
                                <a href="$currentlink$&amp;sorting=authorREV">$_SortSender()$<em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&amp;sorting=author">$_SortSender()$</a>
                               $endif$
                            </th>
							<th style="width: 227px;">
                               $if(params.sorting.titleREV)$
                                <a href="$currentlink$&amp;sorting=title">$_SortDoc()$<em> ▲</em></a>
                               $elseif(params.sorting.title)$
                                <a href="$currentlink$&amp;sorting=titleREV">$_SortDoc()$<em style="text-color:red"> ▼</em></a>
                               $else$
                                <a href="$currentlink$&amp;sorting=title">$_SortDoc()$</a>
                               $endif$
                            </th>
							<th></th>
						</tr>
					</thead>
					
    				<tbody id="selectable">
				      $documents:{document |
      					<tr $if(document.odd)$class="odd"$endif$>
							<td>$if(document.isauthor)$ <input type="checkbox" name="doccheck" value="$document.documentid$" class="check"/> $endif$</td>
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
							<td><a href="$document.doclink$">$document.signatories:{sig | $if(!sig.author)$ $sig.name$ $endif$}$</a></td>
                            <td><a href="$document.doclink$">$document.signatories:{sig | $if(!sig.author)$ $sig.company$ $endif$}$</a></td>
                            <td><a href="$document.doclink$">$document.authorname$</a></td> 
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
					<div class="icon status draft float-left"></div><div class="float-left">$_tablestatusesDraft()$&nbsp;</div>
					<div class="icon status cancelled float-left"></div><div class="float-left">$_tablestatusesCandelled()$&nbsp;</div>
					<div class="icon status sent float-left"></div><div class="float-left">$_tablestatusesSent()$&nbsp;</div>
					<div class="icon status delivered float-left"></div><div class="float-left">$_tablestatusesDelivered()$&nbsp;</div>
					<!--
					<div class="icon status reviewed float-left"></div><div class="float-left">Granskat&nbsp;</div>
					-->
                                        <div class="icon status read float-left"></div><div class="float-left">$_tablestatusesEmailOpened()$&nbsp;</div>
					<div class="icon status opened float-left"></div><div class="float-left">$_tablestatusesViewedOnline()$&nbsp;</div>
					<div class="icon status signed float-left"></div><div class="float-left">$_tablestatusesConfirmed()$&nbsp;</div>
				</div>
                $pagingFooter()$
			</div>
		</div>
	</div>
	<div id="tooltip-draft" class="hidden">
		<div class="icon status draft"></div><p>$_tooltipdraft()$</p>
	</div>
    <div id="tooltip-cancelled" class="hidden">
        <div class="icon status cancelled"></div><p>$_tooltipcancelled()$</p>
    </div>
    <div id="tooltip-sent" class="hidden">
      <div class="icon status sent"></div><p>$_tooltipsent()$</p>
    </div>
    <div id="tooltip-delivered" class="hidden">
      <div class="icon status delivered"></div><p>$_tooltipdelivered()$</p>
    </div>
    <div id="tooltip-reviewed" class="hidden">
        <div class="icon status reviewed"></div><p>$_tooltipreviewed()$</p>
    </div>
    <div id="tooltip-read" class="hidden">
      <div class="icon status read"></div><p>$_tooltipread()$</p>
    </div>
    <div id="tooltip-opened" class="hidden">
        <div class="icon status opened"></div><p>$_tooltipopened()$</p>
    </div>
    <div id="tooltip-signed" class="hidden">
        <div class="icon status signed"></div><p>$_tooltipsigned()$</p>
    </div>
  </form>
$listLocalDialogs()$
<form method="post" name="form" action="$currentlink$" class="overlay redirectsubmitform" id="dialog-list-remind-confirm" rel=".listForm" style="display:none;">
    <input type="hidden" name="remind" value="true" />
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>$_OfferReminderHead()$</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <div class="singleremindmsg">$_OfferReminderBodySingel()$<span class="docname"></span>.
          </div>
          <div class="multipleremindmsg">$_OfferReminderBodyMulti()$<span class="doccount"></span>$_OfferReminderBodyMulti2()$</div>
        </div>
      </div>
      <div class="modal-footer">        
        <a class="cancel close float-left" style="height:40px;">  
          <div class="label">$_cancel()$</div>
        </a>
        <a class="btn-small green float-right submiter" href="#">
          <div class="left"></div>
          <div class="label"><p>$_ok()$</p></div>
          <div class="right"></div>
        </a>
      </div>
    </div>
  </form>

</div>
####
