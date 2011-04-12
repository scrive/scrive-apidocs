pagingFooter=	<div class="table-paginate">
                  $if(elements.firstPageAvaible)$
                    <a href="$currentlink$&page=$elements.lastPage$" class="btn-tiny black float-left">
                        <div class="left"></div>
                        <div class="label">
                            <div class="btn-symbol black arrow-right"></div>
                            <p>Bakåt</p>
                        </div>
                        <div class="right"></div>
                    </a>
                   $endif$ 
                   $if(elements.lastPageAvaible)$
                    <a href="$currentlink$&page=$elements.nextPage$" class="btn-tiny black float-right">
                        <div class="left"></div>
                        <div class="label">
                            <p>Nästa</p>
                            <div class="btn-symbol black arrow-left"></div>
                        </div>
                        <div class="right"></div>
                    </a>
                   $endif$
					<div class="pages">
                        $if(!elements.none)$
						 <strong>$elements.min$$if(!elements.single)$-$elements.max$ $endif$ av $elements.total$</strong>
                        $endif$
						 $pages:{page | 
                         $if(page.current)$
                          $page.nr$
                         $else$
                          <a href="$currentlink$&page=$page.nr$">$page.nr$</a>
                         $endif$
                        }$
                        
					</div>
					<div class="clearfix"></div>
				</div>
###		
searchBox=
<div class="searchBox">
<input type="text" class="tab-search float-left" name="search" value="$if(params.search)$$params.search$$endif$" infotext="$infotext$" />
<a href="#" class="btn-tiny black float-right submit"><div class="left"></div><div class="label">Sök</div><div class="right"></div></a>
</div>
###    
####
listLocalDialogs=
<div class="localdialogs">

  <form method="post" name="form" action="$currentlink$" class="overlay redirectsubmitform" id="dialog-list-delete-confirm" rel=".listForm">
    <input type="hidden" name="archive" value="true" />
    <input type="hidden" name="remove" value="true" />
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>Radera</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <div>
            Är du säker att du vill radera <span class="deletionDetails"></span>.
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
####
listLocalDialogs=
<div class="localdialogs">

  <form method="post" name="form" action="$currentlink$" class="overlay redirectsubmitform" id="dialog-list-delete-confirm" rel=".listForm">
    <input type="hidden" name="archive" value="true" />
    <input type="hidden" name="remove" value="true" />
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>Radera</h2>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <div>
            Är du säker att du vill radera <span class="deletionDetails"></span>.
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
