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
                        $if(elements.firstPageAvaible)$
                            <a href="$currentlink$&page=1"><<</a>
                        $endif$
						 $pages:{page | 
                         $if(page.current)$
                          $page.nr$
                         $else$
                          <a href="$currentlink$&page=$page.nr$">$page.nr$</a>
                         $endif$
                        }$
                        $if(elements.lastPageAvaible)$
                            <a href="$currentlink$&page=$elements.totalPages$">>></a>
                        $endif$
                        
					</div>
					<div class="clearfix"></div>
				</div>
###		
searchBox=
        $if(params.search)$ 
           <input type="text" class="tab-search" name="search" value="$params.search$" />
        $else$
           <input type="text" class="tab-search infotext grayed" name="search" value="$infotext$" />
        $endif$
           <a href="#" class="btn-tiny black float-right submit"><div class="left"></div><div class="label">Sök</div><div class="right"></div></a>
                