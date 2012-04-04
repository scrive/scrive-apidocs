pagingFooter=	<div class="table-paginate">
                  $if(elements.firstPageAvaible)$
                    <a href="$currentlink$&amp;page=$elements.lastPage$" class="btn-tiny black float-left">
                        <div class="left"></div>
                        <div class="label">
                            <div class="btn-symbol black arrow-right"></div>
                            <p>$_pagingFooterPrevious()$</p>
                        </div>
                        <div class="right"></div>
                    </a>
                   $endif$
                   $if(elements.lastPageAvaible)$
                    <a href="$currentlink$&amp;page=$elements.nextPage$" class="btn-tiny black float-right">
                        <div class="left"></div>
                        <div class="label">
                            <p>$_pagingFooterNext()$</p>
                            <div class="btn-symbol black arrow-left"></div>
                        </div>
                        <div class="right"></div>
                    </a>
                   $endif$
					<div class="pages">
                        $if(!elements.none)$
						 <strong>$elements.min$$if(!elements.single)$-$elements.max$ $endif$ $_of()$ $elements.total$</strong>
                        $endif$
						 $pages:{page |
                         $if(page.current)$
                          $page.nr$
                         $else$
                          <a href="$currentlink$&amp;page=$page.nr$">$page.nr$</a>
                         $endif$
                        }$

					</div>
					<div class="clearfix"></div>
				</div>
###
searchBox=
<div class="searchBox float-right">
<a href="#" class="btn-tiny black float-right submit"><div class="left"></div><div class="label">$_searchBoxSearch()$</div><div class="right"></div></a>
<input type="text" class="tab-search float-right" name="search" value="$if(params.search)$$params.search$$endif$" infotext="$infotext$" />

</div>
###
