###
pageAcceptTOS =
    <form method="post" action="/accepttos" id="toscontainer" class="overlay">
    <div class="modal-container">
        <div class="modal-header">
            <span class="modal-icon message"></span>
            <span class="modal-title">
                <h2>$_PleaseAcceptTOS()$</h2>
            </span>
            <a class="modal-close close"> </a>
        </div>
        <div class="modal-body">
            <div class="modal-content">
                $termsPageContents()$
            </div>
        </div>
        <div class="modal-footer">
            <div style="float:left"><input type="checkbox" name="tos" id="tos" /> <label for="tos">$_ReadUnderstoodTOS()$</label><br/> </div>
            <a href="#" class="btn-small green float-right submit" id="editing-invite-text-finished">
                <div class="left"></div>
                <div class="label">
                    <p>$_send()$</p>
                    <div class="btn-symbol green arrow-left"></div>
                </div>
                <div class="right"></div>
            </a>
        </div>
    </div>
    </form>
####
####
statsOrgTotal=$_statsOrgTotal()$
###