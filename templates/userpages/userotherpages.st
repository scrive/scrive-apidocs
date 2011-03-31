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
            <div style="float:left"><input type="checkbox" name="tos" id="tos" /> <label for="tos">Jag har läst och accepterar SkrivaPå Allmänna Villkor</label><br> </div>
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