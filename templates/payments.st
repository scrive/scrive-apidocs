####
paymentsadminpagenotselected= <div style="padding-left:200px"> 
                       <strong>
                        This is admin page for managing payment accounts 
                       </strong>
                       <BR/> 
                        <form action="$action$" method="GET">
                        Please choose account type:  <select class="onSelectAppendAndSubmit"> <option></option> $list:option()$ </select>
                       </form> 
                    </div>
###
paymentssalespagenotselected= <div style="padding-left:200px"> 
                       <strong>
                        This is sales page for managing payment accounts 
                       </strong>
                       <BR/> 
                       <form action="$action$" method="GET">
                         Please choose user: <select class="onSelectAppendAndSubmit"> <option></option> $list:option()$ </select>
                       </form> 
                    </div>
###
###
option= <option class="option" value="$it$">$it$</option>
###