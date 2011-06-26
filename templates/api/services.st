###
serviceList= 

<center><h2>Services list</h2>
<BR/>
<small> Below is the list of services that logged user is administrator <BR/>
        Click on service name to change its settings.
</small>
<BR/><BR/>

<table style="border-collapse: separate;border-spacing: 5px;"><tbody>
    $services:{service |  <tr><td>  <a href="/services/$service.nameforurl$">$service.name$</a> </td> </tr>}$
</tbody> </table>

</center> 
####
serviceAdminPage=



<div id="tab-viewer">
   <div class="tab-header">
        <center><h3> <strong> $name$ </strong></h3>
   </div>
   <div class="tab-container">
       <div class="tab-content account">
           <div class="col">
               <div class="account-header">
                   <h2>UI Settings</h2>
               </div>
               <div class="account-body">
                <form method="POST" action="/services/ui/$nameforurl$" enctype="multipart/form-data">
                   <table>
                       <tr>
                           <td>Mail footer:</td>
                           <td><textarea name="mailfooter" style="width:205px;height:110px">$mailfooter$</textarea></td>
                       </tr>
                       <tr>
                           <td >Buttons body sprite file:</td>
                           <td>
                                $if(buttons)$ <a href="$buttonBodyLink$"> Current</a> $else$ Not set yet $endif$<BR/> 
                                <input type="file" name="button-body" style="width:200px"/>
                           </td>
                       </tr>
                       <tr>
                           <td>Buttons borders sprite file: </td>
                           <td>
                                $if(buttons)$ <a href="$buttonRestLink$"> Current</a> $else$ Not set yet $endif$<BR/> 
                                <input type="file" name="button-rest" style="width:200px"/></td>
                       </tr>
                       <tr>
                           <td>Background color:</td>
                           <td><input type="text" name="background" value="$background$"/></td>
                       </tr>
                       <tr>
                           <td>Overlay background:</td>
                           <td><input type="text" name="overlaybackground" value="$overlaybackground$"/></td>
                       </tr>
                       <tr>
                           <td>Bars color:</td>
                           <td><input type="text" name="barsbackground" value="$barsbackground$"/></td>
                       </tr>
                       <tr>
                           <td>Logo:</td>
                           <td> $if(logo)$ <a href="$logoLink$"> Current</a> $else$ Not set yet $endif$<BR/> 
                                <input type="file" name="logo" style="width:200px" /></td>
                       </tr>
                   </table>
                           <div style="height:30px">
                                <a class="btn-tiny green submit float-left">
                                    <div class="left"></div>
                                    <div class="label"> Save </div> 
                                    <div class="right"></div>
                                </a> 
                                <a class="btn-tiny red submit float-right" rel="clear">
                                    <div class="left"></div>
                                    <div class="label"> Clear </div> 
                                    <div class="right"></div>
                                </a> 
                           </div>  

                 </form>
               </div>
             
           </div>

           <div class="col">
               <div class="account-header">
                   <h2>Password</h2>
               </div>
               <div class="account-body">
                 <form method="POST" action="/services/password/$nameforurl$">
                   <table>
                       <tr>
                           <td>Current password:</td>
                           <td><input type="password" name="oldpassword" infotext=""/>  </td>
                       </tr>
                       <tr>
                           <td>New password:</td>
                           <td><input type="password" name="newpassword1" infotext=""/>  </td>
                       </tr>
                       <tr>
                           <td>Repeat new password:</td>
                           <td><input type="password" name="newpassword2" infotext=""/>  </td>
                       </tr>
                       <tr>
                           <td></td>
                     
                           </td>
                       </tr>
                   </table>
                           <div style="height:30px">
                                <a class="btn-tiny green submit float-left">
                                    <div class="left"></div>
                                    <div class="label"> Save </div> 
                                    <div class="right"></div>
                                </a> 
                           </div>  
                 </form>
               </div>

           </div> <div class="col">
               <div class="account-header">
                   <h2>Settings</h2>
               </div>
               <div class="account-body">
                 <form method="POST" action="/services/settings/$nameforurl$">
                   <table>
                       <tr>
                           <td>Service admin:</td>
                           <td>   <input type="text" name="admin" $if(!allowToChangeSettings)$disabled=""$endif$ value="$admin$"/> </td>
                       </tr>
                       <tr>
                           <td>Service location:</td>
                           <td>   <input type="text" name="location" $if(!allowToChangeSettings)$disabled=""$endif$ value="$location$"/>  </td>
                       </tr>
                       <tr>
                           <td>Mail from address:</td>
                           <td><input type="text" name="mailfromaddress" $if(!allowToChangeSettings)$disabled=""$endif$ value="$mailfromaddress$"/></td>
                       </tr>
                   </table>
                        $if(allowToChangeSettings)$
                           <div style="height:30px">
                                <a class="btn-tiny green submit float-left">
                                    <div class="left"></div>
                                    <div class="label"> Save </div> 
                                    <div class="right"></div>
                                </a> 
                           </div>  
                        $endif$
                 </form>
               </div>

           </div>
           <div class="clearfix"></div>
       </div>
   </div>
</div>



####