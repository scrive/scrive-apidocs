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

<form method="POST">
<center><h2>ServiceID : <strong> $name$ </strong></h2>
<BR/>
<small> This is simple service configuration panel.Since service configuration is not fully speciefied yet, <BR/>
        so options here are experimental. More options will be added as we defined what is needed.
</small>
<table style="border:1px solid #333333;border-collapse: separate;border-spacing: 5px;"><tbody>
<tr>
    <td> 
            Service nice name 
    </td>
    <td> 
       <input type="text" name="nicename" value="$nicename$"/>
    </td>
</tr>
<tr>
    <td> Service invitation mail content <BR/><BR/>
     <small> This is template of mail content (html-mail) that <BR/>
             will be filled when user is sending invitation <BR/>
             Variables that can be used are  \$documenttitle\$, <BR/>
             \$documentid\$ \$documentlink\$ \$email\$
     </small>
    </td>
    <td> 
    <textarea rows="20" cols="60" name="documentinvitationmail">$documentinvitationmail$</textarea>
    </td>
</tr>

</tbody> </table>


<a class="btn-big green submit" style="width:200px;padding:10px">
          <div class="left"></div>
          <div class="label"> Save </div> 
          <div class="right"></div>
        </a>
</center>
</form>
####