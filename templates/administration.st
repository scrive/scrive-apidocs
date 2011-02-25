###
adminsmain=
    <strong>Admin Tools</strong>
    <table><tbody>
        <tr>
          <td style="height:50px;width:200px">
            <a href="/adminonly/alluserstable">
              <button style="width:150px"> Statistics</button>
            </a>
          </td>
          <td style="height:50px">
            <a href="/adminonly/payments">
              <button style="width:150px">Price Plan</button>
            </a>
          </td>
        </tr>
        <tr>
          <td style="height:50px;width:200px"><a href="/stats"><button style="width:150px"> Create User </button> </a> </td>
          <td style="height:50px;width:200px"> <a href="/adminonly/useradmin"><button style="width:150px"> User Admin </button> </a></td>
        </tr>
    </tbody></table>
    <br/>
    <br/>
    <strong> Super Admin Tools Only </strong>
    <table><tbody>
        <tr>
          <td style="height:50px;width:200px"><a href="/adminonly/advuseradmin"><button style="width:150px"> Adv. User Admin </button> </a> </td>
          <td style="height:50px;width:200px"> <a href="/adminonly/advpayments"><button style="width:150px"> App Admin </button> </a></td>
        </tr>
    </tbody></table>
###
adminsmanageall =
    $backtoadminmainlink()$
    <div><strong>SkrivaPa admin only page</strong></div>
    <br/>
    <div>
      <h3>Database files</h3>
      <a href="/adminonly/db/">Show list of database files</a><br/>
      <form method="post" action="/adminonly/cleanup">
        Database incrementally stores backups of itself. Old copies aren't useful in normal situations,
        just in case of failure. It is good to remove old files from time to time.<br/>
        <input type="submit" value="Cleanup" /><br/>
      </form>
      <h3>Users</h3>
      You can see <a href="/adminonly/alluserstable">big table of all users</a>.<br/>
      Here is a CSV version of this table: <a href="/adminonly/skrivapausers.csv">skrivapausers.csv</a>.
      <div>
        $usersfilter(link="/adminonly/advuseradmin")$
      </div>
      <div>
        $userspaging(link="/adminonly/advuseradmin")$
        <table><tbody>
            $users:{user|
            <tr>
              <td>
                $user.usvFullname$
              </td>
              <td>
                &lt;$user.usvEmail$&gt;
              </td>
            </tr>
            }$
        </tbody></table>
      </div>
    </div>
###
usersfilter=
    <form action="$link$" method="GET">
      <div style="border:1px solid #DDDDDD;padding:5px;margin-top:5px">
        Find user:
        <input type="text" style="width:200px;margin-left:5px;margin-right:5px" name="search" value="$search$" />
        <input type="submit" value="Search"/>
    </form>
    <br/><br/>
    <a href="$link$">View list of users </a>
    <br/><br/>
    <form action="$link$" method="GET">
      Find by name:
      $letters:{letter |<button type="submit" name="startletter" class="transparent" value="$letter$"><a>$letter$</a> </button>}$
    </form>
###
userspaging=
    <form action="$link$" method="GET">
      View page:
      $intervals:{interval |<button type="submit" name="page" class="transparent" value="$interval.oValue$"><a>$interval.oText$</a> </button>}$
      <input type="hidden" name="startletter" value="$startletter$" />
      <input type="hidden" name="search" value="$search$" />
    </form>
###
adminusers=
    <strong>USER ADMIN</strong>
    <br/>
    $backtoadminmainlink()$
    $usersfilter(link=adminuserlink)$
    $userspaging()$
    <br/><br/>
    <table>
        <thead>
          <td style="width:200px">Username</td>
          <td style="width:200px">Email</td>
          <td style="width:200px">Company</td>
          <td style="width:100px">Uploaded Docs</td>
          <td style="width:100px">Signed Docs</td>
        </thead>
        $users:{user|<tr>
          <td> <a href="$adminuserlink$/$user.userdetails.usvId$"> $user.userdetails.usvFullname$</a> </td>
          <td> <a href="$adminuserlink$/$user.userdetails.usvId$"> $user.userdetails.usvEmail$ </a> </td>
          <td> $user.userdetails.usvCompany$ </td>
          <td> $user.docstats.doccount$ </td>
          <td> $user.docstats.signaturecount$ </td>
        </tr>}$
      </table>
###
adminuser=
    <a href="$adminlink$">Go back to main admin page</a>
    <a style="padding-left:100px" href="$adminuserslink$">Go back to user admin page</a>
    <br/>
    <br/>
    <form action="" method="POST">
      <table><tbody>
          <tr> <th style="width:200px"></th> <th> </th></tr>
          <tr> <td style="padding-top:10px"><strong>User data</strong></td> <td> </td></tr>
          <tr> <td>First Name:</td>
            <td>
              <input type="text" style="width:200px;margin:0" value="$user.uavuserfstname$"  name="userfstname" />
            </td>
          </tr>
          <tr> <td>Secound Name:</td>
            <td>
              <input type="text" style="width:200px;margin:0" value="$user.uavusersndname$"  name="usersndname" />
            </td>
          </tr>
          <tr> <td>Pers Nr:</td>
            <td>
              <input type="text" style="width:200px;margin:0" value="$user.uavuserpersonalnumber$"  name="userpersonalnumber" />
            </td>
          </tr>
          <tr> <td>Company:</td>
            <td>
              <input type="text" style="width:200px;margin:0" value="$user.uavusercompanyname$"  name="usercompanyname" />
            </td>
          </tr>
          <tr> <td>Company position:</td>
            <td>
              <input type="text" style="width:200px;margin:0" value="$user.uavusercompanyposition$"  name="usercompanyposition" />
            </td>
          </tr>
          <tr> <td>Company nr:</td>
            <td>
              <input type="text" style="width:200px;margin:0" value="$user.uavusercompanynumber$"  name="usercompanynumber" />
            </td>
          </tr>
          <tr> <td style="padding-top:10px"><strong>Location</strong></td>
            <td> </td>
          </tr>
          <tr> <td>Adress:</td>
            <td> <input type="text" style="width:200px;margin:0" value="$user.uavuseraddress$"  name="useraddress" />
            </td>
          </tr>
          <tr> <td>ZIP:</td>
            <td>
              <input type="text" style="width:200px;margin:0" value="$user.uavuserzip$"  name="userzip" />
            </td>
          </tr>
          <tr> <td>City:</td>
            <td>
              <input type="text" style="width:200px;margin:0" value="$user.uavusercity$"  name="usercity" />
            </td>
          </tr>
          <tr> <td>Country:</td>
            <td>
              <input type="text" style="width:200px;margin:0" value="$user.uavusercountry$" name="usercountry" />
            </td>
          </tr>
          <tr> <td style="padding-top:10px" ><strong>Contact</strong></td>
            <td>
            </td>
          </tr>
          <tr> <td>Phone:</td>
            <td>
              <input type="text" style="width:200px;margin:0"  value="$user.uavuserphone$" name="userphone" />
            </td>
          </tr>
          <tr> <td>Mobile:</td>
            <td>
              <input type="text" style="width:200px;margin:0" value="$user.uavusermobile$" name="usermobile" />
            </td>
          </tr>
          <tr> <td>Email:</td>
            <td>
              <input type="text" style="width:200px;margin:0"  value="$user.uavuseremail$" name="useremail" />
            </td>
          </tr>
          <tr>
            <td style="padding-top:10px" ><strong>Account</strong></td>
            <td> </td>
          </tr>
          <tr> <td>Account type:</td>
            <td>
              <select autocomplete="off" name="accounttype">$user.uavaccounttype:options()$</select>
            </td>
          </tr>
          <tr> <td>Account plan:</td>
            <td>
              <select autocomplete="off" name="accountplan">$user.uavaccountplan:options()$</select>
            </td>
          </tr>
          <tr> <td>Signed doc storage:</td>
            <td> $user.uavsigneddocstorage$ </td>
          </tr>
          <tr> <td>Payment method:</td>
            <td>
              <select autocomplete="off" name="userpaymentmethod" >$user.uavuserpaymentmethod:options()$</select>
            </td>
          </tr>
          <tr> <td style="padding-top:10px" ><strong>Special deals</strong></td>
            <td> </td>
          </tr>
          <tr> <td>Free signatures left:</td>
            <td>
              <input type="text" style="width:40px;margin:0" value="$user.uavpaymentaccountfreesignatures$" name="paymentaccountfreesignatures" />
            </td>
          </tr>
          <tr> <td style="padding-top:10px" ><strong>Pricing</strong></td>
            <td>
            </td>
          </tr>
          <tr> <td>Payment plan:</td>
            <td>
              <select autocomplete="off" name="paymentaccounttype">$user.uavpaymentaccounttype:options()$</select>
            </td>
          </tr>
          <tr> <td>Temporary price:</td>
            <td>
              <span>
                <input type="checkbox" value="$if(user.uavtmppaymentchangeenddate)$on$else$off$endif$" autocomplete="off" oldlocation="#hiddenttimestuffbox" location="#datetosigncontainer" rel="#daystosignbox" class="addremovecheckbox" />
                <span id="datetosigncontainer">
                </span>
              </span>
            </td>
          </tr>
      </tbody></table>
      <br/>
      <strong>User price plan</strong>
      <br/><br/>
      <table style="text-align:left"><tbody>
          <tr>
            <th style="width:300px;"></th>
            <th> Standard</th>
            <th style="width:100px"> </th>
            <th> Custom</th>
            <th style="width:100px"> </th>
            <th> Temporary</th>
          </tr>
          <tr>
            <td style="padding-top:10px"><strong>Payment for account </strong> </td>
          </tr>
          <tr>
            <td>For account: </td>
            $paymentmodel.foraccount:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="foraccountcustom" value="$user.uavcustompaymentchange.changeforaccount$"/>}:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="foraccounttemp" value="$user.uavtemppaymentchange.changeforaccount$"/>}:td()$
          </tr>
          <tr>
            <td>For subaccount: </td>
            $paymentmodel.forsubaccount:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="forsubaccountcustom" value="$user.uavcustompaymentchange.changeforsubaccount$"/>}:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="forsubaccounttemp" value="$user.uavtemppaymentchange.changeforsubaccount$"/>}:td()$
          </tr>
          <tr>
            <td style="padding-top:20px"><strong>Payment for signatures </strong> </td>
          </tr>
          <tr>
            <td>For email signature:</td>
            $paymentmodel.foremailsignature:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="foremailsignaturecustom" value="$user.uavcustompaymentchange.changeforemailsignature$"/>}:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="foremailsignaturetemp" value="$user.uavtemppaymentchange.changeforemailsignature$"/>}:td()$
          </tr>
          <tr>
            <td>For eLeg signature:</td>
            $paymentmodel.forelegsignature:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="forelegsignaturecustom" value="$user.uavcustompaymentchange.changeforelegsignature$"/>}:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="forelegsignaturetemp" value="$user.uavtemppaymentchange.changeforelegsignature$"/>}:td()$
          </tr>
          <tr>
            <td> For mobile signature: :</td>
            $paymentmodel.formobiledignature:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="formobiledignaturecustom" value="$user.uavcustompaymentchange.changeformobiledignature$"/>}:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="formobiledignaturetemp" value="$user.uavtemppaymentchange.changeformobiledignature$"/>}:td()$
          </tr>
          <tr>
            <td>For creadit card signature:</td>
            $paymentmodel.forcrediteardsignature:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="forcrediteardsignaturecustom" value="$user.uavcustompaymentchange.changeforcrediteardsignature$"/>}:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="forcrediteardsignaturetemp" value="$user.uavtemppaymentchange.changeforcrediteardsignature$"/>}:td()$
          </tr>
          <tr>
            <td>For iPad signature: </td>
            $paymentmodel.foripadsignature:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="foripadsignaturecustom" value="$user.uavcustompaymentchange.changeforipadsignature$"/>}:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="foripadsignaturetemp" value="$user.uavtemppaymentchange.changeforipadsignature$"/>}:td()$
          </tr>
          <tr>
            <td style="padding-top:20px"><strong>Storage of signed documents</strong> </td>
          </tr>
          <tr>
            <td>For Amazon: </td>
            $paymentmodel.foramazon:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="foramazoncustom" value="$user.uavcustompaymentchange.changeforamazon$"/>}:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="foramazontemp" value="$user.uavtemppaymentchange.changeforamazon$"/>}:td()$
          </tr>
          <tr>
            <td>For TrustWeaver: </td>
            $paymentmodel.fortrustweaver:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="fortrustweavercustom" value="$user.uavcustompaymentchange.changefortrustweaver$"/>}:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="fortrustweavertemp" value="$user.uavtemppaymentchange.changefortrustweaver$"/>}:td()$
          </tr>
          <tr>
            <td style="padding-top:20px">  <strong>Storage of other documents</strong> </td>
          </tr>
          <tr>
            <td>Template:  </td>
            $paymentmodel.fortemplate:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="fortemplatecustom" value="$user.uavcustompaymentchange.changefortemplate$"/>}:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="fortemplatetemp" value="$user.uavtemppaymentchange.changefortemplate$"/>}:td()$
          </tr>
          <tr>
            <td> Draft:  </td>
            $paymentmodel.fordraft:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="fordraftcustom" value="$user.uavcustompaymentchange.changefordraft$"/>}:td()$
            <td></td>
            ${<input type="text" style="width:45px" name="fordrafttemp" value="$user.uavtemppaymentchange.changefordraft$"/>}:td()$
          </tr>
      </tbody></table>
      <br/>
      <input type="submit" style="width:200px;font-weight:bold" value="Change" name="change" />
    </form>
    <br/>
    <form method="post" action="">
      <input type="submit" name="enabletrustweaver" style="width:600px;font-weight:bold" value="Enable TrustWeaver storage for this client (cannot be undone)" />
    </form>
    <div id="hiddenttimestuffbox" style="display:none">
      <span id="daystosignbox">
        <a class="datetodate date" rel="[name='tmppaymentchangeenddate']"> </a>
        <input type="hidden" autocomplete="off" name="tmppaymentchangeenddate" value="$user.uavtmppaymentchangeenddate$" />
      </span>
    </div>
###
allUsersTable=
    $backtoadminmainlink()$
    <div>
      <h1>Alla SkrivaPå anwender</h1>
      <table>
        <thead>
          <td style="width:200px">Username</td>
          <td style="width:200px">Email</td>
          <td>Docs</td>
        </thead>
        $users:{user|<tr>
          <td> $user.userdetails.usvFullname$  </td>
          <td> $user.userdetails.usvEmail$ </td>
          <td> $user.docstats.doccount$ </td>
        </tr>}$
      </table>
    </div>
###
databaseContents=
    $backtoadminmainlink()$
    <div>
      <h1>All database files</h1>
      <ul>
        $files:{file| <li><a href="$file$">$file$</a></li>}$
      </ul>
    </div>
####
pageStats=
    $backtoadminmainlink()$
    <div>
      <h1>Stats page</h1>
      <table>
        <thead>
          <td style="width:50px"></td>
          <td style="width:150px">Documents</td>
          <td style="width:150px">Signatures</td>
          <td style="width:150px">Users</td>
        </thead>
        <tr>
          <td>Totals</td>
          <td>$stats.svDoccount$</td>
          <td>$stats.svSignaturecount$</td>
          <td>$stats.svUsercount$</td>
        </tr>
      </table>
      <br/>
      <form method="post" action="/createuser">
        Create user:<br/>
        <table style="width:600px">
          <tr>
            <td>Full name:</td>
            <td><input type="text" name="fullname"></td>
          </tr>
          <tr>
            <td>Email address:</td>
            <td><input type="email" name="email"></td>
          </tr>
          <tr>
            <td>Free till:</td>
            <td> <input type="checkbox" value="off" autocomplete="off" oldlocation="#hiddenfreetill" location="#freetillcontainer" rel="#freetillbox" class="addremovecheckbox">
                 <span id="freetillcontainer">
                 </span>
           </td>
         </tr>
         <tr>
            <td>Header:</td>
            <td> <input type="button" class="editer" value="Default">
                 <span class="editable" name="custommessage" style="display:none">$mailNewAccountCreatedByAdminContentHeader()$</span>

           </td>
          </tr>
        </table><br>
        </table><br/>
        <input type="submit" value="Create user" />
      </form>

     <div id="hiddenfreetill" style="display:none">
      <span id="freetillbox">
        <a class="datetodate date" rel="[name='freetill']"> </a>
        <input type="hidden" autocomplete="off" name="freetill" value="">
      </span>
     </div>
      <br>
      Disk status:<br>

      <pre>$sysinfo$</pre>
    </div>
####
backtoadminmainlink=
    <a href="$adminlink$">Go back to main admin page</a>
    <br/><br/>
####
