###
adminsmain=
    <strong>Sales Tools</strong>
    <table><tbody>
        <tr>
          <td style="height:50px;width:200px">
            <a href="/adminonly/useradminforsales">
              <button style="width:150px"> User Admin </button>
            </a>
          </td>
         <td style="height:50px;width:200px">
            <a href="/adminonly/useradminforpayments">
              <button style="width:150px"> User Payments </button>
            </a>
          </td>
       </tr>
        <tr>
          <td style="height:50px;width:200px">
            <a href="/adminonly/companyadmin">
              <button style="width:150px"> Company Admin </button>
            </a>
          </td>
       </tr>
   </tbody></table>
    <br/>
    <br/>
    <strong>Admin Tools</strong>
    <table><tbody>
        <tr>
          <td style="height:50px;width:200px">
            <a href="/adminonly/statistics">
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
        <tr>
          <td style="height:50px;width:200px"><a href="/adminonly/services"><button style="width:150px"> Services </button> </a> </td>
          <td style="height:50px;width:200px"><!--<a href="/adminonly/translations"><button style="width:150px"> Translations </button> </a> --> </td>
        </tr>
    </tbody></table>
    <br/>
    <br/>
    <strong> Log Files </strong>
    <ul>
      <li><a href="/adminonly/log/debug.log">debug.log</a></li>
      <li><a href="/adminonly/log/mail.log">mail.log</a></li>
      <li><a href="/adminonly/log/error.log">error.log</a></li>
    </ul>
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
     Here is a CSV version of this table: <a href="/adminonly/skrivapausers.csv">skrivapausers.csv</a>.
      <div>
        $adminlistfilter(link="/adminonly/advuseradmin")$
      </div>
      <div>
        $adminlistpaging(link="/adminonly/advuseradmin")$
        <table><tbody>
            $users:{user|
            <tr>
              <td>
                $user.fullname$
              </td>
              <td>
                &lt;$user.email$&gt;
              </td>
            </tr>
            }$
        </tbody></table>
      </div>
    </div>
###
adminlistfilter=
    <form action="$link$" method="GET">
      <div style="border:1px solid #DDDDDD;padding:5px;margin-top:5px">
        Find $item$:
        <input type="text" style="width:200px;margin-left:5px;margin-right:5px" name="search" value="$search$" />
        <input type="submit" value="Search"/>
      </div>
    </form>
    <br/><br/>
    <a href="$link$">View list of $items$ </a>
    <br/><br/>
    <form action="$link$" method="GET">
      Find by name:
      $letters:{letter |<button type="submit" name="startletter" class="transparent" value="$letter$"><a>$letter$</a> </button>}$
    </form>
###
adminlistpaging=
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
    $adminlistfilter(link=adminlistlink;item="user";items="users")$
    $adminlistpaging()$
    <br/><br/>
    <table>
        <thead>
         <td style="width:200px">Username</td>
          <td style="width:200px">Email</td>
          <td style="width:200px">Company</td>
          <td style="width:200px">Phone</td>
          <td style="width:50px">Uploaded Docs</td>
          <td style="width:50px">Signed (tot)</td>
          <td style="width:50px">Signed (1m)</td>
          <td style="width:50px">Signed (2m)</td>
          <td style="width:50px">Signed (3m)</td>
          <td style="width:50px">Signed (6m)</td>
          <td style="width:50px">Signed (12m)</td>
          <td style="width:50px">Viral Invites</td>
          <td style="width:50px">Admin Invites</td>
        </thead>
        $users:{user|<tr>
          <td><a href="$adminlistlink$/$user.userdetails.id$">$user.userdetails.fullname$</a> </td>
          <td><a href="$adminlistlink$/$user.userdetails.id$">$user.userdetails.email$</a> </td>
          <td> $user.userdetails.company$ </td>
          <td> $user.userdetails.phone$ </td>
          <td> $user.docstats.doccount$ </td>
          <td> $user.docstats.signaturecount$ </td>
          <td> $user.docstats.signaturecount1m$ </td>
          <td> $user.docstats.signaturecount2m$ </td>
          <td> $user.docstats.signaturecount3m$ </td>
          <td> $user.docstats.signaturecount6m$ </td>
          <td> $user.docstats.signaturecount12m$ </td>
          <td> $user.userstats.viralinvitecount$ </td>
          <td> $user.userstats.admininvitecount$ </td>
        </tr>}$
      </table>
###
admincompanies=
    <strong>COMPANY ADMIN</strong>
    <br/>
    $backtoadminmainlink()$
    $adminlistfilter(link=adminlistlink;item="company";items="companies")$
    $adminlistpaging()$
    <br/><br/>
    <table>
        <thead>
          <td style="width:200px">ID</td>
          <td style="width:200px">Name</td>
          <td style="width:200px">Number</td>
          <td style="width:200px">Address</td>
          <td style="width:200px">Zip</td>
          <td style="width:200px">City</td>
          <td style="width:200px">Country</td>
        </thead>
        $companies:{company|<tr>
          <td><a href="$adminlistlink$/$company.companyid$">$company.companyid$</a> </td>
          <td><a href="$adminlistlink$/$company.companyid$">$company.companyname$</a> </td>
          <td> $company.companynumber$ </td>
          <td> $company.companyaddress$ </td>
          <td> $company.companyzip$ </td>
          <td> $company.companycity$ </td>
          <td> $company.companycountry$ </td>
        </tr>}$
      </table>
###
adminUsersForSales=
    <strong>USER ADMIN FOR SALES</strong>
    <br/>
    $backtoadminmainlink()$
    $adminlistfilter(link=adminlistlink;item="user";items="users")$
    $adminlistpaging()$
    <br/><br/>
    <table>
        <thead>
         <td style="width:200px">Username</td>
          <td style="width:200px">Email</td>
          <td style="width:200px">Company</td>
          <td style="width:200px">Phone</td>
          <td style="width:200px">TOS date</td>
          <td style="width:50px">Signed Docs</td>
          <td style="width:50px">Subaccounts</td>
       </thead>
        $users:{user|<tr>
          <td><a href="$adminlistlink$/$user.userdetails.id$">$user.userdetails.fullname$</a> </td>
          <td><a href="$adminlistlink$/$user.userdetails.id$">$user.userdetails.email$</a> </td>
          <td> $user.userdetails.company$</td>
          <td> $user.userdetails.phone$ </td>
          <td> $user.userdetails.TOSDate$ </td>
          <td> $user.docstats.doccount$ </td>
          <td> </td>
       </tr>}$
      </table>
###
adminUsersForPayments=
    <strong>USER ADMIN FOR PAYMENTS</strong>
    <br/>
    $backtoadminmainlink()$
    $adminlistfilter(link=adminlistlink;item="user";items="users")$
    $adminlistpaging()$
    <br/><br/>
    <table>
        <thead>
          <td style="width:200px">Username</td>
          <td style="width:200px">Email</td>
	  <td style="width:200px">Company name</td>
	  <td style="width:200px">Payment plan</td>
	  <td style="width:200px">Next billing date</td>
	  <td style="width:200px">Last billing date</td>
	  <td style="width:200px">Last billing total fee</td>
	  <td style="width:200px">Current plan price</td>
	  <td style="width:200px">Current per signature price</td>
	  <td style="width:200px">Current TW storage price</td>
	</thead>
        $users:{user|<tr>
          <td><a href="$adminlistlink$/$user.userdetails.id$">$user.userdetails.fullname$</a> </td>
          <td><a href="$adminlistlink$/$user.userdetails.id$">$user.userdetails.email$</a> </td>
          <td> $user.userdetails.company$
          </td>
	  <td><select autocomplete="off">$user.adminview.paymentaccounttype:options()$</select></td>
	  <td>?</td>
	  <td>?</td>
	  <td>?</td>
	  <td>0</td>
	  <td>0</td>
	  <td>0</td>
      </tr>}$
      </table>

###
adminuser=
    <a href="$adminlink$">Go back to main admin page</a>
    <a style="padding-left:100px" href="$adminuserslink$">Go back to user admin page</a>
    <br/>
    <br/>
    <form action="" method="POST">
      <div id="tab-viewer">
	<div class="tab-header">
	</div>
	<ul class="tabs">
	  <li class="active"><a href="$adminuserslink$/$user.id$">Användaruppgifter</a></li>
	  <li><a href="#">Payment info</a></li>
	  <li><a href="#">Payment list</a></li>
	  <li><a href="$adminuserslink$/usagestats/$user.id$">Statistics</a></li>
	</ul>
	<div class="tab-container">
	  <div class="tab-content account">
	    <table><tbody>
		<tr> <th style="width:200px"></th> <th> </th></tr>
		<tr> <td style="padding-top:10px"><strong>User data</strong></td> <td> </td></tr>
		<tr> <td>First Name:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$user.fstname$"  name="userfstname" />
		  </td>
		</tr>
		<tr> <td>Secound Name:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$user.sndname$"  name="usersndname" />
		  </td>
		</tr>
		<tr> <td>Pers Nr:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$user.personalnumber$"  name="userpersonalnumber" />
		  </td>
		</tr>
		<tr> <td style="padding-top:10px" ><strong>Contact</strong></td>
		  <td>
		  </td>
		</tr>
		<tr> <td>Phone:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0"  value="$user.phone$" name="userphone" />
		  </td>
		</tr>
		<tr> <td>Mobile:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$user.mobile$" name="usermobile" />
		  </td>
		</tr>
		<tr> <td>Email:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0"  value="$user.email$" name="useremail" />
		  </td>
		</tr>
		<tr> <td>Company position:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$user.companyposition$"  name="usercompanyposition" />
		  </td>
		</tr>
		<tr>
		  <td style="padding-top:10px" ><strong>Account</strong></td>
		  <td> </td>
		</tr>
		<tr> <td>Account type:</td>
		  <td>
		    $if(user.iscompanyaccount)$
		    <a href="/adminonly/companyadmin/$company.companyid$">
		      $company.companyname$ $if(user.iscompanyadmin)$Company Account (Admin)$else$Company Account (Standard)$endif$
		    </a>
		    $else$
		    Private Account<br/>
		    <input type="checkbox" name="upgradetocompany"/> Upgrade to Company
		    $endif$
		  </td>
		</tr>
	    </tbody></table>
	    <br/>
	    <input type="submit" style="width:200px;font-weight:bold"
	    value="Change" name="change" />
	  </div>
	</div>
      </div>
    </form>
####
admincompany=
    <a href="$adminlink$">Go back to main admin page</a>
    <a style="padding-left:100px" href="$admincompanieslink$">Go back to company admin page</a>
    <br/>
    <br/>
    <form action="" method="POST">
      <div id="tab-viewer">
	<div class="tab-header">
	</div>
	<ul class="tabs">
	  <li class="active"><a href="$admincompanieslink$/$companyid$">Company Information</a></li>
	  <li><a href="$admincompanieslink$/usagestats/$companyid$">Statistics</a></li>
	</ul>
	<div class="tab-container">
	  <div class="tab-content account">
	    <table><tbody>
		<tr> <th style="width:200px"></th> <th> </th></tr>
		<tr> <td style="padding-top:10px"><strong>Company data</strong></td> <td> </td></tr>
		<tr> <td>Name:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$companyname$"  name="companyname" />
		  </td>
		</tr>
		<tr> <td>Company nr:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$companynumber$"  name="companynumber" />
		  </td>
		</tr>
		<tr> <td style="padding-top:10px"><strong>Company Location</strong></td>
		  <td> </td>
		</tr>
		<tr> <td>Adress:</td>
		  <td> <input type="text" style="width:200px;margin:0" value="$companyaddress$"  name="companyaddress" />
		  </td>
		</tr>
		<tr> <td>ZIP:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$companyzip$"  name="companyzip" />
		  </td>
		</tr>
		<tr> <td>City:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$companycity$"  name="companycity" />
		  </td>
		</tr>
		<tr> <td>Country:</td>
		  <td>
		    <input type="text" style="width:200px;margin:0" value="$companycountry$" name="companycountry" />
		  </td>
		</tr>
	    </tbody></table>
	    <br/>
	    <input type="submit" style="width:200px;font-weight:bold"
	    value="Change" name="change" />
	  </div>
	</div>
      </div>
    </form>
####
userusagestats=
   <a href="$adminlink$">Go back to main admin page</a>
    <a style="padding-left:100px" href="$adminuserslink$">Go back to user admin page</a>
    <br/>
    <br/>
    <div id="tab-viewer">
      <div class="tab-header">
      </div>
      <ul class="tabs">
	<li><a href="$adminuserslink$/$user.id$">Användaruppgifter</a></li>
	<li><a href="#">Payment info</a></li>
	<li><a href="#">Payment list</a></li>
	<li class="active"><a href="$adminuserslink$/usagestats/$user.id$">Statistics</a></li>
      </ul>
      <div class="tab-container">
	<div class="tab-content account">
	  <table>
            <thead>
              <td style="width:200px">Date</td>
              <td style="width:200px">Documents Closed That Day</td>
              <td style="width:200px">Total Closed Signatures That Day</td>
              <td style="width:200px">Average Signatures / Doc</td>
              <td style="width:200px">Documents Sent That Day</td>
	    </thead>
            $statistics:{stat|<tr>
              <td>$stat.date$</td>
              <td>$stat.closed$ </td>
              <td>$stat.signatures$ </td>
              <td>$stat.avg$ </td>
              <td>$stat.sent$ </td>
	    </tr>}$
	  </table>
	</div>
      </div>
    </div>
####	    
companyusagestats=
    <a href="$adminlink$">Go back to main admin page</a>
    <a style="padding-left:100px" href="$admincompanieslink$">Go back to company admin page</a>
    <br/>
    <br/>
      <div id="tab-viewer">
	<div class="tab-header">
	</div>
	<ul class="tabs">
	  <li><a href="$admincompanieslink$/$companyid$">Company Information</a></li>
	  <li class="active"><a href="$admincompanieslink$/usagestats/$companyid$">Statistics</a></li>
	</ul>
      <div class="tab-container">
	<div class="tab-content account">
	  <table>
            <thead>
              <td style="width:200px">Date</td>
              <td style="width:200px">User</td>
              <td style="width:200px">Documents Closed That Day</td>
              <td style="width:200px">Total Closed Signatures That Day</td>
              <td style="width:200px">Average Signatures / Doc</td>
              <td style="width:200px">Documents Sent That Day</td>
	    </thead>
            $statistics:{stat|<tr class="$if(stat.istotal)$totalline$endif$">
              <td>$stat.date$</td>
              <td>$stat.user$</td>
              <td>$stat.closed$ </td>
              <td>$stat.signatures$ </td>
              <td>$stat.avg$ </td>
              <td>$stat.sent$ </td>
	    </tr>}$
	  </table>
	</div>
      </div>
    </div>
###
statisticsPage=
    $backtoadminmainlink()$
    <div>
      <h1>Stats last month by date</h1>
      <table>
        <thead>
          <td style="width:200px">Date</td>
          <td style="width:200px">All signatures</td>
          <td style="width:200px">Signs in closed</td>
          <td style="width:200px">All users</td>
          <td style="width:200px">All documents</td>
          <td style="width:200px">Preparation</td>
          <td style="width:200px">Pending</td>
          <td style="width:200px">Closed</td>
          <td style="width:200px">AwaitingAuthor</td>
          <td style="width:200px">Rejected</td>
          <td style="width:200px">Canceled</td>
          <td style="width:200px">Timeout</td>
          <td style="width:200px">Error</td>
          <td style="width:200px">Viral invites</td>
          <td style="width:200px">Admin invites</td>
       </thead>
        $lastMonthStats:{stat|<tr>
          <td>$stat.date$</td>
          <td>$stat.documents.signatures$ </td>
          <td>$stat.documents.signaturesInClosed$ </td>
          <td>$stat.users.all$ </td>
          <td>$stat.documents.all$ </td>
          <td>$stat.documents.preparation$</td>
          <td>$stat.documents.pending$ </td>
          <td>$stat.documents.closed$ </td>
          <td>$stat.documents.awaitingauthor$ </td>
          <td>$stat.documents.rejected$ </td>
          <td>$stat.documents.canceled$ </td>
          <td>$stat.documents.timeout$ </td>
          <td>$stat.documents.error$ </td>
          <td>$stat.users.viralInvites$ </td>
          <td>$stat.users.adminInvites$ </td>
       </tr>}$
      </table>
      <h1>Stats by month</h1>
      <table>
        <thead>
          <td style="width:200px">Date</td>
          <td style="width:200px">All signatures</td>
          <td style="width:200px">Signs in closed</td>
          <td style="width:200px">All users</td>
          <td style="width:200px">All documents</td>
          <td style="width:200px">Preparation</td>
          <td style="width:200px">Pending</td>
          <td style="width:200px">Closed</td>
          <td style="width:200px">AwaitingAuthor</td>
          <td style="width:200px">Rejected</td>
          <td style="width:200px">Canceled</td>
          <td style="width:200px">Timeout</td>
          <td style="width:200px">Error</td>
          <td style="width:200px">Viral invites</td>
          <td style="width:200px">Admin invites</td>
       </thead>
        $allMonthsStats:{stat|<tr>
          <td>$stat.date$</td>
          <td>$stat.documents.signatures$ </td>
          <td>$stat.documents.signaturesInClosed$ </td>
          <td>$stat.users.all$ </td>
          <td>$stat.documents.all$ </td>
          <td>$stat.documents.preparation$</td>
          <td>$stat.documents.pending$ </td>
          <td>$stat.documents.closed$ </td>
          <td>$stat.documents.awaitingauthor$ </td>
          <td>$stat.documents.rejected$ </td>
          <td>$stat.documents.canceled$ </td>
          <td>$stat.documents.timeout$ </td>
          <td>$stat.documents.error$ </td>
          <td>$stat.users.viralInvites$ </td>
          <td>$stat.users.adminInvites$ </td>
       </tr>}$
      </table>
    </div>
###
###
databaseContents=
    $backtoadminmainlink()$
    <div>
      <h1>All database files</h1>
      <ul>
        $files:{file| <li><a href="$file$">$file$</a></li>}$
      </ul>
      <form method="POST" action="cleanup">
         <input type="submit" value="Remove old database files"> </input>
      </form>
    </div>
####
pageStats=
    $backtoadminmainlink()$
    <div>
      <h1>Stats page</h1>
      <table>
        <thead>
          <td style="width:50px"></td>
          <td style="width:100px">Documents</td>
          <td style="width:100px">Signatures</td>
          <td style="width:100px">Users</td>
          <td style="width:100px">Viral Invites</td>
          <td style="width:100px">Admin Invites</td>
        </thead>
        <tr>
          <td>Totals</td>
          <td>$stats.svDoccount$</td>
          <td>$stats.svSignaturecount$</td>
          <td>$stats.svUsercount$</td>
          <td>$stats.svViralinvitecount$</td>
          <td>$stats.svAdmininvitecount$</td>
        </tr>
      </table>
      <br/>
      <form method="post" action="/createuser">
        Create user:<br/>
        <table style="width:600px">
          <tr>
            <td>First name:</td>
            <td><input type="text" name="fstname"/></td>
          </tr>
          <tr>
            <td>Second name:</td>
            <td><input type="text" name="sndname"/></td>
          </tr>
          <tr>
            <td>Email address:</td>
            <td><input type="email" name="email"/></td>
          </tr>
          <tr>
            <td>Service:</td>
            <td><select name="systemserver">
                  <option selected="" value="SkrivaPa">SkrivaPå</option>
                  <option value="Scrive">Scrive</option>
                </select>
            </td>
          </tr>
          <tr>
            <td>Free till:</td>
            <td> <input type="checkbox" value="off" autocomplete="off" oldlocation="#hiddenfreetill" location="#freetillcontainer" rel="#freetillbox" class="addremovecheckbox"/>
                 <span id="freetillcontainer">
                 </span>
           </td>
         </tr>
         <tr>
            <td>Header:</td>
            <td> <input type="button" class="editer" value="Default"/>
                 <span class="editable" name="custommessage" style="display:none">$mailNewAccountCreatedByAdminContentHeader()$</span>

           </td>
          </tr>
        </table><br/>
        <input type="submit" value="Create user" />
      </form>

     <div id="hiddenfreetill" style="display:none">
      <span id="freetillbox">
        <a class="datetodate date" rel="[name='freetill']"> </a>
        <input type="hidden" autocomplete="off" name="freetill" value=""/>
      </span>
     </div>
      <br/>
      Disk status:<br/>

      <pre>$sysinfo$</pre>
    </div>
####
backtoadminmainlink=
    <a href="$adminlink$">Go back to main admin page</a>
    <br/><br/>
####
servicesAdmin=
$backtoadminmainlink()$
<div>
<h4 style="margin-left:200px">All services </h4>
<BR/>
<table style="text-align:left">
<thead>
  <tr>
      <th style="width: 200px;"> Name</th>
      <th style="width: 300px;"> Users </th>
      <th style="width: 300px;"> Location </th>
  </tr>
</thead>
<tbody>
$services:{service |<tr> 
    <td>$service.name$ </td>
    <td>$service.admin$ </td>
    <td>$service.location$ </td>
</tr>}$
</tbody>
</table>
</div>
<BR/>
<BR/>
<BR/>
<form method="POST">
<h4 style="margin-left:200px">Create service </h4>
<BR/>
<table><tbody>
<tr> <td style="width:200px;">Name: </td><td><input type="text" name="name"/> </td></tr>
<tr> <td>Admin: </td><td><input type="text" name="admin"/></td></tr>
<tr> <td>Password: </td><td><input type="password" name="password"/> </td></tr>
<tr> <td>Location: </td><td><input type="text" name="location"/> </td></tr>
</tbody></table>
<input type="submit" name="create" value="Create"/>
</form>
<BR/>
<BR/>
###
adminTranslations = 


<center><h2> Translation stats </h2>

<div style="width: 500px;padding:20px; ">
    If you see this page then all csv files have been updated.
    All not used any more texts will be marked with *****.
</div>
</center>

####
