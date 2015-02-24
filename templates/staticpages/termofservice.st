###
termsOfService=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents(nobranding='true')$
    <link href='/service_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
  </head>
<body>
  $header()$
  <div class="mainContainer" >
    <div class="body-container">

      <div class="js-terms-of-service terms-of-service">$termsOfServiceContent()$</div>
      <script>
      require(['jquery', 'legacy_code'], function(jQuery) {
        mixpanel.track('View Terms Of Service Page');
      });
      </script>
    </div>
  </div>
  $pageFooter()$
</body>
$htmlclosingtag()$
###
termsOfServiceWithBranding=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents(nobranding='true')$
    <link href='/login_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
    <div class="js-terms-of-service terms-of-service">
      <div style='width:275px;margin:0px auto;'>
        <div style="margin-top: 64px;text-align:center;">
          <img alt="logo" src="/login_logo/$brandinghash$" />
          <div class="divider-line"></div>
          <div class='label' style="text-align:center;width:275px;">
            E-signing powered by Scrive
          </div>
        </div>
      </div>
    $termsOfServiceContent()$
    </div>
    <script>
      mixpanel.track('View Terms Of Service Page | Branded');
    </script>

  </body>
$htmlclosingtag()$
####
termsOfServiceContent=
<section>
  <div class="ct">
    <header class="bulky">
      <h1>Terms of Service</h1>
      <h2>2012.03</h2>
    </header>
    <div class="content">
      <p>These terms of service (“Terms”) apply to the use of the site www.scrive.com (“Service”) and the electronic service for signing documents (also “Service”) between different parties (“Party” or “Parties”). The Terms are an agreement between you (“You” or “User”) and Scrive AB (“Scrive”). By using, creating an account and/or by logging onto the Service, You accept the Terms. Scrive reserves the right to, at any time, amend, add or remove parts of these Terms. It is Your own responsibility to be informed of any update of the Terms each time You use the Service. Your continued use of the Service after changes have been made in the Terms means that You accept those changes.</p>
      <h1>1. User accounts</h1>
      <p>You are responsible for maintaining the confidentiality of, and protecting Your account information, including passwords. You are responsible for all activity on Your account. You shall immediately notify Scrive on suspicion of unauthorised use or attempted unauthorised use of Your account or Your code, or other security breach.</p>
      <p>
        <strong>a. Account Terms:</strong>
        You agree that You will give Scrive accurate, current and complete information about Yourself and Your invoice/payment information. You shall update all account information by following the procedures provided by the Service. You may not use any automated device, program, algorithm, method, or any similar manual process, to access, acquire, copy, probe, index, manipulate, test or monitor any portion of the Service or any Content, or in any way reproduce or circumvent the Service’s structure or presentation, or to attempt to obtain any materials, documents or information through any means other than the infrastructure created and made available by Scrive, or its partners, to that end. You agree that You will not perform any action that exposes the Service’s infrastructure, or subject any systems or networks that are part of or affiliated with the Service, to an excessive burden.
      </p>
      <p>
        <strong>b. User Content:</strong>
        You agree that You will not use the Service for illegal purposes. This includes disrupting the Service, disseminating content that violates privacy, copyright or proprietary right of any Third-party, or using Scrive for any purposes that are or may be perceived as illegal, obscene, abusive, libellous, threatening, vulgar or otherwise reprehensible. Scrive is not responsible for any failure or delay in removing such content. Scrive has the right to, at any time, delete, reject and litigate against User and User content if Scrive deems it to violate the Terms or otherwise violate any applicable laws and regulations. You are fully responsible for the contents of Your account and any transactions made with Your account through the Service.
      </p>
      <p>
        <strong>c. Costs and payment:</strong>
        Individuals can use the Services free of charge for up to 3 signatures a month. Groups and companies can use the Service according to existing price plan published on the website, or according to a customized offering from Scrive or an authorised reseller. Scrive invoices are due 30 days after invoice date. Reminders will be charged a late payment fee of 15 EUR. The interest rate on late payments is 15% plus the current government borrowing rate.
      </p>
      <p>
        <strong>d. Normal usage:</strong>
        Users using the service according to the price plan published on the website and without a customized offering from Scrive or an authorised reseller can use the Service for a total of 100 signatures and 300 sent documents per month. A signature is here defined as each signature applied by the Parties to a document sent by the User.
      </p>
      <p>
        <strong>e. Suspension of Service or termination:</strong>
        You agree that Scrive at any time, without notice, may freeze Your account or otherwise stop Your access to the Service for (1) alleged breach of the Terms, (2) our attempts to address safety in the software or to protect the contents, (3) modification of the Service, (4) unexpected operational interruptions or problems, (5) deferred payment, (6) breach of limits of normal use as defined in section 1d or (6) explicit action requested from any legal authority or other government authorities. You agree that Scrive also is entitled to, with a week’s notice, terminate Your account for (1) clear violations of the Terms, (2) non-paying Users inactivity for more than 12 months, (3) payment due to the bailiff, or (4) explicit action request from any legal authority or other government authorities. Scrive will not be liable to You or any third-party using Your account based on the Service being frozen or terminated.
      </p>
      <h1>2. Data management</h1>
      <p>Besides the documents and personal information that has been shared with the Services by the Parties, Scrive also collects information about the Parties’ communication and behaviour in the services, for example IP-addresses, language settings and digital fingerprints that can strengthen the Parties legal position (“Information”). Scrive have a purpose to, with the help of the Information, as an independent third party decrease transaction costs, strengthen the evidence trail of signed documents and to decrease administrative and legal costs. Thus, as part of the Services, Scrive provide the Parties, paying or non-paying, with equal opportunities to different forms of storage, handling and processing of the Information through the Services that can be of legal use to the Parties. The assignment requires that Scrive handles Parties Information automatically according to the following guidelines and You accept and authorise Scrive to handle the Parties Information in accordance with these guidelines.</p>
      <p>
        <strong>a. Sharing data:</strong>
        Scrive has the right to communicate with You and also directly or indirectly the invited Parties (an indirectly invited Party is a third party that has been invited by one of Your directly invited Parties) regarding the documents to be signed. Scrive has the right to share the Information that is reasonably needed for the Parties to be able to (1) review the document, (2) examine the identities of the signing parties, (3) to know whether the document is signed or not signed and understand the circumstances surrounding the events and (4) retrospectively examine the stored evidence of the operation.
      </p>
      <p>
        <strong>b. Data storage:</strong>
        All Information is stored per errand with at least two backups per errand and all Parties, paying or non-paying, get equal opportunity to access the errand. When all Parties that had access have deleted an errand, the errand and all backups are permanently deleted from the Service within 6 months. Errands containing documents that have been signed through the Service by all Parties are stored free of charge up to 10 years from the date that the last Party signed. Errands containing documents that have not been signed by all Parties are stored free of charge at least 1 year after the document was created. The service stores no more than 1GB free of charge per account.
      </p>
      <p>
        <strong>c. Data integrity:</strong>
        In order to generate legally and statistically useful materials for the Parties the Service may process the Information automatically. The Service must for example, to be able to generate the electronic original as the final product after all Parties have signed, be able to print extracts of logs, the Scrive seal and the Parties personal information in the documents footer, verification page and extra evidence attachments and thereafter stamp the document with a digital signature. The Information may be used by Scrive in anonymous form for statistical analysis and in order to promote or develop the Service. The Parties documents are not available to Scrive employees for manual handling unless either Party has requested or given their explicit consent of such handling and the document or documents as a result of such request have been made available by specially authorised technicians at Scrive.
      </p>
      <h1>3. Signed documents</h1>
      <p>The following provisions apply to documents signed through the Service.</p>
      <p>
        <strong>a. Scrive is never a Party:</strong>
        As part of the Service Scrive stamp documents signed by all Parties with a special Scrive seal and a digital signature confirming that Scrive has witnessed the transaction. The purpose of Scrive witnessing the transaction is to strengthen the document evidence. By this procedure Scrive is never considered to be a signatory of the document and Scrive shall have no liability whatsoever regarding the validity, content or enforcement of such documents. Scrive can only be bound by the contents of a document if a registered or authorised signatory by a registered user of the Service in its own name and on behalf of Scrive signs a document through the Service.
      </p>
      <p>
        <strong>b. The Parties’ obligations:</strong>
        Once all Parties have signed a document through the Service, and document completion has been confirmed by the Service, the Parties accrue the rights and obligations in accordance with the agreements in the document. Scrive will never be responsible or obliged to monitor or intervene, should the signatories not abide by their obligations in the agreement or improperly exercise their rights as agreed in the signed document.
      </p>
      <p>
        <strong>c. Disputes between Parties:</strong>
        If any disagreement arises between the Parties regarding a document that was completed through Scrive, Scrive will have no liability or obligation with respect to this conflict except to, through the Scrive’s usual customer support, help the Parties to find and manage the Information available through the Service. For further evidence relating to the document in dispute Scrive provides the extra service Scrive Proof that give You the ability to, in accordance with our Special Extract Terms of Extracts, make extra extracts from the Service recorded evidence base.
      </p>
      <p>
        <strong>d. Document’s legal effect:</strong>
        All statements made by Scrive regarding the validity of documents signed electronically are not intended to be, and should not be interpreted as, legal advice. Scrive disclaims any responsibility to ensure that the documents signed by the Service are valid or enforceable under the laws of a particular country, state or other jurisdiction. If You wish to review the validity or enforceability of any documents You plan to sign or have signed through the Service, You should consult with appropriate legal expertise.
      </p>
      <h1>4. Our communication with You</h1>
      <p>By opening an account on the Service, You give permission to Scrive to, through the Service or through a representative, contact You via Your phone or email address. If You do not wish to receive our mailings via email, please send an email to support@scrive.com. Please note that Scrive will still need to communicate with You via email about Your transactions and other account related issues, and that these emails are not marketing and will not be eliminated by the above procedure.</p>
      <h1>5. Proprietary Rights</h1>
      <p>You acknowledge and agree that Scrive (or the Service’s licensors) own all property rights to the Service and all interests therein, including intellectual property rights contained in the Service (whether those rights are registered or not, and wherever in the World those rights may exist). Unless otherwise agreed in writing with Scrive nothing in the Terms gives You the right to use any of the Scrive trade names, trademarks, logos, domain names or other distinguishing marks. Except for the limited license granted in section 5a (“Feedback”), Scrive acknowledges and agrees that Scrive under these conditions obtains no right, title or interest from You (or Your licensors) in or to any content You submit, post, transmit or display on or through the Service, including intellectual property rights which subsist in that content (whether those rights are registered or not, and wherever in the world those rights may exist).</p>
      <p>
        <strong>a. Feedback:</strong>
        You agree that Scrive may, at its own option, publish and otherwise use Your comments and feedback, without payment or other obligations to You. You agree that any comments You make about Scrive directly to Scrive via email, via our contact form, or on our forums, may be used as attributes of the Service and may be used in our marketing without any payment or other obligation to You.
      </p>
      <h1>6. Assignment</h1>
      <p>Unless otherwise provided for in this Agreement, neither party shall transfer, assign or sublicense its rights under this Agreement to any other third party, in whole or in part, without the prior written consent of the other party. Notwithstanding the foregoing, a party may assign this Agreement in connection with its merger, reorganization, or sale of substantially all of its assets or capital stock.</p>
      <h1>7. Disclaimer</h1>
      <p>
        <strong>a. No legal advice:</strong>
        It is Your responsibility to evaluate the accuracy, completeness, or usefulness of any information, opinions, advice, documents, contracts, or other content available through Scrive. No part of the Service shall be regarded as legal advice. Neither Scrive nor its affiliates shall be liable for any errors or omissions in the content, or for the consequences of actions based on reliance on any content.
      </p>
      <p>
        <strong>b. Limitation of liability:</strong>
        You agree to hold Scrive and its parent companies, sister companies, subsidiaries, affiliates, service providers, other users, distributors, licensors, officers, directors and employees free from any claim or demand, including all attorneys’ fees, from You for any direct, indirect, random, special, following or specific injury, including but not limited to, damages for loss of profits, goodwill, use, data or other intangible losses (even if Scrive has been notified of this option), and including injuries resulting from: (i) documents or transactions submitted to the Service where Scrive has not been a direct party to the agreement, (ii) the use or inability to use, including errors/mistakes, interruptions or delays; unauthorised access to or alteration of Your documents or transactions or (iii) any other issue relating to the Service. Scrive’s responsibility shall not exceed, in total, the sum of Your total payments for the use of the Service under this agreement within the last 12-months period.
      </p>
      <h1>8. Applicable law and jurisdiction</h1>
      <p>Documents signed through the Service shall - unless otherwise specified in the document – be governed by and construed in accordance with Swedish law. These Terms and all issues concerning them and all issues relating to the Service are governed by and construed in accordance with Swedish law and must be examined by a Swedish court in the District Court of Stockholm. Regardless of the above procedure to resolve disagreements, we have the right to bring an action on outstanding debts in the courts having jurisdiction over You or any of the Your assets.
</p>
    </div>
  </div>
</section>
