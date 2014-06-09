* Prerequisites

On the target server you need:
* an account
** with enterprise company account (so it's not blocked after 3 docs)
** write down this account's company id (need to pass this to run.sh)
** in account->integration settings enable personal access credentials (and write them down)
* template
** any doc
** author
*** in person delivery method
*** viewing
*** without confirmation
** signatory
*** signing
*** empty details
*** in person delivery
*** without confirmation
** fields
*** unchecked checkbox-1, obligatory, for signatory party1
*** custom field named 'field' placed somewhere, obligatory, for signatory party1
*** signature-2 placed, obligatory, for signatory party1
*** signature-1 not placed (bug?)
** save as template, write down templateid
* a file
** write down its path
** and the number of its pages

* Scenario

This scenario simulates on the target backend server the following process:
* creating a doc from a template using API call
* get API call on that document
* changemainfile API call (to change a document for the one you want to sign)
* update API call, setting the following things:
** signatory details (fstname, lastname, email, mobile)
** success redirect URL for signatory
** delivery method API
* another get API call
* ready API call
* simulated browser signview load for signatory
** load signlink, set cookies, follow redirect, download all resources that are set in html
** load all other resources that are not set in html (e.g. js dependencies, images that result from css rules that are used in signview)
** load signview branding of authors company
** load company logo
** load info about documents evidence attachments
** load all pages 
* simulated browser signing
** checksign API call
** sign API call

* Running

* Make sure you have jmeter in your path and use run.sh (if you open Avis.jmx in jmeter GUI you can tweak number of requests in Thread Group options)
* after the test is done, in /tmp/avis.jtl you have results that can be viewed in jmeter (by visiting Response Time Graph and loading that file)
