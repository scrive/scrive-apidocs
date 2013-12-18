/* 
 * This file defines the content that will be presented where usually the authors view lives.
 */

(function(window){

window.AuthorViewFirstTime = {

    /**
     * Resets the "from FTUE" indicator
     */
    noLongerFromFTUE: function() {
        SessionStorage.set('FTUE', 'visitor', false);
    },

    /**
     * Return true if the user got here from the FTUE
     */
    isFromFTUE: function() {
        return SessionStorage.get('FTUE', 'visitor') === 'true';
    },

    /**
     * Mark this user session as a FTUE session.
     */
    markAsFTUE: function() {
      SessionStorage.set('FTUE', 'visitor', true);
    },


    /**
     * Tell the user about the tracking capabilities of Scrive.
     */
    fromFTUEContent: function() {
        /* We want to highlight the first row of the table body. */
        $('body').addClass('ftue-authorview');

        mixpanel.track('FTUE Authorview shown');

	var container = $('<div></div>');
        container.append($('<h4 class="thanks"></h4>').html(localization.authorview.firsttime.docsent + '<br /><br />' + localization.authorview.firsttime.dochistory));

        container.append(this.fakeHistoryBox());

        container.append(this.callToAction());

        return container;
    },
    fakeHistoryBox: function() {
        // This is a copy of the history box that's usually in the regular authorview.
        var tableContainer = $('<div class="history-box"><div class="document-history-container"><div class="list-container" style="opacity: 1;"><div class="table"><table><thead><tr><th style="width: 46px;"><span>Status</span></th><th style="width: 150px;"><span>'+localization.history.time+'</span></th><th style="width: 200px;"><span>'+localization.history.party+'</span></th><th style="width: 460px;"><span>'+localization.history.description+'</span></th></tr></thead><tbody></tbody></table></div></div></div></div>');
        var tbody = tableContainer.find('tbody');

        var makeRow = function(status, time, party, text) {
          return $('<tr><td class="row "><div class="icon status '+status+'"></div></td><td class="row "><span>'+time+'</span></td><td class="row "><span>'+party+'</span></td><td class="row "><div style="margin-right:30px">'+text+'</div></td></tr>');
        };

        var fakeUser = {name: 'John', email: 'john@john.com'};

        var statuses = [
          {delay: 0, status: 'delivered', party: 'Scrive', time: '08:40', text: localization.authorview.firsttime.emailDelivered},
          {delay: 1000, status: 'read', party: 'Scrive', time: '08:40', text: localization.authorview.firsttime.emailRead},
          {delay: 3000, status: 'opened', party: fakeUser.name, time: '08:41', text: localization.authorview.firsttime.linkOpened},
          {delay: 6000, status: 'signed', party: fakeUser.name, time: '08:41', text: localization.authorview.firsttime.documentSigned},
          {delay: 6100, status: 'sealed', party: 'Scrive', time: '08:41', text: localization.authorview.firsttime.documentSealed}
        ];

        for (var i = 0; i < statuses.length; i++) {
          (function(status) {
            setTimeout(function() {
              var tr = makeRow(status.status, status.time, status.party, status.text.replace('ACTOR', fakeUser.name).replace('EMAIL', fakeUser.email));
              tr.hide().prependTo(tbody).fadeIn();
            }, status.delay);
          })(statuses[i]);
        }

        return tableContainer;
    },
    callToAction: function() {
        var ctacontainer = $("<div class='cta-container'/>");

        ctacontainer.append($('<h4 class="explore-more"></h4>').text(localization.authorview.firsttime.tryYourself));

        ctacontainer.append(new Button({color: 'green', size: 'big', text: localization.authorview.firsttime.upload, shape: 'rounded', onClick: function() { 
          window.location.pathname = '/newdocument';
        }}).el())
        ctacontainer.append(new Button({color: 'black', size: 'big', text: localization.authorview.firsttime.contact, shape: 'rounded', cssClass: 'contact', onClick: function() {
          mixpanel.track('FTUE Authorview contact clicked');
          ctacontainer.find('.button.contact').replaceWith($('<h5 class="contact"></h5>').html(localization.authorview.firsttime.contactdetails));
        }}).el())
        ctacontainer.append(new Button({color: 'black', size: 'big', text: localization.authorview.firsttime.branding, shape: 'rounded', onClick: function() {
          window.location.href = '/account#branding-email';
        }}).el())

        return ctacontainer;
    }

};

})(window);
