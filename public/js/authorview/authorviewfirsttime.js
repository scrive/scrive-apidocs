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
      return true;
        return SessionStorage.get('FTUE', 'visitor') === 'true';
    },

    /**
     * Handles people who may be here from the design view in FTUE mode.
     */
    fromFTUEWelcome: function() {
        if (!this.isFromFTUE())
            return;

        this.noLongerFromFTUE(); // show the special focus on doc history + a call to action

        return this.fromFTUEContent();   
    },


    /**
     * Tell the user about the tracking capabilities of Scrive.
     *
     * @param {object} option Which of the possible subtitles that will be rendered. [1,3].
     */
    fromFTUEContent: function(option) {
        /* We want to highlight the first row of the table body. */
        $('body').addClass('ftue-authorview');

        mixpanel.track('FTUE Authorview shown');

	var container = $('<div></div>');
        container.append($('<h4 class="thanks"></h4>').html(localization.authorview.firsttime.docsent + '<br />' + localization.authorview.firsttime.dochistory));

        var table = $('<table></table>');
        var tableContainer = $('<div class="history-box"><div class="document-history-container"><div class="list-container" style="opacity: 1;"><div class="table"></div></div></div></div>');
        tableContainer.find('div.table').append(table);
        table.append($('<thead><tr><th style="width: 46px;"><span>Status</span></th><th style="width: 150px;"><span>Time</span></th><th style="width: 200px;"><span>Party</span></th><th style="width: 460px;"><span>Description</span></th></tr></thead>'));
        var tbody = $('<tbody></tbody>');
        table.append(tbody);

        var makeRow = function(status, time, party, text) {
          return $('<tr><td class="row "><div class="icon status '+status+'"></div></td><td class="row "><span>'+time+'</span></td><td class="row "><span>'+party+'</span></td><td class="row "><div style="margin-right:30px">'+text+'</div></td></tr>');
        };

        var statuses = [
          {delay: 0, status: 'delivered', party: 'Scrive', time: '08:49', text: localization.authorview.firsttime.emailDelivered},
          {delay: 1000, status: 'read', party: 'Scrive', time: '08:49', text: localization.authorview.firsttime.emailRead},
          {delay: 3000, status: 'opened', party: 'You', time: '08:49', text: localization.authorview.firsttime.linkOpened},
          {delay: 6000, status: 'signed', party: 'You', time: '08:49', text: localization.authorview.firsttime.documentSigned},
          {delay: 6100, status: 'sealed', party: 'Scrive', time: '08:49', text: localization.authorview.firsttime.documentSealed}
        ];

        for (var i = 0; i < statuses.length; i++) {
          (function(status) {
            setTimeout(function() {
              var tr = makeRow(status.status, status.time, status.party, status.text);
              tr.hide().prependTo(tbody).fadeIn();
            }, status.delay);
          })(statuses[i]);
        }

        container.append(tableContainer);

        var ctacontainer = $("<div class='cta-container'/>");

        ctacontainer.append($('<h4 class="explore-more"></h4>').text(localization.authorview.firsttime.tryYourself));

        ctacontainer.append(new Button({color: 'green', size: 'big', text: localization.authorview.firsttime.upload, shape: 'rounded', onClick: function() { 
          window.location.pathname = '/newdocument';
        }}).el())
        ctacontainer.append(new Button({color: 'black', size: 'big', text: localization.authorview.firsttime.contact, shape: 'rounded', onClick: function() {
          ctacontainer.empty();
          ctacontainer.append($('<h4></h4>').text(localization.authorview.firsttime.contactdetails));
        }}).el())
        ctacontainer.append(new Button({color: 'black', size: 'big', text: localization.authorview.firsttime.branding, shape: 'rounded', onClick: function() {
          window.location.pathname = '/account';
          window.location.hash = '#branding-email';
        }}).el())

        container.append(ctacontainer);

        return container;
    }
};

})(window);
