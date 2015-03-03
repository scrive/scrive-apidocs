/** @jsx React.DOM */

define(['legacy_code', 'React'], function(_Legacy, React) {

return React.createClass({
  render: function() {
    var self = this;
    var document = self.props.document;
    var authorattachments = document.authorattachments();
    var sattachments = _.flatten(_.map(document.signatories(),function(s) {return s.attachments()}));
    if (authorattachments.length == 0 && sattachments.length == 0 ) {
      return (<div/>);
    } else {
      return (
        <div className="designview-attachemnts-list">
          <table>
            <thead>
              <tr>
                <th className='icon-td'  />
                <th className='name-td'  />
                <th className='remove-td'/>
              </tr>
            </thead>
            <tbody>
              {_.map(authorattachments, function(a) {
                return (
                  <tr>
                    <td className='icon-td'>
                      <div className='author-attachment-icon'/>
                    </td>
                    <td className='name-td'>
                      {localization.designview.attached + " " + a.name()}
                    </td>
                    <td className='remove-td'>
                      <div
                        className='remove-icon'
                        onClick={function() {
                          document.removeattachment(a);
                          mixpanel.track('Click remove attachment', {  Type: 'Author'});
                        }}
                      />
                    </td>
                  </tr>
                );
              })}
              { _.map(document.signatories(),function(sig) {
                  return _.map(sig.attachments(),function(a) {
                    var sigName = sig.nameOrEmail();
                    if (sig.isCsv())
                      sigName = localization.csv.title;
                    if (sigName == "")
                      sigName = sig.nameInDocument();
                    var nameDiv = $("<div>" + localization.designview.attachmentRequestedFrom + "</div>");
                    $('.put-attachment-name',name).text(a.name());
                    $('.put-person-name',name).text(sigName.trim());
                    return (
                      <tr>
                        <td className='icon-td'>
                          <div className='signatory-attachment-icon'/>
                        </td>
                        <td className='name-td'>
                          {nameDiv.text()}
                        </td>
                        <td className='remove-td'>
                          <div
                            className='remove-icon'
                            onClick={function() {
                              sig.removeAttachment(a);
                              mixpanel.track('Click remove attachment', {
                                Type: 'Signatory'
                              });
                            }}
                          />
                        </td>
                      </tr>
                    );
                  });
                })
              }
            </tbody>
          </table>
        </div>
      );
    }
  }
});


});


