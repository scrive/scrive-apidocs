/** @jsx React.DOM */

define(['legacy_code', 'React','common/htmltextwithsubstitution'], function(_Legacy, React, HtmlTextWithSubstitution) {

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
              {_.map(authorattachments, function(a,i) {
                return (
                  <tr key={i}>
                    <td className='icon-td'>
                      <div className={a.isRequired() ? "required-author-attachment-icon" : "optional-author-attachment-icon"}/>
                    </td>
                    <td className='name-td'>
                       <HtmlTextWithSubstitution
                            secureText={localization.designview.attached}
                            subs={{
                              ".put-attachment-name" : a.name()
                            }}
                          />
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
                  return _.map(sig.attachments(),function(a,i) {
                    var sigName = sig.nameOrEmail();
                    if (sig.isCsv())
                      sigName = localization.csv.title;
                    if (sigName == "")
                      sigName = sig.nameInDocument();
                    return (
                      <tr key={i}>
                        <td className='icon-td'>
                          <div className='signatory-attachment-icon'/>
                        </td>
                        <td className='name-td'>
                          <HtmlTextWithSubstitution
                            secureText={localization.designview.attachmentRequestedFrom}
                            subs={{
                              ".put-attachment-name" : a.name(),
                              ".put-person-name" : sigName.trim()
                            }}
                          />
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


