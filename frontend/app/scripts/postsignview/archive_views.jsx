/** @jsx React.DOM */

define(['React', 'Backbone', 'postsignview/user_service', 'common/language_service', 'legacy_code'], function(React, Backbone, UserService, LanguageService) {

  return React.createClass({
    propTypes: {
      document: React.PropTypes.object.isRequired
    },

    /**
     *  @description
     *  Upload sample pdf for next step in user flow (designview)
     *  and redirect to FTUE when sample pdf have been created.
     */
    uploadSampleAndRedirect: function() {
      var samplePdfUrl = function() {
	var language = LanguageService.currentLanguage();
	var url = '/pdf/sample_document_loremipsumcontract_';
	url += language;
	url += '.base64.pdf';
	return url;
      };
      
      var title = localization.ftue.documentTitle;

      DocumentUploader.uploadByURL(function(documentData) {
	window.location.pathname = '/d/' + documentData.id;
      }, function() {		   
	// In case of fail, just throw them into design view.                                      
	SessionStorage.set('welcome', 'accepted', false);
	window.location.pathname = '/newdocument';
      }, samplePdfUrl(), title);
    },

    /**
     *  @description
     *  Prepare kontraktcja for designview-ftue.
     *  And then redirect to designview in kontraktcja.
     */
    sendExampleDocument: function() {
      mixpanel.register({
	'Welcome modal accepted': true
      });
      
      mixpanel.track('Welcome modal accept');
      LoadingDialog.open();
      SessionStorage.set('welcome', 'accepted', true);

      this.uploadSampleAndRedirect();
    },
    
    render: function() {
      return (
          <div className="wrapper-position-footer">
            <header className="site ">
              <nav>
                <ul className="ct ">
                  <li id="branding">
                    <a id="logo" className="page" href="/">
                      <img src="/img/logo.png"/>
                    </a>
                    <div id="tagline" className="h3"></div>
                  </li>
                </ul>
              </nav>
            </header>
            <div className="mainContainer">
              <div className="body-container">
                <div className="blocking-box"></div>
                <div className="archive psv-archive">
                  <div>
                    <div className="tab-viewer">
                      <div className="list-container"><div className="table">
                          <table>
                            <thead>
                              <tr>
                                <th><div className="checkbox"></div></th>
                                <th><span className="sortable">{ localization.archive.documents.columns.status }</span></th>
                                <th><span className="sortable">{ localization.archive.documents.columns.time }</span></th>
                                <th><span></span></th>
                                <th><span className="sortable">{ localization.archive.documents.columns.sender }</span></th>
                                <th><span></span></th>
                                <th><span className="sortable">{ localization.archive.documents.columns.party }</span></th>
                                <th><span>{ localization.archive.documents.columns.title }</span></th>
                              </tr>
                            </thead>
                            <tbody className="selectable">
                              <tr>
                                <td className="row">
                                  <div className="checkbox"></div>
                                </td>
                                <td className="row">
                                  <div className="icon status signed"></div>
                                </td>
                                <td className="row"><span>{ this.props.document.documentTime() }</span></td>
                                <td className="row"></td>
                                <td className="row"><a>{ this.props.document.author() }</a></td>
                                <td className="row"></td>
                                <td className="row"><a className="expand">{ this.props.document.party() }</a></td>
                                <td className="row"><a className="s-archive-document-title">{ this.props.document.title() }</a></td>
                              </tr>
                            </tbody>
                          </table>
                          <div>
                            <div className="table-paginate">
                              <div className="pages">
                                <div></div>
                              </div>
                            </div>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>

		  {/*if*/ !BrowserInfo.isPadDevice() && !BrowserInfo.isSmallScreen() &&
		  <div className="new-post-sign-view-user">
		    <div className="inner-container">
		      <img src="/img/arrow-grey.png" className="grey-arrow" />
		      <h2>{ localization.archive.ftue.title }</h2>
		      <div className="subtitle">
			<h5>{ localization.archive.ftue.subtitleInJustAFewMinutes }</h5>
			<h5 className="great-thing">{ localization.archive.ftue.subtitleReviewed }</h5>
			<h5 className="great-thing">{ localization.archive.ftue.subtitleReturned }</h5>
			<h5 className="great-thing">{ localization.archive.ftue.subtitleSaved }</h5>
			<h5>{ localization.archive.ftue.subtitleIsntThisHowSimple }</h5>
		      </div>
		      <a onClick={ this.sendExampleDocument } className="design-view-document-buttons-upload-button green button button-large button-green button-round">
			<div className="label">{ localization.archive.ftue.button }</div>
		      </a>
		    </div>
		  </div>
		  }

                </div>
              </div>
            </div>
          </div>
      );
    }
  });
});
