/** @jsx React.DOM */

define(['React', 'Backbone', 'common/common_views', 'postsignview/user_service', 'common/react_mixins', 'common/filters', 'common/utilities_service', 'legacy_code'], function(React, Backbone, CommonViews, UserService, ReactMixins, filters, UtilitiesService) {

  // Legacy dependencies: '/js/signview/docsignviewbranding.js', 
  //     '/js/storage.js', '/js/loading.js', '/js/utils/time.js', '/js/documents',

  var expose = {};

  expose.LandingPage = React.createClass({
    propTypes: {
      documentId: React.PropTypes.string.isRequired,
      signatoryId: React.PropTypes.string.isRequired
    },
    
    render: function() {
      return (
	<LandingPageView documentId={this.props.documentId} signatoryId={this.props.signatoryId} />
      );
    }
  });

  var LandingPageView = React.createClass({
    propTypes: {
      documentId: React.PropTypes.string.isRequired,
      signatoryId: React.PropTypes.string.isRequired
    },

    dataLoaded: false,

    componentWillMount: function() {
      var viewer = new DocumentViewer({
        signatoryid: this.props.signatoryId
      })

      this.currentDocument = new Document({
        id: this.props.documentId,
        readOnlyView: true,
        evidenceAttachments: true,
	viewer: viewer
      });
      this.currentDocument.recall();

      this.branding = new BrandingForSignView({
	documentid: this.props.documentId,
	signatoryid: this.props.signatoryId
      });

      this.branding.fetch({
	processData: true,
	cache : false
      });
    },

    componentDidMount: function() {
      this.dataLoaded = true;
    },

    mixins: [ReactMixins.ModelForceUpdate],
    getBackboneModels: function() {
      return [this.branding];
    },

    registerAndRedirect: function() {
      UserService.registerUser(this.currentDocument).then(function() {
	App.router.navigate('/postsignview/save-safety-copy-step2', true);
      });
    },
    
    render: function() {
      if(!this.dataLoaded) {
	return (<div />);
      }

      // This boilerplate will be removed when this PR is merged to React master
      // https://github.com/facebook/react/pull/760
      var Header = CommonViews.Header;
      var Footer = CommonViews.Footer;

      return (
	<div className='signview'>
	  <Header branding={this.branding} />
	  <LandingPageContent currentDocument={this.currentDocument} registerUser={this.registerAndRedirect} />
	  <Footer branding={this.branding} />
	</div>
      );
    }
  });

  
  var LandingPageContent = React.createClass({
    propTypes: {
      registerUser: React.PropTypes.func.isRequired,
    },
    
    render: function() {

      return (
          <div className="container psv-landing-page">
            
            <div className="fold fold-grey">
              
              <div className="row main-header">
                <div className="col-md-offset-2 col-md-8">
                  <h2>Become an even greater sales manager!</h2>
                  <h3>Scrive will help you increase sales {'&'} get more control over your company{"'"}s deals with e-signing {'&'} contract-management tools.</h3>
                </div>
              </div>
              
              <div className="row">
                <div className="col-md-1">
                  <img src="http://scrive.com/images/site/quote-face-1.png"></img>
                </div>
                <div className="quote col-md-4">
                  <h4><span className="big-quotation big-quotation-left">”</span>Genom att koppla samman signering och arkivering kan våra kunder kvalitetssäkra verksamheten.<span className="big-quotation big-quotation-right">”</span></h4>
                  <p>- Dag Brodin, VP Legal, Norstedts Juridik, Scrive reseller.</p>
                </div>
                <div className="col-md-1">
                  <img src="http://scrive.com/images/site/quote-face-1.png"></img>
                </div>
                <div className="quote col-md-4">
                  <h4><span className="big-quotation big-quotation-left">”</span>Genom att koppla samman signering och arkivering kan våra kunder kvalitetssäkra verksamheten.<span className="big-quotation big-quotation-right">”</span></h4>
                  <p>- Dag Brodin, VP Legal, Norstedts Juridik, Scrive reseller.</p>
                </div>
              </div>
              
              <div className="signup-buttons row">
                <div className="col-md-offset-4 col-md-5">
                  <a className="button button-green button-large" onClick={this.props.registerUser}>Signup for free</a>
                  <span className="text-between-buttons">OR</span>
                  <a className="button button-gray button-large">Request a demo</a>
                </div>
              </div>
              
            </div>
            
            <div className="fold fold-white">
              
              <div className="row">
                
                <div className="col-md-6">
                  <h1>Are your deals taking a long time to close?</h1>
                  
                  <div>
                    <h2><span className="icon-clock"></span>Close deals faster</h2>
                    <p>Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo.</p>
                  </div>
                  
                  
                  <div>
                    <h2><span className="icon-calendar"></span>Shorten cycle times</h2>
                    <p>Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo.</p>
                  </div>
                  
                  <div>
                    <h2><span className="icon-shart-curve"></span>Increase conversion</h2>
                    <p>Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo.</p>
                    
                  </div>
                </div>
                
                <div className="col-md-5">
                  <img src="http://scrive.com/images/site/quote-face-1.png"></img>
                </div>
              </div>
            </div>
            
            <div className="fold fold-grey">
              <div className="row">
                <div className="col-md-6">
                  <img src="http://scrive.com/images/site/quote-face-1.png"></img>
                </div>
                
                <div className="col-md-5">
                  <h2>Do you have lots of deals to keep track of?</h2>
                  <h3><span className="icon-two-documents"></span>Organize your documents in one place</h3>
                  <p>Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo.</p>
                </div>
              </div>
            </div>
            
            <div className="fold fold-white">
              <div className="row">
                <div className="col-md-5">
                  <h2>...and a lot of people to manage?</h2>
                  <h3><span className="icon-two-people"></span>Keep an overview of your salespeople and all their deals</h3>
                  <p>Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo.</p>
                </div>
                
                <div className="col-md-6">
                  <img src="http://scrive.com/images/site/quote-face-1.png"></img>
                </div>
              </div>
              <div className="signup-buttons row">
                <div className="col-md-offset-4 col-md-5">
                  <a className="button button-green button-large" onClick={this.registerUser}>Signup for free</a>
                  <span className="text-between-buttons">OR</span>
                  <a className="button button-gray button-large">Request a demo</a>
                </div>
              </div>
            </div>
            
          </div>
      );
    }
  });
  
  expose.ArchivePSV = React.createClass({
    propTypes: {
      document: React.PropTypes.object.isRequired,
    },

    /**
     *  @description
     *  Upload sample pdf for next step in user flow (designview)
     *  and redirect to FTUE when sample pdf have been created.
     */
    uploadSampleAndRedirect: function() {
      var samplePdfUrl = function() {
	var language = UtilitiesService.getCurrentLanguage();
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
      var documentTimeRaw = this.props.document.get('time'),
      documentTime = new Date(Date.parse(documentTimeRaw)).fullTime();
      
      var promotion;
      if(!BrowserInfo.isPadDevice() && !BrowserInfo.isSmallScreen()) {
        promotion = (
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
        );
      }
            
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
                                <td className="row"><span>{ documentTime }</span></td>
                                <td className="row"></td>
                                <td className="row"><a>{ this.props.document.get('author') }</a></td>
                                <td className="row"></td>
                                <td className="row"><a className="expand">{ filters.truncate(this.props.document.get('party'), 40) }</a></td>
                                <td className="row"><a className="s-archive-document-title">{ filters.truncate(this.props.document.get('title'), 40) }</a></td>
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
                  { promotion }
                </div>
              </div>
            </div>
          </div>
      );
    }
  });
  

 
  return expose;
});
