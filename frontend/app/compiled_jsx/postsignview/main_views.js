/** @jsx React.DOM */

define(['React', 'Backbone', 'common/common_views', 'postsignview/user_service', 'common/react_mixins', 'common/filters', 'legacy_code'], function(React, Backbone, CommonViews, UserService, ReactMixins, filters) {

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
	LandingPageView( {documentId:this.props.documentId, signatoryId:this.props.signatoryId} )
      );
    }
  });

  var LandingPageView = React.createClass({displayName: 'LandingPageView',
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
      UserService.registerUser(currentDocument).then(function() {
	App.router.navigate('/postsignview/save-safety-copy-step2', true);
      });
    },
    
    render: function() {
      if(!this.dataLoaded) {
	return (React.DOM.div(null ));
      }

      // This boilerplate will be removed when this PR is merged to React master
      // https://github.com/facebook/react/pull/760
      var Header = CommonViews.Header;
      var Footer = CommonViews.Footer;

      return (
	React.DOM.div( {className:"signview"}, 
	  Header( {branding:this.branding} ),
	  LandingPageContent( {currentDocument:this.currentDocument, registerUser:this.registerAndRedirect} ),
	  Footer( {branding:this.branding} )
	)
      );
    }
  });

  
  var LandingPageContent = React.createClass({displayName: 'LandingPageContent',
    propTypes: {
      registerUser: React.PropTypes.func.isRequired,
    },
    
    render: function() {

      return (
          React.DOM.div( {className:"container psv-landing-page"}, 
            
            React.DOM.div( {className:"fold fold-grey"}, 
              
              React.DOM.div( {className:"row main-header"}, 
                React.DOM.div( {className:"col-md-offset-2 col-md-8"}, 
                  React.DOM.h2(null, "Become an even greater sales manager!"),
                  React.DOM.h3(null, "Scrive will help you increase sales ", '&', " get more control over your company","'","s deals with e-signing ", '&', " contract-management tools.")
                )
              ),
              
              React.DOM.div( {className:"row"}, 
                React.DOM.div( {className:"col-md-1"}, 
                  React.DOM.img( {src:"http://scrive.com/images/site/quote-face-1.png"})
                ),
                React.DOM.div( {className:"quote col-md-4"}, 
                  React.DOM.h4(null, React.DOM.span( {className:"big-quotation big-quotation-left"}, "”"),"Genom att koppla samman signering och arkivering kan våra kunder kvalitetssäkra verksamheten.",React.DOM.span( {className:"big-quotation big-quotation-right"}, "”")),
                  React.DOM.p(null, "- Dag Brodin, VP Legal, Norstedts Juridik, Scrive reseller.")
                ),
                React.DOM.div( {className:"col-md-1"}, 
                  React.DOM.img( {src:"http://scrive.com/images/site/quote-face-1.png"})
                ),
                React.DOM.div( {className:"quote col-md-4"}, 
                  React.DOM.h4(null, React.DOM.span( {className:"big-quotation big-quotation-left"}, "”"),"Genom att koppla samman signering och arkivering kan våra kunder kvalitetssäkra verksamheten.",React.DOM.span( {className:"big-quotation big-quotation-right"}, "”")),
                  React.DOM.p(null, "- Dag Brodin, VP Legal, Norstedts Juridik, Scrive reseller.")
                )
              ),
              
              React.DOM.div( {className:"signup-buttons row"}, 
                React.DOM.div( {className:"col-md-offset-4 col-md-5"}, 
                  React.DOM.a( {className:"button button-green button-large", onClick:this.props.registerUser}, "Signup for free"),
                  React.DOM.span( {className:"text-between-buttons"}, "OR"),
                  React.DOM.a( {className:"button button-gray button-large"}, "Request a demo")
                )
              )
              
            ),
            
            React.DOM.div( {className:"fold fold-white"}, 
              
              React.DOM.div( {className:"row"}, 
                
                React.DOM.div( {className:"col-md-6"}, 
                  React.DOM.h1(null, "Are your deals taking a long time to close?"),
                  
                  React.DOM.div(null, 
                    React.DOM.h2(null, React.DOM.span( {className:"icon-clock"}),"Close deals faster"),
                    React.DOM.p(null, "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo.")
                  ),
                  
                  
                  React.DOM.div(null, 
                    React.DOM.h2(null, React.DOM.span( {className:"icon-calendar"}),"Shorten cycle times"),
                    React.DOM.p(null, "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo.")
                  ),
                  
                  React.DOM.div(null, 
                    React.DOM.h2(null, React.DOM.span( {className:"icon-shart-curve"}),"Increase conversion"),
                    React.DOM.p(null, "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo.")
                    
                  )
                ),
                
                React.DOM.div( {className:"col-md-5"}, 
                  React.DOM.img( {src:"http://scrive.com/images/site/quote-face-1.png"})
                )
              )
            ),
            
            React.DOM.div( {className:"fold fold-grey"}, 
              React.DOM.div( {className:"row"}, 
                React.DOM.div( {className:"col-md-6"}, 
                  React.DOM.img( {src:"http://scrive.com/images/site/quote-face-1.png"})
                ),
                
                React.DOM.div( {className:"col-md-5"}, 
                  React.DOM.h2(null, "Do you have lots of deals to keep track of?"),
                  React.DOM.h3(null, React.DOM.span( {className:"icon-two-documents"}),"Organize your documents in one place"),
                  React.DOM.p(null, "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo.")
                )
              )
            ),
            
            React.DOM.div( {className:"fold fold-white"}, 
              React.DOM.div( {className:"row"}, 
                React.DOM.div( {className:"col-md-5"}, 
                  React.DOM.h2(null, "...and a lot of people to manage?"),
                  React.DOM.h3(null, React.DOM.span( {className:"icon-two-people"}),"Keep an overview of your salespeople and all their deals"),
                  React.DOM.p(null, "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo.")
                ),
                
                React.DOM.div( {className:"col-md-6"}, 
                  React.DOM.img( {src:"http://scrive.com/images/site/quote-face-1.png"})
                )
              ),
              React.DOM.div( {className:"signup-buttons row"}, 
                React.DOM.div( {className:"col-md-offset-4 col-md-5"}, 
                  React.DOM.a( {className:"button button-green button-large", onClick:this.registerUser}, "Signup for free"),
                  React.DOM.span( {className:"text-between-buttons"}, "OR"),
                  React.DOM.a( {className:"button button-gray button-large"}, "Request a demo")
                )
              )
            )
            
          )
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
	var language = 'SV'; // TODO(jens): should be set dynamically
	var url = '/pdf/sample_document_loremipsumcontract_';
	url += language;
	url += '.base64.pdf';
	return url;
      };
      
      var title = localization.welcomeNewUser.documentTitle;

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
            React.DOM.div( {className:"new-post-sign-view-user"}, 
              React.DOM.div( {className:"inner-container"}, 
                React.DOM.img( {src:"/img/arrow-grey.png", className:"grey-arrow"} ),
                React.DOM.h2(null,  localization.archive.ftue.title ),
                React.DOM.div( {className:"subtitle"}, 
                  React.DOM.h5(null,  localization.archive.ftue.subtitleInJustAFewMinutes ),
                  React.DOM.h5( {className:"great-thing"},  localization.archive.ftue.subtitleReviewed ),
                  React.DOM.h5( {className:"great-thing"},  localization.archive.ftue.subtitleReturned ),
                  React.DOM.h5( {className:"great-thing"},  localization.archive.ftue.subtitleSaved ),
                  React.DOM.h5(null,  localization.archive.ftue.subtitleIsntThisHowSimple )
                ),
                React.DOM.a( {onClick: this.sendExampleDocument,  className:"design-view-document-buttons-upload-button green button button-large button-green button-round"}, 
                  React.DOM.div( {className:"label"},  localization.archive.ftue.button )
                )
              )
            )
        );
      }
            
      return (
          React.DOM.div( {className:"wrapper-position-footer"}, 
            React.DOM.header( {className:"site " }, 
              React.DOM.nav(null, 
                React.DOM.ul( {className:"ct " }, 
                  React.DOM.li( {id:"branding"}, 
                    React.DOM.a( {id:"logo", className:"page", href:"/"}, 
                      React.DOM.img( {src:"/img/logo.png", height:"23", width:"120"} )
                    ),
                    React.DOM.div( {id:"tagline", className:"h3"})
                  )
                )
              )
            ),
            React.DOM.div( {className:"mainContainer"}, 
              React.DOM.div( {className:"body-container"}, 
                React.DOM.div( {className:"blocking-box"}),
                React.DOM.div( {className:"archive"}, 
                  React.DOM.div(null, 
                    React.DOM.div( {className:"tab-viewer"}, 
                      React.DOM.div( {style:{opacity: '1', display: 'block'}, className:"list-container"}, React.DOM.div( {className:"table"}, 
                          React.DOM.table(null, 
                            React.DOM.thead(null, 
                              React.DOM.tr(null, 
                                React.DOM.th( {style:{width: '30px'}}, 
                                  React.DOM.div( {className:"checkbox"})
                                ),
                                React.DOM.th( {style:{width: '62px'}}, React.DOM.span( {className:"sortable"},  localization.archive.documents.columns.status )),
                                React.DOM.th( {style:{width: '105px'}}, React.DOM.span( {className:"sortable"},  localization.archive.documents.columns.time )),
                                React.DOM.th( {style:{width: '5px'}}, React.DOM.span(null)),
                                React.DOM.th( {style:{width: '140px'}}, React.DOM.span( {className:"sortable"},  localization.archive.documents.columns.sender )),
                                React.DOM.th( {style:{width: '5px'}}, React.DOM.span(null)),
                                React.DOM.th( {style:{width: '210px'}}, React.DOM.span( {className:"sortable"},  localization.archive.documents.columns.party )),
                                React.DOM.th( {style:{width: '230px'}}, React.DOM.span(null,  localization.archive.documents.columns.title ))
                              )
                            ),
                            React.DOM.tbody( {className:"selectable"}, 
                              React.DOM.tr(null, 
                                React.DOM.td( {className:"row"}, 
                                  React.DOM.div( {className:"checkbox"})
                                ),
                                React.DOM.td( {className:"row"}, 
                                  React.DOM.div( {className:"icon status signed"})
                                ),
                                React.DOM.td( {className:"row"}, React.DOM.span(null,  documentTime )),
                                React.DOM.td( {className:"row"}),
                                React.DOM.td( {className:"row"}, React.DOM.a(null,  this.props.document.get('author') )),
                                React.DOM.td( {className:"row"}),
                                React.DOM.td( {className:"row"}, React.DOM.a( {className:"expand"},  filters.truncate(this.props.document.get('party'), 40) )),
                                React.DOM.td( {className:"row"}, React.DOM.a( {className:"s-archive-document-title"},  filters.truncate(this.props.document.get('title'), 40) ))
                              )
                            )
                          ),
                          React.DOM.div(null, 
                            React.DOM.div( {className:"table-paginate"}, 
                              React.DOM.div( {className:"pages"}, 
                                React.DOM.div(null)
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                   promotion 
                )
              )
            )
          )
      );
    }
  });
  

 
  return expose;
});
