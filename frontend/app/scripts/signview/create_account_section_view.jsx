/** @jsx React.DOM */

define(['React', 'common/language_service', 'postsignview/questionnaire_view', 'postsignview/create_account_views', 'postsignview/user_service', 'Backbone', 'Underscore', 'legacy_code'], function(React, LanguageService, QuestionareView, CreateAccountViews, UserService, Backbone, _) {
  var expose = {};

  /**
   *  @description
   *  Render different types of create account section on postsignview,
   *  depending on a few different conditions.
   *
   *
   */
  return React.createClass({
    propTypes: {
      document : React.PropTypes.object.isRequired
    },
    isHidden : function() {
      if (BrowserInfo.isIE7orLower())
        return true;
      if (null !== /^avis.*scrive.com/.exec(location.host))
        return true;
      if (null !== /^budget.*scrive.com/.exec(location.host))
        return true;
      //HACK. We should have such option per account. Maybe like this: https://trello.com/c/yKh4n1sG/115-purge-documents-linger-time-should-be-configurable-unionen-v3-t3
      if (this.props.document.author().company() == 'Stanley Security Solutions')
        return true;
    },
    isNJ : function() {
      return null !== /^nj.*scrive.com/.exec(location.host);
    },
    isNotInterestedInQuestionnaire: function() {
      // We need to tell this environment somehow that this is a branded domain. This was a quicky fix for LoxySoft and has to be reworked.
      return null !== /^(backbone|loxysoft|caspeco).scrive.com/.exec(location.host);
    },
    isQuestionaire : function() {
      return (!this.isNotInterestedInQuestionnaire() && this.props.document.currentSignatory().company() !== '');
    },
    isSaveCopy : function() {
      return !this.isHidden() && !this.isNJ() && !this.isQuestionaire();
    },
    componentWillMount : function() {
      var promotionName = "";
      if (this.isNJ()) {
        promotionName = "NJ";
      }
      else if (this.isQuestionaire()) {
        promotionName = "Questionnaire";
      }
      mixpanel.track("Store copy button shown", promotionName ? { promo: promotionName } : {});
    },
    render: function() {
      var document = this.props.document;
      if(this.isHidden()) {
        return (<div/>);
      } else if(this.isNJ()) {
        var BrandedBanner = CreateAccountViews.BrandedBanner;
        return (<BrandedBanner
           bannerType='nj'
           language={LanguageService.currentLanguage()}
           registerUser={function() {
             UserService.registerUser(document).then(function() {
               mixpanelPeopleSetTimeout(
                  {
                   'Accepted Promotion': true,
                   'Promotion': 'NJ'
                  },
                 function() {
                   window.location.pathname = '/newdocument';
                 },
                 300);
             });
           }}
         />
        );
      } else if(this.isQuestionaire()) {
        return (<QuestionareView document={document}/>);
      } else if (this.isSaveCopy()) {
        var SaveBackupCopy = CreateAccountViews.SaveBackupCopy;
        return (<SaveBackupCopy
          isSmallScreen = {BrowserInfo.isSmallScreen()}
          registerUser =  {function() {
            UserService.registerUser(document).then(function() {
              window.location = '/postsignview';
            });
          }}
        />);
      }
    }
  });
});
