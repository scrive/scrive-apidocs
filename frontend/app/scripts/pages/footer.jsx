/** @jsx React.DOM */


define(['React', 'Backbone', 'common/select', 'pages/languageselect'], function(React, Backbone, Select,LanguageSelect) {
  var LoggedInFooter = React.createClass({
    propTypes: {
      thin : React.PropTypes.bool,
      color :  React.PropTypes.string,
      textcolor :  React.PropTypes.string,
      highlightcolor : React.PropTypes.string
    },
    render: function() {
     var mainStyle =  {background : this.props.color, borderTopColor: this.props.highlightcolor};
     var labelStyle = this.props.color ? {textShadow : "none", color : this.props.textcolor} : {};
     return (
       <footer
         className={"site logged-in" + (this.props.thin ? "thin" : "")}
         style={mainStyle}
         >
        <div className="poweredbyscrive">
          {/*if*/ this.props.color &&
            <div className="poweredbyscrive">
              <span className="text" >
                Powered by Scrive
              </span>
            </div>
          }
          {/*else*/ !this.props.color &&
            <div className="poweredbyscrive">
              <div className="text">
                Powered by <div className="logo"/>
              </div>
            </div>
          }
        </div>
       </footer>
     );
    }
  });

  var NotLoggedInFooter = React.createClass({
    propTypes: {
      thin : React.PropTypes.bool,
      httplink :  React.PropTypes.string,
      langprefix : React.PropTypes.string,
      color :  React.PropTypes.string,
      textcolor :  React.PropTypes.string,
      highlightcolor : React.PropTypes.string,
      textcolor :  React.PropTypes.string
    },
    // We don't have static pages for some languages. In that case we should redirect people to version in english.
    langprefixForStaticPages : function() {
      if (this.props.langprefix == "/en/" || this.props.langprefix == "/sv/" )
        return this.props.langprefix;
      return "/en/";
    },
    render: function() {
     var mainStyle =  {background : this.props.color, borderTopColor: this.props.highlightcolor};
     var labelStyle = this.props.color ? {textShadow : "none", color : this.props.textcolor} : {};
     return (
       <footer
          className="site thin not-logged-in"
          style={mainStyle}
          >

         <div className="ct thin">
           <div className="visions">
             <p>E-sign with peace of mind.</p>
           </div>

           <nav>
             <header><h4>{localization.footer.explore}</h4></header>
             <div className="content">
               <ul className="tree">
                 <li className="branch"><a href={this.props.httplink + this.langprefixForStaticPages() + 'api'} id="nav-api"> {localization.footer.api}</a></li>
                 <li className="branch"><a href={"/api-explorer"} id="nav-api-explorer"> {localization.footer.apiExplorer}</a></li>
                 <li className="branch"><a href="http://careers.stackoverflow.com/company/scrive" id="nav-jobs">{localization.footer.jobs}</a></li>
                 <li className="branch"><a href={this.props.langprefix + 'verify'} id="nav-privacy">{localization.footer.verify}</a></li>
                 <li className="branch"><a href={this.props.httplink + this.langprefixForStaticPages() + 'get-started'}>{localization.footer.getStarted}</a></li>
               </ul>
             </div>
           </nav>

           <nav>
             <header><h4>{localization.footer.terms}</h4></header>
             <div className="content">
               <ul className="tree">
                 <li className="branch"><a href={this.props.langprefix + 'terms'} id="nav-terms">{localization.footer.tos}</a></li>
                 <li className="branch"><a href={this.props.httplink + this.langprefixForStaticPages() + 'environment'} id="nav-environment">{localization.footer.environmentalPolicy}</a></li>
                 <li className="branch"><a href={this.props.httplink + this.langprefixForStaticPages() + 'privacy'} id="nav-privacy">{localization.footer.privacyPolicy}</a></li>
               </ul>
             </div>
           </nav>

           <nav>
             <header><h4>{localization.footer.contact}</h4></header>
             <div className="content">
               <ul className="tree">
                   <li className="branch">
                       {localization.footer.workday}
                   </li>
                   <li className="branch">
                       <a href="tel:+46851977900">+46 8 519 779 00</a>
                   </li>
                   <li className="branch">
                       <a href="mailto:info@scrive.com">info@scrive.com</a>
                   </li>
               </ul>
             </div>
           </nav>

           <nav>
             <header><h4>{localization.footer.address}</h4></header>
             <div className="content">
                 <ul className="tree">
                     <li className="branch">{localization.footer.address1}</li>
                     <li className="branch">{localization.footer.address2}</li>
                     <li className="branch">{localization.footer.address3}</li>
                     <li className="branch">{localization.footer.address4}</li>
                 </ul>
             </div>
           </nav>

           <nav className="last">
               <div className="content">
                 <ul className="icons">
                   <li>
                     <a href="http://www.linkedin.com/company/scrive" target="_blank">
                       <img src="/img/sm-linkedin.png" width="32" height="32" alt="LinkedIn" />
                     </a>
                   </li>
                   <li>
                     <a href="https://twitter.com/scrive" target="_blank">
                       <img src="/img/sm-twitter.png" width="32" height="32" alt="Twitter" />
                     </a>
                   </li>
                   <li className="last">
                     <a href="https://www.facebook.com/pages/Scrive/237391196280189" target="_blank">
                       <img src="/img/sm-facebook.png" width="32" height="32" alt="Facebook" />
                     </a>
                   </li>
                 </ul>
                 <ul className="tree">
                   <li className="branch">
                     <LanguageSelect
                      langprefix={this.props.langprefix}
                      border="none"
                      cssClass="change-language"
                      width={107}
                      adjustHeightOnExpand={true}
                     />
                   </li>
                 </ul>
               </div>
           </nav>
         </div>
       </footer>
     );
    }
  });




  return React.createClass({
    propTypes: {
      logged: React.PropTypes.bool,
      thin : React.PropTypes.bool,
      httplink :  React.PropTypes.string,
      langprefix : React.PropTypes.string,
      color :  React.PropTypes.string,
      textcolor :  React.PropTypes.string,
      highlightcolor : React.PropTypes.string
    },
    render: function() {

      if (this.props.logged) {
        return (<LoggedInFooter
                  thin={this.props.thin}
                  color={this.props.color}
                  textcolor={this.props.textcolor}
                  highlightcolor={this.props.highlightcolor}
                />);
      } else {
        return (<NotLoggedInFooter
                  thin={this.props.thin}
                  httplink={this.props.httplink}
                  langprefix={this.props.langprefix}
                  color={this.props.color}
                  textcolor={this.props.textcolor}
                  highlightcolor={this.props.highlightcolor}
                />);
      }
    }
  });

});

