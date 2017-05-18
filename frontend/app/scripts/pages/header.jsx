var React = require("react");
var Button = require("../common/button");
var Track = require("../common/track");

module.exports = React.createClass({
  propTypes: {
    archive: React.PropTypes.bool,
    account: React.PropTypes.bool
  },
  handleNewDocument: function () {
    Track.track_timeout("Click start new process", {}, function (e) {
      window.location.href = "/newdocument";
    });
    return false;
  },
  handleNewFromTemplate: function () {
    Track.track_timeout("Click create from template", {}, function (e) {
            window.location.href = "/fromtemplate";
      });
    return false;
  },
  handleLogout: function () {
    Track.track_timeout("Click Logout", {}, function (e) {
            window.location.href = "/logout";
    });
    return false;
  },
  render: function () {
   return (
    <header className="site">
      <nav>
        <ul className="ct">
          <li id="branding">
            <a id="logo" className="page" href="/newdocument">
              <img
                src={
                  window.cdnbaseurl + "/service_logo/" + window.brandingdomainid + "/" +
                  (window.brandinguserid || "_") + "/" + window.brandinghash}
                style={{"margin": "0px"}}
              />
            </a>
          </li>
          <ul className="right-container">
            <li className="float-right" >
              <a className="page js-logout" onClick={this.handleLogout} href="#">{localization.header.logout}</a>
            </li>
            <li className="float-right" >
              <a className={"page" + (this.props.account ? " active" : "")}
                 id='page-account'
                 href="/account">{localization.header.account}</a>
            </li>
            <li className="page-first float-right" >
              <a className={"s-archive page " + (this.props.archive ? "active" : "")}
                 id='page-archive'
                 href="/d">{localization.header.archive}</a>
            </li>
            <li className="session-create float-right fromtemplate">
              <Button
                cssClass="fromtemplate"
                type="main"
                onClick={this.handleNewFromTemplate}
                text={localization.header.template}
              />
            </li>
            <li className="session-create float-right">
              <Button
                cssClass="js-create-document"
                type="main"
                onClick={this.handleNewDocument}
                text={localization.header.send}
              />
            </li>
          </ul>
        </ul>
      </nav>
    </header>
   );
  }
});
