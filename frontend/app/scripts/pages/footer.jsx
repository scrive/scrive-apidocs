var React = require("react");

module.exports = React.createClass({
  render: function () {
   return (
     <footer
       className={"site logged-in"}
       >
      <div className="poweredbyscrive">
        <div className="poweredbyscrive">
          <div className="text">
            Powered by <div className="logo"/>
          </div>
        </div>
      </div>
     </footer>
   );
  }
});
