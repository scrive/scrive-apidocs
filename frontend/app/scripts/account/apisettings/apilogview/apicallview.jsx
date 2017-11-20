var classNames = require("classnames");
var React = require("react");
var $ = require("jquery");
var _ = require("underscore");

var APICallModel = require("./apicallmodel");
var utils = require("./utils");

var ParametersSectionView = React.createClass({
  propTypes: {
    header: React.PropTypes.string.isRequired,
    params: React.PropTypes.instanceOf(Array).isRequired,
    placeholder: React.PropTypes.string.isRequired
  },
  render: function () {
    return (
      <div className="section params">
        <div className="col col-left">
          <p className="header">{this.props.header}</p>
        </div>

        <div className="col col-right">
          {this.props.params.length == 0 &&
            <p className="placeholder">
              <strong>{this.props.placeholder}</strong>
            </p>
          }

          {this.props.params.length > 0 &&
            <table>
              <thead>
                <tr>
                  <th>{"Key"}</th>
                  <th>{"Value"}</th>
                </tr>
              </thead>

              <tbody>
                {_.map(this.props.params, function (paramSpec, index) {
                  return (
                    <tr key={index}>
                      <td>{paramSpec[0]}</td>
                      <td>{paramSpec[1]}</td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          }
        </div>
      </div>
    );
  }
});

var BodySectionView = React.createClass({
  propTypes: {
    body: React.PropTypes.any,
    header: React.PropTypes.string.isRequired,
    placeholder: React.PropTypes.string.isRequired
  },
  getInitialState: function () {
    return {
      collapsed: true
    };
  },
  componentDidMount: function () {
    if (this.props.body) {
      var $bodyView = $(this.refs.bodyView.getDOMNode());
      var $expander = $(this.refs.expander.getDOMNode());

      if ($bodyView.outerHeight() < 250) {
        $expander.css({display: "none"});
      }
    }
  },
  onCollapserExpanderClick: function () {
    this.setState({collapsed: !this.state.collapsed});
  },
  render: function () {
    var preWrapperClassName = classNames("pre-wrapper", {
      collapsed: this.state.collapsed
    });

    return (
      <div className="section body">
        <div className="col col-left">
          <p className="header">{this.props.header}</p>
        </div>

        <div className="col col-right">
          {!this.props.body &&
            <p className="placeholder">
              <strong>{this.props.placeholder}</strong>
            </p>
          }

          {this.props.body &&
            <div className={preWrapperClassName}>
              <pre ref="bodyView">{JSON.stringify(this.props.body, undefined, 2)}</pre>

              {this.state.collapsed &&
                <div
                  ref="expander"
                  className="expander"
                  onClick={this.onCollapserExpanderClick}
                >
                  <span>{"Expand"} ▼</span>
                </div>
              }

              {!this.state.collapsed &&
                <div
                  ref="collapser"
                  className="collapser"
                  onClick={this.onCollapserExpanderClick}
                >
                  <span>{"Hide"} ▲</span>
                </div>
              }
            </div>
          }
        </div>
      </div>
    );
  }
});

var APICallView = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    call: React.PropTypes.instanceOf(APICallModel).isRequired,
    onClose: React.PropTypes.func.isRequired
  },
  render: function () {
    var statusClassName = classNames(
      "response-code", utils.statusClassName(this.props.call.isSuccessful())
    );

    return (
      <div className="apicallview">
        <div className="section meta">
          <div className="col col-left">
            <p className="uri">
              {this.props.call.get("requestMethod")} {this.props.call.get("requestURI")}
            </p>
            <p className="time">{this.props.call.displayTime()}</p>
          </div>

          <div className="col col-right">
            <p>{"HTTP Status"}</p>
            <p className={statusClassName}>
              {this.props.call.get("responseCode")}
            </p>
          </div>
        </div>

        <ParametersSectionView
          header={"GET Parameters"}
          params={this.props.call.get("requestParamsGET")}
          placeholder={"No GET Parameters"}
        />

        <ParametersSectionView
          header={"POST Parameters"}
          params={this.props.call.get("requestParamsPOST")}
          placeholder={"No POST Parameters"}
        />

        <BodySectionView
          body={this.props.call.get("responseBody")}
          header={"Response Body"}
          placeholder={"No Response Body"}
        />

        <div
          ref="closer"
          className="closer"
          onClick={this.props.onClose}
        />
      </div>
    );
  }
});

module.exports = {
  ParametersSectionView: ParametersSectionView,
  BodySectionView: BodySectionView,
  APICallView: APICallView
};
