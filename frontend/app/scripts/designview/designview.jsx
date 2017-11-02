var React = require("react");
var _ = require("underscore");
var $ = require("jquery");

var BackboneMixin = require("../common/backbone_mixin");
var BrowserInfo = require("../../js/utils/browserinfo").BrowserInfo;
var ButtonBarView = require("./buttonbar/buttonbarview");
var Document = require("../../js/documents.js").Document;
var DocumentView = require("./documentview");
var DraggablesTabView = require("./editdocument/draggablesview");
var ParticipantsTabView = require("./participants/participants");
var ProcessSettingsTabView = require("./processsettings/processsettings");
var TabBarView = require("./tabbar").TabBarView;
var TabView = require("./tabbar").TabView;
var Subscription = require("../account/subscription");
var DragAndDropUploaderView = require("./draganddropuploaderview");
var UsageInfo = require("./usageinfo/usageinfo");

module.exports = React.createClass({
  mixins: [BackboneMixin.BackboneMixin],
  propTypes: {
    model: React.PropTypes.instanceOf(Document).isRequired
  },
  getBackboneModels: function (argument) {
    return [this.props.model];
  },
  getInitialState: function () {
    return {
      topBarStyle: {},
      frameStyle: {},
      isDnDUploaderVisible: false
    };
  },
  componentWillMount: function () {
    this.dragAndDropEnabled = (
      BrowserInfo.hasDragAndDrop() && BrowserInfo.hasFormData()
    );
    this.fixed = undefined;
    this.topBarHeight = 0;

    this.props.model.setReferenceScreenshot("author");
  },
  componentDidMount: function () {
    $(window).on("resize scroll", this.onWindowResizeScroll);
  },
  componentWillUnmount: function () {
    $(window).off("resize scroll", this.onWindowResizeScroll);
  },
  proxiedShowCoordinateAxes: function () {
    if (this.refs.documentView) {
      return this.refs.documentView.showCoordinateAxes;
    } else {
      return _.noop;
    }
  },
  proxiedHideCoordinateAxes: function () {
    if (this.refs.documentView) {
      return this.refs.documentView.hideCoordinateAxes;
    } else {
      return _.noop;
    }
  },
  proxiedMoveCoordinateAxes: function () {
    if (this.refs.documentView) {
      return this.refs.documentView.moveCoordinateAxes;
    } else {
      return _.noop;
    }
  },
  proxiedOpenTypeSetterFor: function () {
    if (this.refs.documentView) {
      return this.refs.documentView.openTypeSetterFor;
    } else {
      return _.noop;
    }
  },
  fix: function () {
    if (this.fixed) {
      return;
    }

    var topBarEl = $(React.findDOMNode(this.refs.topBar));

    this.fixed = true;
    this.topBarHeight = topBarEl.outerHeight();

    this.setState({
      topBarStyle: {
        position: "fixed",
        top: 0,
        left: topBarEl.offset().left
      },
      frameStyle: {
        paddingTop: this.topBarHeight
      }
    });
  },
  unfix: function () {
    if (!this.fixed) {
      return;
    }

    var topBarEl = $(React.findDOMNode(this.refs.topBar));

    this.fixed = false;
    this.topBarHeight = topBarEl.outerHeight();

    this.setState({
      topBarStyle: {
        position: "relative",
        top: "",
        left: ""
      },
      frameStyle: {}
    });
  },
  affix: function () {
    var frameEl = $(React.findDOMNode(this.refs.frame));
    var blockingEl = $(React.findDOMNode(this.refs.blocking));

    var st = $(window).scrollTop();
    var top = frameEl.offset().top + blockingEl.height();

    if (st > top) {
      this.fix();
    } else {
      this.unfix();
    }
  },
  onWindowResizeScroll: function () {
    this.affix();
  },
  onDnDUploaderStart: function () {
    this.setState({isDnDUploaderVisible: true});
  },
  onDnDUploaderEnd: function () {
    this.setState({isDnDUploaderVisible: false});
  },
  render: function () {
    var self = this;

    return (
      <div className="design-view-frame" ref="frame" style={this.state.frameStyle}>
        <div
          className="design-view-blocking"
          style={{opacity: this.state.topBarStyle.position == "fixed" ? "0" : ""}}
          ref="blocking"
        >
          <UsageInfo/>
        </div>
        <div className="design-view-frame-top-bar" ref="topBar" style={this.state.topBarStyle}>
          <TabBarView document={this.props.model}>
            <TabView
              text={localization.designview.editParticipants}
              pagehash="participants"
            >
              <ParticipantsTabView document={this.props.model} />
            </TabView>
            <TabView
              text={localization.designview.editDocument}
              pagehash="placements"
              isInactive={() => self.props.model.mainfile() == undefined}
            >
              <DraggablesTabView
                document={this.props.model}
                proxiedShowCoordinateAxes={this.proxiedShowCoordinateAxes}
                proxiedHideCoordinateAxes={this.proxiedHideCoordinateAxes}
                proxiedMoveCoordinateAxes={this.proxiedMoveCoordinateAxes}
                proxiedOpenTypeSetterFor={this.proxiedOpenTypeSetterFor}
              />
            </TabView>
            <TabView
              text={localization.designview.editSigningProcess}
              pagehash="process"
            >
              <ProcessSettingsTabView document={this.props.model} />
            </TabView>
          </TabBarView>
        </div>

        <DocumentView
          document={this.props.model}
          ref="documentView"
          dragAndDropEnabled={this.dragAndDropEnabled}
          isDnDUploaderVisible={this.state.isDnDUploaderVisible}
        />

        <ButtonBarView document={this.props.model} />

        {this.props.model.mainfile() == undefined && this.dragAndDropEnabled &&
          <DragAndDropUploaderView
            document={this.props.model}
            onStart={this.onDnDUploaderStart}
            onEnd={this.onDnDUploaderEnd}
          />
        }

      </div>
    );
  }
});
