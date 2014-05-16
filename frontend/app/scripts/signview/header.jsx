/** @jsx React.DOM */


define(['React', 'Backbone', 'common/backbone_mixim'], function(React, Backbone, BackboneMixin) {

  return React.createClass({
    propTypes: {
      signviewbranding: React.PropTypes.signviewbranding,
      forPad : React.PropTypes.pad
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.signviewbranding];
    },
    render: function() {
      var signviewbranding = this.props.signviewbranding;
      var forPad =  this.props.forPad;
      var backToList = true;
      var showHeader = signviewbranding.ready() && !BrowserInfo.isSmallScreen() && signviewbranding.showheader();

      var bgImage = signviewbranding.signviewbarscolour() != undefined ?  'none' : ''
      var bgColor = signviewbranding.signviewbarscolour() != undefined ?  signviewbranding.signviewbarscolour() : ''
      var color = signviewbranding.signviewbarstextcolour() != undefined ? signviewbranding.signviewbarstextcolour() : ''
      var font = signviewbranding.signviewtextfont() != undefined ? signviewbranding.signviewtextfont() : ''


      if (!showHeader)
        return (<div/>);
      else
        return (
          <div className="pageheader" style={{backgroundImage: bgImage, backgroundColor: bgColor, color: color,fontFamily : font}}>
            <div className="content">
              <div className="logowrapper">
                <img className="logo"
                     src={(signviewbranding.signviewlogo() != undefined && signviewbranding.signviewlogo() != "") ? signviewbranding.signviewlogo() : '/img/logo.png'}>
                </img>
              </div>
              <div className="sender" style={{color: color,fontFamily : font}}>

                {/*if*/ forPad &&

                  <div className="inner">
                    {/*if*/ backToList &&
                      <a className='link' href={"/to-sign"} style={{color: color}}>
                        Back to list
                      </a>
                    }
                    {/*else*/ !backToList &&
                      <a className='link' href={"/d/" + signviewbranding.documentid()} style={{color: color}}>
                        Back to system
                      </a>
                    }
                  </div>

                }
                {/*else*/ !forPad &&
                  <div className="inner">
                    <div className='name'>
                      {signviewbranding.fullname()}
                    </div>
                    <div className='phone'>
                      {signviewbranding.phone()}
                    </div>
                  </div>
                }

              </div>
              <div className="clearfix"/>
            </div>
          </div>
      );
    }
  });

});