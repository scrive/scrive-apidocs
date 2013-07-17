
(function(window){


var SignatureDrawer = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
        this.height = args.height;
        this.width = args.width;
        this.modal = args.modal;
        this.empty = true;
        this.render();
    },
    startDrawing : function()
    {
        this.drawing = true;
        document.ontouchmove = function(e){
             e.preventDefault();
        };
        this.modal.css("-ms-touch-action","none");
    },
    stopDrawing : function() {
        var view = this;
        this.drawing = false;

        document.ontouchmove = function(e){
            return true;
        };
        this.modal.css("-ms-touch-action","auto");
    },
    modal : function() {
      return this.get("modal");
    },
    lineWith : function() {
        return 3;
    },
    updateDrawingPoint : function(x,y) {
      this.drawingPointX = x;
      this.drawingPointY = y;
    },
    drawCircle : function(x,y) {

            if (!this.drawing)
              this.picture.beginPath();
            this.picture.fillStyle = "#000000";
            this.picture.arc(x, y,  2 , 0,  Math.PI*2, true);
            this.picture.fill();
            if (!this.drawing)
              this.picture.closePath();
    },
    drawingtoolDown : function(x,y) {
      var view = this;
      this.empty = false;
      this.startDrawing();
      view.uped = false;
      var circleDraw = function(i) {
          if (view.uped)
            {
                view.drawCircle(x,y);
            }
          else if (i < 2 )
              setTimeout(function() {circleDraw(i+1)}, 100);
      };
      circleDraw(0);
      this.picture.beginPath();
      this.picture.moveTo(x, y);
      this.x_ = undefined;
      this.y_ = undefined;
      this.x = x;
      this.y = y;
      this.picture.lineWidth = this.lineWith();
      this.picture.lineCap = 'round';
      this.picture.lineJoin = 'round';



    },
    drawingtoolMove : function(x,y) {
      if (this.drawing) {
        var moved = function(x1,x2) { return (x1 * 2 + x2 * 1) / 3; };
        if (this.x_ != undefined && this.y_ != undefined) {
            this.drawNiceCurve(this.x_, this.y_ ,this.x, this.y, moved(this.x,x), moved(this.y,y));
            this.drawNiceLine(moved(this.x,x), moved(this.y,y),moved(x,this.x), moved(y,this.y));
        }
        else
            this.drawNiceLine(this.x, this.y,moved(x,this.x), moved(y,this.y));
        this.x_ = moved(x,this.x);
        this.y_ = moved(y,this.y);
        this.x = x;
        this.y = y;
      }
    },
    drawNiceLine : function(sx,sy,ex,ey) {
        this.drawLine(sx,sy,ex,ey,this.lineWith() + 1, "#FEFEFE", 'butt');
        this.drawLine(sx,sy,ex,ey,this.lineWith()    , "#555555", 'round');
        this.drawLine(sx,sy,ex,ey,this.lineWith() - 1, "#222222", 'round');
        this.drawLine(sx,sy,ex,ey,this.lineWith() - 2, "#000000", 'round');
    },
    drawNiceCurve : function(sx,sy,cx,cy,ex,ey) {
        this.drawCurve(sx,sy,cx,cy,ex,ey,this.lineWith() + 1, "#FEFEFE", 'butt');
        this.drawCurve(sx,sy,cx,cy,ex,ey,this.lineWith()    , "#555555", 'round');
        this.drawCurve(sx,sy,cx,cy,ex,ey,this.lineWith() - 1, "#222222", 'round');
        this.drawCurve(sx,sy,cx,cy,ex,ey,this.lineWith() - 2, "#000000", 'round');
    },
    drawCurve : function(sx,sy,cx,cy,ex,ey,w,c ,lc) {
        this.picture.closePath();
        this.picture.beginPath();
        this.picture.moveTo(sx, sy);
        this.picture.strokeStyle = c;
        this.picture.lineWidth = w;
        this.picture.lineCap = lc;
        this.picture.quadraticCurveTo(cx,cy,ex,ey);
        this.picture.stroke();
    },
    drawLine : function(sx,sy,ex,ey,w,c, lc)
    {   this.picture.closePath();
        this.picture.beginPath();
        this.picture.moveTo(sx, sy);
        this.picture.strokeStyle = c;
        this.picture.lineWidth = w;
        this.picture.lineCap = lc;
        this.picture.lineTo(ex, ey);
        this.picture.stroke();

    },
    drawingtoolUp : function(x,y) {
      this.picture.lineTo(x, y);
      this.picture.closePath();
      this.stopDrawing();
      this.uped = true;
    },
    xPos : function(e) {
      if (e.changedTouches != undefined && e.changedTouches[0] != undefined) e = e.changedTouches[0];
      return e.pageX - this.canvas.offset().left;
    },
    yPos : function(e) {
      if (e.changedTouches != undefined && e.changedTouches[0] != undefined) e = e.changedTouches[0];
      var extra = 0;
      return e.pageY + extra - this.canvas.offset().top;
    },
    initDrawing : function() {
           var view = this;
           if ('ontouchstart' in document.documentElement) {
            this.canvas[0].addEventListener('touchstart',function(e) {e.preventDefault(); e.stopPropagation();e.preventDefault(); e.stopPropagation(); view.drawingtoolDown(view.xPos(e), view.yPos(e));});
            this.canvas[0].addEventListener('touchmove',function(e) {e.preventDefault(); e.stopPropagation();view.drawingtoolMove(view.xPos(e), view.yPos(e));});
            this.canvas[0].addEventListener('touchend',function(e) {e.preventDefault(); e.stopPropagation();view.drawingtoolUp(view.xPos(e), view.yPos(e));});
           } else if (navigator.msPointerEnabled) {
            this.canvas[0].addEventListener("MSPointerDown",function(e) {e.preventDefault(); e.stopPropagation(); view.drawingtoolDown(view.xPos(e), view.yPos(e)); return false;},true);
            this.canvas[0].addEventListener("MSPointerMove",function(e) {e.preventDefault(); e.stopPropagation();view.drawingtoolMove(view.xPos(e), view.yPos(e)); return false;},true);
            this.canvas[0].addEventListener("MSPointerUp",function(e) {e.preventDefault(); e.stopPropagation();view.drawingtoolUp(view.xPos(e), view.yPos(e)); return false;},true);
           } else {
            this.canvas.mousedown(function(e) {e.preventDefault(); e.stopPropagation();e.target.style.cursor = 'default';view.drawingtoolDown(view.xPos(e), view.yPos(e),e);});
            this.canvas.mousemove(function(e) {e.preventDefault(); e.stopPropagation();view.drawingtoolMove(view.xPos(e), view.yPos(e));});
            this.canvas.mouseup(function(e){e.preventDefault(); e.stopPropagation();view.drawingtoolUp(view.xPos(e), view.yPos(e));} );
           }
    },
    saveImage : function(callback) {
        if (this.empty) {
          this.model.setValue("");
          if (callback != undefined) callback();
        } else {
          var self = this;
          var field = this.model;
          var image = this.canvas[0].toDataURL("image/png",1.0);
          var img = new Image();
          img.type = 'image/png';
          img.src = image;
          img.onload = function() {
               var canvas = $("<canvas class='signatureCanvas' />");
               canvas.attr("width",4* self.width);
               canvas.attr("height",4* self.height);
               canvas[0].getContext('2d').fillStyle = "#ffffff";
               canvas[0].getContext('2d').fillRect (0,0,4*self.width,4*self.height);
               canvas[0].getContext('2d').drawImage(img,0,0,4*self.width,4*self.height);


               field.setValue(canvas[0].toDataURL("image/jpeg",1.0));
               field.setValueTMP(image);

               if (callback != undefined) callback();
         };
       }
    },
    clear: function() {
          this.canvas[0].getContext('2d').clearRect(0,0,820,820 * this.height / this.width);
          this.canvas[0].width = this.canvas[0].width;
          this.empty  = true;
    },
    render: function () {
        var signature = this.model;
        var view = this;
        this.container = $(this.el);
        this.container.addClass("signatureDrawingBox");
        this.canvas = $("<canvas class='signatureCanvas' />");
        this.canvas.attr("width",820);
        this.canvas.width(820);
        this.canvas.attr("height",820 * this.height / this.width);
        this.canvas.height(820 * this.height / this.width);
        this.picture =  this.canvas[0].getContext('2d');
        if (this.model.value() != "" && this.model.valueTMP() != undefined && this.model.valueTMP() != "") {
          var img = new Image();
          img.type = 'image/png';
          img.src = this.model.valueTMP() ;
          this.canvas[0].getContext('2d').drawImage(img,0,0,820,820 * this.height / this.width);
          this.empty = false;
        }
        this.initDrawing();
        this.container.append(this.canvas);
        return this;
    }
});


var SignatureDrawerWrapper = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.onClose = args.onClose;
        this.height = args.height;
        this.width = args.width;
        this.modal = args.modal;
        this.render();
    },
    header: function() {
        var h = $("<h1>").text(localization.pad.drawSignatureBoxHeader);
        return $("<div class='header'/>").append(h);
    },
    drawingBox : function() {
        var div = $("<div class='signatureDrawingBoxWrapper'>");
        //this.drawer = new SignatureDrawer({model : this.model, height: this.height, width: this.width, modal : this.modal});
        //div.append(this.drawer.el);
        this.typer = new SignatureTyper({text : this.model.signatory().nameOrEmail(), height: this.height, width: this.width});
        div.append(this.typer.el());
        div.width(820);
        div.height(820 * this.height / this.width);
        return div;
    },
    acceptButton : function() {
        var view = this;
        var field = this.model;
        var signatory = field.signatory();
        var document = signatory.document();
        return new Button({
                    color : 'green',
                    size: BrowserInfo.isSmallScreen() ? 'small' : 'tiny',
                    text: localization.signature.confirmSignature,
                    onClick : function(){
                        //view.drawer.saveImage();
                        view.onClose();
                        return false;
                    }
            }).el();
    },
    clearButton : function() {
        var view = this;
        return new Button({
                color : 'red',
                size: BrowserInfo.isSmallScreen() ? 'small' : 'tiny',
                text: localization.pad.cleanImage,
                onClick : function() {
                    //view.drawer.clear();
                    return false;
                }
        }).el();
    },
    footer : function() {
           var self = this;
           var signatory = this.model.signatory();
           var abutton = this.acceptButton();
           abutton.addClass("float-right");
           var detailsBox = $("<div class='details-box' />");
           var name = signatory.nameOrEmail();
           var company = signatory.company();
           var textinput = $("<input type='text'>").val(signatory.nameOrEmail())
                                                   .change(function() {self.typer.setText(textinput.val())})
                                                   .keyup(function() {self.typer.setText(textinput.val())});

           detailsBox.append($("<h1/>").text(name));
           detailsBox.append($("<h2/>").text(company ));
           detailsBox.append(textinput);
           var cbutton = this.clearButton();
           cbutton.addClass("float-left");
           return $("<div class='footer'/>").append(cbutton).append(abutton).append(detailsBox);
    },
    render: function () {
        var box = $(this.el);
        box.append(this.header());
        box.append(this.drawingBox());
        box.append(this.footer());
        return this;
    }
});


window.SignatureDrawerPopup = function(args){

        var self = this;
        if (BrowserInfo.isIE8orLower())
        {
            alert('Drawing signature is not avaible for older versions of Internet Explorer. Please update your browser.');
            return;
        }
        var modal = $("<div class='modal'></div>");

        var width = BrowserInfo.isSmallScreen() ? 980 : 900;
        var container = $("<div class='modal-container drawing-modal'/>").css("width",width);

        if(BrowserInfo.isSmallScreen()) container.addClass("small-screen");
        container.css("top",$(window).scrollTop());
        container.css("margin-top",$(window).height() > 700 ? 200 : 100);
        container.css("left","0px");
        var left = Math.floor(((window.innerWidth ? window.innerWidth : $(window).width()) - width) / 2);
        container.css("margin-left",left > 20 ? left : 20);

        var close =  function() {
              console.log("Closing");
              modal.removeClass('active');
              document.ontouchmove = function(e){
                 return true;
              };
              setTimeout(function() {modal.detach();},500);
            };

        var closeButton = $("<div class='close modal-close float-right' style='margin-right:40px;margin-top:30px'/>").click(close);

        container.append(closeButton);
        self.dw = new SignatureDrawerWrapper({model : args.field, width: args.width, height: args.height, onClose : close, modal : modal});
        container.append(self.dw.el);
        modal.append(container);

        $('body').append(modal );
        modal.addClass('active');
};

})(window);
