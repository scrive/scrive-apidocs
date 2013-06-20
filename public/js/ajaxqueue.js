/* Ajax queue - used to provide order when performing ajax events. Works with Submits.
 *
 * All the added will be submitted, but in the presence of errors,
 * intermediate submissions are silently dropped.  The intented use
 * case is that intermediate submissions save snapshots (or drafts) of
 * the client-side state as a backup in case the session is lost.
 *
 * However, the last submission is the one that matters, and its
 * content must subsume all intermediate submissions, since the queue
 * may drop all intermediate submissions and just send the last.
 *
 * The last submission can be retransmitted, so it must be idempotent.
 * That is, the end result should be the same even if the submission
 * is sent more than once.
 *
 * REVIEW: If it's true that intermediate submissions can be dropped
 * and merely are snapshots, then wouldn't it be better to limit the
 * queue so that there is only one pending submission?  So the 'add'
 * method would simply overwrite a pending submission if there is one.
 * This suggests that this class should be called something else than
 * 'queue'.
 *
 * Usage
 *  var queue =  new AjaxQueue();
 *  queue.add(new Submit({url: "..."});
 *  queue.add(new Submit({url: "..."});
 *  queue.finishWith(fin);
 *
 *  This queue guarantees that finishWith function 'fin' will be
 *  called after last added submit is successfully sent.
 *
 *  Submit requests have 10 sec timeout set. If they don't succeed
 *  within this time, or if they fail, they will be silently dropped
 *  from the queue if they are intermediate.  If last Submit in the
 *  queue fails or if it times out, it will be restarted.

 *  'fin' will be called when last Submit is sent with success.
 *
 *  If last event in queue will fail, error message will be displayed
 *  to the user, forcing him to refresh the page.
 *
 *  After calling finishWith nothing can be added to the queue, until
 *  queue is empty and 'fin' starts executing.
 */

(function(window){
var QueueRequest = Backbone.Model.extend({
  defaults : {
      send : false,
      submit : undefined,
      result : undefined,      // True <=> ajaxsuccess , False <=> ajaxerror
      errorCode : 0
  },
  initialize: function() {
    var queueRequest = this;
    var submit = this.get("submit");
    var successCallback = submit.get("ajaxsuccess");
    var errorCallback = submit.get("ajaxerror");

    submit.set({ajax: true,
                ajaxerror: function(res) {
                  queueRequest.set({result:false, errorCode : res.status});
                  queueRequest.trigger("error");
                  if (errorCallback != undefined) errorCallback();
                },
                ajaxsuccess: function() {
                  queueRequest.set({result:true});
                  queueRequest.trigger("success");
                  if (successCallback != undefined) successCallback();
                },
                ajaxtimeout: 9900});
  },
  started : function() {
     return this.get("send");
  },
  errorCode : function() {
     return this.get("errorCode");
  },
  restart : function() {
      this.set("result", undefined);
      this.run();
  },
  execute : function() {
       if (this.started()) return;
       this.run();
  },
  run : function() {
       this.set({"send" : true});
       var queueRequest = this;
       var submit = this.get("submit");
       submit.send();
       setTimeout(function() {if (queueRequest.get("result") == undefined) queueRequest.trigger("error"); }, 10000);
  }
});

window.AjaxQueue = Backbone.Model.extend({
  defaults : {
      queue : [] ,
      num_retries : 0,
      finishWithFunction : undefined
  },
  num_retries: function() {
    return this.get('num_retries');
  },
  run : function() {
     var requestQueue = this.get("queue");
     if (requestQueue[0] != undefined && !requestQueue[0].started())
         requestQueue[0].execute();
     else if (requestQueue.length == 0 && this.get("finishWithFunction") != undefined)
     {
         var finishWith = this.get("finishWithFunction");
         this.set({"finishWithFunction" : undefined});
         finishWith();
     }

  },
  add : function(s, errorCallback){
       var ajaxQueue = this;
       var requestQueue = this.get("queue");
       if (this.get("finishWithFunction") != undefined)
       {
         console.log("Trying to add to closed ajax queue");
         return;
       }
       var qr = new QueueRequest({submit : s});
       qr.bind("success", function() {
                if (requestQueue[0] != qr) return; // This is a lost request and we ignore it
                requestQueue.shift();
                ajaxQueue.run();
            });
       qr.bind("error", function() {
                if (errorCallback != undefined) errorCallback(qr.errorCode());
                if (requestQueue[0] != qr) return; // This is a lost request and we ignore it
                if (requestQueue.length == 1) {
                  if (ajaxQueue.num_retries() < 12) {
                     setTimeout(function() {
                       ajaxQueue.set('num_retries', ajaxQueue.num_retries() + 1);
                       requestQueue[0].restart();
                     }, 5000);
                  } else {
                    var button =  Button.init({color: 'blue',
                                               size: 'small',
                                               text: 'Reload page',
                                               onClick: function() {
                                                 document.location.reload(true);
                                               }
                                              });
                    var content = $('<div/>');
                    content.append($(localization.problemContactingServer));
                    content.append($('<div style="margin-top: 40px;" />'));
                    content.append(button.input());

                    ScreenBlockingDialog.open({header: content});
                  }
                }
                else {
                    requestQueue.shift();
                    ajaxQueue.run();
                }
            });
       requestQueue.push(qr);
       ajaxQueue.run();
  },
  finishWith : function(f){
       if (this.get("finishWithFunction") != undefined)
       {
         console.log("Trying to close closed ajax queue");
         return;
       }
       this.set({"finishWithFunction" : f});
       this.run();
  }
});


})(window);
