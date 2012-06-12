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
 *  called after last added submit is successfully sent.  Submits put
 *  in the queue should not use "ajaxsuccess" or "ajaxerror"
 *  properties as they will be overwritten.
 *
 *  Submit requests have 10 sec timeout set. If they don't succeed
 *  within this time, or if they fail, they will be silently dropped
 *  from the queue if they are intermediate.  If last Submit in the
 *  queue fails or if it times out, it will be restarted.

 *  'fin' will be called when last Submit is sent with success.
 *
 *  REVIEW: Is the next sentence true?  I think not.
 *  If last event in queue will fail, it will be removed.
 *
 *  After calling finishWith nothing can be added to the queue, until
 *  queue is empty and 'fin' starts executing.
 */

(function(window){
var QueueRequest = Backbone.Model.extend({
  defaults : {
      send : false,
      submit : undefined,
      result : undefined      // True <=> ajaxsuccess , False <=> ajaxerror
  },
  started : function() {
     return this.get("send");
  },
  restart : function() {
      this.set({"result" : undefined});
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
       submit.set({   ajax : true
                   ,  ajaxerror: function() {
                        queueRequest.set({result:false});
                        queueRequest.trigger("error");
                       }
                   ,  ajaxsuccess: function() {
                        queueRequest.set({result:true});
                        queueRequest.trigger("success");
                       }
                   , ajaxtimeout : 9900
                  });
       submit.send();
       setTimeout(function() {if (queueRequest.get("result") == undefined) queueRequest.trigger("error"); }, 10000);
  }
});

window.AjaxQueue = Backbone.Model.extend({
  defaults : {
      queue : [] ,
      finishWithFunction : undefined
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
  add : function(s){
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
            })
       qr.bind("error", function() {
                if (requestQueue[0] != qr) return; // This is a lost request and we ignore it
                if (requestQueue.length == 1)
                    // REVIEW: What prevents the client from getting
                    // stuck in an infinite loop here if there is
                    // something wrong with e.g. the parameters in the
                    // last submission?  Consider giving up after a
                    // while and call a failure function.
                    setTimeout(function() {requestQueue[0].restart();},5000);
                else {
                    requestQueue.shift();
                    ajaxQueue.run();
                }
            })
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
