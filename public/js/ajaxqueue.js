/* Ajax queue - used to provide order when performing ajax events. Works with Submits.
 * Usage
 *  var queue =  new AjaxQueue();
 *  queue.add(new Submit({url: "...", ajax : true});
 *  queue.add(new Submit({url: "...", ajax : true});
 *  queue.finishWith(f);
 *  This queue guarantees that finishWith function will be called after last added submit is successfully send.
 *  Submits put in the queue should not use "ajaxsuccess" or "ajaxerror" properties as they will be overwritten.
 *  Submits requests have 10 sec timeout set. If they don't success or fail in this time queue will decide that they got lost and autoremove them.
 *  finishWith function will be performed when last Submit will be send with success. If last Submit in the queue fails, it will be restarted.
 *  If last event in queue will fail, it will be removed.
 *  After calling finishWith nothing can be added to the queue, until queue is empty and current finishWith starts executing. 
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
  done : function() {
     return this.get("result") == undefined; 
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
     else if (requestQueue.length == 0 && this.get("finishWith") != undefined)
     {
         var finishWith = this.get("finishWith");
         this.set({"finishWith" : undefined});
         finishWith();
     }
         
  },
  add : function(s){
       var ajaxQueue = this;
       var requestQueue = this.get("queue");
       if (this.get("finishWith") != undefined)
       {
         console.log("Trying to add to closed ajax queue");
         return;
       }
       var qr = new QueueRequest({submit : s});
       qr.bind("success", function() {
                requestQueue.shift();
                ajaxQueue.run();
            })
       qr.bind("error", function() {
                if (requestQueue.length == 1)
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
       if (this.get("finishWith") != undefined)
       {
         console.log("Trying to close closed ajax queue");
         return;
       }
       this.set({"finishWith" : f});
       this.run();
  }
});


})(window);
