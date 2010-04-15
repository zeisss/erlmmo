"use strict";

/* global MODEL: false, UI: false, $: false */


/**
 * The System Log provides a little window containing all events in the system the user should know about.
 * Mainly these are error messages, when he can't do something or the fight messages.
 */
function system_log_model() {
  var listeners = [];
  var that = {
    'messages': [],
    
    'init': function() {
      
    },
    
    'addListener': function(listener) {
      listeners.push(listener);
    },
    
    'addMessage':function(message) {
      this.messages.push(message);
    },
    
    'fireEvent': function(event) {
      for ( var x in listeners) {
        listeners[x].handleEvent(event);
      }
    },
    
    'handleEvent': function(event) {
      this.messages.push(event);
      this.fireEvent(['new_event']);
    }
  };
  that.init();
  return that;
}

function system_log_ui() {
  var that = {
    'model': system_log_model(),
    
    'init': function() {
      this.model.addListener(this);
      
      var log = $("#system_log_ui").dialog({
        'title': 'System Log',
        'minWidth': 400,
        'minHeight': 300,
        'height': 300,
        'width': 400
      });
      
      log.parent().css("top", "");
      log.parent().css("bottom", "20px");
      log.parent().css("left", "");
      log.parent().css("right", "20px");
    },
    
    
    'renderMessages': function() {
      var div = $("#system_log_ui").html('');
      for ( var x in this.model.messages) {
        var msg = this.model.messages[x];
        
        if ( typeof(msg)  == "string") {
          div.append('<div>' + msg + '</div>');
        } else if ( msg.type == "error") {
          div.append('<div>ERROR: ' + msg.message + '(' + msg.code + ')</div>');
        } else {
          div.append('<div>Unknown event: ' + msg.type + '</div>');
        }
      }
    },
    
    'handleEvent': function(event) {
      this.renderMessages();
    }
  };
  return that;
}