"use strict";

// Global variables for later use
var MODEL = false;
var UI = false;

var MODEL = {
    'sessionkey': false,
    'listeners': [],
    
    'init': function() {
      setInterval(function() { MODEL.handle_events(); }, 1000);
    },
    
    'login': function(username, password) {
      auth_login(username, password, function(sessionkey) {
        MODEL.setSessionkey(sessionkey);
      });
    },
    
    'logout': function() {
      auth_logout(this.sessionkey);
      this.sessionkey = false; // we kill the sessionkey directly, so no one can accicidently contiue to use it.
    },
    
    'setSessionkey': function(sessionkey) {
      MODEL.sessionkey = sessionkey;	  
    },
    
    
    'handle_events': function() {
      
      if ( MODEL.sessionkey === false) {
        return;
      }
      
      
      event_get(MODEL.sessionkey, function(events) {
        for( var i in events) {
          MODEL.handleEvent(events[i]);
        }
      });
    },
    
    'handleEvent': function(event) {
      var handled = false;
      for ( var x in this.listeners ) {
        if ( this.listeners[x].handleEvent(event) ) {
          handled = true;
        }
      }
      
      if ( !handled ) {
        UI.SYSTEM_LOG.model.handleEvent(event);
      }
      
    }
  };
   
var UI = function () {
  var that = {
    'model': MODEL,
    
    'CHAT': false,
    'SYSTEM_LOG': false,
    'ZONE': false,
     
    'navbar': false,
     
    'init': function() {
      MODEL.init();
       
      this.navbar = $("#navbar");
       
      // Register AJAX Error Handler
      $('body').ajaxError(function(event, xml, options, error) {
        that.logError({
          'event': event,
          'options': options,
          'error': error
        });
        auth_logout(that.model.sessionkey, function() {
          that.model.setSessionkey(false); // We kill ourself 
        });
         
      });
       
      
              
      this.CHAT = chat_ui();
      this.SYSTEM_LOG = system_log_ui();
      this.ZONE = zone_ui();
      
      this.registerNavBarAction({
        'label': 'Logout',
        'callback': function() {
          that.logout();
        }
      });
    },
    
    'registerNavBarAction': function (obj) {
      var html = '';
      
      if ( obj.icon ) {
        html = '<img src="' + obj.icon + '" border=0 />';
      } else {
        html = '<span>' + obj.label + '</span>';
      }
      var code = $(html).click(function() {
        obj.callback();
      });
      that.navbar.append(code);
    },
    
    'logout': function() {
      if (that.model.sessionkey) {
        that.model.logout();  
      }
      that.showLoginDialog();
    },
     
    'logError': function(Error) {
       alert("Error: " + Error.error);
    },
     
     
    'input': function(parameters) {
      $("#ui_input").dialog({
        'modal': true,
        'closeOnEscape': true,
        'title': parameters.message,
        'buttons': {
        'OK': function() {
            parameters.callback($("#ui_input input.answer").val());
            $("#ui_input").dialog('destroy');
          }
        }
      });
    },
     
    /**
     * Clear the session and show the login dialog
     */
    'showLoginDialog': function() {
      // Reset the password field
      $("#password").val('');
    
      $('#login_dialog').dialog({
        'modal':true,
        'closeOnEscape': false,
        'draggable': false,
        'title': 'Login',
        'buttons': {
          'Login': function() {
            that.model.login($("#username").val(), $("#password").val());
            $("#login_dialog").dialog('destroy');
          }
        },
      
        'beforeclose':function(event, ui) {
          return that.model.sessionkey !== false;
        }
      });
    }
  
  }
  return that;
}();
   
$(document).ready(function() {
  UI.init();
  UI.showLoginDialog();
});