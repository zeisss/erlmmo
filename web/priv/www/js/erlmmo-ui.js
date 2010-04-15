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
   
   var UI = {
     'CHAT': false,
     'SYSTEM_LOG': false,
     'ZONE': false,
     
     'init': function() {
       MODEL.init();
       
       this.CHAT = chat_ui();
       this.SYSTEM_LOG = system_log_ui();
       this.ZONE = zone_ui();
       
       // Register AJAX Error Handler
       $('body').ajaxError(function(event, xml, options, error) {
         UI.logError({
           'event': event,
           'options': options,
           'error': error
         });
         MODEL.setSessionkey(false); // We kill ourself
       });
       
       this.CHAT.init();
       this.SYSTEM_LOG.init();
       this.ZONE.init();
     },
     
     'logout': function() {
       if (MODEL.sessionkey) {
         MODEL.logout();  
       }
       UI.showLoginDialog();
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
          MODEL.login($("#username").val(), $("#password").val());
          $("#login_dialog").dialog('destroy');
        }
      },
      
      'beforeclose':function(event, ui) {
        return MODEL.sessionkey !== false;
      }
    });
  },
  
  'showChatChannelDialog': function() {
    var chat = $("#chat_channels_dialog").dialog({
      'closeOnEscape': true,
      'draggable': true,
      'title': 'Channels',
      'buttons': {
        'New': function() {
          UI.input({
            'message': 'Enter the name of the new channel:',
            'callback': function(input_text) {
              chat_join(MODEL.sessionkey, input_text);
              $("#chat_channels_dialog").dialog('destroy');
            }
          });
        }
      }
    });
    
    $("#chat_channels_dialog ul").html("<li>Loading ...</li>");
    
    chat_get_all_channels(function(channels) {
      var ul = $("#chat_channels_dialog ul");
      ul.html('');
      
      for (var i in channels) {
        ul.append('<li>' + channels[i] + '</li>');
      }
      
      $("#chat_channels_dialog ul li").click(function() {
        var channelName = $(this).text();
        chat_join(MODEL.sessionkey, channelName);
        $("#chat_channels_dialog").dialog('destroy');
      });
    });
  }
     
     
};
   
$(document).ready(function() {
  UI.init();
  UI.showLoginDialog();
});