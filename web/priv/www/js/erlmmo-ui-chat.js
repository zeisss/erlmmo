"use strict";

/* global MODEL: false, UI: false, $: false */


function chat_model() {
  var that = {
    'listeners': [],
    /*
    channel: {
      'name': 'NAME',
      'sessions': [
        {'name':'NAME', 'gravatar_id':'GRAVATAR'}
        
      ],
      'messages': [
        ?MESSAGE_EVENT
      ]
    }
    */
    'channels': [],
    
    'init': function () {
      MODEL.listeners.push(this);
      
    },
    
    'handleEvent': function (event) {
      if (event.type === "chat_join_self") {
        this.newChannel(event);
      } else if (event.type === "chat_join") {
        this.addChannelPlayer(
          event.name,
          event.player
        );
      } else if (event.type === "chat_part_self") {
        this.deleteChannel(event.name);
      } else if (event.type === "chat_part") {
        this.removeChannelPlayer(event.name, event.player);
      } else if (event.type === "chat_send") {
        this.addChannelMessage(event.name, event.player, event.message);
      } else {
        return false;
      }
      return true;
    },
     
    'fireEvent': function (event_object) {
      for (var x in this.listeners ) {
        if (typeof this.listeners[x] === 'function') {
          this.listeners[x](event_object);
        }
      }
    },
     
    'newChannel': function (Channel) {
      this.channels[Channel.name] = {
        'name': Channel.name,
        'messages': [],
        'players': Channel.players
      };
      
      
      this.fireEvent(['chat_join', Channel.name]);
    },
     
    'deleteChannel': function (ChannelName) {
      delete(this.channels[ChannelName]);
      this.fireEvent(['chat_part', ChannelName]);
    },
     
    'addChannelPlayer': function (ChannelName, Player) {
      this.channels[ChannelName].players.push(Player);
      this.fireEvent(['players_changed', ChannelName]);
    },
     
    'removeChannelPlayer': function (ChannelName, Player) {
      var Players = this.channels[ChannelName].players.filter(function (x) {
        return x !== Player;
      });
      this.channels[ChannelName].players = Players;
      this.fireEvent(['players_changed', ChannelName]);
    },
     
    'addChannelMessage': function (ChannelName, Player, Message) {
      if (this.channels[ChannelName]) {
        this.channels[ChannelName].messages.push({
          'player': Player,
          'message': Message
        });
        this.fireEvent(['chat_send', ChannelName]);
      }
    },
     
     
    'partChannel': function (Channel) {
      chat_part(MODEL.sessionkey, Channel);
    }
      
  };
  
  return that;
}
   
   
function chat_ui() {
  var that = {
    'model': chat_model(),
    'currentChannel': false,
    
    'init': function () {
      this.model.init();
      
      this.model.listeners.push(function (event) {
        return that.handleChatModelEvent(event);
      });
      
      UI.registerNavBarAction({
        'label': 'Channels',
        'callback': function() {
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
      });
      
      
      UI.registerNavBarAction({
       'label': 'Chat',
       'callback': function() {
         that.show();
       }
      });
      
      var chat = $('#chat_window').dialog({
        'title': 'Chat',
        'minHeight': 300,
        'height': 300,
        'minWidth': 400,
        'width': 490
      });
      chat.parent().css("top", "");
      chat.parent().css("bottom", "20px");
      chat.parent().css("left", "20px");
      
      
      
      
      // The "Say" button
      $("#chat_window").submit(function(event) {
        that.eventSendMessage();
        return false;
      });
      
      $("#chat_part").click(function() {
        that.model.partChannel(UI.CHAT.currentChannel);
      });
    },
    
    'selectChannel': function (ChannelName) {
      // Channel exists
      if ( this.model.channels[ChannelName])
      {
        this.currentChannel = ChannelName;
        this.renderPlayerList(ChannelName);
        this.renderChatMessages(ChannelName);
        this.renderSelectedChannelTab();
      }
    }, // selectChannel
    
    'eventSendMessage': function () {
      if ( !MODEL.sessionkey || !this.currentChannel) {
        alert("Not logged in yet!");
        return false;
      }
      
      try {
        var Text = $('#chat_input').val();
        chat_send(MODEL.sessionkey, this.currentChannel, Text);
        $('#chat_input').val('');
      } catch (E) {
        UI.logError(E);
      }
      return false;
    },
    
    'handleChatModelEvent': function (event) {
      if ( event[0] == "chat_join") {
        this.renderChannelTabs();
        if ( this.currentChannel === false) {
          this.selectChannel(event[1]);
        }
      }
      else if ( event[0] == "chat_part") {
        this.renderChannelTabs();
        
        if ( event[1] == this.currentChannel) {
          for ( var x in this.model.channels ) {
            this.selectChannel(x);
            return; // simply the select the first channel
          }
        }
      }
      else if ( event[0] == 'players_changed')
      {
        if ( event[1] == this.currentChannel ) {
          // The players list has been updated
          this.renderPlayerList(event[1]);
        } else {
          // we do not look at this channel currently, so we don't care
        }
      } else if ( event[0] == "chat_send") {
        if ( this.currentChannel == event[1]) {
          this.renderChatMessages(event[1]);
        }
      } else {
        $("#chat_messages").append("<div>Unknown chat model event: " + event[0] + "</div>");  
      }
    },
       
    'renderChatMessages': function(ChannelName) {
      var div = $("#chat_messages");
      div.html('');
      for ( var x in this.model.channels[ChannelName].messages) {
        var msg = this.model.channels[ChannelName].messages[x];
        div.append("<div><b>" + msg.player + "</b>: " + msg.message + "</div>" );
      }
    },
       
    'renderSelectedChannelTab': function() {
        var ChannelName = this.currentChannel;
        var ul = $("#chat_window .channels ul li");
        ul.each(function() {
        if ( $(this).text() == ChannelName) {
          $(this).addClass("selected");
        } else {
          $(this).removeClass("selected");
        }
      });
    },
       
    'renderChannelTabs': function() {
      var ul = $("#chat_window .channels ul");
      ul.html('Local' in this.model.channels ? '<li>Local</li>' : '');
      for ( var name in this.model.channels) {
        if ( name != "Local") {
          ul.append("<li>" + name + "</li>");
        }
      }
      
      
      
      this.renderSelectedChannelTab();
         
      $("#chat_window .channels ul li").click(function() {
        UI.CHAT.selectChannel($(this).text());
      });
    },
       
    'renderPlayerList': function(ChannelName) {
      var Channel = this.model.channels[ChannelName];
         
      var ul = $("#chat_window .channel_sessions ul");
      ul.html(''); // Clear
      for( var x in Channel.players)
      {
        ul.append('<li>' + Channel.players[x] + '</li>');
      }
    },
    
    'show': function() {
      $("#chat_window").dialog('open');
    }
  };
  that.init();
  return that;
}