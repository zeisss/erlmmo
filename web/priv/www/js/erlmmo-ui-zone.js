"use strict";

/* global MODEL: false, UI: false, $: false */

function zone_model() {
  var that = {
    'zone_name': false,
    // Where is the ship currently?
    'coordinates': false,
  
    // Who am I? :)
    'selfObject': false,
  
    'listeners': [],
    'objects': [],
  
    'handleEvent': function(event) {
      if ( event.type == "zone_info" ) {
        that.zone_name = event.name;
        this.fireEvent(['zone_info', that.zone_name]);
        return true;
      }
      else if ( event.type == "zone_status" ) {
        this.selfObject = event.self;
      
        this.updateObjects(event.objects);
        return true;
      }
      return false;
    },
  
    'updateObjects': function(objects) {
      this.objects = [];
     
      for ( var x in objects ) {
        var obj = objects[x];
        // each obj: {coord:Ê{x:0, y:1}, player:'Miro'}
        this.objects.push(obj);  
      }
      this.fireEvent(['objects_updated']);
    },
  
    'fireEvent': function(event) {
      for ( var x in this.listeners) {
        this.listeners[x].handleEvent(event);
      }
    } 
  };
  MODEL.listeners.push(that);
  return that;
}
   

/**
 * The zone_ui renders the frontend of a zone in the browser. For this it utilitzes the new canvas 2d drawing-api.
 * 
 */
function zone_ui() {
  var that = {
    'model': zone_model(),
    'context': false,
       
    'visibleFieldsHorizontal': 11,
    'visibleFieldsVertical': 7,
       
    'fieldRenderWidth' : 100,
    'fieldRenderHeight':  100,
       
    'init': function() {
      this.model.listeners.push(this);
      this.context = document.getElementById("zone_ui_canvas").getContext("2d");
      this.context.canvas.width = this.visibleFieldsHorizontal * this.fieldRenderWidth;
      this.context.canvas.height = this.visibleFieldsVertical * this.fieldRenderHeight;
         
      var list = $("#zone_list").dialog({
        'title':'Zone Objects',
        'width': 250,
        'height': 550
      });
      list.parent().css("top", "60px");
      list.parent().css("left", "");
      list.parent().css("right", "20px");
         
      $("#zone_ui_canvas").click(function(event) {
        return that.event_click(event);
      }).mousemove(function(event) {
        return that.event_mousemove(event);
      });
         
      this.redraw();
    },
    'event_click': function(event) {
         
    },
       
    'event_mousemove': function(event) {
        
      var x = (event.pageX - this.context.canvas.offsetLeft) / this.context.canvas.clientWidth;
      var y = (event.pageY - this.context.canvas.offsetTop) / this.context.canvas.clientHeight;
      
      this.x = x * this.context.canvas.width;
      this.y = y * this.context.canvas.height;
      this.redraw();
    },
       
    'handleEvent': function(event) {
      if ( event[0] == "objects_updated") {
        this.renderObjectList();
        this.redraw();
      } else if ( event[0] == "zone_info") {
        this.renderObjectList();
      }
    },
       
    /**
     * Translate a game x,y coordinate into the upper left point of the field on the canvas
     *
     * Returns: [canvas_x, canvas_y]
     */
    'coordinates2point': function(x,y) {
      if ( !this.model.selfObject ) {
        return [x,y];
      }
      
      var posX = Math.floor(this.visibleFieldsHorizontal / 2); // 5
      var posY = Math.floor(this.visibleFieldsVertical / 2); // 4
      
      var coords = this.model.selfObject.coord;
      
      var tmpX = (posX + (x - coords.x)) * this.fieldRenderWidth;
      var tmpY = (posY + (y - coords.y)) * this.fieldRenderHeight;
      return [ tmpX, tmpY ];
    },
    
    
    
    
    'renderObjectList': function() {
      $("#zone_list .zone_name").text(this.model.zone_name);
      
      var ul = $("#zone_list ul").html('');
      for ( var x in this.model.objects) {
        var obj = this.model.objects[x];
        
        ul.append('<li>' + obj.name + ' (' + obj.prototype.name + ') [' + obj.coord.x + ', ' + obj.coord.y + ']</li>');
      }
    },

    // Use the this.context to 'draw' the known objects on to the 'background' of the UI
    'redraw': function() {
      var ctx = this.context;
      ctx.save();
      var w = ctx.canvas.width;
      var h = ctx.canvas.height;
      ctx.clearRect(0,0, w, h);
      
      this.drawZoneGrid(ctx);
      this.drawZoneObjects(ctx);
      this.drawZoneMouse(ctx);
      ctx.restore();
    },
    
    
    'drawZoneMouse': function(ctx) {
      ctx.save();
      
      ctx.beginPath();
      ctx.arc(this.x, this.y, this.fieldRenderHeight * 0.4, 0, 360, false);
      ctx.fill();
      
      
      ctx.restore();
    },
    
    'drawZoneGrid': function(ctx) {
      ctx.save();
      ctx.strokeStyle = "rgb(190,190,190)";
      
      for ( var x = 0; x <= ctx.canvas.width; x += this.fieldRenderWidth) {
        for ( var y = 0; y <= ctx.canvas.height; y+= this.fieldRenderHeight) {
          ctx.strokeRect(x,y, this.fieldRenderWidth, this.fieldRenderHeight);
          
        }
      }
      
      ctx.restore();
    },
    
    'drawZoneObjects': function(ctx) {
      ctx.save();
      
      for ( var i in this.model.objects ) {
        var object = this.model.objects[i];
        var Coord = this.coordinates2point(object.coord.x, object.coord.y);
        
        ctx.beginPath();
        
        if ( object.prototype.size === 1)
        {
         
             ctx.moveTo(5 + Coord[0], 5 + Coord[1]);
             ctx.lineTo(-5 + Coord[0] + this.fieldRenderWidth,
                        -5 + Coord[1] + this.fieldRenderHeight);
             
             ctx.moveTo(Coord[0] + this.fieldRenderWidth - 5, 5 + Coord[1]);
             ctx.lineTo(Coord[0] + 5, Coord[1] + this.fieldRenderHeight - 5);
             ctx.stroke();
              
        } else {
             var x = Coord[0] + this.fieldRenderWidth / 2;
             var y = Coord[1] + this.fieldRenderHeight / 2;
             
             var range = this.fieldRenderWidth / 2 + (this.fieldRenderWidth * (object.prototype.size -1));
             
             ctx.arc (
                   x,
                   y,
                   range,
                   0,
                   360,
                   false);
             ctx.fill();
        }
        ctx.closePath();
        
      }
      
      
      ctx.restore();
    }
  };
  that.init();
  return that;
}