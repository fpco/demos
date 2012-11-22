$(function() {

  // SERVER HANDLING CLASS
  Simulation.Server = function(opts) {
    // What to call when a new world is available to be rendered
    if(opts.callback) this.callback = opts.callback;
    // What to call when the current FPS is known
    if(opts.callbackFps) this.callbackFps = opts.callbackFps;
  };

  // Reset. Fetch the world json again and start playing
  Simulation.Server.prototype.reset = function(worldName) {
    var self = this;
    // Pause the simulation while we fetch world info
    self.pause();
    // Get the World description
    var worldUrl = Simulation.urls[worldName];
    if(worldUrl) {
      self.ajaxGet(worldUrl, function(world) {
        // The Play button may have been pressed while we
        // were fetching the world information
        self.pause();
        // Store the world description
        self.setWorld(world);
        // Play simulation
        self.play();
      });
    }
  };

  // Set the world variable
  Simulation.Server.prototype.setWorld = function(world) {
    this.world = world;
  };

  // Play/Resume simulation
  Simulation.Server.prototype.play = function(){
    // Reset anything that needs to be reset
    this.startTime = 0;
    this.actualFps = 0;
    this.running = true;
    // Run one step of the simulation and schedule the next step
    this.step();
  };

  // Pause the simulation
  Simulation.Server.prototype.pause = function() {
    this.stopAjax();
    this.running = false;
    clearTimeout(this.timer);
    this.timer = null;
  };

  // AJAX REQUESTS
  // Get JSON from the server
  Simulation.Server.prototype.ajaxGet = function(url, success) {
    this.stopAjax();
    this.xhr = $.getJSON(url).success(success);
  };

  // Post JSON to the server
  Simulation.Server.prototype.ajaxPost = function(opts, success) {
    this.stopAjax();
    this.xhr = $.ajax(opts).success(success);
  };

  // Stop any XHR requests
  Simulation.Server.prototype.stopAjax = function() {
    if(this.xhr) {
      this.xhr.abort();
      this.xhr = null;
    }
  };

  // Toggle the simulation
  // Returns the current state of the simulation
  Simulation.Server.prototype.toggle = function() {
    if(this.running) this.pause();
    else this.play();
    return this.running;
  };

  // Make one step of the simulation and schedule the next step
  Simulation.Server.prototype.step = function() {
    var self = this;
    // Save bandwidth by not sending bhtree back
    self.world.bhTree = null;
    self.ajaxPost({
      "data"    : JSON.stringify(self.world),
      "type"    : "POST",
      "url"     : Simulation.advanceUrl
    }, function(newWorld) {
      // We got new world data to render
      // Render it on screen
      self.world = newWorld;
      // Note the time when we finished processing this frame
      //  (inclusive of timer, fetching data, and rendering)
      //  and calculate the actual FPS
      if(self.startTime) self.actualFps = Math.round(1000/((new Date())-self.startTime));
      else self.actualFps = 0;
      // If we are still running
      if(self.running) {
        // Set the startTime for the next frame of the simulation
        self.startTime = new Date();
        // Set the timeout
        self.timer = setTimeout(function(){
          self.timer=null;
          self.step();
        }, 1000/Simulation.fps);
        // Render
        self.callback(self.world);
        self.callbackFps(self.actualFps);
      }
    });
  };

  // CONTROLLER CLASS
  // Gets the Server and the View objects to use
  Simulation.Controller = function() {
    var self = this;
    this.server = new Simulation.Server({
      callback: function(world) {
        // Draw the world
        self.view.draw(world);
        // Discard particles that go out of bounds
        if(self.discardx) {
          world.parts = world.parts.filter(function(p) {return self.view.isVisible(p,world.pixInM);});
          self.server.setWorld(world);
        }
      },
      callbackFps: function(fps) { self.view.drawFps(fps); }
    });
    this.view   = new Simulation.View({
      resetcb    : function() {self.reset();},
      togglecb   : function() {return self.server.toggle();},
      selectcb   : function(worldName) {self.reset(worldName);},
      discardxcb : function(b) {self.setDiscardx(b);}
    });
    // Discard particles that escape the window bounds
    this.discardx = true;
    // Set the default world
    this.reset('solar');
  };

  Simulation.Controller.prototype.setDiscardx = function(b) {
    this.discardx = b;
  };

  Simulation.Controller.prototype.reset = function(worldName) {
    if(worldName) this.worldName = worldName;
    this.server.reset(this.worldName);
    // We automatically play after a reset
    this.view.setPlay(true);
  };

  // VIEW CLASS
  Simulation.View = function(opts){
    var self = this;

    // Cache ref to canvas context
    var canvas = document.getElementById('sky');
    this.ctx = canvas.getContext('2d');
    // The dimensions of the canvas
    this.dim = canvas.width;

    // Should we show the bhtree?
    this.showtree = true;

    // Attach handlers
    $("#reset").click(function() { opts.resetcb(); });
    $("#pause").click(function() { self.setPlay(opts.togglecb()); });
    $("select").change(function() { opts.selectcb($("select option:selected").val()); });
    $("#discardx").change(function() { opts.discardxcb(this.checked); });
    $("#bhtree").change(function() { self.showtree = this.checked; });
  };

  Simulation.View.prototype.setPlay = function(playing) {
    if(playing) {
      $('#pause').text("Pause");
    } else {
      $('#pause').text("Play");
    }
  };

  // Check if a particle is visible on the view
  Simulation.View.prototype.isVisible = function(part, pixInM) {
    var x = this.dim/2 + pixInM * part.pos.x;
    var y = this.dim/2 + pixInM * part.pos.y;
    return (x>-10 && x<this.dim+10 && y>-10 && y<this.dim+10);
  };

  // Display FPS
  Simulation.View.prototype.drawFps = function(fps) {
    $('#fps').text(fps);
  };

  // A Function that draws a list of particles
  var drawParticles = function(ctx, parts, pixInM, pixInKg, dim) {
    // Draw particles
    for (var j = 0; j < parts.length; j++) {
      var part = parts[j];
      var size = Math.log(part.mass/pixInKg) / Math.LN10;
      if (size < 2) size = 2;
      var x = dim/2 + pixInM * part.pos.x;
      var y = dim/2 + pixInM * part.pos.y;

      // Is the particle visible?
      if ( x > -10 && x < dim + 10 && y > -10 && y < dim + 10) {
        ctx.beginPath();
        ctx.arc(x, y, size/2, 0, Math.PI * 2, true);
        ctx.closePath();
        ctx.fill();
      }
    }
  };

  // A Function that draws a BHTree
  var drawTree = function(ctx, tree, pixInM, pixInKg, dim) {
    // only draw bounds for parent nodes
    if(tree.bhTreeBranch.length > 0) {
      // Draw circle
      var size = 30*Math.log(tree.bhTreeMass/pixInKg) / Math.LN10;
      var centerx = dim/2 + pixInM * tree.bhTreeCenterX;
      var centery = dim/2 + pixInM * tree.bhTreeCenterY;
      if(size > 2 && centerx > 0 && centery > 0) {
        ctx.beginPath();
        ctx.arc(centerx, centery, size/2, 0, Math.PI * 2, true);
        ctx.closePath();
        ctx.fill();
      }
      // Draw bounds
      var lx = dim/2 + pixInM * tree.bhTreeBox.boxLowerLeftX;
      var ux = dim/2 + pixInM * tree.bhTreeBox.boxUpperRightX;
      var ly = dim/2 + pixInM * tree.bhTreeBox.boxLowerLeftY;
      var uy = dim/2 + pixInM * tree.bhTreeBox.boxUpperRightY;
      ctx.strokeRect(lx, ly, (ux-lx), (uy-ly));

      // Recursively draw subtrees
      for (var j = 0; j < tree.bhTreeBranch.length; j++) {
        drawTree(ctx, tree.bhTreeBranch[j], pixInM, pixInKg, dim);
      }
    }
  };

  // Draws the world on the view canvas
  Simulation.View.prototype.draw = function(world) {
    if(!world) return; // Might happen

    // Clear
    this.ctx.clearRect(0,0,this.dim,this.dim);
    this.ctx.lineWidth = 2;

    // Draw BHTree
    if (this.showtree && world.bhTree !== null) {
      this.ctx.fillStyle = "rgba(32, 45, 21, 0.3)";
      this.ctx.strokeStyle = "rgba(50, 100, 50, 0.8)";
      this.ctx.lineWidth = 1;
      drawTree(this.ctx, world.bhTree, world.pixInM, world.pixInKg, this.dim);
    }

    // Draw Particles
    this.ctx.fillStyle = "white";
    drawParticles(this.ctx, world.parts, world.pixInM, world.pixInKg, this.dim);
  };

  // Start everything on page load
  (new Simulation.Controller());

});

