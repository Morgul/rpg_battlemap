RPGB.MapView = Ember.View.extend({
	templateName: 'map',
	content: null,
	viewHeight: '100%',
	zoom: 1,
	panX: 0,
	panY: 0,
	panning: false,

	transformString: function(){
		return 'translate(' + this.get('panX') + ' ' + this.get('panY') + ') scale(' + this.get('zoom') + ')';
	}.property('zoom','panX','panY'),

	init: function(){
		this._super();
		window.mapViewThing = this;
		var thisRef = this;
		$(window).resize(function(){
			thisRef.windowResized();
		});
	},

	didInsertElement: function(){
		this.windowResized();
		// bind events
		var thisRef = this;
		this.$().mousewheel(function(ev, delta){
			thisRef.scrollEvent(ev, delta);
		});

		var dragData = {
			lastX: 0,
			lastY: 0
		};

		this.$().mousedown(function(ev){
			thisRef.set('panning', true);
			dragData.lastX = ev.pageX;
			dragData.lastY = ev.pageY;
			return false;
		});
		this.$().mouseup(function(){
			thisRef.set('panning', false);
			return false;
		});
		this.$().mousemove(function(ev){
			if(thisRef.get('panning')){
				var deltaX = ev.pageX - dragData.lastX;
				var deltaY = ev.pageY - dragData.lastY;
				dragData.lastX = ev.pageX;
				dragData.lastY = ev.pageY;
				thisRef.panEvent(deltaX, deltaY);
			}
		});
		this.$().mouseout(function(){
			thisRef.set('panning', false);
			return false;
		});
	},

	scrollEvent: function(ev, delta){
		if(isNaN(delta)){
			return false;
		}
		if(delta > 0){
			delta = 0.1;
		} else {
			delta = -0.1;
		}
		this.adjustZoom(delta);
		return false;
	},

	adjustZoom: function(delta){
		var oldZoom = this.get('zoom');
		var newZoom = oldZoom + delta;
		if(newZoom < 0.1){
			newZoom = 0.1;
		} else if(newZoom > 3){
			newZoom = 3;
		}
		this.set('zoom', newZoom);
	},

	panEvent: function(dx, dy){
		var pannedX = this.get('panX');
		var pannedY = this.get('panY');
		this.set('panX', pannedX + dx);
		this.set('panY', pannedY + dy);
	},

	windowResized: function(){
		var headerHeight = $('#head').height();
		var toolbarHeight = $('#tools').height();
		var guessHeight = window.innerHeight - (headerHeight + toolbarHeight + 28 + 20 + 20);
		this.set('viewHeight', guessHeight + 'px');
	},

	clicked:function(){
		console.log('twas clicked');
	}
});
