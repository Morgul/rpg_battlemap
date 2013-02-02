Ember.TEMPLATES['mapTool'] = Ember.Handlebars.compile(
	'{{ content.currentTool }}'
);

Ember.TEMPLATES['mapToolBar'] = Ember.Handlebars.compile(
	'<button class="btn btn-mini" {{ bindStyle display="combatantSelected" }} {{action setToolToMoveCombatant target="view"}}>Move Combatant</button>' +
	'<button class="btn btn-mini" {{ action setToolToAddCombatant target="view" }}>Add Combatant</button>'
);

RPGB.MapToolView = Ember.View.extend({
	templateName: 'mapTool',
	classNames: ['pull-right', 'label']
});

RPGB.MapToolbar = Ember.View.extend({
	templateName: 'mapToolBar',
	classNames: ['pull-right', 'btn-group'],

	didInsertElement: function(){
		//window.gu = this;
		this.$().attr('data-toggle', 'buttons-radio');
	},

	clickedCellChanged: function(){
		var currentTool = this.get('content.currentTool');
		if(this[currentTool]){
			var clickedCell = this.get('content.clickedCell');
			this[currentTool](clickedCell);
			return;
		}
		console.log('no action taken for tool ', currentTool, clickedCell);
	}.observes('content.clickedCell'),

	combatantSelected: function(){
		if(this.get('content.combatants.selected')){
			return 'inline-block';
		}
		return 'none';
	}.property('content.combatants.selected'),

	setToolToMoveCombatant: function(ev){
		this.set('content.currentTool', 'moveCombatant');
	},

	setToolToAddCombatant: function(ev){
		this.set('content.currentTool', 'addCombatant');
	},

	moveCombatant: function(cell){
		var combatant = this.get('content.combatants.selected');
		if(! combatant){
			return;
		}
		combatant.setProperties({'x':cell.x, 'y':cell.y});
	}
});

RPGB.MapView = Ember.View.extend({
	templateName: 'map',
	content: null,
	viewHeight: '100%',
	/*zoom: 1,
	panX: 0,
	panY: 0,*/
	//panning: false,
	//_tool: undefined,

	transformString: function(){
		return 'translate(' + this.get('content.panX') + ' ' + this.get('content.panY') + ') scale(' + this.get('content.zoom') + ')';
	}.property('content.zoom','content.panX','content.panY'),

	/*currentTool: function(){
		if(! this._tool){
			return false;
		}
		return this._tool.name;
	}.property('_tool'),*/

	init: function(){
		window.mapView = this;
		this._super();
		this._tool = this.panTool();
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

		/*var dragData = {
			lastX: 0,
			lastY: 0
		};*/

		this.$().mousedown(function(ev){
			thisRef.toolDispatch('mousedown', ev);
		});
		this.$().mouseup(function(ev){
			thisRef.toolDispatch('mouseup', ev);
		});
		this.$().mousemove(function(ev){
			thisRef.toolDispatch('mousemove', ev);
		});
		this.$().mouseout(function(ev){
			thisRef.toolDispatch('mouseout', ev);
		});
		/*this.$().mousedown(function(ev){
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
				thisRef._suppressNextClick = true;
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
		});*/
	},

	panTool: function(){
		var dragData = {
			lastX: 0,
			lastY: 0
		};
		var panning = false;

		var mousedown = function(_x, _y, event, map){
			panning = true;
			dragData.lastX = event.pageX;
			dragData.lastY = event.pageY;
			return false;
		};
		var mouseup = function(_x, _y, event, map){
			panning = false;
			return false;
		};
		var mousemove = function(_x, _y, event, map){
			if(! panning){
				return;
			}
			var deltaX = event.pageX - dragData.lastX;
			var deltaY = event.pageY - dragData.lastY;
			dragData.lastX = event.pageX;
			dragData.lastY = event.pageY;
			map.panEvent(deltaX, deltaY);
		};
		var mouseout = function(){
			panning = false;
		};

		return {
			'name': 'Pan Map',
			'mousedown': mousedown,
			'mouseup': mouseup,
			'mousemove': mousemove,
			'mouseout': mouseout
		};
	},

	toolDispatch: function(eventName, event){
		var content = this.get('content');
		var pixelX = event.pageX;
		var pixelY = event.pageY;
		var cellXY = this.pixelsToCell(pixelX, pixelY);
		var x = cellXY[0];
		var y = cellXY[1];
		return content.toolDispatch(eventName, x, y, event);
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
		this.get('content').adjustZoom(delta);
		return false;
	},

	windowResized: function(){
		var headerHeight = $('#head').height();
		var toolbarHeight = $('#tools').height();
		var guessHeight = window.innerHeight - (headerHeight + toolbarHeight + 28 + 20 + 20 + 20);
		this.set('viewHeight', guessHeight + 'px');
		var offsets = this.$('svg').offset();
		this.set('content.offsetX', offsets.left);
		this.set('content.offsetY', offsets.top);
	},

	clicked: function(){ },
	/*clicked:function(ev){
		if(this._suppressNextClick){
			this._suppressNextClick = false;
			return false;
		}
		var xy = this.containingCell(ev.offsetX, ev.offsetY);
		var xyObj = Ember.Object.create({
			x: xy[0],
			y: xy[1]
		});
		this.set('content.clickedCell', xyObj);
		this.set('content.nearestCell', xyObj);
		return false;
	},*/

	/*pixelsToCell: function(x,y){
		var outx = x * RPGB.CELL_SIZE;
		var outy = y * RPGB.CELL_SIZE;
		return [outx,outy];
	},*/

	pixelsToCell: function(x,y){
		var cellx = this.dimensionToCell(x, this.get('panX'));
		var celly = this.dimensionToCell(y, this.get('panY'));
		return [cellx, celly];
	},

	dimensionToCell: function(d, pan){
		var cell = RPGB.CELL_SIZE;
		var out = ( ( (d - pan) ) / this.get('zoom') ) * cell;
		return out;
	},

	/*containingCell: function(x,y){
		var cell = RPGB.CELL_SIZE;
		var cellX = Math.floor((x - this.get('panX')) / (this.get('zoom') * cell));
		var cellY = Math.floor((y - this.get('panY')) / (this.get('zoom') * cell));
		return [cellX,cellY];
	},

	nearestCell: function(x,y){
		var cell = RPGB.CELL_SIZE;
		var cellX = Math.round((x - this.get('panX')) / (this.get('zoom') * cell));
		var cellY = Math.round((y - this.get('panY')) / (this.get('zoom') * cell));
		return [cellX,cellY];
	}*/

});
