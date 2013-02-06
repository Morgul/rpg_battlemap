Ember.TEMPLATES['mapToolBar'] = Ember.Handlebars.compile(
	'<div class="btn-group" data-toggle="buttons-radio">' +
		'<button class="btn btn-mini" {{ bindStyle display="combatantSelected" }} {{action setToolToMoveCombatant target="content"}}>Move Combatant</button>' +
		'<button class="btn btn-mini" {{ action setToolToAddCombatant target="content" }}>Add Combatant</button>' +
		'<button class="btn btn-mini" {{ action setToolToRuler target="content"}}>Measure</button>' +
		'<button class="btn btn-mini active" {{ action setToolToPanMap target="content"}}>Pan Map</button>' +
	'</div>' +
	'<div class="label">{{ content.toolName }}</div>'
);

Ember.TEMPLATES['mapRuler'] = Ember.Handlebars.compile(
	'{{#if content.ruler.isMeasuring}}' +
		'<line ' +
			'{{bindAttr x1="view.draw.start.x"}} ' +
			'{{bindAttr y1="view.draw.start.y"}} ' +
			'{{bindAttr x2="view.draw.end.x" }} ' +
			'{{bindAttr y2="view.draw.end.y" }} ' +
			'stroke-width="13" ' +
			'stroke="black" ' +
			'stroke-linecap="round" ' +
		'/>' +
		'<line ' +
			'{{bindAttr x1="view.draw.start.x"}} ' +
			'{{bindAttr y1="view.draw.start.y"}} ' +
			'{{bindAttr x2="view.draw.end.x" }} ' +
			'{{bindAttr y2="view.draw.end.y" }} ' +
			'stroke-width="7" ' +
			'stroke="white" ' +
			'stroke-linecap="round" ' +
		'/>' +
		'<rect ' +
			'{{bindAttr x="view.textBg.x"}} ' +
			'{{bindAttr y="view.textBg.y"}} ' +
			'{{bindAttr width="view.textBg.width"}} ' +
			'{{bindAttr height="view.textBg.height"}} ' +
			'{{bindAttr rx="view.textBg.radius"}} ' +
			'stroke="black" ' +
			'fill="black" ' +
		'/>' +
		'<text ' +
			'{{bindAttr font-size="view.fontSize"}} ' +
			'{{bindAttr x="view.mid.x"}} ' +
			'{{bindAttr y="view.mid.y"}} ' +
			'stroke="white" ' +
			'fill="white" ' +
			'text-anchor="middle" ' +
		'>' +
		'{{ content.ruler.length }}' +
		'</text>' +
	'{{/if}}'
)

RPGB.MapToolbar = Ember.View.extend({
	templateName: 'mapToolBar',
	classNames: ['pull-right'],

	combatantSelected: function(){
		if(this.get('content.combatants.selected')){
			return 'inline-block';
		}
		return 'none';
	}.property('content.combatants.selected')
});

RPGB.MapRulerView = Ember.View.extend({
	templateName: 'mapRuler',
	tagName: 'g',
	content: null,

	draw: function(){
		var map = this.get('context.content');
		var ruler = map.get('ruler');
		if(ruler.get('isMeasuring')){
			startxy = map.cellToPixels(ruler.get('start.x') + 0.5, ruler.get('start.y') + 0.5);
			endxy = map.cellToPixels(ruler.get('end.x') + 0.5, ruler.get('end.y') + 0.5);
			return {
				start: {
					x: startxy[0],
					y: startxy[1]
				},
				end: {
					x: endxy[0],
					y: endxy[1]
				}
			};
		}
		return {};
	}.property('context.content.ruler.start', 'context.content.ruler.end'),

	mid: function(){
		var map = this.get('context.content');
		var ruler = map.get('ruler');
		if(ruler.get('isMeasuring')){
			var start = ruler.get('start');
			var end = ruler.get('end');
			midx = (start.x + end.x) / 2;
			midy = (start.y + end.y) / 2;
			midxy = map.cellToPixels(midx, midy);
			return {
				'x': midxy[0],
				'y': midxy[1]
			};
		}
		return null;
	}.property('context.content.ruler.start', 'context.content.ruler.end'),

	fontSize: function(){
		var zoom = this.get('context.content.zoom');
		var size = 12 * ( 1 / zoom);
		if(size < 12){
			size = 12;
		}
		return size + 'px';
	}.property('context.content.zoom'),

	textBg: function(){
		var mid = this.get('mid');
		if(mid == null){
			return null;
		}
		var fontSize = this.get('fontSize');
		fontSize = parseFloat(fontSize.substring(0, fontSize.length - 2));
		var length = this.get('context.content.ruler.length');
		var width = length.toString().length * fontSize;
		var x = mid.x - (width / 2);
		var y = mid.y - fontSize;
		var rx = fontSize / 2;
		var out = {
			'x': x,
			'y': y,
			'width': width,
			'height': fontSize + 5,
			'radius': rx
		};
		console.log('der out', out);
		return out;
	}.property('mid', 'fontSize').cacheable(false),

	inverseZoom: function(){
		var map = this.get('context.content');
		var panx = map.get('panX');
		var pany = map.get('panY');
		var zoom = map.get('zoom');
		zoom = 1 / zoom;
		panx = panx * -1;
		pany = pany * -1;
		var str = 'scale(' + zoom + ') translate(' + panx + ' ' + pany + ')';
		return str;
	}.property('context.content.zoom', 'context.content.panX', 'context.content.panY')
});

RPGB.MapView = Ember.View.extend({
	templateName: 'map',
	content: null,
	viewHeight: '100%',

	transformString: function(){
		return 'translate(' + this.get('content.panX') + ' ' + this.get('content.panY') + ') scale(' + this.get('content.zoom') + ')';
	}.property('content.zoom','content.panX','content.panY'),

	init: function(){
		window.mapView = this;
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
		this.$().mouseleave(function(ev){
			thisRef.toolDispatch('mouseleave', ev);
		});
		this.$().click(function(ev){
			thisRef.toolDispatch('click', ev);
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

	toolDispatch: function(eventName, event){
		var content = this.get('content');
		/*var pixelX = event.pageX;
		var pixelY = event.pageY;
		var cellXY = this.pixelsToCell(pixelX, pixelY);
		var x = cellXY[0];
		var y = cellXY[1];*/
		return content.toolDispatch(eventName, event);
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

	/*pixelsToCell: function(x,y){
		var cellx = this.dimensionToCell(x, this.get('panX'));
		var celly = this.dimensionToCell(y, this.get('panY'));
		return [cellx, celly];
	},

	dimensionToCell: function(d, pan){
		var cell = RPGB.CELL_SIZE;
		var out = ( ( (d - pan) ) / this.get('zoom') ) * cell;
		return out;
	},*/

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
