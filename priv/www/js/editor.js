/******************************************************************************
Class Editor

A class for managing the editor.
******************************************************************************/
function Editor(battlemap){
	this.battlemap = battlemap;
	this.zones = [];
	this.currentZone = null;
	this.pointer = new Pointer(this.battlemap, 4, "#09f", "#05d");
	this.bindEvents();
}

Editor.prototype = {
	get pointStroke(){
		return this._pointStroke;
	},
	set pointStroke(val){
		this._pointStroke = val;
		this.triggerUpdate();
	},

	get pointFill(){
		return this._pointFill;
	},
	set pointFill(val){
		this._pointFill = val;
		this.triggerUpdate();
	}
}

Editor.prototype.bindEvents = function(){
	$(this.pointer).mouseup($.proxy(this.clickHandler, this));
}

Editor.prototype.triggerUpdate = function(){
	$(this).trigger('prop-update', undefined);
}

Editor.prototype.clickHandler = function(ev){
	if(this.currentZone == null){
		this.currentZone = new EditZone(this.battlemap, this);
	}


	var zone = this.currentZone;
	if (zone.getPoint(this.pointer.position.x, this.pointer.position.y) == null){
		this.currentZone.addPoint(this.pointer.position.x, this.pointer.position.y);
	} else {
		console.log('Already got a point.');
	}
}

/******************************************************************************
Class EditZone

A class for managing the editor.
******************************************************************************/
function EditZone(battlemap, editor){
	this.battlemap = battlemap;
	this.editor = editor;
	this.points = {};
}

EditZone.prototype.p2s = function(x, y){
	return "" + x + "," + y;
}

EditZone.prototype.addPoint = function(x, y){
	var point = new Point(this.battlemap, x, y);
	this.points[this.p2s(x, y)] = point;

	return point;
}

EditZone.prototype.getPoint = function(x, y){
	var point = this.points[this.p2s(x, y)];
	if (typeof point == 'undefined'){
		return null;
	} else {
		return point;
	}
}
/******************************************************************************
Class Pointer

A simple class the represents the svg element acting as our pointer on the
battlemap grid.

Events
* mouseup - The mouseup event from the battlemap canvas. This is for your
			convienence; it allows you to treat the pointer object as the
			object recieving click events.

* mousedown - The mousedown event from the battlemap canvas. This is for your
			convienence; it allows you to treat the pointer object as the
			object recieving click events.

* mousemove - The mousemove event from the battlemap canvas. This is for your
			convienence; it allows you to treat the pointer object as the
			object recieving click events.

Properties
* transform :: string() - The transform string used by the internal svg element.

* position :: {x, y, rawX, rawY} - Represents the position of the pointer. The
			x,y properties are in cell coordinates, while rawX,rawY are in page
			coordinates.

* offset :: {x, y} - Object with x and y parameters. Represents the offset from
			page coordinates.

 * svgElement :: Object() - The underlying svg element
******************************************************************************/
function Pointer(battlemap, size, fill, stroke) {
	this.battlemap = battlemap;
	this.position = {x: 0, y: 0, rawX: 0, rawY: 0};

	// Set offset
	var offset_ = $(this.battlemap.actionElem).offset();
	this.offset = {x: offset_.left, y: offset_.top};

	// Build svg element
	//this.svgElement = this.battlemap.toolPaper.circle(0, 0, size);
	this.svgElement = this.battlemap.svgPaper.circle(0, 0, size);

	if (typeof fill !== 'undefined'){
		this.svgElement.attr("fill", fill);
	}

	if (typeof stroke !== 'undefined'){
		this.svgElement.attr("stroke", stroke);
	}

	this.bindEvents();
}

Pointer.prototype = {
	get transform(){
		return this._transform
	},

	set transform(val){
		var trans = Raphael.parseTransformString(val);
		this.svgElement.transform("T" + trans[1][1] + "," + trans[1][2]);
	}
}

Pointer.prototype.bindEvents = function(){
	$(this.battlemap.actionElem).mouseup($.proxy(this.mouseupHandler, this));
	$(this.battlemap.actionElem).mousedown($.proxy(this.mousedownHandler, this));
	$(this.battlemap.actionElem).mousemove($.proxy(this.mousemoveHandler, this));
	$(this.battlemap).bind("viewChanged", $.proxy(this.mousemoveHandler, this));
}

Pointer.prototype.mouseupHandler = function(ev){
	$(this).trigger('mouseup', ev);
}

Pointer.prototype.mousedownHandler = function(ev){
	$(this).trigger('mousedown', ev);
}

Pointer.prototype.mousemoveHandler = function(ev){
	if (typeof ev.pageX == 'undefined'){
		ev.pageX = this.position.rawX;
	}
	if (typeof ev.pageY == 'undefined'){
		ev.pageY = this.position.rawY;
	}

	// handle our special logic
	this.position.rawX = ev.pageX;
	this.position.rawY = ev.pageY;
	var cell = this.battlemap.getNearestCell(ev.pageX - this.offset.x, ev.pageY - this.offset.y);
	this.position.x = cell[0];
	this.position.y = cell[1];
	this.transform = this.battlemap.getTransformString(cell[0], cell[1]);

	// trigger our mousemove event
	$(this).trigger('mousemove', ev);
}

/******************************************************************************
Class Point

A simple class the represents a user-interactable point on the grid.

Events
* mouseup - The mouseup event from the battlemap canvas. This is for your
			convienence; it allows you to treat the pointer object as the
			object recieving click events.

* mousedown - The mousedown event from the battlemap canvas. This is for your
			convienence; it allows you to treat the pointer object as the
			object recieving click events.

* mousemove - The mousemove event from the battlemap canvas. This is for your
			convienence; it allows you to treat the pointer object as the
			object recieving click events.

Properties
* position :: {x, y, rawX, rawY} - Represents the position of the pointer. The
			x,y properties are in cell coordinates, while rawX,rawY are in page
			coordinates.

 * svgElement :: Object() - The underlying svg element
******************************************************************************/
function Point(battlemap, posX, posY, size, fill, stroke) {
	this.battlemap = battlemap;
	this._position = {x: 0, y:0};

	// Build svg element
	if (typeof size == 'undefined'){
		size = 3;
	}
	this.size = size;
	//this.svgElement = this.battlemap.toolPaper.circle(0, 0, size);
	this.svgElement = this.battlemap.svgPaper.circle(0, 0, size);

	if (typeof fill == 'undefined'){
		fill = "#777";
	}
	this.fillColor = fill;

	if (typeof stroke == 'undefined'){
		stroke = "#777";
	}
	this.strokeColor = stroke;

	if ((typeof posX !== 'undefined') && (typeof posY !== 'undefined')){
		this.position = {x: posX, y: posY};
	}

	$(this.battlemap).bind("viewChanged", {context: this}, this.moveHandler);
}

Point.prototype = {
	get position(){
		return this._position;
	},
	set position(val){
		this._position.x = val.x;
		this._position.y = val.y;

		var trans = Raphael.parseTransformString(this.battlemap.getTransformString(val.x, val.y));
		this.svgElement.transform("T" + trans[1][1] + "," + trans[1][2]);
	},

	get size(){
		return this._size;
	},
	set size(val){
		if (typeof val !== 'undefined'){
			//TODO: Update the size of the SVG Element
			this._size = val;
		}
	},

	get strokeColor(){
		return this._strokeColor;
	},
	set strokeColor(val){
		if (typeof val !== 'undefined'){
			this._strokeColor = val;
			this.svgElement.attr("stroke", val);
		}
	},

	get fillColor(){
		return this._fillColor;
	},
	set fillColor(val){
		if (typeof val !== 'undefined'){
			this._fillColor = val;
			this.svgElement.attr("fill", val);
		}
	}
}

Point.prototype.moveHandler = function(ev){
	var context = ev.data.context;
	var trans = Raphael.parseTransformString(context.battlemap.getTransformString(context.position.x, context.position.y));
	context.svgElement.transform("T" + trans[1][1] + "," + trans[1][2]);
}


// ----------------------------------------------------------------------------

$().ready(function(){
	addColorPicker('.grid', 'grid_color');
	addColorPicker('.map', 'map_color');

	// Grid Line Color editor
	$('#grid_color').val(rgb2Hex(battleMap.gridlineColor));
	$('#grid_alpha').val(getAlphaFromRGBA(battleMap.gridlineColor));
	$('#grid_color').change(function(){
		var color = color2Hex($('#grid_color').val());
		var alpha = $('#grid_alpha').val();
		battleMap.gridlineColor = hex2rgb(color, alpha);
		battleMap.drawGrid();
	});
	$('#grid_color').blur(function(){
		var color = color2Hex($('#grid_color').val());
		var alpha = $('#grid_alpha').val();
		battleMap.gridlineColor = hex2rgb(color, alpha);
		battleMap.drawGrid();
	});
	$('#grid_alpha').change(function(){
		$('#grid_color').change();
	});
	$('#grid_alpha').bind('input', function(){
		$('#grid_color').change();
	});
	$('#grid_color').change();

	// Background Color editor
	$('#map_color').val(rgb2Hex(battleMap.backgroundColor));
	$('#map_color').change(function(){
		var color = color2Hex($('#map_color').val());
		battleMap.backgroundColor = color;
	});
	$('#map_color').blur(function(){
		var color = color2Hex($('#map_color').val());
		battleMap.backgroundColor = color;
	});
	$('#map_color').change();

	// Create an editor
	window.editor = new Editor(battleMap);
});


