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
	this.svgElement = this.battlemap.toolPaper.circle(0, 0, size);

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
	$(this.battlemap.actionElem).mouseup({context: this}, this.mouseupHandler)
	$(this.battlemap.actionElem).mouseup({context: this}, this.mousedownHandler)
	$(this.battlemap.actionElem).mousemove({context: this}, this.mousemoveHandler);
	$(this.battlemap).bind("viewChanged", {context: this}, this.mousemoveHandler);
}

Pointer.prototype.mouseupHandler = function(ev){
	var context = ev.data.context;
	$(context).trigger('mouseup', ev);
}

Pointer.prototype.mousedownHandler = function(ev){
	var context = ev.data.context;
	$(context).trigger('mouseup', ev);
}

Pointer.prototype.mousemoveHandler = function(ev){
	var context = ev.data.context;

	if (typeof ev.pageX == 'undefined'){
		ev.pageX = context.position.rawX;
	}
	if (typeof ev.pageY == 'undefined'){
		ev.pageY = context.position.rawY;
	}

	// handle our special logic
	context.position.rawX = ev.pageX;
	context.position.rawY = ev.pageY;
	context.position.x = ev.pageX - context.offset.x;
	context.position.y = ev.pageY - context.offset.y;
	var cell = context.battlemap.getNearestCell(context.position.x, context.position.y);
	context.transform = context.battlemap.getTransformString(cell[0], cell[1]);

	// trigger our mousemove event
	$(context).trigger('mousemove', ev);
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

	// Draw the editor 'pointer'
	window.pointer = new Pointer(battleMap, 4, "#09f", "#05d");
});


