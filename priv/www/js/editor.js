/******************************************************************************
Class Pointer

A simple class the represents the svg element acting as our pointer on the
battlemap grid.

* position :: {x, y} - Object with x parameter, and y parameters. This is in
						cell-space.
* offset :: {x, y} - Object with x and y parameters. Represents the offset from
						page coordinates.
* svgElement :: Object() - The underlying svg element
******************************************************************************/
function Pointer(size, fill, stroke) {
	this.position = {x: 0, y: 0};

	// Set offset
	var offset_ = $(battleMap.actionElem).offset();
	this.offset = {x: offset_.left, y: offset_.top};

	// Build svg element
	this.svgElement = battleMap.svgPaper.circle(0, 0, size);

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
		//TODO: Strip scale information from this.
		this.svgElement.transform(val);
	}
}

Pointer.prototype.bindEvents = function(){
	var _this = this;
	this.mousemoveHandler = function(ev) {_this.mousemove_handler(ev);}
	$(battleMap.actionElem).mousemove(this.mousemoveHandler);
}

Pointer.prototype.mousemove_handler = function(ev){
	this.position.x = ev.pageX - this.offset.x;
	this.position.y = ev.pageY - this.offset.y;
	var cell = battleMap.getNearestCell(this.position.x, this.position.y);
	this.transform = battleMap.getTransformString(cell[0], cell[1]);
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
	window.pointer = new Pointer(4, "#09f", "#05d");
});


