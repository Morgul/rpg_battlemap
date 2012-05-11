/******************************************************************************
Class Editor

A class for managing the editor.
******************************************************************************/
function Editor(battlemap){
	this._battlemap = battlemap;
	this.zones = [];
	this._currentZone = null;
	this.pointer = new Pointer(this._battlemap, 4, "#09f", "#05d");
	this.bindEvents();

	// Mouse handling
	this.dragging = false;
	this.dblclick= false;
	this.rightClick= false;
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
	},

	get currentZone(){
		return this._currentZone;
	},
	set currentZone(val){
		if(this._currentZone){
			this._currentZone.unselect();
		}
		this._currentZone = val;
		this.zoneChanged();
	},

	get battlemap(){
		return this._battlemap;
	},
	set battlemap(map){
		this._battlemap = map;
		this.pointer = new Pointer(this._battlemap, 4, "#09f", "#05d");
	}
}

Editor.prototype.bindEvents = function(){
	$(this.pointer).mouseup($.proxy(this.mouseupHandler, this));
	$(this.pointer).mousedown($.proxy(this.mousedownHandler, this));
	$(this.pointer).dblclick($.proxy(this.dblclickHandler, this));
	$(this.pointer).bind('contextmenu', $.proxy(this.contextmenuHandler, this));
}

Editor.prototype.zoneChanged = function(){
	$(this).trigger('zone-changed', undefined);
}

Editor.prototype.triggerUpdate = function(){
	$(this).trigger('prop-update', undefined);
}

Editor.prototype.mouseupHandler = function(ev){
	this.dragging = false;
}

Editor.prototype.mousedownHandler = function(ev){
	this.dragging = true;

	setTimeout($.proxy(function() {
		if (this.dragging == false && this.dblclick == false){
			if(this.currentZone == null){
				this.currentZone = new EditZone(this.battlemap, this);
			}

			var zone = this.currentZone;
			var startPos = null;
			if (this.currentZone.points.length > 0){
				var startPos = this.currentZone.points[0].position;
			}
			var currentPos = this.pointer.position;

			var point = this.currentZone.addPoint(currentPos.x, currentPos.y);
			if (this.rightClick){
				point.pointType = "M";
				this.currentZone.updateZone();
				this.rightClick = false;
			}

			if (startPos != null && startPos.x == currentPos.x &&
				startPos.y == currentPos.y) {
				// Clicking on the first point closes the zone.
				this.currentZone.finishZone();
			}
		} else {
			this.dblclick = false;
		}
	}, this), 200);
}

Editor.prototype.dblclickHandler = function(ev){
	this.dblclick = true;

	if(this.currentZone != null){
		// Doubleclicking finished the zone with a moveTo.
		this.currentZone.addPoint(this.pointer.position.x, this.pointer.position.y);
		this.currentZone.finishZone(true);
	}
}

Editor.prototype.contextmenuHandler = function(ev){
	this.rightClick = true;
}


/******************************************************************************
Class EditZone

A class for managing the editor.
******************************************************************************/
function EditZone(battlemap, editor, inZone){
	this.battlemap = battlemap;
	this.editor = editor;
	this.points = new Array();
	if(! inZone){
		this.zone = new CombatZone(this.battlemap, {path: "Mz"});
	} else {
		this.zone = inZone;
	}
	this.finishPrefix = "L";

	this.updateProperties();
	this._highlightPoints();
}

EditZone.prototype.unselect = function(){
	this.points.map(function(point){
		point.svgElement.remove();
	});
}

EditZone.prototype._highlightPoints = function(){
	this.unselect();
	var pathStr = this.zone.path;
	var segments = Raphael.parsePathString(pathStr);
	var thisRef = this;
	segments.map(function(segment){
		var last = segment.length - 1;
		thisRef.addPoint(segment[last - 1], segment[last]);
	});
}

EditZone.prototype.updateProperties = function(){
	var name = $('#zone_name').val();
	var strokeColor = $('#zone_color').val();
	var alpha = $('#zone_alpha').val();
	var strokeWidth = $('#zone_stroke').val();
	var fill = $('#fill_color').val();

	if (name != ""){
		this.zone.name = name;
	}

	if (strokeColor != ""){
		this.zone.strokeColor = strokeColor;
	}

	if (alpha != ""){
		this.zone.strokeOpacity = alpha;
	}

	if (strokeWidth != ""){
		this.zone.strokeWidth = strokeWidth;
	}

	if (fill != ""){
		this.zone.color = fill;
	}
}

EditZone.prototype.p2s = function(x, y){
	return "" + x + "," + y;
}

EditZone.prototype.addPoint = function(x, y){
	var point = new Point(this.battlemap, x, y);
	this.points.push(point);

	this.updateZone();

	return point;
}

EditZone.prototype.getPoint = function(x, y){
	var found = null;
	$(this.points).each(function(index, point) {
		if (point != null && point.position.x == x && point.position.y == y){
			found = point;
			return false;
		}
	});

	return found;
}

EditZone.prototype.updateZone = function(){
	var pathArray = new Array();
	var totalPoints = this.points.length;
	var finishPrefix = this.finishPrefix;
	$(this.points).each(function(index, point){
		if (index == 0)
		{
			// We're the starting point.
			pathArray.push("M" + point.position.x + "," + point.position.y);
		} else if (index == totalPoints - 1){
			// We're the last point.
			pathArray.push(finishPrefix + point.position.x + "," + point.position.y);
		} else {
			pathArray.push(point.pointType + point.position.x + "," + point.position.y);
		}
	});

	this.zone.path = "M " + pathArray.join(" ") + " z";
}

EditZone.prototype.finishZone = function(moveTo){
	if (typeof moveTo !== 'undefined'){
		// Change this.finishPrefix from L (default) to M.
		this.finishPrefix = "M";
	}

	// Finish the path with a line/moveto back to the starting point.
	var point = new Point(this.battlemap, this.points[0].position.x, this.points[0].position.y);
	this.points.push(point);

	this.updateZone();

	// We're done with this zone, so inform the editor we've gone away now.
	this.editor.currentZone = null;
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
	/*get transform(){
		return this._transform
	},

	set transform(val){
		var trans = Raphael.parseTransformString(val);
		this.svgElement.transform("T" + trans[1][1] + "," + trans[1][2]);
	}*/
}

Pointer.prototype.bindEvents = function(){
	$(this.battlemap.actionElem).mouseup($.proxy(this.mouseupHandler, this));
	$(this.battlemap.actionElem).mousedown($.proxy(this.mousedownHandler, this));
	$(this.battlemap.actionElem).dblclick($.proxy(this.dblclickHandler, this));
	$(this.battlemap.actionElem).bind('contextmenu', $.proxy(this.contextmenuHandler, this));
	$(this.battlemap.actionElem).mousemove($.proxy(this.mousemoveHandler, this));
	$(this.battlemap).bind("viewChanged", $.proxy(this.viewChangeHandler, this));
}

Pointer.prototype.viewChangeHandler = function(){
	var trans = this.battlemap.getTransformString();
	this.svgElement.transform(trans);
}

Pointer.prototype.mouseupHandler = function(ev){
	$(this).trigger('mouseup', ev);
}

Pointer.prototype.mousedownHandler = function(ev){
	$(this).trigger('mousedown', ev);
}

Pointer.prototype.dblclickHandler = function(ev){
	$(this).trigger('dblclick', ev);
}

Pointer.prototype.contextmenuHandler = function(ev){
	ev.preventDefault();
	$(this).trigger('contextmenu', ev);
	return false;
}

Pointer.prototype.updatePosition = function(){
	var centerx = this.position.x * this.battlemap.gridSpacing;
	var centery = this.position.y * this.battlemap.gridSpacing;
	this.svgElement.attr({cx:centerx,cy:centery});
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
	this.updatePosition();
	//this.transform = this.battlemap.getTransformString(cell[0], cell[1]);

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
	this._size = size;
	this.svgElement = this.battlemap.svgPaper.circle(0, 0, size);

	if (typeof fill == 'undefined'){
		fill = "#777";
	}
	this._fillColor = fill;

	if (typeof stroke == 'undefined'){
		stroke = "#777";
	}
	this._strokeColor = stroke;

	if ((typeof posX !== 'undefined') && (typeof posY !== 'undefined')){
		this.position = {x: posX, y: posY};
	}

	this.pointType = "L" // "L", "M", "c"

	$(this.battlemap).bind("viewChanged", $.proxy(this.viewChangeHandler, this));
}

Point.prototype = {
	get position(){
		return this._position;
	},
	set position(val){
		this._position.x = val.x;
		this._position.y = val.y;

		this.svgElement.attr({
			cx:this._position.x * this.battlemap.gridSpacing,
			cy:this._position.y * this.battlemap.gridSpacing
		});
	},

	get size(){
		return this._size;
	},
	set size(val){
		if (typeof val !== 'undefined'){
			this._size = val;
			this.svgElement.attr('r',this._size);
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

Point.prototype.viewChangeHandler = function(){
	var trans = this.battlemap.getTransformString();
	this.svgElement.transform(trans);
}

Point.prototype.moveHandler = function(ev){
	var context = ev.data.context;
	context.svgElement.attr({
		cx:context.position.x * context.battlemap.gridSpacing,
		cy:context.position.y * context.battlemap.gridSpacing
	});
}

/******************************************************************************
Utility function to easily rebuild the list of zones
*/
	// zone list updating
function updateZoneList(){
	$('#zoneList').empty();
	battleMap.zones.map(function(zone, index){
		var lielem = $('<li></li>')
		.appendTo('#zoneList');

		$('<button>' + zone.name + '</button>')
		.css('boxShadow', 'inset 0 0 10px 2px ' + zone.color)
		.css('width', '80%')
		.attr('zoneIndex', index)
		.click(function(){
			var zoneListInd = this.getAttribute('zoneIndex');
			var zone = battleMap.zones[zoneListInd];
			if(editor.currentZone){
				if(editor.currentZone.zone == zone){
					editor.currentZone = null;
					return false;
				}
			}
			editor.currentZone = new EditZone(battleMap, editor, zone);
		})
		.button()
		.appendTo(lielem);

		$('<button>X</button>')
		.appendTo(lielem)
		.button({'text':false,'icons':{'primary':'ui-icon-closethick'}})
		.attr('zoneIndex', index)
		.click(function(){
			var zoneListInd = this.getAttribute('zoneIndex');
			var zone = battleMap.zones[zoneListInd];
			if(editor.currentZone){
				if(editor.currentZone.zone == zone){
					editor.currentZone = null;
				}
			}
			zone.remove();
			updateZoneList();
		});
	});
}

// ----------------------------------------------------------------------------

$().ready(function(){
	//TODO: Move this into the editor class. Or split it up somehow.

	addColorPicker('.grid', 'grid_color');
	addColorPicker('.map', 'map_color');
	addColorPicker('.zone', 'zone_color');
	addColorPicker('.fill', 'fill_color');

	// Grid Line Color editor
	$('#grid_color').val(rgb2Hex(battleMap.gridlineColor));
	$('#grid_alpha').val(getAlphaFromRGBA(battleMap.gridlineColor));
	$('#grid_stroke').val(battleMap.gridStroke);
	$('#grid_color').change(function(){
		var color = color2Hex($('#grid_color').val());
		var alpha = $('#grid_alpha').val();
		var stroke = $('#grid_stroke').val();
		battleMap.gridlineColor = hex2rgb(color, alpha);
		battleMap.gridStroke = stroke;
	});
	$('#grid_color').blur(function(){
		var color = color2Hex($('#grid_color').val());
		var alpha = $('#grid_alpha').val();
		var stroke = $('#grid_stroke').val();
		battleMap.gridlineColor = hex2rgb(color, alpha);
		battleMap.gridStroke = stroke;
	});

	// Grid Line alpha change handlers
	$('#grid_alpha').change(function(){
		$('#grid_color').change();
	});
	$('#grid_alpha').bind('input', function(){
		$('#grid_color').change();
	});

	// Grid Line stroke change handlers
	$('#grid_stroke').change(function(){
		$('#grid_color').change();
	});
	$('#grid_stroke').bind('input', function(){
		$('#grid_color').change();
	});

	// Fire the change event
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

	// ------------------------------------------------------------------------

	// Create an editor
	window.editor = new Editor(battleMap);

	// ------------------------------------------------------------------------

	// Zone Color editor
	$('#zone_color').change(function(){
		if(editor.currentZone != null){
			var zone = editor.currentZone.zone;
			var color = color2Hex($('#zone_color').val());
			var fill = color2Hex($('#fill_color').val());
			var alpha = $('#zone_alpha').val();
			var stroke = $('#zone_stroke').val();
			zone.strokeColor = hex2rgb(color);
			zone.strokeOpacity = alpha;
			zone.strokeWidth = stroke;
			zone.color = fill;
		}
	});
	$('#zone_color').blur(function(){
		if(editor.currentZone != null){
			var zone = editor.currentZone.zone;
			var color = color2Hex($('#zone_color').val());
			var fill = color2Hex($('#fill_color').val());
			var alpha = $('#zone_alpha').val();
			var stroke = $('#zone_stroke').val();
			zone.strokeColor = hex2rgb(color);
			zone.strokeOpacity = alpha;
			zone.strokeWidth = stroke;
			zone.color = fill;
		}
	});
	// Zone alpha change handlers
	$('#zone_alpha').change(function(){
		$('#zone_color').change();
	});
	$('#zone_alpha').bind('input', function(){
		$('#zone_color').change();
	});

	// Zone stroke change handlers
	$('#zone_stroke').change(function(){
		$('#zone_color').change();
	});
	$('#zone_stroke').bind('input', function(){
		$('#zone_color').change();
	});

	// Zone fill color change handler
	$('#fill_color').change(function(){
		$('#zone_color').change();
	});

	$('#zone_name').val("New Zone");
	$('#zone_color').val("black");
	$('#zone_alpha').val(".7");
	$('#zone_stroke').val("5");
	$('#fill_color').val("#777777");

	$('.zone div').css('background-color', '#000000');
	$('.fill div').css('background-color', '#777777');

	$(editor).bind('zone-changed', function(){
		if(editor.currentZone != null){
			var zone = editor.currentZone;
			$('#zone_name').val(zone.zone.name);
			$('#zone_color').val(zone.zone.strokeColor);
			$('#zone_alpha').val(zone.zone.strokeOpacity);
			$('#zone_stroke').val(zone.zone.strokeWidth);
			$('#fill_color').val(zone.zone.color);

			$('#zone_color').change();
			$('#fill_color').change();
		}
	});

	// set up the save/load controls
	$('#saveButton').battlemapSaveButton({
		'battlemap':window.battleMap,
		'save':function(){
			console.log('save done', this, arguments);
			$('#savedMapsList').battlemapLocker('refresh');
		}
	});

	$('#savedMapsList').battlemapLocker({
		'load':function(mapData){
			console.log('data loaded', mapData);
			window.battleMap = new BattleMap('#drawingBoard', mapData);
			$('#saveButton').battlemapSaveButton('option', 'battlemap', window.battleMap);
			window.editor.battlemap = window.battleMap;
			updateZoneList();
		}
	});

	// make the left column a better accordian than accordian.
	$('#leftColumnItems').multiAccordion({
		autoHeight:false,
		active:[0,1,2,3, 4]
	});

	// ------------------------------------------------------------------------

	// Window resize handler
	//var zoneList = $('#zones');
	/*$(window).resize(function(){
		zoneList.height($(window).height() - zoneList.offset().top - 2);
	});
	// Don't ask about the extra 14 pixels. I have no clue. But, this works.
	zoneList.height($(window).height() - zoneList.offset().top - 16);*/
});


