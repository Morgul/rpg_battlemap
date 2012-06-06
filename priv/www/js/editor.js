/******************************************************************************
Class Editor

A class for managing the editor.
******************************************************************************/
function Editor(battlemap){
	this._battlemap = battlemap;
	this.zones = [];
	this._currentZone = null;
	this._mode = "addPoint";
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
	},

	get mode(){
		return this._mode;
	},
	set mode(val){
		this._mode = val;
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
			var currentPos = this.pointer.position;
			if(this._currentZone == null){
				var z = new CombatZone(this.battlemap, {
					'path':[["M", currentPos.x, currentPos.y]]
				});
				this._currentZone = new EditZone(this.battlemap, this, z);
				this.zoneChanged();
				return;
			}

			var zone = this._currentZone.zone;
			var last = zone.path.length;
			if(this.rightClick){
				zone.path.splice(last, 0, ["M", currentPos.x, currentPos.y]);	
				this.rightClick = false;
			} else {
				zone.path.splice(last, 0, ["L", currentPos.x, currentPos.y]);
			}
			zone.updatePath();
			this._currentZone.select();

		} else {
			this.dblclick = false;
		}
	}, this), 200);
}

Editor.prototype.dblclickHandler = function(ev){
	this.dblclick = true;

	if(this.currentZone != null){
		this.currentZone.zone.path.push(["z"]);
		this.currentZone.zone.updatePath();
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
	this.finishPrefix = "L";
	if(! inZone){
		this.zone = new CombatZone(this.battlemap, {path: "Mz"});
		this.updateProperties();
	} else {
		this.zone = inZone;
		if(this.zone.path.toString() === this.zone.path){
			var midPath = "M " + this.zone.startCell[0] + " " + this.zone.startCell[1] + this.zone.path;
			midPath = Raphael.parsePathString(midPath);
			midPath.shift();
			this.zone.path = midPath;
		}
		//this.zone.path = Raphael.parsePathString(this.zone.path);
		$('#zone_name').val(this.zone.name);
		$('#zone_color').val(this.zone.strokeColor);
		$('#zone_alpha').val(this.zone.strokeOpacity);
		$('#zone_stroke').val(this.zone.strokeWidth);
		$('#fill_color').val(this.zone.color);
	}

	this.select();
}

EditZone.prototype.unselect = function(){
	this.points.forEach(function(point){
		point.remove();
	});
	this.points = [];
}

EditZone.prototype.select = function(){
	this.unselect();
	var pointsBuilding = [];
	var thisRef = this;
	this.zone.path.forEach(function(segment, ind){
		if(segment[0].toLowerCase() == "z"){
			return;
		}
		var last = segment.length - 1;
		var newPoint = new Point(thisRef.zone, {
			'x': segment[last - 1],
			'y': segment[last],
			'index': ind,
			'pointType': segment[0],
			'editor': thisRef,
			'zone': thisRef.zone
		});
		pointsBuilding.push(newPoint);
	});
	this.points = pointsBuilding;
	this.points.map(function(p){
		$(p).bind('click', function(fev, ev){
			console.log(this, thisRef.mode, arguments);
			var wasSelected = this.selected;
			if(!ev.shiftKey){
				thisRef.points.forEach(function(apoint){
					apoint.selected = false;
				});
			}
			this.selected = ! wasSelected;
			/*switch(thisRef.mode){
				case "setMovePoint":
					this.pointType = "M";
					break;
				case "setLinePoint":
					this.pointType = "L";
					break;
				case "setClosePoint":
					this.pointType = "Z";
					break;
				}*/
			this.setZone();
			ev.stopPropagation();
			return false;
		});
	});
}

EditZone.prototype.removePoints = function(){
	var last = this.points.length - 1;
	for(last; last >= 0; last--){
		if(this.points[last].selected){
			this.zone.path.splice(this.points[last].index, 1);
		}
	}
	this.zone.updatePath();
	this.select();
}

EditZone.prototype.addPoints = function(){
	var last = this.points.length - 1;
	var selectedList = [];
	for(last; last > 0; last--){
		if(this.points[last].selected && this.points[last - 1].selected){
			var p1 = this.points[last - 1].position;
			var p2 = this.points[last].position;
			avgx = (p1.x + p2.x) / 2;
			avgy = (p1.y + p2.y) / 2;
			var ind = this.points[last].index;
			this.zone.path.splice(ind, 0, ["L",avgx,avgy]);
			selectedList.unshift(true);
			selectedList.unshift(true);
		} else {
			selectedList.unshift(this.points[last].selected);
		}
	}
	selectedList.unshift(this.points[0].selected);
	this.zone.updatePath();
	this.select();
	var thisRef = this;
	selectedList.map(function(selected, selInd){
		thisRef.points[selInd].selected = selected;
	});
}

EditZone.prototype.setPointsType = function(type){
	this.points.forEach(function(p){
		if(p.selected){
			p.pointType = type;
		}
	});
}

EditZone.prototype.togglePointsPathClose = function(){
	var last = this.points.length - 1;
	var selectedList = [];
	for(last; last >= 0; last--){
		if(! this.points[last].selected){
			selectedList.unshift(false);
			continue;
		}
		selectedList.unshift(true);
		var index = this.points[last].index;
		if((index + 1) >= this.zone.path.length){
			this.zone.path.push(["z"]);
			continue;
		}
		if(this.zone.path[index + 1][0].toLowerCase() == "z"){
			this.zone.path.splice(index + 1, 1);
			continue;
		}
		this.zone.path.splice(index + 1, 0, ["z"]);
	}
	this.zone.updatePath();
	this.select();
	var thisRef = this;
	selectedList.forEach(function(selBool, ind){
		thisRef.points[ind].selected = selBool;
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
	var point = new Point(this.zone, {'x':x, 'y':y,'editor':this});
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
	//var start = [0,0];
	$(this.points).each(function(index, point){
		if (index == totalPoints - 1){
			// We're the last point.
			pathArray.push(finishPrefix + point.position.x + "," + point.position.y);
		} else {
			pathArray.push(point.pointType + point.position.x + "," + point.position.y);
		}
	});

	this.zone.startCell = start;
	this.zone.path = pathArray.join(" ") + " z";
}

EditZone.prototype.finishZone = function(moveTo){
	if (typeof moveTo !== 'undefined'){
		// Change this.finishPrefix from L (default) to M.
		this.finishPrefix = "M";
	}

	// Finish the path with a line/moveto back to the starting point.
	var point = new Point(this.zone, {
		'x': this.points[0].position.x,
		'y': this.points[0].position.y
	});
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

function Point(zone, options){
	this.battlemap = zone.battlemap;
	this.zone = zone;
	this._ready = false;
	this._index = 0;
	this._position = {x:0, y:0};
	this._size = 3;
	this._fillColor = "#777";
	this._strokeColor = "#777";
	this._pointType = "L" // "L", "M", "c"
	this._selected = false;
	this.svgElement = this.battlemap.svgPaper.circle(0, 0, this._size);
	this.svgElement.attr({
		'fill':this._fillColor,
		'stroke':this._strokeColor
	});
	for(var i in options){
		this[i] = options[i];
	}

	this.svgElement.click(function(){
		$(this).trigger('click', arguments);
	}, this);

	this.svgElement.drag(
		// onmove
		function(dx, dy, px, py, ev){
			// determine some data used for drag actions
			var boundingRect = $(this.battlemap.actionElem)[0].getBoundingClientRect();
			this.deltaX = boundingRect.left;
			this.deltaY = boundingRect['top'];
			this.lastCell = [this.cellX, this.cellY];
			var x = px - this.deltaX;
			var y = py - this.deltaY;
			var cell = this.battlemap.getNearestCell(x,y);
			this.position = {'x':cell[0], 'y':cell[1]};
			ev.stopPropagation();
			return false;
		},
		// onstart
		function(x,y,ev){
			console.log('start', this.index, arguments);
			ev.stopPropagation();
			return false;
		},
		// onend
		function(x,y,ev){
			console.log('end', this.index, arguments);
			return false;
		}, this, this, this
	);
	this._ready = true;
	$(this.battlemap).bind("viewChanged", $.proxy(this.viewChangeHandler, this));
	this.viewChangeHandler();
}

Point.prototype = {
	get position(){
		return this._position;
	},
	set position(val){
		this._position.x = val.x;
		this._position.y = val.y;
		var xy = [
			this._position.x * this.battlemap.gridSpacing,
			this._position.y * this.battlemap.gridSpacing
		];
		this.svgElement.attr({
			cx:xy[0],
			cy:xy[1]
		});

		this.setZone();
	},

	get x(){
		return this._position.x;
	},
	set x(val){
		this.position = {'x':val,'y':this.y};
	},

	get y(){
		return this._position.y;
	},
	set y(val){
		this.position = {'x':this.x,'y':val};
	},

	get size(){
		return this._size;
	},
	set size(val){
		if (typeof val !== 'undefined'){
			this._size = val;
			var size;
			if(this._selected){
				size = this._size * 1.3;
			} else {
				size = this._size
			}
			this.svgElement.attr('r',size);
		}
	},

	get strokeColor(){
		return this._strokeColor;
	},
	set strokeColor(val){
		if (typeof val !== 'undefined'){
			this._strokeColor = val;
			if(this.selected){
				this.svgElement.attr("stroke", "white");
			} else {
				this.svgElement.attr("stroke", val);
			}
		}
	},

	get fillColor(){
		return this._fillColor;
	},
	set fillColor(val){
		if (typeof val !== 'undefined'){
			this._fillColor = val;
			if(this.selected){
				this.svgElement.attr("fill", "black");
			} else {
				this.svgElement.attr("fill", val);
			}
		}
	},

	get index(){
		return this._index;
	},
	set index(val){
		this._index = val;
	},

	get pointType(){
		return this._pointType;
	},
	set pointType(val){
		this._pointType = val;
		this.setZone();
	},

	get selected(){
		return this._selected;
	},
	set selected(val){
		this._selected = !!val;
		this.size = this.size;
		this.strokeColor = this.strokeColor;
		this.fillColor = this.fillColor;
	}
}

Point.prototype.remove = function(){
	try{
		this.svgElement.remove();
	} catch(err){
		return false;
	}
	$(this.battlemap).unbind("viewChanged", $.proxy(this.viewChangeHandler, this));
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

Point.prototype.setZone = function(){
	if(! this._ready){
		return;
	}
	var datalength = this.zone.path[this.index].length;
	if(datalength == 1){
		return;
	}
	if(datalength == 2){
		// TODO add h and v support
		return;
	}
	if(datalength == 3){
		this.zone.path[this.index] = [this.pointType, this.x, this.y];
		this.zone.updatePath();
		return;
	}
	// TODO add support for more line types.
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
				editor.currentZone.unselect();
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

	datadump(battleMap, '#editGridProperties');

	$('#editGridProperties input[object-property]').change(function(ev){
		var property = ev.target.getAttribute('object-property');
		var val = ev.target.value;
		console.log('changing', property, battleMap[property], val);
		battleMap[property] = val;
	});
	
	// ------------------------------------------------------------------------

	// Create an editor
	window.editor = new Editor(battleMap);

	// ------------------------------------------------------------------------

	$('#zoneEditor [object-property]').change(function(ev){
		if(editor.currentZone == null){
			return;
		}
		var property = ev.target.getAttribute('object-property');
		var val = ev.target.value;
		var zone = editor.currentZone.zone;
		if(ev.target.type == 'checkbox'){
			val = ev.target.checked;
		}
		console.log('zone changing', property, zone[property], val, ev.target);
		zone[property] = val;
	});

	$('#zone_name').val("New Zone");
	$('#zone_color').val("black");
	$('#zone_alpha').val(".7");
	$('#zone_stroke').val("5");
	$('#fill_color').val("#777777");

	$('.zone div').css('background-color', '#000000');
	$('.fill div').css('background-color', '#777777');

	var zoneChangedRef = {
		zoneCount: 0
	};
	$(editor).bind('zone-changed', function(){
		if(editor.currentZone != null){
			var zone = editor.currentZone.zone;
			datadump(zone, 'form#zoneEditor');
			var zoneColorHex = color2Hex($('#zone_color').val());
			var fillColorHex = color2Hex($('#fill_color').val());
			$('.zone div').css('background-color', zoneColorHex);
			$('.fill div').css('background-color', fillColorHex);
		}
		if(zoneChangedRef.zoneCount != battleMap.zones.length){
			updateZoneList();
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
			window.editor.pointer.viewChangeHandler();
			datadump(battleMap, '#editGridProperties');
			$('.grid div').css('background-color', color2Hex(battleMap.gridlineColor));
			$('.map div').css('background-color', color2Hex(battleMap.backgroundColor));
			updateZoneList();
		}
	});

	// set up the toolbox
	$('#delPoints').click(function(){
		if(editor.currentZone){
			editor.currentZone.removePoints();
		}
		return false;
	});
	$('#splitPoints').click(function(){
		if(editor.currentZone){
			editor.currentZone.addPoints();
		}
	});
	$('#makeLinePoint').click(function(){
		if(editor.currentZone){
			editor.currentZone.setPointsType("L");
		}
	});
	$('#makeMovePoint').click(function(){
		if(editor.currentZone){
			editor.currentZone.setPointsType("M");
		}
	});
	$('#makeClosePoint').click(function(){
		if(editor.currentZone){
			editor.currentZone.togglePointsPathClose();
		}
	});

	// make the left column a better accordian than accordian.
	$('#leftColumnItems').multiAccordion({
		autoHeight:false,
		active:[0,1,2,4]
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


