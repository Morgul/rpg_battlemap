// dependency on rapheal and jquery.

/***********************************************************************
Class BattleMap

Grid and battle ground context and data.  Responsible for drawing a
grid and placing entities such they are aligned to the grid.  Entities
such as combatants and terrain need only concern themselves with which
grid cell they are in.

Events:
* viewChanged :: undefined - Either the pan or the zoom has changed, and
the bound object should request a new transformation string.

* actionElem :: string() - Id of the element to use for svg drawing.
* gridElem :: string() - Id of the canvas to use for drawing the grid and
other limited interactive elements.
* opts :: Object() - Other parameters to assign to the battlemap.  This can
be used to override the default zoom, translateX, transalteY, and
gridSpacing as well as set additional functions.
***********************************************************************/
function BattleMap(actionElem, opts){
	this.name = "Default Map";

	this._actionElem = '#' + actionElem;
	//this._gridElem = '#' + gridElem;
	//this._gridCtx = $(this._gridElem)[0].getContext('2d');

	this._svgPaper = Raphael(actionElem, '100%', '100%');

	this._zoom = 1; // as long as it's above 0, we're okay.
	this._translateX = 0; // translate as in motion on a 2d plane
	this._translateY = 0;
	this._gridSpacing = 32; // pixels

	// look and feel
	this._backgroundColor = "#e0e0e0";
	this._gridlineColor = "rgba(0,0,0,.5)";
	this._gridStroke = 1;

	var attr;
	this._gridPattern = document.createElementNS(this._svgPaper.canvas.namespaceURI, 'pattern');
	var patternAttr = {
		'x':'0',
		'y':'0',
		'width':'32',
		'height':'32',
		'id':'gridPattern',
		'patternUnits':'userSpaceOnUse'
	};
	for(attr in patternAttr){
		this._gridPattern.setAttribute(attr, patternAttr[attr]);
	}

	this._patternRect = document.createElementNS(this._svgPaper.canvas.namespaceURI, 'rect');
	var patternRectAttr = {
		'x':'0',
		'y':'0',
		'width':'100%',
		'height':'100%',
		'stroke':this._gridlineColor,
		'stroke-width':this._gridStroke,
		'stroke-opacity':1,
		'fill-opacity':0
	};
	for(attr in patternRectAttr){
		this._patternRect.setAttribute(attr, patternRectAttr[attr]);
	}

	$('defs', this._svgPaper.node).append(this._gridPattern);
	$(this._gridPattern).append(this._patternRect);

	this._gridRect = this._svgPaper.rect(0,0,'100%','100%');
	this._gridRect.attr('stroke-opacity','0');
	this._gridRect.node.setAttribute('fill','url(#gridPattern)');
	
	// Raphael has a "bug" where if you create two papers, attached to the same element,
	// the calculated position for the element is based on document flow, not the x,y
	// position of the parent element, making it impossible to have a paper ontop of another
	// paper, while sharing the same parent. Luckily, we can specify an absolute screen coordinate
	// for the second paper instead.
	/*var offset = $(this._actionElem).offset();
	this._toolPaper = Raphael(offset.left, offset.top, svgWidth, svgHeight);
	//$(this.toolPaper.canvas).css("z-index", "99");
	$(this._toolPaper.canvas).css("pointer-events", "none");*/

	this.combatElements = [];
	for(var i in opts){
		this[i] = opts[i]
	}

	var dragData = {
		mapRef: this
	};
	$(this.actionElem).mousedown(this, function(ev){
		dragData.dragging = true;
		dragData.lastDragX = ev.pageX;
		dragData.lastDragY = ev.pageY;
		return false;
	})
	.mouseup(function(){
		if(dragData.dragging){
			dragData.dragging = false;
			return false;
		}
	})
	// TODO a bug exists where if you lift the mouse button outside the
	// svg element, you continue to drag when you re-enter w/o the button
	// pressed until you click.  /oi.  Thus, just stopped when we leave.
	.mouseleave(function(){
		dragData.dragging = false;
	})
	.mousemove(function(ev){
		if(dragData.dragging){
			var deltaX = ev.pageX - dragData.lastDragX;
			var deltaY = ev.pageY - dragData.lastDragY;
			dragData.lastDragX = ev.pageX;
			dragData.lastDragY = ev.pageY;
			deltaX = deltaX / dragData.mapRef.zoom;
			deltaY = deltaY / dragData.mapRef.zoom;
			dragData.mapRef.pan(deltaX, deltaY);
			return false;
		}
		return true;
	});
}

BattleMap.prototype = {
	get actionElem(){
		return this._actionElem;
	},

	get gridElem(){
		return this._gridElem;
	},

	get gridCtx(){
		return this._gridCtx;
	},

	get svgPaper(){
		return this._svgPaper;
	},

	get zoom(){
		return this._zoom;
	},
	set zoom(val){
		if(val < .1){
			val = .1;
		}
		if(val > 3){
			val = 3;
		}
		this._zoom = val;
		/*this._gridPattern.setAttribute('width',this._gridSpacing * val);
		this._gridPattern.setAttribute('height',this._gridSpacing * val);*/
		this._gridPattern.setAttribute('patternTransform','scale(' + val + ')');
		this._triggerTransformListeners();
	},

	get translateX(){
		return this._translateX;
	},
	set translateX(val){
		this._translateX = val;
		this._gridPattern.setAttribute('x',val);
		this._triggerTransformListeners();
	},

	get translateY(){
		return this._translateY;
	},
	set translateY(val){
		this._translateY = val;
		this._gridPattern.setAttribute('y',val);
		this._triggerTransformListeners();
	},

	get translation(){
		return [this._translateX, this._translateY];
	},
	set translateion(val){
		this._translateX = val[0];
		this._translateY = val[1];
		this._triggerTransformListeners();
	},

	get gridSpacing(){
		return this._gridSpacing;
	},
	set gridSpacing(val){
		this._gridSpacing = val;
	},

	get backgroundColor() {
		return this._backgroundColor;
	},
	set backgroundColor(val) {
		this._backgroundColor = val;
		$(this.actionElem).css('background-color', val);
	},

	get gridlineColor(){
		return this._gridlineColor;
	},
	set gridlineColor(val){
		this._gridlineColor;
		this._gridRect.setAttribute('stroke',this._gridlineColor);
	},

	get gridStroke(){
		return this._gridStroke
	},
	set gridStroke(val){
		this._gridStroke = val;
		this._gridRect.setAttribute('stroke-width',this._gridStroke);
	}

};

BattleMap.prototype._getOffset = function(translate){
	var offset = translate % this._gridSpacing;
	return offset * this._zoom;
}

BattleMap.prototype._triggerTransformListeners = function(){
	$(this).trigger('viewChanged', undefined);
}

BattleMap.prototype.pan = function(deltax,deltay){
	this._translateX += deltax;
	this._translateY += deltay;
	this._gridPattern.setAttribute('x',this._translateX);
	this._gridPattern.setAttribute('y',this._translateY);
	this._triggerTransformListeners();
}

/* a combat element has 3 required properties:  layer, zIndex, and
svgObject.  Layer and zIndex are used for comparing.  If layer is equal,
zIndex is compared.  svgObject is expected to a rapheal element object.
order is ground -> action -> sky.
*/
BattleMap.prototype.addCombatElement = function(combatElem){
	this.combatElements.push(combatElem);
	this.setPaintOrder();
}

BattleMap.prototype.removeCombatElement = function(combatElem){
	this.combatElements = this.combatElements.filter(function(elem){
		return (elem != combatElem);
	});
	this.setPaintOrder();
}

BattleMap.prototype.setPaintOrder = function(){
	$(this._gridRect).remove();
	$(this._svgPaper.canvas).append(this._gridRect);
	var sorted = this.combatElements.sort(BattleMap.layerSort);
	for(var i = 0; i < sorted.length; i++){
		sorted[i].svgObject.toBack();
	}
}

BattleMap.prototype.getTransformString = function(cellX, cellY){
	var transX = ((this.gridSpacing * cellX) + this.translateX) * this.zoom;
	var transY = ((this.gridSpacing * cellY) + this.translateY) * this.zoom;
	var out = "S" + this.zoom + "," + this.zoom + ",0,0T" + transX + "," + transY;
	return out;
}

BattleMap.prototype.getCell = function(x,y){
	var cellX = Math.floor((x - (this.translateX * this.zoom)) / (this.zoom * this.gridSpacing));
	var cellY = Math.floor((y - (this.translateY * this.zoom)) / (this.zoom * this.gridSpacing));
	return [cellX,cellY];
}

BattleMap.prototype.getNearestCell = function(x,y){
	var cellX = Math.round((x - (this.translateX * this.zoom)) / (this.zoom * this.gridSpacing));
	var cellY = Math.round((y - (this.translateY * this.zoom)) / (this.zoom * this.gridSpacing));
	return [cellX,cellY];
}

BattleMap.prototype.highlight = function(cellX, cellY, size){
	if(size == undefined){
		size = 1;
	}
	if(this.highlighted){
		try{
			this.highlighted.remove();
		} catch (ex) {
			console.log('error with removal', ex);
		}
		delete this.highlighted;
	}
	var size = this.gridSpacing * size;
	this.highlighted = this.svgPaper.rect(0,0,size,size);
	this.highlighted.attr({
		'fill-opacity':0,
		'stroke':'gray',
		'stroke-width':3
	});
	this.highlighted.transform(this.getTransformString(cellX,cellY));
}

BattleMap.prototype.unhighlight = function(){
	this.highlighted.remove();
	delete this.highlighted;
}

BattleMap.layerSort = function(thinga,thingb){
	if(thinga.layer == thingb.layer){
		return thingb.zIndex - thinga.zIndex;
	}
	if(thinga.layer == "sky"){
		return -1;
	}
	if(thingb.layer == "sky"){
		return 1;
	}
	if(thinga.layer == "action"){
		return -1;
	}
	if(thingb.layer == "action"){
		return 1;
	}
	return -1;
}

/***********************************************************************
Battlemap tests:  TODO move to other file
***********************************************************************/

var tests = {};
tests.battleLayerSort = function(){
	var battleLayerMembers = [{layer:'ground'}, {layer:'action'},
	{layer:'sky'}];
	var battleLayerMembersSorted = battleLayerMembers.sort(BattleMap.layerSort);
	var expected = ['sky','action','ground'];
	var got = battleLayerMembersSorted.map(function(a){
		return a.layer;
	});
	for(var i = 0; i < expected.length; i++){
		if(expected[i] != got[i]){
			console.error('test fail', expected, got);
			return false;
		}
	}
	return true;
}

tests.battleLayerSortZ = function(){
	var battleLayerMembers = [{layer:'ground',zIndex:1,key:1},{layer:'ground',zIndex:2,key:2}];
	var expected = [2,1];
	var battleLayerMembersSorted = battleLayerMembers.sort(BattleMap.layerSort);
	var got = battleLayerMembersSorted.map(function(a){
		return a.key;
	});
	for(var i = 0; i < expected.length; i++){
		if(expected[i] != got[i]){
			console.error('test fail', expected, got);
			return false;
		}
	}
	return true;
}

/***********************************************************************
Class MapLocker

Manages load and save for both remote and local, as well as a simple
sync.
***********************************************************************/

Storage.prototype.setObject = function(key, value){
	this.setItem(key, JSON.stringify(value));
}

Storage.prototype.getObject = function(key){
	var value = this.getItem(key);
	return value && JSON.parse(value);
}

function MapLocker(){
	throw new Exception('lib, not for instanciation');
}

MapLocker._seq = function(max){
	var a = [];
	for(var i = 0; i < max; i++){
		(function(n){
			a[n] = n+1;
		})(i);
	}
	return a;
}

MapLocker.listLocal = function(){
	var seq = MapLocker._seq(localStorage.length);
	var mapped = seq.map(function(val, index){
		var key = localStorage.key(index);
		var obj = localStorage.getObject(key);
		return {'name':obj.name, 'url':obj.url};
	});
	return mapped;
}

MapLocker.listRemote = function(){
	return $.get('/battles');
}

// overwrite remote means save in the cloud ignoring any mismatch for
// etags.  This always saves remotely..
MapLocker.save = function(battleMap, overwriteRemote){
	var mapObj = MapLocker._battlemapToObj(battleMap);
	MapLocker._saveLocal(battleMap);
	var ajaxOpts = {};
	if(battleMap.url){
		ajaxOpts = {
			'url':battleMap.url,
			'contentType':'application/json',
			'data':JSON.stringify(mapObj),
			'type':'PUT',
			'processData':false
		};
	} else {
		ajaxOpts = {
			'url':'/battles/create',
			'contentType':'application/json',
			'data':JSON.stringify(mapObj),
			'type':'POST',
			'processData':false
		};
		if(overwriteRemote){
			ajaxOpts.headers = {'If-Match':'*'};
		}
	}
	return $.ajax(ajaxOpts).success(function(){
		MapLocker._setRemoteSync(battleMap, true);
	});
}

MapLocker._battlemapToObj = function(battleMap){
	var mapForStorage = {
		name: battleMap.name,
		zoom: battleMap.zoom,
		translateX: battleMap.translateX,
		translateY: battleMap.translateY,
		gridSpacing: battleMap.gridSpacing,
		combatants: [],
		zones: []
	};
	if(battleMap.url){
		mapForStorage.url = battleMap.url;
	}
	var storeCombatant = function(combatDude){
		var tmpCombat = {
			name: combatDude.name,
			zIndex: combatDude.zIndex,
			color: combatDude.color,
			cellX: combatDude.cellX,
			cellY: combatDude.cellY,
			size: combatDude.size,
			hp: combatDude.hp,
			initiative: combatDude.initiative,
			conditions: combatDude.conditions
		};
		mapForStorage.combatants.push(tmpCombat);
	}
	for(var cname in combatants){
		storeCombatant(combatants[cname]);
	}
	mapForStorage.zones = zoneList.map(function(zone){
		var tmpZone = {
			startCell: zone.startCell,
			name: zone.name,
			color: zone.color,
			layer: zone.layer,
			zIndex: zone.zIndex,
			rotation: zone.rotation,
			path: zone.path,
			strokeOpacity: zone.strokeOpacity,
			strokeColor: zone.strokeColor
		};
		return tmpZone;
	});
	return mapForStorage;
}

MapLocker._saveLocal = function(battleMap){
	var mapObj = MapLocker._battlemapToObj(battleMap);
	mapObj._remoteSynced = false;
	localStorage.setObject(mapObj.name, mapObj);
}

MapLocker._setRemoteSync = function(battleMap, synced){
	var obj = localStorage.getObject(battleMap.name);
	obj._remoteSynced = synced;
	localStorage.setObject(obj.name, obj);
}

/***********************************************************************
Class Combatant

Events:
* transformUpdated :: Combatant() - when the updateTransformation
function was called, this is triggered.
* removed :: Combatant() - when this has been removed from the battlemap.
***********************************************************************/

function Combatant(battlemap, opts){
	this.battlemap = battlemap;
	// opts should override most of these.
	this.name = "Jethro";
	this.color = "green";
	this.cellX = 0;
	this.cellY = 0;
	this.size = 1;
	this.hp = 5;
	this.initiative = 10;
	this.conditions = [];
	this.zIndex = 1;
	for(var i in opts){
		this[i] = opts[i];
	}

	// determine some data used for drag actions
	var boundingRect = $(battlemap.actionElem)[0].getBoundingClientRect();
	this.deltaX = boundingRect.left;
	this.deltaY = boundingRect['top'];
	this.lastCell = [this.cellX, this.cellY];

	// svg elements
	var cellSize = this.battlemap.gridSpacing;
	var pap = this.battlemap.svgPaper;
	this.svgData = {};
	this.svgObject = pap.set();
	this.svgData.colorRect = pap.rect(0,0,cellSize * this.size, cellSize * this.size);
	this.svgData.colorRect.attr({ fill:this.color });
	this.svgObject.push(this.svgData.colorRect);
	if(this.image){
		var padding = cellSize / 32;
		this.svgData.image = pap.image(this.image, padding, padding,
			(cellSize * this.size) - (padding * 2), (cellSize * this.size) - (padding * 2));
		this.svgObject.push(this.svgData.image);
	}
	this.svgObject.drag(function(moveX, moveY, pageX, pageY, ev){
		var x = pageX - this.deltaX;
		var y = pageY - this.deltaY;
		var cells = this.battlemap.getCell(x,y);
		this.battlemap.highlight(cells[0], cells[1], this.size);
		this.lastCell = cells;
		ev.stopPropagation();
		return false;
	}, function(pageX, pageY, ev){
		var x = pageX - this.deltaX;
		var y = pageY - this.deltaY;
		var cells = this.battlemap.getCell(x,y);
		this.battlemap.highlight(cells[0], cells[1], this.size);
		ev.stopPropagation();
		return false;
	}, function(){
		this.moveTo(this.lastCell[0], this.lastCell[1]);
		this.battlemap.unhighlight();
		return false;
	}, this, this, this);
	if(this.onMouseOver){
		this.svgObject.mouseover(this.onMouseOver);
	}

	var theThis = this;
	this.battlemap.addCombatElement(this);
	this.viewChangedHandler = function(){
		theThis.updateTransform();
	};
	$(this.battlemap).bind('viewChanged', this.viewChangedHandler);
 	this.updateTransform();
}

Combatant.prototype.layer = "action";

Combatant.prototype.updateTransform = function(){
	var transformStr = this.battlemap.getTransformString(this.cellX, this.cellY);
	this.svgObject.transform(transformStr);
	$(this).trigger("transformUpdated", this);
}

Combatant.prototype.remove = function(){
	this.svgObject.remove();
	$(this.battlemap).unbind("viewChanged", this.viewChangedHandler);
	this.battlemap.removeCombatElement(this);
	$(this).trigger("removed", this);
}

Combatant.prototype.moveTo = function(newX, newY){
	this.cellX = newX;
	this.cellY = newY;
	this.updateTransform();
}

Combatant.prototype.setColor = function(color){
	this.color = color;
	this.svgData.colorRect.attr({fill:color});
}

Combatant.prototype.setSize = function(size){
	this.size = size;
	var cellSize = this.battlemap.gridSpacing;
	var rectSize = cellSize * this.size;
	this.svgData.colorRect.attr({width:rectSize,height:rectSize});
	if(this.image){
		var padding = cellSize / 32;
		var imageSize = (cellSize * this.size) - (padding * 2);
		this.svgData.image.attr({width:imageSize,height:imageSize});
	}
}

Combatant.prototype.startPulsating = function(){
	this.stopPulsating();
	var theSvg = this.svgObject;
	var regXY = 0;
	var regHW = this.size * this.battlemap.gridSpacing;
	var pulseSize = this.battlemap.gridSpacing / 10;
	var xy = pulseSize * -1;
	var hw = regHW + (pulseSize * 2);
	var bigAttr = {'x':xy,'y':xy,'width':hw,'height':hw};
	var regAttr = {'x':-xy,'y':-xy,'width':regHW - pulseSize * 2, 'height':regHW - pulseSize * 2};
	var pulseGrow = function(){
		theSvg.animate(bigAttr,1000,pulseShrink);
	};
	var pulseShrink = function(){
		theSvg.animate(regAttr,1000,pulseGrow);
	}
	pulseGrow();
	this.pulsating = true;
}

Combatant.prototype.stopPulsating = function(){
	this.svgObject.stop();
	this.svgObject.attr({'x':0,'y':0});
	this.setSize(this.size);
	this.pulsating = false;
}

Combatant.sortByInitiative = function(combater1, combater2){
	return combater2.initiative - combater1.initiative;
}

/***********************************************************************
Class for holding freeform shaped areas.  Use cases are:
* Walls of rooms or other blocking terrain, but not taking up a cell
* marking difficult or dangerous terrain that take up cells.
* marking non-blocking, but still important areas, like a poison cloud.

To support this, a combat zone is basically a path that can float above
the combatants or below, be filled or not, be closed or not, and be
transparent or not.
***********************************************************************/

function CombatZone(battlemap, opts){
	this.battlemap = battlemap;
	//this.cells = CombatZone.square;
	this.startCell = [0,0];
	//this.closed = (this.cells[0] == this.cells[this.cells.length - 1]);
	this.name = 'CombatZone of Doom';
	this._color = 'darkgreen';
	this.layer = "ground";
	this.zIndex = 1;
	this.rotation = "none"; // none, cw, ccw, about
	this.path = CombatZone.makeSquare(1);
	this._strokeOpacity = .5;
	this._strokeColor = "black";
	this._strokeWidth = 5;

	for(var i in opts){
		this[i] = opts[i];
	}

	//var svgPathString = this.makeSvgPathString();
	var pap = this.battlemap.svgPaper;
	this.svgObject = pap.set();
	this.floor = pap.path();
	this.walls = pap.path();
	var opacity = .5;
	if(this.layer == "ground"){
		opacity = 1;
	}
	this.floor.attr({
		'fill':this.color,
		'fill-opacity':opacity,
		'stroke-opacity':0
	});
	this.floor.node.setAttribute('fill-rule','evenodd');
	this.walls.attr({
		'fill-opacity':0,
		'stroke-opacity':this.strokeOpacity,
		'stroke':this.strokeColor,
		'stroke-width':this.strokeWidth
	});
	this.svgObject.push(this.floor,this.walls);
	this.setPath(this.path);
	var theThis = this;
	this.viewChangedHandler = function(){
		theThis.updateTransform();
	};

	$(this.battlemap).bind('viewChanged', this.viewChangedHandler);
 	this.battlemap.addCombatElement(this);
	this.floor.node.setAttribute('pointer-events', 'none');
	this.walls.node.setAttribute('pointer-events', 'none');
	this.updateTransform();
}

CombatZone.prototype = {
	get strokeColor(){
		return this._strokeColor;
	},
	set strokeColor(val){
		this._strokeColor = val;
		this.walls.attr({'stroke': val});
	},
	get strokeOpacity(){
		return this._strokeOpacity;
	},
	set strokeOpacity(val){
		this._strokeOpacity= val;
		this.walls.attr({'stroke-opacity': val});
	},
	get strokeWidth(){
		return this._strokeWidth;
	},
	set strokeWidth(val){
		this._strokeWidth= val;
		this.walls.attr({'stroke-width': val});
	},
	get color(){
		return this._color;
	},
	set color(val){
		this._color = val;
		this.floor.attr({'fill': val});
	}
}

CombatZone.prototype.updateTransform = function(){
	var transformStr = this.battlemap.getTransformString(this.startCell[0], this.startCell[1]);
	var rotate = "0";
	switch(this.rotation){
		case "ccw":
			rotate = "r-90";
			break;
		case "cw":
			rotate = "r90";
			break;
		case "about":
			rotate = "r180";
			break;
		case "none":
			rotate = "r0";
			break;
		default:
			rotate = this.rotation;
	}
	this.svgObject.transform(transformStr + rotate + " 0 0");
}

CombatZone.prototype.toGrid = function(xory){
	return xory * this.battlemap.gridSpacing;
}

CombatZone.prototype.alterXY = function(modFunc){
	var pathArray = Raphael.parsePathString(this.path);
	var newPathArr = pathArray.map(function(parts){
		switch(parts[0]){
			case "M":
			case "L":
			case "T":
			case "m":
			case "l":
			case "t":
				return [parts[0], modFunc(parts[1]), modFunc(parts[2])];
				break;
			case "H":
			case "V":
			case "h":
			case "v":
				return [parts[0], modFunc(parts[1])];
				break;
			case "S":
			case "Q":
			case "R":
			case "s":
			case "q":
			case "r":
				return [parts[0], modFunc(parts[1]), modFunc(parts[2]), modFunc(parts[3]), modFunc(parts[4])];
				break;
			case "C":
			case "c":
				var modded = [parts[1], parts[2], parts[3], parts[4], parts[5], parts[6]].map(modFunc);
				modded.unshift(parts[0]);
				return modded;
				break;
			case "A":
			case "a":
				return [parts[0], modFunc(parts[1]), modFunc(parts[2]), parts[3],
					parts[4], parts[5], modFunc(parts[6]), modFunc(parts[7])];
				break;
			default:
				return parts;
		}
	});
	return newPathArr.map(function(parts){
		return parts.join(" ");
	}).join(" ");
}

CombatZone.prototype.makeSvgPathString = function(){
	var cellSize = this.battlemap.gridSpacing;
	var pathArray = Raphael.parsePathString(this.path);
	var toGrid = function(xory){
		return xory * cellSize;
	};
	var outPath = "";
	pathArray.map(function(parts){
		switch(parts[0]){
			case "M":
			case "L":
			case "T":
			case "m":
			case "l":
			case "t":
				outPath += parts[0] + [parts[1], parts[2]].map(toGrid).join(" ");
				break;
			case "H":
			case "V":
			case "h":
			case "v":
				outPath += parts[0] + [parts[1]].map(toGrid).join(" ");
				break;
			case "S":
			case "Q":
			case "R":
			case "s":
			case "q":
			case "r":
				outPath += parts[0] + [parts[1], parts[2], parts[3], parts[4]].map(toGrid).join(" ");
				break;
			case "C":
			case "c":
				outPath += parts[0] + [parts[1], parts[2], parts[3], parts[4], parts[5], parts[6]].map(toGrid).join(" ");
				break;
			case "A":
			case "a":
				outPath += [parts[0], parts[1] * cellSize, parts[2] * cellSize,
					parts[3], parts[4], parts[5], parts[6] * cellSize,
					parts[7] * cellSize].join(" ");
				break;
		}
	});
	return outPath;
}

CombatZone.prototype.setStart = function(cellX, cellY){
	this.startCell = [cellX,cellY];
	this.updateTransform();
}

CombatZone.prototype.setRotation = function(rotation){
	this.rotation = rotation;
	this.updateTransform();
}

CombatZone.prototype.setColor = function(color){
	this.color = color;
	this.svgObject.attr({
		'fill': color
	});
};

CombatZone.prototype.setStroke = function(color, opacity){
	if(opacity == undefined){
		opacity = this.strokeOpacity;
	}
	if(color == undefined){
		color = this.strokeColor;
	}
	this.strokeColor = color;
	this.strokeOpacity = opacity;
	this.walls.attr({
		'stroke':color,
		'stroke-opacity':opacity
	});
}

CombatZone.prototype.setLayer = function(layer){
	if(layer != "sky"){
		layer = "ground";
	}
	this.layer = layer;
	var opacity = 0.5;
	if(this.layer == "ground"){
		opacity = 1;
	}
	this.floor.attr({
		'fill-opacity':opacity
	});
	this.battlemap.setPaintOrder();
}

CombatZone.prototype.setPath = function(pathStr){
	this.path = pathStr;
	this.updatePath();
}

/*CombatZone.prototype.addCell = function(xy, pos){
	if(pos == undefined){
		this.cells.push(pos);
	} else {
		this.cells.splice(pos, 0, xy);
	}
	this.updatePath();
}

CombatZone.prototype.removeCell = function(cellref){
	if(cellref instanceof Array){
		var ind = this.cells.indexOf(cellref);
		if(ind > -1){
			this.cells.splice(ind, 1);
		}
	} else {
		this.cells.splice(cellref, 1);
	}
	this.updatePath();
}

// true:  anywhere
// false:  nowhere
// append:  last cell to close the path
// prepend:  first cell to close the path
CombatZone.prototype.safeCell = function(xy){
	var ind = this.cells.indexOf(xy);
	if(ind < 0){
		return true;
	}
	if(xy == this.cells[0] && xy != this.cells[this.cells.length - 1]){
		return "append";
	}
	if(xy != this.cells[0] && xy == this.cells[this.cells.length - 1]){
		return "prepend";
	}
	return false;
}*/

CombatZone.prototype.updatePath = function(){
	var pathStr = this.makeSvgPathString();
	var floorPath = pathStr.replace("m","l").replace("M","L");
	floorPath =  pathStr[0] + floorPath.substr(1);
	this.floor.attr({'path':floorPath});
	this.walls.attr({'path':pathStr});
	this.updateTransform();
}

CombatZone.prototype.remove = function(){
	this.svgObject.remove();
	$(this.battlemap).unbind("viewChanged", this.viewChangedHandler);
	this.battlemap.removeCombatElement(this);
}

CombatZone.makeSquare = function(size){
	return Raphael.format("M 0 0 l {0} 0 l 0 {0} l -{0} 0 l 0 -{0} z", size);
}

CombatZone.makeOctogon = function(size){
	return Raphael.format("M {0} 0 l {0} 0 l {0} {0} l 0 {0} l -{0} {0} l -{0} 0 l -{0} -{0} l 0 -{0} l {0} -{0} z", size);
	//return Raphael.format("M{0} 0L{1} 0L{2} {0}L{2} {1}L{1} {2}L{0} {2}L0 {1}L0 {0}L{0} 0Z", size, size * 2, size * 3);
}

// CombatZone basic shapes
CombatZone.square = CombatZone.makeSquare(1);
CombatZone.largeSquare = CombatZone.makeSquare(2);
CombatZone.hugeSquare = CombatZone.makeSquare(3);

/***********************************************************************
Utility functions to make life easier.
***********************************************************************/

function datadump(dataHolder, topNode){
	$('[object-property]', topNode).each(function(index, elem){
		var defaultVal = elem.getAttribute("display-default") || "";
		var displayAttr = elem.getAttribute("display-attr") || "innerHTML";
		var objectProperty = elem.getAttribute("object-property");
		var displayValue = dataHolder[objectProperty];
		if(dataHolder[objectProperty] == undefined){
			displayValue = defaultVal;
		}

		if(displayAttr == "style"){
			var styleProperty = elem.getAttribute("style-display");
			elem.style[styleProperty] = displayValue;
			return;
		}

		var notAttrs = ["innerHTML", "innerText"];
		if(notAttrs.indexOf(displayAttr) > -1){
			elem[displayAttr] = displayValue;
			return;
		}

		elem.setAttribute(displayAttr, displayValue);
	});
}
