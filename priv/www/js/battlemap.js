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
function BattleMap(actionElem, gridElem, opts){
	this.name = "Default Map";
	this.actionElem = '#' + actionElem;
	this.gridElem = '#' + gridElem;
	this.gridCtx = $(this.gridElem)[0].getContext('2d');
	
	var svgHeight = $(this.actionElem).height();
	var svgWidth = $(this.actionElem).width();
	this.svgPaper = Raphael(actionElem, svgWidth, svgHeight);

	this.zoom = 1; // as long as it's above 0, we're okay.
	this.translateX = 0; // translate as in motion on a 2d plane
	this.translateY = 0;
	this.gridSpacing = 32; // pixels

	// look and feel
	this.backgroundColor = "#e0e0e0";
	this.gridlineColor = "rgba(0,0,0,0,5)";

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
	})
	.css('background-color', this.backgroundColor);
}

/* clears the canvas and redraws the grid. */
BattleMap.prototype.drawGrid = function(){
	this.skipDraw = true;
	// resize canvas to avoid weird grid scaling.
	var parentWidth = $(this.gridElem).parent().width();
	var parentHeight = $(this.gridElem).parent().height();
	$(this.gridElem).width(parentWidth).height(parentHeight);
	var height = $(this.gridElem).height();
	var width = $(this.gridElem).width();
	var topcornerx = 0 + this.translateX;
	var topcornery = 0 + this.translateY;
	this.gridCtx.clearRect(0,0, width, height);
	//this.gridCtx.setFillColor("black");
	this.gridCtx.fillStyle = this.gridlineColor;
	this.gridCtx.strokeStyle = "rgba(80,80,80, 0.5)";
	this.drawVerticalsGrid(width, height);
	this.drawHorizontalsGrid(width, height);
	this.svgPaper.setSize(width, height);
	this.triggerTransformListeners();
}

BattleMap.prototype._getOffset = function(translate){
	var offset = translate % this.gridSpacing;
	return offset * this.zoom;
}

BattleMap.prototype.drawVerticalsGrid = function(width, height){
	var offset = (this.translateX % this.gridSpacing) * this.zoom;
	for(offset; offset <= width; offset += (this.gridSpacing * this.zoom)){
		this.gridCtx.fillRect(offset, 0, 1, height);
	}
}

BattleMap.prototype.drawHorizontalsGrid = function(width, height){
	var offset = (this.translateY % this.gridSpacing) * this.zoom;
	for(offset; offset < height; offset += (this.gridSpacing * this.zoom)){
		this.gridCtx.fillRect(0,offset,width,1);
	}
}

/* set the zoom and redraw the grid */
BattleMap.prototype.setZoom = function(z){
	if(z < .1){
		z = .1; 
	}
	if(z > 3){
		z = 3;
	}
	this.zoom = z;
	this.drawGrid();
}

/* sets the spacing and redraws the grid */
BattleMap.prototype.setGridSpacing = function(spacing){
	this.gridSpacing = spacing;
	this.drawGrid();
}

/* sets the translation and redraws the grid. */
BattleMap.prototype.setTranslation = function(x, y){
	this.translateX = x;
	this.translateY = y;
	this.drawGrid();
}

/* sets the translation using deltas instead of absolute values.  redraws
the grid. */
BattleMap.prototype.pan = function(deltaX, deltaY){
	this.translateX += deltaX;
	this.translateY += deltaY;
	this.drawGrid();
}

BattleMap.prototype.triggerTransformListeners = function(){
	$(this).trigger('viewChanged', undefined);
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
		thingb.zIndex - thinga.zIndex;
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
	$(this.battlemap).bind('viewChanged', function(){
		theThis.updateTransform();
	});
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
	$(this.battlemap).unbind("viewChanged");
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
	var theSvg = this.svgObject;
	var regXY = 0;
	var regHW = this.size * this.battlemap.gridSpacing;
	var pulseSize = this.battlemap.gridSpacing / 10;
	var xy = pulseSize * -1;
	var hw = regHW + (pulseSize * 2);
	var bigAttr = {'x':xy,'y':xy,'width':hw,'height':hw};
	var regAttr = {'x':0,'y':0,'width':regHW, 'height':regHW};
	var pulseGrow = function(){
		theSvg.animate(bigAttr,1000,pulseShrink);
	};
	var pulseShrink = function(){
		theSvg.animate(regAttr,1000,pulseGrow);
	}
	pulseGrow();
}

Combatant.prototype.stopPulsating = function(){
	this.svgObject.stop();
	this.svgObject.attr({'x':0,'y':0});
	this.setSize(this.size);
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
	this.color = 'darkgreen';
	this.layer = "ground";
	this.zIndex = 1;
	this.rotation = "none"; // none, cw, ccw, about
	this.path = CombatZone.makeSquare(1);
	this.strokeOpacity = .25;
	this.strokeColor = "black";

	for(var i in opts){
		this[i] = opts[i];
	}

	var svgPathString = this.makeSvgPathString();
	var pap = this.battlemap.svgPaper;
	this.svgObject = pap.path(svgPathString);
	var opacity = .5;
	var strokeWidth = 5;
	if(this.layer == "ground"){
		opacity = 1;
	}
	this.svgObject.attr({
		fill: this.color,
		'fill-opacity': opacity,
		'stroke-width': strokeWidth,
		'stroke': this.strokeColor,
		'stroke-opacity': this.strokeOpacity
	});
	this.svgObject.node.setAttribute('fill-rule', 'evenodd');
	var theThis = this;
	$(this.battlemap).bind('viewChanged', function(){
		theThis.updateTransform();
	});
	this.battlemap.addCombatElement(this);
	this.svgObject.node.setAttribute('pointer-events', 'none');
	this.updateTransform();
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
	this.svgObject.transform(transformStr + rotate + " " + this.startCell[0] + " " + this.startCell[1]);
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
	this.svgObject.attr({
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
	this.svgObject.attr({
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
	this.svgObject.attr({
		'path':pathStr
	});
	this.updateTransform();
}

CombatZone.prototype.remove = function(){
	this.svgObject.remove();
	$(this.battlemap).unbind("viewChanged");
	this.battlemap.removeCombatElement(this);
}

CombatZone.makeSquare = function(size){
	return Raphael.format("M0 0L{0} 0L{0} {0}L0 {0}L0 0Z", size);
}

CombatZone.makeOctogon = function(size){
	return Raphael.format("M{0} 0L{1} 0L{2} {0}L{2} {1}L{1} {2}L{0} {2}L0 {1}L0 {0}L{0} 0Z", size, size * 2, size * 3);
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
