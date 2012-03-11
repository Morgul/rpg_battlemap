// dependency on rapheal and jquery.

/***********************************************************************
Class BattleMap

Grid and battle ground context and data.  Responsible for drawing a
grid and placing entities such they are aligned to the grid.  Entities
such as combatants and terrain need only concern themselves with which
grid cell they are in.
* actionElem :: string() - Id of the element to use for svg drawing.
* gridElem :: string() - Id of the canvas to use for drawing the grid and
other limited interactive elements.
* opts :: Object() - Other parameters to assign to the battlemap.  This can
be used to override the default zoom, translateX, transalteY, and 
gridSpacing as well as set additional functions.
***********************************************************************/
function BattleMap(actionElem, gridElem, opts){
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

	this.transformListeners = [];
	for(var i in opts){
		this[i] = opts[i]
	}
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
	this.gridCtx.setFillColor("black");
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
	for(var i = 0; i < this.transformListeners.length; i++){
		try{
			this.transformListeners[i].updateTransform();
		} catch (ex) {
			console.log('listener fail', this.transformListeners[i]);
			this.removeTransformListener(this.transformListeners[i]);
		}
	}
}

BattleMap.prototype.addTransformListener = function(obj){
	this.transformListeners.push(obj);
}

BattleMap.prototype.removeTransformListener = function(obj){
	this.transformListeners = this.transformListeners.filter(function(x){
		return (obj != x);
	});
}

BattleMap.prototype.getTransformString = function(cellX, cellY){
	var transX = ((this.gridSpacing * cellX) + this.translateX) * this.zoom;
	var transY = ((this.gridSpacing * cellY) + this.translateY) * this.zoom;
	var out = "S" + this.zoom + "," + this.zoom + ",0,0T" + transX + "," + transY;
	return out;
}

/***********************************************************************
Class Combatant


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
	for(var i in opts){
		this[i] = opts[i];
	}

	// svg elements
	var cellSize = this.battlemap.gridSpacing;
	var pap = this.battlemap.svgPaper;
	this.svgData = {};
	this.svgData.set = pap.set();
	this.svgData.colorRect = pap.rect(0,0,cellSize * this.size, cellSize * this.size);
	this.svgData.colorRect.attr({ fill:this.color });
	this.svgData.set.push(this.svgData.colorRect);
	if(this.image){
		var padding = cellSize / 32;
		this.svgData.image = pap.image(this.image, padding, padding,
			(cellSize * this.size) - (padding * 2), (cellSize * this.size) - (padding * 2));
		this.svgData.set.push(this.svgData.image);
	}
	if(this.onMouseOver){
		this.svgData.set.mouseover(this.onMouseOver);
	}

	this.battlemap.addTransformListener(this);
	this.updateTransform();
}

Combatant.prototype.updateTransform = function(){
	var transformStr = this.battlemap.getTransformString(this.cellX, this.cellY);
	this.svgData.set.transform(transformStr);
}

Combatant.prototype.remove = function(){
	this.svgData.set.remove();
	this.battlemap.removeTransformListener(this);
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

Combatant.sortByInitiative = function(combater1, combater2){
	return combater1.initiative - combater2.initiative;
}

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
