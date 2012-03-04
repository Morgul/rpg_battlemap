// dependency on rapheal and jquery.

function BattleMap(actionElem, gridElem, opts){
	this.actionElem = '#' + actionElem;
	this.gridElem = '#' + gridElem;
	this.gridCtx = $(this.gridElem)[0].getContext('2d');
	this.zoom = 1; // as long as it's above 0, we're okay.
	this.translateX = 0; // translate as in motion on a 2d plane
	this.translateY = 0;
	this.gridSpacing = 32; // pixels
	for(var i in opts){
		this[i] = opts[i]
	}
}

BattleMap.prototype.drawGrid = function(){
	// resize canvas to avoid weird grid scaling.
	var parentWidth = $(this.gridElem).parent().width();
	var parentHeight = $(this.gridElem).parent().height();
	$(this.gridElem).width(parentWidth).height(parentHeight);
	var height = $(this.gridElem).height();
	var width = $(this.gridElem).width();
	var topcornerx = 0 + this.translateX;
	var topcornery = 0 + this.translateY;
	this.gridCtx.clearRect(topcornerx, topcornery, width, height);
	this.gridCtx.setFillColor("black");
	this.drawVerticalsGrid(width, height);
	this.drawHorizontalsGrid(width, height);
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

BattleMap.prototype.setZoom = function(z){
	this.zoom = z;
	this.drawGrid();
}

BattleMap.prototype.setGridSpacing = function(spacing){
	this.gridSpacing = spacing;
	this.drawGrid();
}

BattleMap.prototype.setTranslation = function(x, y){
	this.translateX = x;
	this.translateY = y;
	this.drawGrid();
}

BattleMap.prototype.pan = function(deltaX, deltaY){
	this.translateX += deltaX;
	this.translateY += deltaY;
	this.drawGrid();
}
