// dependency on rapheal and jquery.

// Usefule for easily getting/setting jsonables in storages.
Storage.prototype.setObject = function(key, value){
	this.setItem(key, JSON.stringify(value));
}

Storage.prototype.getObject = function(key){
	var value = this.getItem(key);
	return value && JSON.parse(value);
}

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
	this._gridRect.node.style.pointerEvents = "none";
	
	// Raphael has a "bug" where if you create two papers, attached to the same element,
	// the calculated position for the element is based on document flow, not the x,y
	// position of the parent element, making it impossible to have a paper ontop of another
	// paper, while sharing the same parent. Luckily, we can specify an absolute screen coordinate
	// for the second paper instead.
	/*var offset = $(this._actionElem).offset();
	this._toolPaper = Raphael(offset.left, offset.top, svgWidth, svgHeight);
	//$(this.toolPaper.canvas).css("z-index", "99");
	$(this._toolPaper.canvas).css("pointer-events", "none");*/

	this._combatants = [];
	this._zones = [];

	//this.combatElements = [];
	var i;
	var tmpCombatants = opts.combatants;
	var tmpZones = opts.zones;
	delete opts.combatants;
	delete opts.zones;
	for(i in opts){
		this[i] = opts[i]
	}
	for(i in tmpCombatants){
		this.addCombatant(tmpCombatants[i]);
	}
	for(i in tmpZones){
		this.addZone(tmpZones[i]);
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
			//deltaX = deltaX / dragData.mapRef.zoom;
			//deltaY = deltaY / dragData.mapRef.zoom;
			dragData.mapRef.pan(deltaX, deltaY);
			return false;
		}
		return true;
	});

	var wheelData = {
		mapRef: this
	};
	$(this.actionElem).mousewheel(function(ev, delta){
		var sensitivity = 10;
		if(isNaN(delta)){
			delta = ev.originalEvent.wheelDelta;
		}
		delta = delta * (sensitivity / 10)
		wheelData.mapRef.zoom = (wheelData.mapRef.zoom + delta);
		return false;
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
		this._triggerTransformListeners();
	},

	get translateX(){
		return this._translateX;
	},
	set translateX(val){
		this._translateX = val;
		this._triggerTransformListeners();
	},

	get translateY(){
		return this._translateY;
	},
	set translateY(val){
		this._translateY = val;
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
		this._gridRect.attr('stroke',this._gridlineColor);
	},

	get gridStroke(){
		return this._gridStroke
	},
	set gridStroke(val){
		this._gridStroke = val;
		this._gridRect.attr('stroke-width',this._gridStroke);
	},

	get combatants(){
		return this._combatants;
	},

	get zones(){
		return this._zones;
	},
	get skys(){
		return this.zones.filter(function(z){
			return z.layer == "sky";
		});
	},
	get grounds(){
		return this.zones.filter(function(z){
			return z.layer == "ground";
		});
	}
};

BattleMap.prototype._getOffset = function(translate){
	var offset = translate % this._gridSpacing;
	return offset * this._zoom;
}

BattleMap.prototype._triggerTransformListeners = function(){
	var patternMatrix = this.getTransformMatrix(0,0);
	
	this._gridPattern.setAttribute('patternTransform',patternMatrix.toString());
	$(this).trigger('viewChanged', patternMatrix);
}

BattleMap.prototype.pan = function(deltax,deltay){
	this._translateX += deltax;
	this._translateY += deltay;
	this._triggerTransformListeners();
}

/* a combat element has 3 required properties:  layer, zIndex, and
svgObject.  Layer and zIndex are used for comparing.  If layer is equal,
zIndex is compared.  svgObject is expected to a rapheal element object.
order is ground -> action -> sky.
*/
/*BattleMap.prototype.addCombatElement = function(combatElem){
	this.combatElements.push(combatElem);
	this.setPaintOrder();
}*/

BattleMap.prototype.addCombatant = function(combatant){
	if(this._combatants.length == 0){
		this._combatants.push(combatant);
		this._triggerTransformListeners();
		this.setPaintOrder();
		return combatant;
	}

	var sorted = this._combatants.sort(function(ca, cb){
		return cb.initiative - ca.initiative;
	});
	
	// Eventually we'll use a binary search.
	// for now, just brute force it.
	for(var i = 0; i < sorted.length; i++){
		if(sorted[i].initiative > combatant.intiative){
			if(i == 0){
				this._combatants.unshift(combatant);
				break;
			} else if(sorted[i - 1].initiative != combatant.initiative){
				this._combatants.splice(i, 0, combatant);
			} else {
				if((this._combatants[i].intiative - this._combatants[i-1].initiative) > 1){
					combatant.initiative = this._combatants[i - 1].initiative + 1;
				} else {
					var newInit = (this._combatants[i].initiative + this._combatants[i - 1].initiative) / 2;
					combatant.initiative = newInit;
				}
				this._combatants.splice(i, 0, combatant);
			}
			this.setPaintOrder();
			return combatant;
		}
	}
	i = sorted.length - 1;
	if(sorted[i].initiative == combatant.initiative){
		combatant.initiative = sorted[i].initiative + 1;
	}
	this._combatants.push(combatant);
	this.setPaintOrder();
	return combatant;
}

BattleMap.prototype.removeCombatant = function(combatant){
	this._combatants = this._combatants.filter(function(elem){
		if(elem != combatant){
			return true;
		}
		try{
			combatant.remove();
		} catch(err){
			console.log('err removing combatant', err);
		}
		return false;
	});
	this.setPaintOrder();
}

BattleMap.prototype.addZone = function(zone){
	this._zones.push(zone);
	this._triggerTransformListeners();
	this.setPaintOrder();
}

BattleMap.prototype.removeZone = function(zone){
	this._zones = this._zones.filter(function(elem){
		if(elem != zone){
			return true;
		}
		try{
			zone.remove();
		} catch(err){
			console.log('err removing zone', err);
		}
		return false;
	});
	this.setPaintOrder();
}

BattleMap.prototype.setPaintOrder = function(){
	this._gridRect.toBack();
	var skyZones = this.skys;
	var groundZones = this.grounds;
	var sortFunc = function(zoneA, zoneB){
		return zoneB.zIndex - zoneA.zIndex;
	};
	skyZones = skyZones.sort(sortFunc);
	groundZones = groundZones.sort(sortFunc);
	var toBackFunc = function(elem){
		elem.svgObject.toBack();
	}
	skyZones.map(toBackFunc);
	this._combatants.map(toBackFunc);
	groundZones.map(toBackFunc);
}

BattleMap.prototype.getTransformMatrix = function(){
	return Raphael.matrix(this.zoom,0,0,this.zoom,this.translateX,this.translateY);
}
	
BattleMap.prototype.getTransformString = function(cellX, cellY){
	var m = this.getTransformMatrix();
	return m.toTransformString();
}

BattleMap.prototype.getCellXY = function(x,y){
	var outx = x * this.gridSpacing;
	var outy = y * this.gridSpacing;
	return [outx,outy];
}

BattleMap.prototype.getCell = function(x,y){
	var cellX = Math.floor((x - this.translateX) / (this.zoom * this.gridSpacing));
	var cellY = Math.floor((y - this.translateY) / (this.zoom * this.gridSpacing));
	return [cellX,cellY];
}

BattleMap.prototype.getNearestCell = function(x,y){
	var cellX = Math.round((x - this.translateX) / (this.zoom * this.gridSpacing));
	var cellY = Math.round((y - this.translateY) / (this.zoom * this.gridSpacing));
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
	var xy = this.getCellXY(cellX,cellY);
	this.highlighted = this.svgPaper.rect(xy[0],xy[1],size,size);
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

BattleMap.prototype.saveRemote = function(force){
	var mapObj = this.toJsonable();
	var ajaxOpts = {};
	var def = $.Deferred();
	if(this.url){
		ajaxOpts = {
			'url':this.url,
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
		if(force){
			ajaxOpts.headers = {'If-Match':'*'};
		} else {
			ajaxOpts.headers = {'If-Match':mapObj.etag};
		}
	}
	$.ajax(ajaxOpts).success(function(obj,success,xhr){
		if(! this.url){
			this.url = obj;
		} else {
			this.etag = xhr.getResponseHeader('etag');
		}
		def.resolve(this);
	}).fail(function(obj,fail,xhr){
		def.reject(obj);
	});
	return def;
}

BattleMap.prototype.saveLocal = function(){
	var mapObj = this.toJsonable();
	localStorage.setObject(this.name, mapObj);
}

BattleMap.loadRemote = function(actionElem, url){
	var def = $.Deferred();
	$.get(url).success(function(obj,success,xhr){
		obj.url = url;
		obj.etag = xhr.getResponseHeader('etag');
		var map = new BattleMap(actionElem, obj);
		def.respond(map);
	}).fail(function(obj,fail,xhr){
		def.reject(obj);
	});
	return def;
}

BattleMap.loadLocal = function(actionElem, name){
	var obj = localStorage.getObject(name);
	return new BattleMap(actionElem, obj);
}

BattleMap.prototype.deleteLocal = function(){
	localStorage.removeItem(this.name);
}

BattleMap.prototype.deleteRemote = function(){
	return $.ajax({
		url:this.url,
		type:'DELETE'
	});
}

BattleMap.listRemote = function(){
	return $.get('/battles');
}

BattleMap.listLocal = function(){
	var seq = BattleMap._seq(localStorage.length);
	var mapped = seq.map(function(val, index){
		var key = localStorage.key(index);
		var obj = localStorage.getObject(key);
		return {'name':obj.name, 'url':obj.url};
	});
	return mapped;
}

BattleMap.prototype.toJsonable = function(){
	var mapForStorage = {
		name: this.name,
		zoom: this.zoom,
		translateX: this.translateX,
		translateY: this.translateY,
		gridSpacing: this.gridSpacing,
		combatants: [],
		zones: []
	};
	if(this.url){
		mapForStorage.url = this.url;
	}
	mapForStorage.combatants = this._combatants.map(function(elem){
		return elem.toJsonable();
	});
	mapForStorage.zones = this._zones.map(function(elem){
		return elem.toJsonable();
	});
	return mapForStorage;
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

BattleMap._seq = function(max){
	var a = [];
	for(var i = 0; i < max; i++){
		(function(n){
			a[n] = n+1;
		})(i);
	}
	return a;
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
	this._color = "green";
	this._cellX = 0;
	this._cellY = 0;
	this._size = 1;
	this.hp = 5;
	this.initiative = 10;
	this.conditions = [];
	this.zIndex = 1;

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
			(cellSize * this._size) - (padding * 2), (cellSize * this._size) - (padding * 2));
		this.svgData.image.node.setAttribute('pointer-events','none');
		this.svgObject.push(this.svgData.image);
	}

	for(var i in opts){
		this[i] = opts[i];
	}

	if(! this.supressAdd){
		battlemap.addCombatant(this);
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
	//this.battlemap.addCombatant(this);
	this.viewChangedHandler = function(){
		theThis.updateTransform();
	};
	$(this.battlemap).bind('viewChanged', this.viewChangedHandler);
 	this.updateTransform();
}

Combatant.prototype.toJsonable = function(){
	return {
		name: this.name,
		zIndex: this.zIndex,
		color: this.color,
		cellX: this.cellX,
		cellY: this.cellY,
		size: this.size,
		hp: this.hp,
		initiative: this.initiative,
		conditions: this.conditions
	};
}

Combatant.prototype = {
	get layer(){
		return "action";
	},

	get color(){
		return this._color;
	},
	set color(val){
		this._color = val;
		this.svgData.colorRect.attr({fill:val});
	},

	get size(){
		return this._size;
	},
	set size(val){
		this._size = val;
		var cellSize = this.battlemap.gridSpacing;
		var rectSize = cellSize * this._size;
		this.svgData.colorRect.attr({width:rectSize,height:rectSize});
		if(this.image){
			var padding = cellSize / 32;
			var imageSize = (cellSize * this._size) - (padding * 2);
			this.svgData.image.attr({width:imageSize,height:imageSize});
		}
	},

	get cellX(){
		return this._cellX;
	},
	set cellX(val){
		this._cellX = val;
		this.updateTransform();
	},

	get cellY(){
		return this._cellY;
	},
	set cellY(val){
		this._cellY = val;
		this.updateTransform();
	},

	get cell(){
		return [this._cellX, this._cellY];
	},
	set cell(xy){
		this._cellX = xy[0];
		this._cellY = xy[1];
		this.updateTransform();
	},

	get image(){
		return this._image;
	},
	set image(val){
		if(this._image){
			this.svgData.image.attr({image:val});
			this._image = val;
		} else {
			this.svgData.image = this.battlemap.svgPaper.image(val);
			this.svgData.image.node.setAttribute('pointer-events','none');
			this.svgObject.push(this.svgData.image);
			this._image = val;
			this.updateTransform();
		}
	}
}
		
Combatant.prototype.updateTransform = function(){
	var basex = this._cellX * this.battlemap.gridSpacing;
	var basey = this._cellY * this.battlemap.gridSpacing;
	this.svgData.colorRect.attr({ x:basex, y:basey});
	if(this._image){
		var padding = this.battlemap.gridSpacing * 0.1;
		this.svgData.image.attr({x:basex + padding, y:basey + padding,
			width:(this.battlemap.gridSpacing * this._size) - (padding * 2),
			height:(this.battlemap.gridSpacing * this._size) - (padding * 2)
		});
	}
	var transformStr = this.battlemap.getTransformString(this.cellX, this.cellY);
	this.svgObject.transform(transformStr);
	$(this).trigger("transformUpdated", this);
}

Combatant.prototype.remove = function(){
	this.svgObject.remove();
	$(this.battlemap).unbind("viewChanged", this.viewChangedHandler);
	this.battlemap.removeCombatant(this);
	$(this).trigger("removed", this);
}

Combatant.prototype.moveTo = function(newX, newY){
	this.cellX = newX;
	this.cellY = newY;
	this.updateTransform();
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
	this.size = this._size;
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
	this._startCell = [0,0];
	//this.closed = (this.cells[0] == this.cells[this.cells.length - 1]);
	this.name = 'CombatZone of Doom';
	this._color = 'darkgreen';
	this._layer = "ground";
	this._zIndex = 1;
	this._rotation = "none"; // none, cw, ccw, about
	this._path = CombatZone.makeSquare(1);
	this._strokeOpacity = .5;
	this._strokeColor = "black";
	this._strokeWidth = 5;
	this._fillRule = 'evenodd';

	//var svgPathString = this.makeSvgPathString();
	var pap = this.battlemap.svgPaper;
	this.svgObject = pap.set();
	this.floor = pap.path();
	this.walls = pap.path();
	var opacity = .5;
	if(this._layer == "ground"){
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
	//this.setPath(this.path);
	var theThis = this;
	this.viewChangedHandler = function(){
		theThis.updateTransform();
	};

	for(var i in opts){
		this[i] = opts[i];
	}

	if(! this.suppressAdd){
		battlemap.addZone(this);
	}

	$(this.battlemap).bind('viewChanged', this.viewChangedHandler);
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
	},

	get startCell(){
		return this._startCell;
	},
	set startCell(xyArr){
		this._startCell = xyArr;
		this.updateTransform();
		this.updatePath();
	},

	get rotation(){
		return this._rotation;
	},
	set rotation(val){
		this._rotation = val;
		this.updateTransform();
	},

	get layer(){
		return this._layer;
	},
	set layer(val){
		if(val != "sky"){
			val = "ground";
		}
		this._layer = val;
		var opacity = 0.5;
		if(val == "ground"){
			opacity = 1;
		}
		this.floor.attr({
			'fill-opacity':opacity
		});
		this.battlemap.setPaintOrder();
	},

	get path(){
		return this._path;
	},
	set path(val){
		this._path = val;
		this.updatePath();
	},

	get zIndex(){
		return this._zIndex;
	},
	set zIndex(val){
		this._zIndex = val;
		this.battlemap.setPaintOrder();
	},

	get fillRule(){
		return this._fillRule;
	},
	set fillRule(val){
		this._fillRule = val;
		this.floor.node.setAttribute('fill-rule', val);
	}
}

CombatZone.prototype.updateTransform = function(){
	var transformStr = this.battlemap.getTransformString(this._startCell[0], this._startCell[1]);
	var rotate = "0";
	var startCellPart = " " + (this._startCell[0] * this.battlemap.gridSpacing) + " " + (this._startCell[1] * this.battlemap.gridSpacing) + " ";
	switch(this._rotation){
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
			rotate = this._rotation;
	}
	this.svgObject.transform(transformStr + rotate + startCellPart);
}

CombatZone.prototype.toGrid = function(xory){
	return xory * this.battlemap.gridSpacing;
}

CombatZone.prototype.alterXY = function(modFunc){
	var pathArray = Raphael.parsePathString(this._path);
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
	var pathArray = Raphael.parsePathString(this._path);
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
	var firstMove = "M " + this._startCell[0] * this.battlemap.gridSpacing + " " + this._startCell[1] * this.battlemap.gridSpacing;
	return firstMove + " " + outPath;
}

CombatZone.prototype.toJsonable = function(){
	return {
		startCell: this.startCell,
		name: this.name,
		color: this.color,
		layer: this.layer,
		zIndex: this.zIndex,
		rotation: this.rotation,
		path: this.path,
		strokeOpacity: this.strokeOpacity,
		strokeColor: this.strokeColor
	};
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
	$(this.battlemap).unbind("viewChanged", this.viewChangedHandler);
	this.svgObject.remove();
	this.battlemap.removeZone(this);
}

CombatZone.makeSquare = function(size){
	return Raphael.format("l {0} 0 l 0 {0} l -{0} 0 l 0 -{0} z", size);
}

CombatZone.makeOctogon = function(size){
	return Raphael.format("m {0} 0 l {0} 0 l {0} {0} l 0 {0} l -{0} {0} l -{0} 0 l -{0} -{0} l 0 -{0} l {0} -{0} z", size);
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
