Storage.prototype.setObject = function(key, value){
	this.setItem(key, JSON.stringify(value));
}

Storage.prototype.getObject = function(key){
	var value = this.getItem(key);
	return value && JSON.parse(value);
}

function mapToSerializableObj(){
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

function saveBattleMapCloud(){
	var mapForStorage = mapToSerializableObj();
	var ajaxOpts = {
		'url':'/battles/create',
		'contentType':'application/json',
		'data':JSON.stringify(mapForStorage),
		'error':function(){
			console.log('save error', arguments);
		},
		'processData':false,
		'success':function(){
			console.log('save success', arguments);
			saveBattleMapLocal();
		},
		'type':'POST',
	};
	if(mapForStorage.url){
		ajaxOpts.url = mapForStorage.url;
		ajaxOpts.type = 'PUT';
	}
	$.ajax(ajaxOpts);
}

function deleteBattleMapCloud(mapUri){
	$.ajax({
		'url':mapUri,
		'error':function(){
			console.log('delete error',arguments);
		},
		'success':function(){
			console.log('delete success',arguments);
		},
		'type':'DELETE'
	});
}

function deleteBattleMapLocal(mapName){
	localStorage.removeItem(mapName);
}

function saveBattleMapLocal(){
	var mapForStorage = mapToSerializableObj();
	localStorage.setObject(battleMap.name, mapForStorage);
}

function loadBattleMapLocal(mapname){
	if(localStorage.getObject(mapname) == null){
		return;
	}
	var rawMap = localStorage.getObject(mapname);
	$('#drawingBoard svg').remove();
	window.battleMap = new BattleMap('drawingBoard', 'canvasBoard', rawMap);
	rawMap.combatants.map(function(rawDude){
		rawDude.onMouseOver = function(){
			datadump(newCombatant, '#combatantInfo');
			$('#combatantConditions').html(newCombatant.conditions.join(", "));
		};
		newCombatant = new Combatant(window.battleMap, rawDude);
		insertCombatant(newCombatant);
	});
	zoneList = [];
	rawMap.zones.map(function(rawZone){
		newZone = new CombatZone(battleMap, rawZone);
		zoneList.push(newZone);
	});
	rebuildZoneList();
	$('#mapname').attr('value', mapname);
	resizeBattleMap();
}

function resizeBattleMap() {
	var headerHeight = $('#head').height();
	var guessheight = window.innerHeight - (headerHeight + 20) ;
	$('#drawingBoard').height(guessheight);
	var newHeight = $('#drawingBoard').height();
	var newWidth = $('#drawingBoard').width();
	$('#canvasBoard').attr("width", newWidth)
	$('#canvasBoard').attr("height", newHeight);

	window.battleMap.drawGrid();
}

function cellListToString(cells){
	var cellStr = cells.map(function(xy){
		return xy[0] + ',' + xy[1];
	});
	return cellStr.join('\n');
}

function stringToCells(string, cellCoordDelim){
	if(! cellCoordDelim){
		cellCoordDelim = "\n";
	}
	var strPairs = string.split(cellCoordDelim);
	var pairs = strPairs.map(function(strP){
		return strP.split(",").map(function(x){
			return parseInt(x);
		});
	});
	return pairs;
}

function populateMapList(selector){
	var mapNameLi;
	var localmap;
	for(var i = 0; i < localStorage.length; i++){
		localmap = localStorage.key(i);
		mapNameLi = document.createElement('li');
		mapNameLi.innerHTML = localmap;
		/*mapNameLi.onclick = function(){
			loadBattleMapLocal(localmap);
		};*/
		$(selector).append(mapNameLi);
	}
	$(selector + ' li').click(function(){
		loadBattleMapLocal(this.innerHTML);
	});
}

function addColorPicker(selector){
	$(selector).ColorPicker({
		color: '#0000ff',
		onShow: function (colpkr) {
			$(colpkr).fadeIn(500);
			return false;
		},
		onHide: function (colpkr) {
			$(colpkr).fadeOut(500);
			return false;
		},
		onChange: function (hsb, hex, rgb) {
			$(selector + ' div').css('background-color', '#' + hex);
		}
	});
}

$().ready(function(){

	// Draw the battlemap
	window.battleMap = new BattleMap('drawingBoard', 'canvasBoard', {});
	$(window).resize(resizeBattleMap);

	$('#drawingBoard').mousewheel(function(ev, delta){
		var sensitivity = 10;
		if(isNaN(delta)){
			delta = ev.originalEvent.wheelDelta;
		}
		delta = (delta / 100) * sensitivity;
		battleMap.setZoom(battleMap.zoom + delta);
		return false;
	});

	resizeBattleMap();
	window.battleMap.drawGrid();

});
