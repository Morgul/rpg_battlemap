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

function insertCombatant(combatant) {
	var combatantList = $('#combatantList li');
	combatants.push(combatant);

	var listItem = generateInitListItem(combatants.length - 1, combatant);
	//TODO: This would be faster as a binary search
	var max = combatantList.length - 1;
	if(max == -1){
		$('#combatantList').append(listItem);
		return;
	}

	var i = combatantList.length - 1;
	var combatantInd;
	for(i; i >= 0; i--){
		combatantInd = combatantList[i].getAttribute('combatantIndex');
		combatantInd = parseInt(combatantInd);
		if(combatants[combatantInd].initiative > combatant.initiative){
			$(combatantList[i]).after(listItem);
			return;
		}
	}

	$(combatantList[0]).before(listItem);
}

function sanatizeInt(given, defVal){
	if(defVal == undefined){
		defVal = 0;
	}
	var retVal = parseInt(given);
	if(isNaN(retVal)){
		return defVal;
	}
	return retVal;
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

function createZoneListItem(zone){
	var liElem = document.createElement('li');
	liElem.style.boxShadow = "inset 0 0 10px 2px " + zone.color;
	liElem.innerHTML = zone.name;
	$(liElem).click(function(){
		var zoneListInd = this.getAttribute('zoneIndex');
		var zone = zoneList[zoneListInd];
		var editorForm = $('#zoneEditor form')[0];
		for(var prop in zone){
			if(editorForm[prop] && (editorForm[prop].value != undefined)){
				editorForm[prop].value = zone[prop];
			}
		}
		editorForm.cellX.value = zone.startCell[0];
		editorForm.cellY.value = zone.startCell[1];
		editorForm.zoneIndex.value = zoneListInd;
		editorForm.strokeOpacity.value = zone.strokeOpacity * 100;
	});
	return liElem;
}

function rebuildZoneList(){
	var zoneListElem = $('#zoneList')
	zoneListElem.empty();
	zoneList.map(function(zone, index){
		var liElem = createZoneListItem(zone);
		liElem.setAttribute('zoneIndex', index);
		zoneListElem.append(liElem);
	});
}

$().ready(function(){
	$('#combatantList').sortable({
		update: function(event, ui) {
			var combatantInd = parseInt(ui.item[0].getAttribute('combatantIndex'));
			var combatantListIndexes = [];
			$('#combatantList li').map(function(_ind,item){
				combatantListIndexes.push(parseInt(item.getAttribute('combatantIndex')));
				return true;
			});
			var combatant = combatants[combatantInd];
			var lastInd = combatantListIndexes.length - 1;

			if(combatantListIndexes.length == 1){
				return;
			}

			if(combatantInd == combatantListIndexes[0]){
				combatant.initiative = combatants[combatantListIndexes[1]].initiative + 1;
			} else if(combatantInd == (combatantListIndexes[lastInd])){
				combatant.initiative = combatants[combatantListIndexes[lastInd - 1]].initiative - 1;
			} else {
				var indexOf = combatantListIndexes.indexOf(combatantInd);
				if(indexOf == -1){
					return;
				}
				var higherInd = combatantListIndexes[indexOf - 1];
				var lowerInd = combatantListIndexes[indexOf + 1];
				var higher = combatants[higherInd].initiative;
				var lower = combatants[lowerInd].initiative;

				combatant.initiative = lower + ((higher - lower) / 2);
			}
		}
	});

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

	window.zoneList = [];
	//window.combatants = {};
	window.combatants = [];

	window.generateInitListItem = function(index, combatant)
	{
		var style = 'style="box-shadow: inset 0 0 10px 2px ' + combatant.color + ';"';
		return '<li combatantIndex="' + index + '" class="combatant"' + style + '>' + combatant.name + '</li>';
	}

	$('#addCombatant form').submit(function(){
		var creationObj = {};
		var newCombatant;
		for(var i = 0; i < this.elements.length; i++){
			creationObj[this.elements[i].name] = this.elements[i].value;
		}
		creationObj.onMouseOver = function(){
			datadump(newCombatant, '#combatantInfo');
			$('#combatantConditions').html(newCombatant.conditions.join(", "));
		}
		var init = parseFloat(creationObj.initiative);
		if(isNaN(init)){
			init = 0;
		}
		creationObj.initiative = init;
		newCombatant = new Combatant(window.battleMap, creationObj);
		insertCombatant(newCombatant);
		return false;
	});

	$('#leftColumnItems').height($('#drawingBoard').height()).accordion({
		autoHeight:false
	});

	$('#newZone').submit(function(){
		var creationObj = {};
		var newZone;
		for(var i = 0; i < this.elements.length; i++){
			creationObj[this.elements[i].name] = this.elements[i].value;
		}
		var size = sanatizeInt(this.size.value, 1);
		var cells;
		if(this.shape.value == "square"){
			cells = CombatZone.makeSquare(size);
		} else if(this.shape.value == "octogon"){
			cells = CombatZone.makeOctogon(size);
		} else {
			cells = [[0,0],[0,1],[1,1],[0,0]];
		}
		creationObj.cells = cells;
		newZone = new CombatZone(battleMap, creationObj);
		zoneList.push(newZone);

		var liElem = createZoneListItem(newZone);
		liElem.setAttribute('zoneIndex', zoneList.length - 1);
		$('#zoneList').append(liElem);
		return false;
	});

	$('#zoneEditor form').submit(function(){
		var index = parseInt(this.zoneIndex.value);
		var zone = zoneList[index];
		zone.setStart(sanatizeInt(this.cellX.value),sanatizeInt(this.cellY.value));
		zone.setColor(this.color.value);
		zone.setLayer(this.layer.value);
		zone.setPath(this.path.value);
		zone.setRotation(this.rotation.value);
		zone.setStroke(this.strokeColor.value, parseFloat(this.strokeOpacity.value) / 100);
		return false;
	});

	$('#editZonesToggle').click(function(){
		for(var i = 0; i < zoneList.length; i++){
			zoneList[i].svgObject.node.setAttribute('pointer-events', 'none');
		}
		var form = $('#zoneEditor form')[0];
		var ind = parseInt(form.zoneIndex.value);
		if(isNaN(ind)){
			return;
		}
		if(this.checked){
			zoneList[ind].svgObject.node.setAttribute('pointer-events', 'visible');
		}
	});

	var mapNameLi;
	var localmap;
	for(var i = 0; i < localStorage.length; i++){
		localmap = localStorage.key(i);
		mapNameLi = document.createElement('li');
		mapNameLi.innerHTML = localmap;
		/*mapNameLi.onclick = function(){
			loadBattleMapLocal(localmap);
		};*/
		$('#savedMapsList').append(mapNameLi);
	}
	$('#savedMapsList li').click(function(){
		loadBattleMapLocal(this.innerHTML);
	});

	$('#combatantTrashcan').droppable({
		accept:'#combatantList li[combatantIndex]',
		drop:function(ev, ui){
			var elem = ui.draggable[0];
			var combatantInd = parseInt(elem.getAttribute('combatantIndex'));
			elem.parentElement.removeChild(elem);
			combatants[combatantInd].remove();
			combatants.splice(combatantInd,1);
			$('#combatantList li').map(function(_ind, lielem){
				var cind = parseInt(lielem.getAttribute('combatantIndex'));
				if(cind > combatantInd){
					lielem.setAttribute('combatantIndex', cind - 1);
				}
			});
		}
	});

	$('#combatantList').click(function(ev){
		var combatantInd = parseInt(ev.target.getAttribute('combatantIndex'));
		combatants.map(function(combatant, ind){
			if(combatantInd == ind){
				if(combatant.pulsating){
					combatant.stopPulsating();
					return true;
				}
				combatant.startPulsating();
				return true;
			}
			combatant.stopPulsating();
			return false;
		});
	});

	$('#zoneTrashcan').click(function(ev){
		var zoneInd = parseInt(document.forms['zoneEditor'].zoneIndex.value);
		if(isNaN(zoneInd)){
			return false;
		}
		zoneList[zoneInd].remove();
		zoneList.splice(zoneInd, 1);
		rebuildZoneList();
	});
});
