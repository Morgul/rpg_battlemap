Storage.prototype.setObject = function(key, value){
	this.setItem(key, JSON.stringify(value));
}

Storage.prototype.getObject = function(key){
	var value = this.getItem(key);
	return value && JSON.parse(value);
}

function saveBattleMapLocal(){
	var mapForStorage = {
		name: battleMap.name,
		zoom: battleMap.zoom,
		translateX: battleMap.translateX,
		translateY: battleMap.translateY,
		gridSpacing: battleMap.gridSpacing,
		combatants: [],
		zones: []
	};
	var storeCombatant = function(combatDude){
		var tmpCombat = {
			name: combatDude.name,
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
			cells: zone.cells,
			startCell: zone.startCell,
			type: zone.type,
			color: zone.color
		};
		return tmpZone;
	});
	localStorage.setObject(battleMap.name, mapForStorage);
}

function loadBattleMapLocal(mapname){
	if(localStorage.getObject(mapname) == null){
		return;
	}
	var rawMap = localStorage.getObject(mapname);
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
}

function insertCombatant(combatant) {
	var combatantList = $('#combatantList').sortable("toArray");
	combatants[combatant.name] = combatant;

	// Sanatize intiative
	sanatizeInit(combatant);

	var added = false;
	//TODO: This would be faster as a binary search
	$(combatantList).each(function(index) {
		var item = combatantList[index];

		// This will always add to the bottom of same-initiative
		if (combatants[item].initiative < combatant.initiative)
		{
			$(generateInitListItem(combatant)).insertBefore('#' + item);
			added = true;

			// Break out of the .each()
			return false;
		}
	});

	// We go at the bottom of the list.
	if (added == false)
	{
		$('#combatantList').append(generateInitListItem(combatant));
	}
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
	liElem.innerHTML = zone.type;
	$(liElem).click(function(){
		var zoneListInd = this.getAttribute('zoneIndex');
		var zone = zoneList[zoneListInd];
		var editorForm = $('#zoneEditor form')[0];
		editorForm.cellX.value = zone.startCell[0];
		editorForm.cellY.value = zone.startCell[1];
		editorForm.color.value = zone.color;
		editorForm.type.value = zone.type;
		editorForm.zoneIndex.value = zoneListInd;
		editorForm.cellData.value = cellListToString(zone.cells);
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
			var combatant = combatants[$(ui.item).text()];
			var combatantList = $('#combatantList').sortable("toArray");
			var index = combatantList.indexOf(combatant.name);

			if (index == 0)
			{
				combatant.initiative = parseInt(combatants[combatantList[1]].initiative) + 1;
			}
			else if (index == (combatantList.length - 1))
			{
				combatant.initiative = parseInt(combatants[combatantList[index - 1]].initiative) - 1;
			}
			else
			{
				var higher = parseInt(combatants[combatantList[index - 1]].initiative);
				var lower = parseInt(combatants[combatantList[index + 1]].initiative);

				combatant.initiative = lower + ((higher - lower) / 2);
			}
		}
	});

	window.battleMap = new BattleMap('drawingBoard', 'canvasBoard', {});
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
	window.combatants = {};

	window.generateInitListItem = function(combatant)
	{
		var style = 'style="box-shadow: inset 0 0 10px 2px ' + combatant.color + ';"';
		return '<li id="' + combatant.name + '" class="combatant"' + style + '>' + combatant.name + '</li>';
	}

	// Makes sure that instead of a string, we are dealing with a real number for initiative.
	window.sanatizeInit = function(combatant){
		var init = parseInt(combatant.initiative);
		if (isNaN(init)) {
			combatant.initiative = 0;
		} else {
			combatant.initiative = init;
		}
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
		newCombatant = new Combatant(window.battleMap, creationObj);
		insertCombatant(newCombatant);
		return false;
	});

	$('#leftColumn').height($('#drawingBoard').height()).accordion({
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
		zone.setType(this.type.value);
		zone.setCells(stringToCells(this.cellData.value));
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
			zoneList[ind].svgObject.node.setAttribute('pointer-events', 'visibleStroke');
		}
	});
});
