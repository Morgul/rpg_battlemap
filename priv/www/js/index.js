function combatantChangedHandler(combatant, property){
	var syncables = ['name', 'color', 'initiative', 'pulsating'];
	if(syncables.indexOf(property) < 0){
		return;
	}
	syncCombatantList();
}

function insertCombatant(combatant) {
	var combatantList = $('#combatantList li');
	$(combatant).bind('propertyChanged', combatantChangedHandler);
	battleMap.addCombatant(combatant);
	syncCombatantList();
	/*var listItem = generateInitListItem(battleMap.combatants.length - 1, combatant);

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
		if(battleMap.combatants[combatantInd].initiative > combatant.initiative){
			$(combatantList[i]).after(listItem);
			return;
		}
	}

	$(combatantList[0]).before(listItem);*/
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

function createZoneListItem(zone){
	var liElem = document.createElement('li');
	liElem.style.boxShadow = "inset 0 0 10px 2px " + zone.color;
	liElem.innerHTML = zone.name;
	$(liElem).click(function(){
		var zoneListInd = this.getAttribute('zoneIndex');
		var zone = battleMap.zones[zoneListInd];
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
	battleMap.zones.map(function(zone, index){
		var liElem = createZoneListItem(zone);
		liElem.setAttribute('zoneIndex', index);
		zoneListElem.append(liElem);
	});
}

function syncCombatantList(){
	var countCombatants = battleMap.combatants.length;
	var combatantUl = $('#combatantList');
	while(combatantUl.children().length < countCombatants){
		combatantUl.append('<li>Name Here</li>');
	}
	while(combatantUl.children().length > countCombatants){
		combatantUl.children().first().remove();
	}
	battleMap.combatants.map(function(combatant, index){
		var li = combatantUl.children()[index];
		$(li)
		.attr('combatantIndex', index)
		.addClass('combatant')
		.css('box-shadow', 'inset 0 0 10px 2px ' + combatant.color)
		.html(combatant.name);
		return true;
	});
}

$().ready(function(){
	$('#combatantList').sortable({
		update: function(event, ui) {
			var combatantInd = parseInt(ui.item[0].getAttribute('combatantIndex'));
			var combatant = battleMap.combatants[combatantInd];
			var newInd = $('#combatantList li').index(ui.item);
			battleMap.reorderCombatant(combatant, newInd);
			syncCombatantList();
		}
	});

	window.generateInitListItem = function(index, combatant)
	{
		var style = 'style="box-shadow: inset 0 0 10px 2px ' + combatant.color + ';"';
		return '<li combatantIndex="' + index + '" class="combatant"' + style + '>' + combatant.name + '</li>';
	}

	addColorPicker('.colorSelector', 'color');

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
		creationObj.size = parseInt(creationObj.size);
		creationObj.initiative = init;
		newCombatant = new Combatant(window.battleMap, creationObj);
		insertCombatant(newCombatant);
		return false;
	});

	$('#leftColumnItems').accordion({
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

		var liElem = createZoneListItem(newZone);
		liElem.setAttribute('zoneIndex', window.battleMap.zones.length - 1);
		$('#zoneList').append(liElem);
		return false;
	});

	$('#zoneEditor form').submit(function(){
		var index = parseInt(this.zoneIndex.value);
		var zone = window.battleMap.zones[index];
		zone.startCell = [sanatizeInt(this.cellX.value),sanatizeInt(this.cellY.value)];
		zone.color = this.color.value;
		zone.layer = this.layer.value;
		zone.path = this.path.value;
		zone.rotation = this.rotation.value;
		zone.strokeColor = this.strokeColor.value;
		zone.strokeOpacity = parseFloat(this.strokeOpacity.value) / 100;
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

	$('#savedMapsList').battlemapLocker({
		'load':function(mapData){
			window.battleMap = new BattleMap('#drawingBoard', mapData);
			$('#saveButton').battlemapSaveButton('option', 'battlemap', window.battleMap);
			rebuildZoneList();
			battleMap.combatants.map(function(c){
				return $(c).bind('propertyChanged', combatantChangedHandler);
			});
			syncCombatantList();
		}
	});

	$('#saveButton').battlemapSaveButton({
		'battlemap':window.battleMap,
		'save':function(){
			$('#savedMapsList').battlemapLocker('refresh');
		}
	});

	$('#combatantTrashcan').droppable({
		accept:'#combatantList li[combatantIndex]',
		tolerance: 'touch',
		drop:function(ev, ui){
			var elem = ui.draggable[0];
			var combatantInd = parseInt(elem.getAttribute('combatantIndex'));
			elem.parentElement.removeChild(elem);
			battleMap.combatants[combatantInd].remove();
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
		window.battleMap.combatants.map(function(combatant, ind){
			if(combatantInd == ind){
				if(combatant.pulsating){
					combatant.pulsating = false;
					return true;
				}
				combatant.pulsating = true;
				return true;
			}
			combatant.pulsating = false;
			return false;
		});
		return false;
	});

	$('#zoneTrashcan').click(function(ev){
		var zoneInd = parseInt(document.forms['zoneEditor'].zoneIndex.value);
		if(isNaN(zoneInd)){
			return false;
		}
		var zone = battleMap.zones[zoneInd];
		zone.remove();
		rebuildZoneList();
	});
});

