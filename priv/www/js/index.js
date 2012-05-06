function insertCombatant(combatant) {
	var combatantList = $('#combatantList li');
	battleMap.addCombatant(combatant);

	var listItem = generateInitListItem(battleMap.combatants.length - 1, combatant);

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
		zoneList.push(newZone);

		var liElem = createZoneListItem(newZone);
		liElem.setAttribute('zoneIndex', zoneList.length - 1);
		$('#zoneList').append(liElem);
		return false;
	});

	$('#zoneEditor form').submit(function(){
		var index = parseInt(this.zoneIndex.value);
		var zone = zoneList[index];
		zone.start = [sanatizeInt(this.cellX.value),sanatizeInt(this.cellY.value)];
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

	$('#savedMapsList').battlemapLocker();

	$('#saveButton').battlemapSaveButton({'battlemap':battleMap});

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
		var zone = battleMap.zones[zoneInd];
		zone.remove();
		rebuildZoneList();
	});
});

