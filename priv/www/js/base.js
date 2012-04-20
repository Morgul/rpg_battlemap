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

// Hardcoded lists are ugly. They're also fast, and 100% cross-browser compliant. Meh.
function color2Hex(color)
{
    var colors = {
		"aliceblue":"#f0f8ff","antiquewhite":"#faebd7","aqua":"#00ffff","aquamarine":"#7fffd4","azure":"#f0ffff",
		"beige":"#f5f5dc","bisque":"#ffe4c4","black":"#000000","blanchedalmond":"#ffebcd","blue":"#0000ff",
		"blueviolet":"#8a2be2","brown":"#a52a2a","burlywood":"#deb887","cadetblue":"#5f9ea0",
		"chartreuse":"#7fff00","chocolate":"#d2691e","coral":"#ff7f50","cornflowerblue":"#6495ed",
		"cornsilk":"#fff8dc","crimson":"#dc143c","cyan":"#00ffff", "darkblue":"#00008b","darkcyan":"#008b8b",
		"darkgoldenrod":"#b8860b","darkgray":"#a9a9a9","darkgreen":"#006400","darkkhaki":"#bdb76b",
		"darkmagenta":"#8b008b","darkolivegreen":"#556b2f", "darkorange":"#ff8c00","darkorchid":"#9932cc",
		"darkred":"#8b0000","darksalmon":"#e9967a","darkseagreen":"#8fbc8f","darkslateblue":"#483d8b",
		"darkslategray":"#2f4f4f","darkturquoise":"#00ced1", "darkviolet":"#9400d3","deeppink":"#ff1493",
		"deepskyblue":"#00bfff","dimgray":"#696969","dodgerblue":"#1e90ff", "firebrick":"#b22222",
		"floralwhite":"#fffaf0","forestgreen":"#228b22","fuchsia":"#ff00ff", "gainsboro":"#dcdcdc",
		"ghostwhite":"#f8f8ff","gold":"#ffd700","goldenrod":"#daa520","gray":"#808080","green":"#008000",
		"greenyellow":"#adff2f", "honeydew":"#f0fff0","hotpink":"#ff69b4", "indianred ":"#cd5c5c",
		"indigo ":"#4b0082","ivory":"#fffff0","khaki":"#f0e68c", "lavender":"#e6e6fa","lavenderblush":"#fff0f5",
		"lawngreen":"#7cfc00","lemonchiffon":"#fffacd","lightblue":"#add8e6","lightcoral":"#f08080",
		"lightcyan":"#e0ffff","lightgoldenrodyellow":"#fafad2", "lightgrey":"#d3d3d3","lightgreen":"#90ee90",
		"lightpink":"#ffb6c1","lightsalmon":"#ffa07a","lightseagreen":"#20b2aa","lightskyblue":"#87cefa",
		"lightslategray":"#778899","lightsteelblue":"#b0c4de", "lightyellow":"#ffffe0","lime":"#00ff00",
		"limegreen":"#32cd32","linen":"#faf0e6", "magenta":"#ff00ff","maroon":"#800000","mediumaquamarine":"#66cdaa",
		"mediumblue":"#0000cd","mediumorchid":"#ba55d3","mediumpurple":"#9370d8","mediumseagreen":"#3cb371",
		"mediumslateblue":"#7b68ee", "mediumspringgreen":"#00fa9a","mediumturquoise":"#48d1cc",
		"mediumvioletred":"#c71585","midnightblue":"#191970","mintcream":"#f5fffa","mistyrose":"#ffe4e1",
		"moccasin":"#ffe4b5", "navajowhite":"#ffdead","navy":"#000080", "oldlace":"#fdf5e6","olive":"#808000",
		"olivedrab":"#6b8e23","orange":"#ffa500","orangered":"#ff4500","orchid":"#da70d6", "palegoldenrod":"#eee8aa",
		"palegreen":"#98fb98","paleturquoise":"#afeeee","palevioletred":"#d87093","papayawhip":"#ffefd5",
		"peachpuff":"#ffdab9","peru":"#cd853f","pink":"#ffc0cb","plum":"#dda0dd","powderblue":"#b0e0e6",
		"purple":"#800080", "red":"#ff0000","rosybrown":"#bc8f8f","royalblue":"#4169e1", "saddlebrown":"#8b4513",
		"salmon":"#fa8072","sandybrown":"#f4a460","seagreen":"#2e8b57","seashell":"#fff5ee","sienna":"#a0522d",
		"silver":"#c0c0c0","skyblue":"#87ceeb","slateblue":"#6a5acd","slategray":"#708090","snow":"#fffafa",
		"springgreen":"#00ff7f","steelblue":"#4682b4", "tan":"#d2b48c","teal":"#008080","thistle":"#d8bfd8",
		"tomato":"#ff6347","turquoise":"#40e0d0", "violet":"#ee82ee", "wheat":"#f5deb3","white":"#ffffff",
		"whitesmoke":"#f5f5f5", "yellow":"#ffff00","yellowgreen":"#9acd32"
	};

    if (typeof colors[color.toLowerCase()] != 'undefined')
        return colors[color.toLowerCase()];

    return color;
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
