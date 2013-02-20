Ember.TEMPLATES['zoneAuraList'] = Ember.Handlebars.compile(
	'<div class="btn-group" {{bindStyle display="displayZoneAuraDropdown"}}>' +
		'<a class="btn dropdown-toggle" data-toggle="dropdown" href="#">' +
			'Zones &amp; Auras' +
			'<span class="caret"></span>' +
		'</a>' +

		'<div class="dropdown-menu">' +
			'<p>Zones</p>' +
			'{{#each content.selected.zones}}' +
				'{{view RPGB.ZoneAuraListItemView tagName="p" class="zoneAuraItem"}}' +
			'{{/each}}' +

			'<p>' +
				'{{view Ember.TextField valueBinding="newZoneName"}}' +
				'<a href="#" role="button" class="btn" {{action "createNewLineZone"}}>Lines</a>' +
				'<a href="#" role="button" class="btn" {{action "createNewFilledZone"}}>Filled</a>' +
			'</p>' +

			'<p class="divider"></p>' +

			'<p>Auras</p>' +
			'{{#each content.selected.auras}}' +
				'{{view RPGB.ZoneAuraListItemView tagName="p" class="zoneAuraItem"}}' +
			'{{/each}}' +

			'<p>' +
				'{{view Ember.TextField valueBinding="newAuraName"}}' +
				'<a href="#" role="button" class="btn" {{action "createNewLineAura"}}>Lines</a>' +
				'<a href="#" role="button" class="btn" {{action "createNewFilledAura"}}>Filled</a>' +
			'</p>' +

			'<p class="divider"></p>' +

			'<p>' +
				'<a href="#" role="button" class="btn" {{action "deleteSelected"}}>Delete</a>' +
			'</p>' +
		'</div>' +
	'</div>');

Ember.TEMPLATES['zoneAuraListItem'] = Ember.Handlebars.compile(
	'{{name}}');

Ember.TEMPLATES['zoneAuraSvg'] = Ember.Handlebars.compile(
	'{{#if view.isPolyLine}}' +
		'<polyline ' +
			'{{bindAttr points="view.correctedPoints"}} ' +
			'{{bindAttr stroke="stroke_color"}} ' +
			'{{bindAttr stroke-width="stroke_width"}} ' +
			'{{bindAttr stroke-opacity="stroke_opacity"}} ' +
		'>' +
	'{{/if}}' +

	'{{#if view.isPolygon}}' +
		'<polygon ' +
			'{{bindAttr points="view.correctedPoints"}} ' +
			//'{{bindAttr points="element_attrs.points"}} ' +
			'{{bindAttr stroke="stroke_color"}} ' +
			'{{bindAttr stroke-width="stroke_width"}} ' +
			'{{bindAttr stroke-opacity="stroke_opacity"}} ' +
			'{{bindAttr fill="fill_color"}} ' +
			'{{bindAttr fill-opacity="opacity"}} ' +
		'>' +
	'{{/if}}');

Ember.TEMPLATES['editPolyRegion'] = Ember.Handlebars.compile(
	'{{#if content.regionEditor.isEditing}}' +
		'{{#each content.regionEditor.points}}' +
			'{{view RPGB.EditPolyRegionPoint}}' +
		'{{/each}}' +
	'{{/if}}'
);

Ember.TEMPLATES['editPolyRegionPoint'] = Ember.Handlebars.compile(
	'<circle ' +
		'{{bindAttr cx="view.circle.cx"}} ' +
		'{{bindAttr cy="view.circle.cy"}} ' +
		'{{bindAttr r="view.circle.radius"}} ' +
		'stroke="black" ' +
		'stroke-width="3" ' +
		'fill="white" ' +
	'>' +
	'</circle>'
);

RPGB.ZoneAuraListView = Ember.View.extend({
	templateName: 'zoneAuraList',
	content: null,
	newZoneName: '',
	newAuraName: '',

	displayZoneAuraDropdown: function(){
		var selected = this.get('content.selected');
		if(selected){
			return 'inline-block';
		}
		return 'none';
	}.property('content.selected'),

	didInsertElement: function(){
		this.$('input, selected').on('click', function(ev){
			ev.stopPropagation();
		});
	},

	createNewLineZone: function(ev){
		var name = this.get('newZoneName');
		this.createZoneAura('zone', 'polyline', name);
	},

	createNewFilledZone: function(){
		var name = this.get('newZoneName');
		this.createZoneAura('zone', 'polygon', name);
	},

	createNewLineAura: function(){
		var name = this.get('newAuraName');
		this.createZoneAura('aura', 'polyline', name);
	},

	createNewFilledAura: function(){
		var name = this.get('newAuraName');
		this.createZoneAura('aura', 'polygon', name);
	},

	createZoneAura: function(type, element, name){
		var selected = this.get('content.selected');
		if(! selected){
			return;
		}
		selected.createZoneAura(type, element, name);
	}
});

RPGB.ZoneAuraListItemView = Ember.View.extend({
	templateName: 'zoneAuraListItem',
	classNameBindings: 'context.selected',

	click: function(){
		var selected = this.get('context.selected');
		if(selected){
			this.set('context.selected', false);
			return;
		}
		this.set('parentView.content.selected.selectedZoneAura', this.get('context'));
	}
});

RPGB.ZoneAuraSVGView = Ember.View.extend({
	templateName: 'zoneAuraSvg',
	tagName: 'g',

	isPolyLine: function(){
		var elemType = this.get('context.element_type');
		return elemType == 'polyline';
	}.property('context.element_type'),

	isPolygon: function(){
		var elemType = this.get('context.element_type');
		return elemType == 'polygon';
	}.property('context.element_type'),

	correctedPoints: function(){
		console.log('ver boing!');
		var elem = document.createElementNS('http://www.w3.org/2000/svg', 'polyline');
		elem.setAttribute('points', this.get('context.element_attrs.points'));
		var mapFun = function(point){
			var pixels = RPGB.mapController.cellToPixels(point.x, point.y);
			return pixels[0] + ' ' + pixels[1];
		}
		var pointsArr = [];
		for(var i = 0; i < elem.animatedPoints.numberOfItems; i++){
			var point = elem.animatedPoints.getItem(i);
			pointsArr.push(mapFun(point));
		}
		return pointsArr.join(' ');
	}.property('context.element_attrs.points')
});

RPGB.EditPolyRegionView = Ember.View.extend({
	templateName: 'editPolyRegion',
	tagName:'g'
});

RPGB.EditPolyRegionPoint = Ember.View.extend({
	templateName: 'editPolyRegionPoint',
	tagName: 'g',

	didInsertElement: function(){
		this._super();
		window.pv = this.get('parentView');
		this.$('rect').mousedown(function(){
			console.log('rect md!');
		});
	},

	circle: function(){
		var x = this.get('context.x');
		var y = this.get('context.y');
		var xy = RPGB.mapController.cellToPixels(x, y);
		return {
			'cx': xy[0],
			'cy': xy[1],
			'radius': RPGB.CELL_SIZE / 4
		};
	}.property('context.x', 'context.y'),

	isSelectedAsOpacity: function(){
		if(this.get('context.selected')){
			return 1;
		}
		return 0;
	}.property('context.selected'),

	mouseDown: function(){
		console.log('imma stab a ho!');
		RPGB.editRegionController.set('selectedPoint', this.get('context'));
	}
});
