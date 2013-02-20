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
				'<a href="#" role="button" class="btn" {{action "deleteSelected" }}>Delete</a>' +
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
			'{{ bindAttr fill="fill_color"}} ' +
			'{{ bindAttr fill-opacity="view.opacity"}} ' +
		'>' +
	'{{/if}}' +

	'{{#if view.isPolygon}}' +
		'<polygon ' +
			'{{bindAttr points="view.correctedPoints"}} ' +
			'{{bindAttr stroke="stroke_color"}} ' +
			'{{bindAttr stroke-width="stroke_width"}} ' +
			'{{bindAttr stroke-opacity="stroke_opacity"}} ' +
			'{{bindAttr fill="fill_color"}} ' +
			'{{bindAttr fill-opacity="view.opacity"}} ' +
		'>' +
	'{{/if}}');

Ember.TEMPLATES['zoneAuraDropdown'] = Ember.Handlebars.compile(
	'<div class="btn-group" {{bindStyle display="displayZoneAuraDropdown"}}>' +
		'<a class="btn dropdown-toggle" data-toggle="dropdown" href="#">' +
			'{{content.name}}' +
			'<span class="caret"></span>' +
		'</a>' + 
		'<div class="dropdown-menu">' +

			'<p>' +
				'<label>Name</label>' +
				'{{view Ember.TextField valueBinding="content.name"}}' +
			'</p>' +

			'<p>' +
				'<label>Border Color</label>' +
				'<input type="color" ' +
					'{{bindAttr value="content.stroke_color" }}' +
					'{{ action "strokeColorChange" on="change" }}' +
				'/>' +
			'</p>' +

			'<p>' +
				'<label>Border Width</label>' +
				'<input type="number" min="0" max="20" step="1" ' +
					'{{ bindAttr value="content.stroke_width" }} ' +
					'{{ action "strokeWidthChange" on="change" }} ' +
				'/>' +
			'</p>' +

			'<p>' +
				'<label>Border Opacity</label>' +
				'<input type="number" min="0" max="1" step="0.1" ' +
					'{{ bindAttr value="content.stroke_opacity" }} ' +
					'{{ action "strokeOpacityChange" on="change" }} ' +
				'/>' +
			'</p>' +

			'<p>' +
				'<label>Fill Color</label>' +
				'<input type="color" ' +
					'{{ bindAttr value="content.fill_color" }} ' +
					'{{ action "fillColorChange" on="change" }} ' +
				'/>' +
			'</p>' +

			'<p>' +
				'<label>Fill Opacity</label>' +
				'<input type="number" min="0" max="1" step="0.1" ' +
					'{{ bindAttr value="content.fill_opacity" }} ' +
					'{{ action "fillOpacityChange" on="change" }} ' +
				'/>' +
			'</p>' +

		'</div>' +
	'</div>'
);

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
		'{{bindAttr stroke="view.stroke" }} ' +
		'stroke-width="3" ' +
		'{{bindAttr stroke-dasharray="view.strokeDasharray" }} ' +
		'{{bindAttr fill="view.fill"}} ' +
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
	},

	deleteSelected: function(){
		var selectedLayer = this.get('content.selected');
		if(selectedLayer){
			selectedLayer.deleteSelected();
		}
	}
});

RPGB.ZoneAuraListItemView = Ember.View.extend({
	templateName: 'zoneAuraListItem',
	classNameBindings: 'context.selected',

	click: function(ev){
		var selected = this.get('context.selected');
		if(selected){
			this.set('context.selected', false);
			return;
		}
		this.set('parentView.content.selected.selectedZoneAura', this.get('context'));
		ev.stopPropagation();
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

	opacity: function(){
		var type = this.get('context.type');
		var opacity = this.get('context.fill_opacity');
		var coefficient = 1;
		if(type == "aura"){
			coefficient = 0.3;
		}
		return opacity * coefficient + '';
	}.property('context.type', 'context.fill_opacity'),

	correctedPoints: function(){
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

RPGB.ZoneAuraDropDown = Ember.View.extend({
	templateName: 'zoneAuraDropdown',

	displayZoneAuraDropdown: function(){
		if(this.get('content')){
			return 'inline-block';
		}
		return 'none';
	}.property('content'),

	didInsertElement: function(){
		this.$('input, select').on('click', function(ev){
			ev.stopPropagation();
		});
	},

	strokeColorChange: function(ev){
		this.set('content.stroke_color', ev.target.value);
	},

	strokeWidthChange: function(ev){
		this.set('content.stroke_width', parseInt(ev.target.value));
	},

	strokeOpacityChange: function(ev){
		this.set('content.stroke_opacity', parseFloat(ev.target.value));
	},

	fillColorChange: function(ev){
		this.set('content.fill_color', ev.target.value);
	},

	fillOpacityChange: function(ev){
		this.set('content.fill_opacity', parseFloat(ev.target.value));
	}
});

RPGB.EditPolyRegionView = Ember.View.extend({
	templateName: 'editPolyRegion',
	tagName:'g'
});

RPGB.EditPolyRegionPoint = Ember.View.extend({
	templateName: 'editPolyRegionPoint',
	tagName: 'g',
	hover: false,

	didInsertElement: function(){
		this._super();
		var thisRef = this;
		this.$('circle').on('mouseenter', function(){
			if(thisRef.isDestroyed){
				return;
			}
			thisRef.set('hover', true);
		}).on('mouseleave', function(){
			if(thisRef.isDestroyed){
				return;
			}
			thisRef.set('hover', false);
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

	strokeDasharray: function(){
		if(this.get('context.selected')){
			return "4 2";
		}
		return "none";
	}.property('context.selected'),

	stroke: function(){
		if(this.get('hover')){
			return "white";
		}
		return "black";
	}.property('hover'),

	fill: function(){
		if(this.get('hover')){
			return "black";
		}
		return "white";
	}.property('hover'),

	mouseDown: function(ev){
		if(ev.shiftKey){
			this.set('context.selected', true);
			return;
		}
		RPGB.editRegionController.set('selectedPoint', this.get('context'));
	}
});
