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
			'{{bindAttr points="element_attrs.points"}} ' +
			'{{bindAttr stroke="stroke_color"}} ' +
			'{{bindAttr stroke-width="stroke_width"}} ' +
			'{{bindAttr stroke-opacity="stroke_opacity"}} ' +
		'>' +
	'{{/if}}' +

	'{{#if view.isPolygon}}' +
		'<polygon ' +
			'{{bindAttr points="element_attrs.points"}} ' +
			'{{bindAttr stroke="stroke_color"}} ' +
			'{{bindAttr stroke-width="stroke_width"}} ' +
			'{{bindAttr stroke-opacity="stroke_opacity"}} ' +
			'{{bindAttr fill="fill_color"}} ' +
			'{{bindAttr fill-opacity="opacity"}} ' +
		'>' +
	'{{/if}}');

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
	}.property('context.element_type')
});