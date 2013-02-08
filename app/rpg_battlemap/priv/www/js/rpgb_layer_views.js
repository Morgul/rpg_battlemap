Ember.TEMPLATES['layerList'] = Ember.Handlebars.compile(
'<div class="btn-group">' +
	'<a class="btn dropdown-toggle" data-toggle="dropdown" href="#">' +
		'Layers' +
		'<span class="caret"></span>' +
	'</a>' +
'<div class="dropdown-menu">' +

'{{#each content.topToBottom}}' +
	'{{view RPGB.LayerListItemView tagName="p" class="layerItem"}}' +
'{{/each}}' +

'<p>' +
	'{{view RPGB.LayerNameField valueBinding="newName"}} <button {{action "createLayer"}}>+</button>' +
'</p>' +

'<p>' +
	'<button {{action "deleteSelectedLayer" target="content"}}>Delete Selected</button>' +
'</p>' +

'</div>' +
'</div>');


Ember.TEMPLATES['layerListItem'] = Ember.Handlebars.compile(
	'<button {{bindAttr class="visibility"}} class="layerVisibility" {{action "cycleVisibility" }}></button>' +
	'{{ name }}'
);

Ember.TEMPLATES['layerSVG'] = Ember.Handlebars.compile(
'{{#each view.context.zones}}' +
	'{{view RPGB.ZoneAuraSVGView}}' +
'{{/each}}' +

'{{#each view.context.combatants}}' +
	'{{view RPGB.CombatantItemSVGView }}' +
'{{/each}}' +

'{{#each view.context.auras}}' +
	'{{view RPGB.ZoneAuraSVGView}}' +
'{{/each}}'
);

RPGB.LayerListView = Ember.View.extend({
	templateName: 'layerList',
	content: null,
	showLayers: 'none',
	newName: '',

	toggleShowLayers: function(){
		if(this.get('showLayers') == 'none'){
			this.set('showLayers', 'block');
		} else {
			this.set('showLayers', 'none');
		}
	},

	createLayer: function(){
		var layerArgs = {
			'name': this.get('newName')
		};
		this.get('content').createLayer(layerArgs);
	},

	setVisibilityMode: function(mode){
		console.log('set visibility mode', mode);
	},

	setSelected: function(layerObj){
		this.set('content.selected', layerObj);
	}
});

RPGB.LayerListItemView = Ember.View.extend({
	templateName: 'layerListItem',
	classNameBindings: 'context.selected',
	//content: null,

	// event handlers
	click: function(ev){
		this.set('parentView.content.selected', this.get('context'));
		//ev.stopPropagation();
		return false;
	},

	// actions
	cycleVisibility: function(){
		this.set('parentView.content.visibilityMode', 'custom');
		var visibility = this.get('context.visibility');
		switch(visibility){
			case "full":
				visibility = "half";
				break;
			case "half":
				visibility = "none";
				break;
			default:
				visibility = "full";
		}
		this.set('context.visibility', visibility);
		return false;
	}
});

RPGB.LayerNameField = Ember.TextField.extend({
	change: function(evt){
		this.set('newName', evt.target.value);
		return false;
	},

	click: function(){
		return false;
	}
});

RPGB.LayerItemSVGView = Ember.View.extend({
	templateName: 'layerSVG',

	init: function(){
		this._super();
	}
});
