Ember.TEMPLATES['layerList'] = Ember.Handlebars.compile(
'<h3 {{ action "toggleShowLayers" }}>Layers</h3>' +
'<span {{bindStyle display="showLayers"}} class="toolbarDropdown">' +

'{{#each content.topToBottom}}' +
	'{{view RPGB.LayerListItemView tagName="p"}}' +
'{{/each}}' +

'<p>' +
	'{{view RPGB.LayerNameField valueBinding="newName"}} <button {{action "createLayer"}}>+</button>' +
'</p>' +

'</span>');


Ember.TEMPLATES['layerListItem'] = Ember.Handlebars.compile(
	'<div {{bindAttr class="visibility"}}></div>' +
	'{{ name }}');

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
	}
});

RPGB.LayerListItemView = Ember.View.extend({
	templateName: 'layerListItem',
	content: null,
	visibility: 'full'
});

RPGB.LayerNameField = Ember.TextField.extend({
	change: function(evt){
		this.set('newName', evt.target.value);
		return false;
	}
})
