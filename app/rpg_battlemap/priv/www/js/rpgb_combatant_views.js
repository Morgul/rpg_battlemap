Ember.TEMPLATES['combatantList'] = Ember.Handlebars.compile(
'<div class="btn-group">' +
	'<a class="btn dropdown-toggle" data-toggle="dropdown" href="#">' +
		'Combatants' +
		'<span class="caret"></span>' +
	'</a>' +
	'<div class="dropdown-menu">' +

		'{{#each content}}' +
			'{{view RPGB.CombatantListItemView tagName="p" class="combatantItem"}}' +
		'{{/each}}' +

		'<p>' +
			'<a href="#" role="button" class="btn" data-target="#newCombatantDialog" data-toggle="modal" {{action "activateAddCombatantTool" }}>New...</a>' +
		'</p>' +

	'</div>' +

	'{{view RPGB.CombatantCreateView }}' +
'</div>');

Ember.TEMPLATES['combatantCreateView'] = Ember.Handlebars.compile(
	'<div id="newCombatantDialog" class="modal hide fade" role="dialog">' +

		'<div class="modal-header">' +
			'<button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>' +
			'<h3>New Combatant</h3>' +
		'</div>' +

		'<div class="modal-body">' +

			'<p>Set up a new combatant here. While using the \'place combatant\' tool, ' +
				'a new combatant with these settings will be created for you, with an ' +
				'increment appended to the name if needed' +
			'</p>' +

			/*'<p>' +
				'{{content.newCombatant.name}}' +
			'</p>' +*/

			'<p>' +
				'<label>Name</label>' +
				'{{view Ember.TextField valueBinding="content.newCombatant.name"}}' +
			'</p>' +

			'<p>' +
				'<label>Size</label>' +
				'<input type="number" step="1" min="1" max="7" ' +
					'{{ bindAttr value="content.newCombatant.size" }}' +
					'{{ action "sizeChange" on="change" target="view"}}/>' +
			'</p>' +

			'<p>' +
				'<label>Color</label>' +
				'<input type="color"' +
					'{{ bindAttr vlaue="content.newCombatant.color" }}' +
					'{{ action "colorChange" on="change" }}/>' +
			'</p>' +

			'<p>' +
				'<label>Initiative</label>' +
				'<input type="number" step="1" min="-100" max="100" ' +
					'{{ bindAttr value="content.newCombatant.initiative" }}' +
					'{{ action "initiativeChange" on="change" target="view"}}/>' +
			'</p>' +

			'<p>' +
				'<label title="An aura is a zone that surrounds a combatant and moves ' +
					'with them.">Aura Radius</label>' +
				'<input type="number" step="1" min="0" max="20" ' +
					'{{ bindAttr value="content.newCombatant.aura_size" }}' +
					'{{ action "auraSizeChange" on="change" target="view"}}/>' +
			'</p>' +

			'<p>' +
				'<label>Aura Color</label>' +
				'<input type="color"' +
					'{{ bindAttr value="content.newCombatant.aura_color" }}' +
					'{{ action "auraColorChange" on="change" }}/>' +
			'</p>' +
/*
			'<label>Line Opacity</label>' +
			'<input type="number" step="0.1" min="0.0" max="1.0" ' +
				'{{ bindAttr value="content.grid_opacity" }}' +
				'{{ action "gridOpacityChange" on="change" }}/>' +
		'</p>' +

		'<p>' +
			'<label>Background</label>' +
			'<input type="color"' +
				'{{ bindAttr value="content.background_color" }}' +
				'{{ action "backgroundColorChange" on="change" }}/>' +
*/

		'</div>' +

		'<div class="modal-footer">' +
 			'<button class="btn btn-primary" data-dismiss="modal">Done</button>' +
 		'</div>' +

	'</div>'

		//'<button {{action "showCreateCombatant"}}>New...</button>' +
		//'{{name}} - {{map.name}} - {{context.name}} - {{context.map.name}} - {{content.name}} - {{content.map.name}}'
);

Ember.TEMPLATES['combatantListView'] = Ember.Handlebars.compile(
'<img {{bindAttr src="context.token_url"}} />' +
'{{ content.name }}'
);

Ember.TEMPLATES['combatantSVG'] = Ember.Handlebars.compile('');

RPGB.CombatantListView = Ember.View.extend({
	templateName: 'combatantList',
	content: null,
	showCombatants: 'none',

	toggleShowCombatants: function(){
		if(this.get('showCombatants') == 'none'){
			this.set('showCombatants', 'block');
		} else {
			this.set('showCombatants', 'none');
		}
	},

	setSelected: function(combatantObj){
		this.set('content.selected', combatantObj);
	},

	activateAddCombatantTool: function(){
		this.set('content.map.currentTool', 'addCombatant');
	}
});

RPGB.CombatantCreateView = Ember.View.extend({
	templateName: 'combatantCreateView',

	createCombatantAt: function(){
		if(this._creationEnabled){
			console.log('creation enabled!');
		}
	}.observes('context.content.map.clickedCell'),

	setCreation: function(){
		var activeTool = this.get('context.content.map.currentTool');
		if(activeTool == 'addCombatant'){
			this._creationEnabled = true;
			return;
		}
		this._creationEnabled = false;
	}.observes('context.content.map.currentTool'),

	auraSizeChange: function(ev){
		this.set('context.content.newCombatant.aura_size', parseInt(ev.target.value));
	},

	auraColorChange: function(ev){
		this.set('context.content.newCombatant.aura_color', ev.target.value);
	},

	colorChange: function(ev){
		this.set('context.content.newCombatant.color', ev.target.value);
	},

	sizeChange: function(ev){
		window.gu = this;
		this.set('context.content.newCombatant.size', parseInt(ev.target.value));
	},

	initiativeChange: function(ev){
		this.set('context.content.newCombatant.initiative', parseInt(ev.target.value));
	}
});

RPGB.CombatantListItemView = Ember.View.extend({
	templateName: 'combatantListItem',
	classNameBindings: 'context.selected',

	click: function(){
		this.set('parentView.content.selected', this.get('context'));
		return false;
	}
});

RPGB.CombatantItemSVGView = Ember.View.extend({
	templateName: 'combatantSVG',

	init: function(){
		this._super();
	}
});