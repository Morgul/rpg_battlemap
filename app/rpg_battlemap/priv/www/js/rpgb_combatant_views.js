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

		'</div>' +

		'<div class="modal-footer">' +
 			'<button class="btn btn-primary" data-dismiss="modal">Done</button>' +
 		'</div>' +

	'</div>'

		//'<button {{action "showCreateCombatant"}}>New...</button>' +
		//'{{name}} - {{map.name}} - {{context.name}} - {{context.map.name}} - {{content.name}} - {{content.map.name}}'
);

Ember.TEMPLATES['combatantListItem'] = Ember.Handlebars.compile(
'<img {{bindAttr src="token_url"}} {{ bindStyle border-color="color" }} class="listToken" />' +
'{{ name }}'
);

Ember.TEMPLATES['combatantDropDown'] = Ember.Handlebars.compile(
'<div class="btn-group" {{bindStyle display="displayCombatantDropdown"}}>' +
	'<a class="btn dropdown-toggle" data-toggle="dropdown" href="#">' +
		'{{content.selected.name}}' +
		'<span class="caret"></span>' +
	'</a>' +
	'<div class="dropdown-menu">' +

		'<img class="pull-left avatar" {{bindAttr src="content.select.avatar_url"}} />' +

		'<p>' +
			'<label>Name</label>' +
			'{{view Ember.TextField valueBinding="content.selected.name"}}' +
		'</p>' +

		'<p>' +
			'<label>Size</label>' +
			'<input type="number" min="1" max="10" step="1"' +
				'{{ bindAttr value="content.selected.size" }}' +
				'{{ action "sizeChange" on="change" }}/>' +
		'</p>' +

		'<p>' +
			'<label>Color</label>' +
			'<input type="color"' +
				'{{ bindAttr value="content.selected.color" }}' +
				'{{ action "colorChange" on="change" }}/>' +
		'</p>' +

		'<p>' +
			'<label>Aura Size</label>' +
			'<input type="number" min="0" max="10" step="1"' +
				'{{ bindAttr value="content.selected.aura_size" }}' +
				'{{ action "auraSizeChange" on="change" }}/>' +
		'</p>' +

		'<p>' +
			'<label>Aura Color</label>' +
			'<input type="color"' +
				'{{ bindAttr value="content.selected.aura_color" }}' +
				'{{ action "auraColorChange" on="change" }}/>' +
		'</p>' +

		'<p>' +
			'<label>Layer</label>' +
			'{{view Ember.Select contentBinding="view.layers"' +
				'optionValuePath="content.id" ' +
				'optionLabelPath="content.name" ' +
				'valueBinding="view.content.selected.layer_id"}}' +
		'</p>' +

	'</div>' +

'</div>'
);

Ember.TEMPLATES['combatantSVG'] = Ember.Handlebars.compile(
	'<defs>' +
		'<circle ' +
			'{{bindAttr cx="view.cellXCenter"}} ' +
			'{{bindAttr cy="view.cellYCenter"}} ' +
			'{{ bindAttr r="view.radius" }} ' +
			'{{ bindAttr fill="view.context.color"}} ' +
			'{{ bindAttr stroke-opacity="view.isSelectedAsOpacity" }} ' +
			'stroke="blue" ' +
			'stroke-width="3" ' +
			'stroke-dasharray="20 5" ' +
		'>' +
			'<animate attributeName="stroke-dashoffset" ' +
				'attributeType="XML" ' +
				'dur="15s" ' +
				'from="0%" ' +
				'to="100%" ' +
				'repeatCount="indefinite">' +
			'</animate>' +
		'</circle>' +
		'<image {{bindAttr x="view.cellXCorner"}} ' +
			'{{bindAttr y="view.cellYCorner"}} ' +
			'{{bindAttr width="view.size"}} ' +
			'{{bindAttr height="view.size"}} ' +
		'data-xlink-href="" />' +
		'<mask>' +
			'<circle ' +
				'{{bindAttr cx="view.cellXCenter"}} ' +
				'{{bindAttr cy="view.cellYCenter"}} ' +
				'{{ bindAttr r="view.radius" }} ' +
				'fill="white" />' +
		'</mask>' +
	'</defs>' +
	'{{#if view.hasAura }}' +
	'<circle ' +
		'{{ bindAttr cx="view.cellXCenter"}} ' +
		'{{ bindAttr cy="view.cellYCenter"}} ' +
		'{{ bindAttr r="view.auraRadius" }} ' +
		'{{ bindAttr fill="view.context.aura_color"}} ' +
		'fill-opacity="0.2" ' +
		'{{ bindAttr stroke="view.context.aura_color"}} ' +
		'stroke-width="5px" ' +
	'></circle>' +
	'{{/if}}' +
/*	'{{#if hasGhost}}' +
		'{{#if view.hasAura }}' +
			'<circle ' +
				'{{ bindAttr cx="view.ghostCellXCenter"}} ' +
				'{{ bindAttr cy="view.ghostCellYCenter"}} ' +
				'{{ bindAttr r="view.auraRadius"}} ' +
				'fill-opacity="0" ' +
				'stroke-width="5px" ' +
				'stroke="gray" ' +
			'></circle>' +
		'{{/if}}' +
		'<circle ' +
			'{{ bindAttr cx="view.ghostCellXCenter"}} ' +
			'{{ bindAttr cy="view.ghostCellYCenter"}} ' +
			'{{ bindAttr r="view.radius"}} ' +
			'fill-opacity="0" ' +
			'stroke="gray" ' +
			'stroke-width="5px" ' +
		'></circle>' +
	'{{/if}}' +*/
	'<use data-xlink-href="circle" {{action "setSelected" }} draggable="true"></use>' +
	'<use data-xlink-href="image" mask="mask" style="display:inline;"></use>'
);

RPGB.CombatantListView = Ember.View.extend({
	templateName: 'combatantList',
	content: null,
	showCombatants: 'none',

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
		if(! this._creationEnabled){
			return;
		}
		console.log('creation enabled!');
		var combatantBase = this.get('context.content.newCombatant');
		var clickedCell = this.get('context.content.map.clickedCell');
		var combatantArgs = {
			name: this.get('context.content').nextName(combatantBase.get('name')),
			size: combatantBase.get('size'),
			color: combatantBase.get('color'),
			aura_size: combatantBase.get('aura_size'),
			aura_color: combatantBase.get('aura_color'),
			x: clickedCell.get('x'),
			y: clickedCell.get('y')
		};
		this.get('context.content').createCombatant(combatantArgs);
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

RPGB.CombatantDropDown = Ember.View.extend({
	templateName: 'combatantDropDown',

	init: function(){
		this._super();
	},

	didInsertElement: function(){
		this.$('input, selected').on('click', function(ev){
			ev.stopPropagation();
		});
	},

	displayCombatantDropdown: function(){
		var selected = this.get('content.selected');
		if(selected){
			return 'inline-block';
		}
		return 'none';
	}.property('content.selected'),

	layers: function(){
		var layers = this.get('content.map.layers.content').copy();
		layers.unshift({'name': 'none', 'id':null});
		return layers;
	}.property('content.map.layers.@each'),

	sizeChange: function(ev){
		this.set('content.selected.size', parseInt(ev.target.value));
		return false;
	},

	colorChange: function(ev){
		this.set('content.selected.color', ev.target.value);
		return false;
	},

	auraSizeChange: function(ev){
		this.set('content.selected.aura_size', parseInt(ev.target.value));
		return false;
	},

	auraColorChange: function(ev){
		this.set('content.selected.aura_color', ev.target.value);
		return false;
	}
});

RPGB.CombatantItemSVGView = Ember.View.extend({
	templateName: 'combatantSVG',
	tagName: 'g',

	init: function(){
		this._super();
	},

	didInsertElement: function(){
		this.$('defs circle').attr('id', this._elemId('circle'));
		this.$('defs image').attr('id', this._elemId('image'));
		this.$('defs mask').attr('id', this._elemId('mask'));

		var thisRef = this;
		this.get('context').addObserver('token_image', function(){
			thisRef._fixImageElement();
		});
		this._fixImageElement();
		this._fixUseElements();

		var circleUse = this.$('use[mask]')[0];
		circleUse.setAttribute('pointer-events', 'visiblePainted');
		$(circleUse).mousedown(function(ev){
			thisRef.setSelected(ev);
		});

		$(circleUse).on('mousedown', function(ev){
			console.log('combatant mouse down', ev);
		});
	},

	_elemId: function(base){
		return base + '-' + this.$().attr('id');
	},

	_fixUseElements: function(){
		var thisRef = this;
		this.$('use').each(function(index, elem){
			var targetElem = $(elem).attr('data-xlink-href');
			var corrected = '#' + thisRef._elemId(targetElem);
			$(elem)[0].setAttributeNS('http://www.w3.org/1999/xlink', 'href', corrected);
			if($(elem).attr('mask')){
				$(elem).attr('mask', 'url(#' + thisRef._elemId('mask') + ')');
			}
		});
	},

	_fixImageElement: function(){
		var tokenImage = this.get('context.token_image');
		if(! tokenImage){
			tokenImage = "";
		}
		this.$('defs image')[0].setAttributeNS('http://www.w3.org/1999/xlink', 'href', tokenImage);
	},

	_moveCombatantImageFix: function(){
		var use = this.$('use[mask]')[0];
		var hideIt = function(){ use.style.display = "none"; };
		var showIt = function(){ use.style.display = "inline"; };
		setTimeout(function(){
			hideIt();
			setTimeout(function(){
				showIt();
			}, 1);
		}, 1);
	}.observes('context.x', 'context.y'),

	setSelected: function(ev){
		console.log('combatant clicked');
		window.combatantClicked = this;
		this.set('context.map.combatants.selected', this.get('context'));
		//ev.stopPropagation();
		//return false;
	},

	circleId: function(){
		return 'base-circle-' + this.$().attr('id');
	},

	hasAura: function(){
		var auraSize = this.get('context.aura_size');
		return auraSize > 0;
	}.property('context.aura_size'),

	auraRadius: function(){
		var auraSize = this.get('context.aura_size');
		var combatantSize = this.get('context.size');
		return (auraSize * RPGB.CELL_SIZE) + (combatantSize * RPGB.CELL_HALF_SIZE);
	}.property('context.size', 'context.aura_size'),

	radius: function(){
		var size = this.get('context.size');
		return size * RPGB.CELL_HALF_SIZE;
	}.property('context.size'),

	token_image_radius: function(){
		var r = this.get('radius');
		return r * 0.9;
	}.property(),

	_cellAxisToCenter: function(xory){
		var size = this.get('context.size');
		return xory * RPGB.CELL_SIZE + (size * RPGB.CELL_HALF_SIZE);
	},

	cellXCenter: function(){
		var x = this.get('context.x');
		return this._cellAxisToCenter(x);
	}.property('context.x', 'context.size'),

	cellYCenter: function(){
		return this._cellAxisToCenter(this.get('context.y'));
	}.property('context.y', 'context.size'),

	cellXCorner: function(){
		return this.get('context.x') * RPGB.CELL_SIZE;
	}.property('context.x'),

	cellYCorner: function(){
		return this.get('context.y') * RPGB.CELL_SIZE;
	}.property('context.y'),

	size: function(){
		return this.get('context.size') * RPGB.CELL_SIZE;
	}.property('context.size'),

	isSelectedAsOpacity: function(){
		if(this.get('context.selected')){
			return 1;
		}
		return 0;
	}.property('context.selected')
});
