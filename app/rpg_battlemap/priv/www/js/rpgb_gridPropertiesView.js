Ember.TEMPLATES['gridSettings'] = Ember.Handlebars.compile(
'<div class="btn-group">' +
	'<a class="btn dropdown-toggle" data-toggle="dropdown" href="#">Grid<span class="caret"></span></a>' +
	'<div class="dropdown-menu">' +

		'<p>' +
			'<label>Grid Lines</label>' +
			'<input type="color"' +
				'{{ bindAttr value="content.gridline_color" }}' +
				'{{ action "gridlineColorChange" on="change" }}/>' +
		'</p>' +

		'<p>' +
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
		'</p>' +

	'</div>' +
'</div>');

RPGB.GridPropertiesView = Ember.View.extend({
	templateName: 'gridSettings',
	content: null,
	showProperties: 'none',

	/*toggleShowProperties: function(){
		if(this.get('showProperties') == 'none'){
			this.set('showProperties', 'block');
		} else {
			this.set('showProperties', 'none');
		}
	},*/

	didInsertElement: function(){
		$('input').click(function(ev){
			ev.stopPropagation();
		});
	},

	backgroundColorChange: function(ev){
		this.set('content.background_color', ev.target.value);
		return false;
	},

	gridOpacityChange: function(ev){
		this.set('content.grid_opacity', parseFloat(ev.target.value));
		return false;
	},

	gridlineColorChange: function(ev){
		this.set('content.gridline_color', ev.target.value);
		return false;
	}
});
