$().ready(function(){
	addColorPicker('.grid', 'grid_color');
	addColorPicker('.map', 'map_color');

	// Grid Line Color editor
	$('#grid_color').val(rgb2Hex(battleMap.gridlineColor));
	$('#grid_alpha').val(getAlphaFromRGBA(battleMap.gridlineColor));
	$('#grid_color').change(function(){
		var color = color2Hex($('#grid_color').val());
		var alpha = $('#grid_alpha').val();
		battleMap.gridlineColor = hex2rgb(color, alpha);
		battleMap.drawGrid();
	});
	$('#grid_color').blur(function(){
		var color = color2Hex($('#grid_color').val());
		var alpha = $('#grid_alpha').val();
		battleMap.gridlineColor = hex2rgb(color, alpha);
		battleMap.drawGrid();
	});
	$('#grid_alpha').change(function() {
		$('#grid_color').change();
	});
	$('#grid_alpha').bind('input', function() {
		$('#grid_color').change();
	});
	$('#grid_color').change();

	// Background Color editor
	$('#map_color').val(rgb2Hex(battleMap.backgroundColor));
	$('#map_color').change(function(){
		var color = color2Hex($('#map_color').val());
		$('#drawingBoard').css("background-color", hex2rgb(color));
		battleMap.drawGrid();
	});
	$('#map_color').blur(function(){
		var color = color2Hex($('#map_color').val());
		$('#drawingBoard').css("background-color", hex2rgb(color));
		battleMap.drawGrid();
	});
	$('#map_color').change();

	// Draw the editor 'pointer'
	//window.pointer =

});


