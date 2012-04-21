$().ready(function(){
	$('.grid').ColorPicker({
		color: '#0000ff',
		onShow: function (colpkr) {
			$(colpkr).fadeIn(500);
			return false;
		},
		onHide: function (colpkr) {
			hex = $('.colorpicker_hex input', colpkr).val();
			var hexString = "#" + hex;
			$('input[name="grid_color"]').val(hex2Color(hexString));
			$('.grid' + ' div').css('background-color', '#' + hex);

			$(colpkr).fadeOut(500);
			return false;
		},
		onChange: function (hsb, hex, rgb) {
			$('.grid' + ' div').css('background-color', '#' + hex);
		},
		onSubmit: function(hsb, hex, rgb, el) {
			$('.grid' + ' div').css('background-color', '#' + hex);
			var hexString = "#" + hex;
			$('input[name="grid_color"]').val(hex2Color(hexString));

			$(el).ColorPickerHide();
		},
		onBeforeShow: function () {
			var hex = color2Hex($('input[name="grid_color"]').val());

			if (hex) {
				$(this).ColorPickerSetColor(hex);
			}
		}
	});
	$('input[name="grid_color"]').change(function(){
		if ($(this).val()) {
			var hex = color2Hex($(this).val());
			$('.grid' + ' div').css('background-color', hex);
		}
	});

	$('.map').ColorPicker({
		color: '#0000ff',
		onShow: function (colpkr) {
			$(colpkr).fadeIn(500);
			return false;
		},
		onHide: function (colpkr) {
			hex = $('.colorpicker_hex input', colpkr).val();
			var hexString = "#" + hex;
			$('input[name="map_color"]').val(hex2Color(hexString));
			$('.map' + ' div').css('background-color', '#' + hex);

			$(colpkr).fadeOut(500);
			return false;
		},
		onChange: function (hsb, hex, rgb) {
			$('.map' + ' div').css('background-color', '#' + hex);
		},
		onSubmit: function(hsb, hex, rgb, el) {
			$('.map' + ' div').css('background-color', '#' + hex);
			var hexString = "#" + hex;
			$('input[name="map_color"]').val(hex2Color(hexString));

			$(el).ColorPickerHide();
		},
		onBeforeShow: function () {
			var hex = color2Hex($('input[name="map_color"]').val());

			if (hex) {
				$(this).ColorPickerSetColor(hex);
			}
		}
	});
	$('input[name="map_color"]').change(function(){
		if ($(this).val()) {
			var hex = color2Hex($(this).val());
			$('.map' + ' div').css('background-color', hex);
		}
	});
});


