// Peronsa support

$().ready(function(){
	$('#signinlink').click(function(){
		navigator.id.request();
	});

	$('#signoutlink').click(function(){
		navigator.id.logout();
	});

	navigator.id.watch({
		loggedInUser: currentUser,
		onlogin: function(assertion){
			console.log('onlogin', currentUser);
			$.ajax({
				type: 'POST',
				url: loginUrl,
				data: JSON.stringify({assertion: assertion}),
				contentType: 'application/json',
				success: function(res, status, xhr){ window.location.reload(); },
				error: function(xhr, status, err){ console.log('login fail', err); }
			});
		},
		onlogout: function(){
			console.log('onlogout', currentUser);
			$.ajax({
				type: 'POST',
				url: logoutUrl,
				success: function(res, status, xhr){ window.location.reload(); },
				error: function(xhr, status, err){ console.log('logout fail', err); }
			});
		}
	});
});



//----------------------------------------------------------------------------------------------------------------------
// Main Angular application.
//
// @module app.js
//----------------------------------------------------------------------------------------------------------------------

angular.module("battlemap", ['ngResource', 'battlemap.controllers'])
	.config(['$routeProvider', function($routeProvider) {
		$routeProvider
			.when('/', {templateUrl: '/partials/list_maps.html',   controller: 'ListMapsCtrl'})
			.when('/map/:mapid', {templateUrl: '/partials/map.html',   controller: 'ViewMapCtrl'})
			.when('/map/:mapid/edit', {templateUrl: '/partials/map.html',   controller: 'EditMapCtrl'})
			.otherwise({redirectTo: '/'});
	}])
	//FIXME: Remove this; it's only for demo
	.run(function($rootScope){
		$rootScope.maps = [
				{
					id: 1,
					name: "Test Map",
					background_color: "#007500",
					grid_opacity: 0.9,
					grid_spacing: 32,
					gridline_color: "black",
					combatants: [],
					combatants_url: "https://localhost:9090/map/2/combatants",
					layers: [],
					layers_url: "https://localhost:9090/map/2/layers",
					owner_id: 1,
					participant_ids: [1, 2],
					rating: "g",
					translate_x: 0,
					translate_y: 0,
					url: "https://localhost:9090/map/2",
					websocketUrl: "wss://localhost:9090/map/2",
					zoom: 1
				},
				{
					id: 2,
					name: "Test Map 2",
					background_color: "#007500",
					grid_opacity: 0.9,
					grid_spacing: 32,
					gridline_color: "black",
					combatants: [],
					combatants_url: "https://localhost:9090/map/2/combatants",
					layers: [],
					layers_url: "https://localhost:9090/map/2/layers",
					owner_id: 1,
					participant_ids: [1, 2],
					rating: "g",
					translate_x: 0,
					translate_y: 0,
					url: "https://localhost:9090/map/2",
					websocketUrl: "wss://localhost:9090/map/2",
					zoom: 1
				},
				{
					id: 3,
					name: "Test Map 3",
					background_color: "#007500",
					grid_opacity: 0.9,
					grid_spacing: 32,
					gridline_color: "black",
					combatants: [],
					combatants_url: "https://localhost:9090/map/2/combatants",
					layers: [],
					layers_url: "https://localhost:9090/map/2/layers",
					owner_id: 2,
					participant_ids: [1, 2],
					rating: "g",
					translate_x: 0,
					translate_y: 0,
					url: "https://localhost:9090/map/2",
					websocketUrl: "wss://localhost:9090/map/2",
					zoom: 1
				}
			];
	});

//----------------------------------------------------------------------------------------------------------------------
