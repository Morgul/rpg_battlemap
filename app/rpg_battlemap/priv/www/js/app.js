//----------------------------------------------------------------------------------------------------------------------
// Main Angular application.
//
// @module app.js
//----------------------------------------------------------------------------------------------------------------------

angular.module("battlemap", ['ngResource', 'battlemap.controllers'])
	.config(['$locationProvider', function($locationProvider) {
		$locationProvider.html5Mode(true);
	}])
	.config(['$routeProvider', function($routeProvider) {
		$routeProvider
			.when('/', {templateUrl: '/partials/list_maps.html',   controller: 'ListMapsCtrl'})
			.when('/maps/:mapid', {templateUrl: '/partials/map.html',   controller: 'ViewMapCtrl'})
			.when('/maps/:mapid/edit', {templateUrl: '/partials/map.html',   controller: 'EditMapCtrl'})
			.otherwise({redirectTo: '/'});
	}])
	//FIXME: Remove this; it's only for demo
	.run(function($rootScope){
		$rootScope.user = window.currentUser;
		$rootScope.loginUrl = window.loginUrl;
		$rootScope.logoutUrl = window.logoutUrl;
		// the resource thing doesn't really do hateaos well, but then again
		// neither does the browser. ah well.
		/*var Map = $resource('/maps/:mapid', {}, {'save': {'method':'PUT'}, 'create':{'method':'POST'}, 'query':{'method':'GET', 'isArray':true, 'params':{'mapid':''}}});
		var thing = Map.query(function(maps){
			console.log('der maps', maps);
		});
		console.log('thing?', thing);*/
		/*$rootScope.maps = [
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
			];*/
	});

//----------------------------------------------------------------------------------------------------------------------
