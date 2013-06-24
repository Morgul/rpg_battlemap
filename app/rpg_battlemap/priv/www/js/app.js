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
	.run(function($rootScope, $resource){
		$rootScope.user = window.currentUser;
		$rootScope.loginUrl = window.loginUrl;
		$rootScope.logoutUrl = window.logoutUrl;

		$rootScope.Map = $resource('/maps/:mapid', {}, {
		'save': {'method':'PUT'},
		'create':{'method':'POST', },
		'query':{'method':'GET', 'isArray':true, 'params':{'mapid':''}}});

		$rootScope.stopPropagation = function(ev){
			ev.stopPropagation();
		}

	});

//----------------------------------------------------------------------------------------------------------------------
