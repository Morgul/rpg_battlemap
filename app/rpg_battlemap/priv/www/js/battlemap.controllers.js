//----------------------------------------------------------------------------------------------------------------------
// Main page controllers for Battlemap
//
// @module battlemap.controllers.js
//----------------------------------------------------------------------------------------------------------------------

Controllers = angular.module("battlemap.controllers", []);

//----------------------------------------------------------------------------------------------------------------------

Controllers.controller("ListMapsCtrl", function($scope, $rootScope) {
	//FIXME: Only for demo
	$scope.maps = $rootScope.maps;

	// Disable the toolbar from the main nav bar.
	$scope.noToolbar = true;

	// Get the maps we're participating in.
	$scope.getParticipating = function(maps) {
		var partList = [];
		maps.forEach(function(map){

			//FIXME: We hardcode a user id of '1' for the demo.
			if(map.participant_ids.indexOf(1) >= 0) {
				partList.push(map);
			}
		});

		return partList;
	}
});

Controllers.controller("ViewMapCtrl", function($scope, $routeParams, $rootScope) {
	//FIXME: For demo, only
	$scope.map = $rootScope.maps[0];

	$scope.buttons = [
		{ name: 'Combatants', menu: [] },
		{ name: 'Zones & Auras', menu: [] },
		{ name: 'Edit Map', icons: ['icon-edit'], url:"#/map/" + $routeParams.mapid + '/edit' }
	];

	$scope.tools = [
		{ name: 'Normal', icon: 'icon-hand-up' },
		{ name: 'Measure', icon: 'icon-map-marker' }
	];
});

Controllers.controller("EditMapCtrl", function($scope, $rootScope) {
	//FIXME: For demo, only
	$scope.map = $rootScope.maps[0];

	$scope.buttons = [
		{ name: 'Layers', icon: 'icon-map-marker', menu: [] },
		{ name: 'Zones & Auras', icon: 'icon-map-marker', menu: [] },
		{ name: 'Combatants', icon: 'icon-hand-up', menu: [] },
		{ name: 'New Combatant', icons: ['icon-plus icon-small', 'icon-user'], class: "btn-primary" }
	];

	$scope.tools = [
		{ name: 'Pan', icon: 'icon-fullscreen' }
	];
});

//----------------------------------------------------------------------------------------------------------------------

Controllers.controller("HeaderCtrl", function($scope) {
	$scope.topBarToggle = 'open';

	// Handles the case of the collapse button being clicked, so we can pick up on the fact that we need to change the
	// icon. Seems the best way to handle this.
	$scope.handleToggle = function() {
		$scope.topBarToggle = $scope.getCollapseState();
	};

	// Gets the current state of the top bar; either open, or closed.
	$scope.getCollapseState = function() {
		var topBar = angular.element("#top-bar");

		if(!topBar.hasClass('in')) {
			return 'open';
		} else {
			return 'closed';
		}
	}
});

Controllers.controller("GridCtrl", function($scope) {
	var header = angular.element("#main-header");
	var topBar = $("#top-bar");
	var docElem = angular.element(window);

	function calcGridheight(buffer) {
		buffer = buffer || 0;
		var height = docElem.height() - buffer;
		return height;
	}

	// Catch window resize events
	docElem.bind('resize', function(){
		$scope.$apply(function(){
			var buffer = header.height() + 15;
			$scope.gridHeight = calcGridheight(buffer);
		});
	});

	// Catch header shown events.
	topBar.on('hidden', function(){
		$scope.$apply(function(){
			var buffer = header.height() + 15;
			$scope.gridHeight = calcGridheight(buffer);
		});
	});

	// Catch header shown events.
	topBar.on('shown', function(){
		$scope.$apply(function(){
			var buffer = header.height() + 15;
			$scope.gridHeight = calcGridheight(buffer);
		});
	});

	$scope.gridHeight = calcGridheight(header.height() + 15);
});

//----------------------------------------------------------------------------------------------------------------------
