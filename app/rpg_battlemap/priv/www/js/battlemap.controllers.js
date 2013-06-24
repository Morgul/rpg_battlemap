//----------------------------------------------------------------------------------------------------------------------
// Main page controllers for Battlemap
//
// @module battlemap.controllers.js
//----------------------------------------------------------------------------------------------------------------------

Controllers = angular.module("battlemap.controllers", []);

//----------------------------------------------------------------------------------------------------------------------

Controllers.controller("PersonaCtrl", function($scope, $rootScope){

	// why yes, pulling this from the window is a horrible idea.
	$scope.signin = function(){
		navigator.id.request();
	}

	$scope.signout = function(){
		navigator.id.logout();
	}

	$scope.user = window.currentUser;
	$scope.loginUrl = window.loginUrl;
	$scope.logoutUrl = window.logoutUrl;

	navigator.id.watch({
		loggedInUser: $scope.user,
		onlogin: function(assertion){
			console.log('onlogin', $scope.user);
			$.ajax({
				type: 'POST',
				url: $scope.loginUrl,
				data: JSON.stringify({assertion: assertion}),
				contentType: 'application/json',
				success: function(res, status, xhr){ window.location.reload(); },
				error: function(xhr, status, err){ console.log('login fail', err); }
			});
		},
		onlogout: function(){
			console.log('onlogout', $scope.currentUser);
			$.ajax({
				type: 'POST',
				url: $scope.logoutUrl,
				contentType: 'application/json',
				success: function(res, status, xhr){ window.location.reload(); },
				error: function(xhr, status, err){ console.log('logout fail', err); }
			});
		}

	});

});


Controllers.controller("ListMapsCtrl", function($scope, $rootScope, $resource) {
		// the resource thing doesn't really do hateaos well, but then again
		// neither does the browser. ah well.
	$rootScope.maps = [];

	var mapsPromise = $rootScope.Map.query();
	mapsPromise.$then(function(success){
		$rootScope.maps = success.data;
	}, function(error){
		console.error('failed to get maps', error);
	});

	// Disable the toolbar from the main nav bar.
	$scope.noToolbar = true;

	// Get the maps we're participating in.
	$scope.getParticipating = function(maps) {
		return maps;
	};

	$scope.createMap = function(){
		var mapPromise = Map.create({name: $scope.newMapName});
		mapPromise.$then(function(success){
			$rootScope.maps.push(success.data);
			$scope.newMapName = '';
		},
		function(error){
			console.error('could not make map', error);
			$scope.newMapName = '';
		})
	};

	$scope.deleteMap = function(map){
		var delPromise = Map.delete({mapid: map.id});
		delPromise.$then(function(success){
			var index = $rootScope.maps.indexOf(map);
			if(index < 0) {
				return;
			}
			$rootScope.maps.splice(index, 1);
		})
	};

});

Controllers.controller("ViewMapCtrl", function($scope, $routeParams, $rootScope, $resource, MapSocket) {
	$scope.map = {};

	var mapPromise = $rootScope.Map.get($routeParams);
	mapPromise.$then(function (success) {
		console.log('map data gotten');
		$scope.map = success.data;

		var connectDefer = MapSocket($scope.map);
		connectDefer.then(function(success){
			console.log('connect defer');
			var socketPromise = MapSocket.get('map', parseInt($routeParams.mapid, 10));
			socketPromise.then(function(success){
				console.log('der success', success);
				$scope.map = success;
			},
			function(fail){
				console.log('failed to get map', fail);
			})
		},
		function(fail){
			console.error("websocket failed to connect", fail);
		})
	},
	function(error){
		console.error('some error', error);
	});

	$scope.buttons = [
		{ name: 'Combatants', menu: [] },
		{ name: 'Zones & Auras', menu: [] },
	];

	$scope.tools = [
		{ name: 'Normal', icon: 'icon-hand-up' },
		{ name: 'Measure', icon: 'icon-map-marker' }
	];

	$scope.mapBackgroundCssObject = function(){
		var out = {'backgroundColor': $scope.map.background_color};
		return out;
	}
});

Controllers.controller("EditMapCtrl", function($scope, $rootScope) {
	//FIXME: For demo, only
	$scope.map = ($rootScope.maps && $rootScope.maps[0]) || {};

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
	};

	$scope.logit = function(){
		console.log('der thangs', arguments);
	}
});

Controllers.controller("GridCtrl", function($scope, $rootScope, MapSocket) {
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
