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
	.factory('MapSocket', ['$q', '$rootScope', function($q, $rootScope){

		var nextReplyToId = 0;
		var requests = {};
		var mapUrl = false;
		var ws = false;
		var listeners = {};

		var connectMap = function(mapObj){
			var connectingDefer = $q.defer();

			console.log('connecting to map ws', mapObj.websocketUrl, ws ? true : false);
			if(ws){
				ws.close();
			}

			mapUrl = mapObj.websocketUrl;
			ws = new WebSocket(mapUrl);

			ws.onopen = function(ev){
				console.log('resolving connection', $rootScope);
				$rootScope.$apply(connectingDefer.resolve(ev));
			};

			ws.onmessage = function(ev){
				onMessage(ev);
			};

			ws.onclose = function(ev){
				console.log('connection closing', connectingDefer);
				if(connectingDefer){
					$rootScope.$apply(connectingDefer.reject("connection closed"));
				}
			}

			return connectingDefer.promise;
		}

		var nextReplyId = function(){
			nextReplyToId++;
			if(nextReplyToId > 10000){
				nextReplyToId = 0;
			}
			return nextReplyToId;
		}

		var sendRequest = function(action, type, id, data){
			var request = {'action':action, 'type': type};
			if(id){
				request.id = id;
			}
			if(data){
				request.data = data;
			}
			request.reply_with = nextReplyId();

			var defer = $q.defer();
			requests[request.reply_with] = {
				'posted': new Date(),
				'defer': defer
			};
			if(! ws){
				defer.reject("no websocket connected");
			} else {
				ws.send(JSON.stringify(request));
			}
			console.log('the defer', defer);
			return defer.promise;
		}

		var onMessage = function(ev){
			var messageObj = JSON.parse(ev.data);
			console.log('messageObj', messageObj);
			if(messageObj.type == 'reply'){
				maybeReply(messageObj);
				return;
			}
			var eventName = messageObj.action + '_' + messageObj.type;
			if(messageObj.hasOwnProperty('type_id') && messageObj.action != 'delete'){
				eventName += '_' + messageObj.type_id;
			}
			var emitData = messageObj.action == 'delete' ? messageObj.type_id : messageObj.data;
			$rootScope.$apply($rootScope.$emit(eventName, emitData));
		};

		var maybeReply = function(reply){
			console.log('maybe reply', reply);
			if(! reply.type_id){
				return;
			}
			if(requests.hasOwnProperty(reply.type_id)){
				var defer = requests[reply.type_id].defer;
				if(reply.accepted){
					$rootScope.$apply(defer.resolve(reply.data));
				} else {
					$rootScope.$apply(defer.reject(reply.data));
				}
				delete requests[reply.type_id];
			}
		};

		var query = function(type){
			var outDefer = $q.defer();
			var q = sendRequest('get', type);
			q.then(function(success){
				success = success.map(function(obj){
					return attach(type, obj);
				});
				outDefer.resolve(success);
			},
			function(fail){
				outDefer.reject(fail);
			});
			return outDefer.promise;
		};

		var get = function(type, id){
			var outDefer = $q.defer();
			var q = sendRequest('get', type, id);
			q.then(function(success){
				attach(type, success);
				/*success.save = function(){
					return sendRequest('put', type, this.id, this);
				};
				success.delete = function(){
					return sendRequest('delete', type, this.id);
				};*/
				outDefer.resolve(success);
			},
			function(fail){
				outDefer.reject(fail);
			});
			return outDefer.promise;
		};

		var attach = function(type, obj){
			obj.$save = function(){
				return sendRequest('put', type, this.id, this);
			};
			obj.$delete = function(){
				return sendRequest('delete', type, this.id);
			};
			var eventName = 'put_' + type + '_' + obj.id;
			$rootScope.$on(eventName, function(ev, newObjParams){
				var k;
				for(k in newObjParams){
					if(newObjParams.hasOwnProperty(k)){
						obj[k] = newObjParams[k];
					}
				}
				console.log('put event', obj.id, ev, newObjParams);
			})
		};

		connectMap.sendRequest = sendRequest;
		connectMap.query = query;
		connectMap.get = get;
		connectMap.attach = attach;
		return connectMap;
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
