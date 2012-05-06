(function($){
	$.widget("ui.battlemapLocker", {

		options: {
			load: function(){
				console.log('load complete', arguments);
			},
			save: function(){
				console.log('save complete', arguments);
			}
		},

		_create: function(){
			var theThis = this;
			theThis._ulElement = $('<div></div>').addClass("ui-widget ui-battlemapLocker").appendTo(theThis.element);
			var localList = BattleMap.listLocal();
			localList.map(function(mapData){
				theThis._addMapItem(mapData);
			});
			BattleMap.listRemote().done($.proxy(function(results){
				console.log('hi', theThis);
			}, theThis));
		},

		destroy: function(){
			$(this._ulElement).remove();
		},

		refresh: function(){
			$(this._ulElement).children.remove();
			var localList = BattleMap.listLocal();
			var theThis = this;
			localList.map(function(mapData){
				theThis._addMapItem(mapData);
			});
			BattleMap.listRemote().done($.proxy(function(results){
				console.log('hi', theThis);
			}, theThis));
		},

		_addMapItem: function(mapItem){
			var theThis = this;
			console.log(mapItem, this, this._ulElement, this.element, self);

			var mapDiv = $('<div></div>').attr({
				'mapUrl':mapItem.url,
				'mapEtag':mapItem.etag
			}).appendTo(this._ulElement);

			if(mapItem.url){
				$('<span></span>').
					addClass('ui-battlemapLocker-pull ui-icon ui-icon-arrowthick-1-s').
					click(function(){
						BattleMap.loadRemote(mapItem.url).done($.proxy(function(){
							theThis.load.apply(theThis, arguments);
						}));
						return false;
					}).
					attr('title', 'Load the map using the remote data rather than local data').
					prependTo(mapDiv);
			}

			$('<span></span>').
				addClass('ui-icon ui-icon-closethick ui-battlemapLocker-delete').
				click(function(){
					if(mapItem.url){
						BattleMap.deleteRemote(mapItem.url).done(function(){
							BattleMap.deleteLocal(mapItem.name);
							mapDiv.remove();
						});
						return false;
					}
					BattleMap.deleteLocal(mapItem.name);
					mapDiv.remove();
					return false;
				}).
				attr('title','delete the map from both local and remote').
				appendTo(mapDiv);

			$(mapDiv).append(mapItem.name);
		}
	});
})(jQuery);

// load (from remote if available);
// save local
// push to remote
// pull from remote
// determine if there's a difference between remote and local, which 
// sets if the 'remote' is a push or pull
// delete (local and remote)
