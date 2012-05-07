// a List that can list, load, and delete battlemaps locally and remotely.

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
			var topDiv = $('<div></div>').addClass("ui-widget ui-battlemapLocker").appendTo(theThis.element);
			theThis._ulElement = topDiv;
			var localList = BattleMap.listLocal();
			localList.map(function(mapData){
				theThis._addMapItem(mapData);
			});
			BattleMap.listRemote().done($.proxy(function(results){
				results.map(function(remMapInfo){
					$('div[mapName="' + remMapInfo.name + '"]', theThis._ulElement, topDiv).each(function(ind, elem){
						if($(elem).attr('mapUrl')){
							return true;
						}
						$(elem).attr('mapUrl',remMapInfo.url);
						$('<span></span>').
							addClass('ui-battlemapLocker-pull ui-icon ui-icon-arrowthick-1-s').
							click(function(){
								BattleMap.loadRemote(remMapInfo.url).done($.proxy(function(){
									theThis.options.load.apply(theThis, arguments);
								}));
								return false;
							}).
							attr('title', 'Load the map using the remote data rather than local data').
							prependTo($(elem));
					});
				});
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
				'mapEtag':mapItem.etag,
				'mapName':mapItem.name
			}).appendTo(this._ulElement);

			if(mapItem.url){
				$('<span></span>').
					addClass('ui-battlemapLocker-pull ui-icon ui-icon-arrowthick-1-s').
					click(function(){
						BattleMap.loadRemote(mapItem.url).done($.proxy(function(){
							theThis.options.load.apply(theThis, arguments);
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

// a button to conveiently save a battlemap both locally and remotely.
(function($){

	$.widget("ui.battlemapSaveButton", {

		options: {
			battlemap: false,
		},

		_create: function(){
			var theThis = this;
			$(this.element).
				click(function(){
					if(theThis.options.battlemap){
						theThis.options.battlemap.saveLocal();
					}
					return false;
				}).
				addClass('ui-battlemap-savebutton-local').
				button();

			$('<button>&nbsp;</button>').
				click(function(){
					if(theThis.options.battlemap){
						theThis.options.battlemap.saveRemote();
					}
					return false;
				}).
				addClass('ui-battlemap-savebutton-remote').
				insertAfter(this.element).
				button({'text':false,'icons':{'primary':'ui-icon-arrowthick-1-n'}})
		},

		remove: function(){
			$(this.element).children.remove();
		},
	});

})(jQuery);
