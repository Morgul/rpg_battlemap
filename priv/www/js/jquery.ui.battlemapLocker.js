(function($){
	$.widget("ui.battlemapLocker", {

		options: {
			loadCallback: function(){
				console.log('load complete', arguments);
			},
			saveCallback: function(){
				console.log('save complete', arguments);
			}
		},

		_create: function(){
			var self = this;
			self._ulElement = $('<div></div>').addClass("ui-widget ui-battlemapLocker").appendTo(self.element);
			var localList = BattleMap.listLocal();
			localList.map(function(mapData){
				self._addMapItem(mapData);
			});
			BattleMap.listRemote().done($.proxy(function(results){
				console.log('hi', self);
			}, self));
		},

		destroy: function(){
			$(this._ulElement).remove();
		},

		_addMapItem: function(mapItem){
			console.log(mapItem, this, this._ulElement, this.element, self);
			$('<div>' + mapItem.name + '</div>').attr({
				'mapUrl':mapItem.url,
				'mapEtag':mapItem.etag
			}).appendTo(this._ulElement);
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
