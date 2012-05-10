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
				results.map(function(mapInfo){
					var localDiv = $('div[mapName="' + mapInfo.name + '"]');
					if(localDiv.length){
						localDiv.battlemapLoadButton("option","mapInfo",mapInfo);
						return true;
					}

					theThis._addMapItem(mapInfo, true);
					return true;
				});
				var localDivs = $('div[mapName]', theThis.element);
				
				console.log(results);
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

		_addMapItem: function(mapItem, supressLocalLoad){
			var theThis = this;
			console.log(mapItem, this, this._ulElement, this.element, self);

			var mapDiv = $('<div></div>').attr({
				'mapUrl':mapItem.url,
				'mapEtag':mapItem.etag,
				'mapName':mapItem.name
			}).appendTo(this._ulElement);

			$(mapDiv).battlemapLoadButton({
				'mapInfo': mapItem,
				'supressLocalLoad': supressLocalLoad
			});
		}
	});
})(jQuery);

// a set of 3 buttons for remote load, load, and delete.
(function($){
	$.widget("ui.battlemapLoadButton", {

		options: {
			mapInfo: {},
			suppressLocalLoad: false,
			load: function(){
				console.log('load', arguments);
			}
		},

		_create: function(){
			var theThis = this;
			this._remoteLoadButton = this._makeRemoteLoadButton();
			this._localLoadButton = this._makeLocalLoadButton();
			this._deleteButton = this._makeDeleteButton();
			if(!this.options.mapInfo.url){
				this._remoteLoadButton.attr('disabled','disabled');
			}
			if(this.options.suppressLocalLoad){
				this._localLoadButton.attr('disabled','disabled');
			}
		},

		_setOption: function(option, value){
			$.Widget.prototype._setOption.apply(this, arguments);
			switch(option){
				case "mapInfo":
					if(value.url){
						this._remoteLoadButton.removeAttr('disabled');
					} else {
						this._remoteLoadButotn.attr('disabled', 'disabled');
					}
					break;
				case "supressLocalLoad":
					if(value){
						this._localLoadButton.attr('disabled','disabled');
					} else {
						this._localLoadButotn.removeAttr('disabled');
					}
					break;
			}
		},

		_makeRemoteLoadButton: function(){
			var theThis = this;
			return $('<button>&nbsp;</button>').
				addClass('ui-battlemapLocker-pull').
				click(function(){
					if(! theThis.options.mapInfo.url){
						return false;
					}
					BattleMap.loadRemote(theThis.options.mapInfo.url).done($.proxy(function(results){
						this.options.load(results);
					}, theThis));
					return false;
				}).
				appendTo(this.element).
				button({'text':false,'icons':{'primary':'ui-icon-arrowthick-1-s'}});
		},

		_makeLocalLoadButton: function(){
			var theThis = this;
			return $('<button>' + this.options.mapInfo.name + '</button>').
				addClass('ui-battlemapLocker-localload').
				click(function(){
					if(theThis.options.suppressLocal){
						return false;
					}
					var mapObj = BattleMap.loadLocal(theThis.options.mapInfo.name);
					theThis.options.load(mapObj);
				}).
				appendTo(this.element).
				button();
		},

		_makeDeleteButton: function(){
			var theThis = this;
			return $('<button>&nbsp</button>').
				addClass('ui-battlemapLocker-delete').
				click(function(){
					if(theThis.options.mapInfo.url){
						BattleMap.deleteRemote(theThis.options.mapInfo.url).done($.proxy(function(){
							BattleMap.deleteLocal(this.options.mapInfo.name);
							theThis.remove();
						}, theThis));
						return false;
					}
					BattleMap.deleteLocal(theThis.options.mapInfo.name);
					theThis.remove();
					return false;
				}).
				appendTo(this.element).
				button({'text':false,'icons':{'primary':'ui-icon-closethick'}});
		},

		remove: function(){
			this._remoteLoadButton.remove();
			this._localLoadButton.remove();
			this._deleteButton.remove();
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
				
			this._remoteSaveButton = $(this.element).
				click(function(){
					if(theThis.options.battlemap){
						theThis.options.battlemap.name = theThis._nameEditor.attr('value');;
						theThis.options.battlemap.saveRemote();
					}
					return false;
				}).
				addClass('ui-battlemap-savebutton-remote').
				button({'text':false,'icons':{'primary':'ui-icon-arrowthick-1-n'}});

			this._nameEditor = $('<input class="ui-battlemap-savebutton-name" />').
				insertAfter(this.element);

			if(this.options.battlemap){
				this._nameEditor.attr('value', this.options.battlemap.name);
			}

			this._localSaveButton = $('<button>&nbsp;</button>').
				click(function(){
					if(theThis.options.battlemap){
						theThis.options.battlemap.name = theThis._nameEditor.attr('value');;
						theThis.options.battlemap.saveLocal();
					}
					return false;
				}).
				addClass('ui-battlemap-savebutton-local').
				insertAfter(this._nameEditor).
				button({'text':false,'icons':{'primary':'ui-icon-disk'}});
		},

		_setOption: function(option, value){
			$.Widget.prototype._setOption.apply(this, arguments);
			switch(option){
				case "battlemap":
					if(value){
						this._nameEditor.attr('value', value.name);
					} else {
						this._nameEditor.attr('value', '');
					}
					break;
			}
		},

		remove: function(){
			$(this.element).children.remove();
		}
	});

})(jQuery);
