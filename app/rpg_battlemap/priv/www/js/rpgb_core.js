var RPGB = Ember.Application.create({ });

/****************************************************
* A base object to build auto commiting objects upon.
****************************************************/

RPGB.RestObject = Ember.Object.extend({
	all: [],
	_lockedCommits: 0,

	find: function(){
		if(! allUrl){
			return this.all;
		}

		var thisRef = this;
		var defer = $.ajax(allUrl);
		defer.success(function(data){
			var dataObjs = data.map(function(item){
				return thisRef.prototype.create(itme);
			});
			while(this.all.length){
				thisRef.all.removeAt(0);
			}
			thisRef.all.addObjects(dataObjs);
			return true;
		});

		return this.all;
	},

	load: function(url){
		var obj = this.prototype.create({
			'url': url
		});

		var defer = $.ajax(url, {
			context: obj
		});
		defer.success(function(data){
			var prop;
			for(prop in data){
				this.set(prop, data[prop]);
			}
		});
		return obj;
	},

	setUnknownProperty: function(key, value){
		var thisRef = this;
		this[key] = value;
		Ember.addObserver(thisRef, key, thisRef, 'propertyDidChange');
		this.propertyWillChange(this, key);
	},

	propertyDidChange: function(object, key){
		console.log('prop did change', arguments);
		if(this._lockedCommits){
			return;
		}

		dataObj = {};
		dataObj[key] = object.get(key);

		if(this.webSocket){
			this.webSocket.send(JSON.stringify({
				'type':this.type,
				'id':this.id,
				action:'update',
				'payload': dataObj
			}));
			return;
		}
		
		if(this.url) {
			var deferred = $.ajax(this.url, {
				'contentType':'application/json',
				'context':this,
				data:JSON.stringify(dataObj),
				dataType: 'application/json',
				type:'put'
			});
			return;
		}

		console.log('no remote connection set', this);
	},

	lockCommits: function(){
		this._lockedCommits++;
	},

	unlockCommits: function(){
		if(this._lockedCommits){
			this._lockedCommits--;
		}
	}
});
