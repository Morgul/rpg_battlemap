var RPGB = Ember.Application.create({ });

RPGB.CELL_SIZE = 32;
RPGB.CELL_HALF_SIZE = 16;

/****************************************************
* A base object to build auto commiting objects upon.
****************************************************/

RPGB.RestObject = Ember.Object.extend({
	all: [],
	_lockedCommits: 0,
	_restProps: ['url'],

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
				this._restProps.push(prop);
			}
		});
		return obj;
	},

	safeSet: function(key, value){
		this.lockCommits();
		this.set(key, value);
		this.unlockCommits();
	},

	safeSetProperties: function(obj){
		this.lockCommits();
		this.setProperties(obj);
		this.unlockCommits();
	},

	commitProperties: function(){
		var dataObj = {};
		var i = 0;
		for(i; i < arguments.length; i++){
			dataObj[arguments[i]] = this.get(arguments[i]);
		}

		if(this.webSocket){
			this.webSocket.send(JSON.stringify({
				'type':this.type,
				'id':this.id,
				'action': 'update',
				'payload': dataObj
			}));
			return;
		}

		if(this.url){
			var deferred = $.ajax(this.url, {
				'contentType': 'application/json',
				'context': this,
				'data': JSON.stringify(dataObj),
				'dataType': 'application/json',
				'type': 'put'
			});
			return;
		}

		console.log('no remote connection set');
	},

	addRestfulProperty: function(propName){
		if(this._restProps.indexOf(propName) == -1){
			this._restProps.push(propName);
		}
	},

	removeRestfulProperty: function(propName){
		var ind = this._restProps.indexOf(propName);
		if(ind > -1){
			this._restProps.removeAt(ind);
		}
	},

	setUnknownProperty: function(key, value){
		var thisRef = this;
		this[key] = value;
		Ember.addObserver(thisRef, key, thisRef, 'propertyDidChange');
		this.propertyWillChange(this, key);
	},

	propertyDidChange: function(object, key){
		if(this._lockedCommits){
			return;
		}

		if(this._restProps.indexOf(key) == -1){
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
