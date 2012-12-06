/****************************************************
* Bind an attribute, but set the view using setAttribute
****************************************************/

Ember.Handlebars.registerHelper('bindXMLAttr', function(options) {
	var fmt = Ember.String.fmt;
	var attrs = options.hash;

	Ember.assert("Specify at least one hash arguments to bindXMLAttra", !!Ember.keys(attrs).length);

	var view = options.data.view;
	var ret = [];
	var ctx = this;

  // Generate a unique id for this element. This will be added as a
  // data attribute to the element so it can be looked up when
  // the bound property changes.
  var dataId = ++jQuery.uuid;

  var attrKeys = Ember.keys(attrs);
  // For each attribute passed, create an observer and emit the
  // current value of the property as an attribute.
  attrKeys.forEach(function(attr) {
    var property = attrs[attr];

    Ember.assert(fmt("You must provide a String for a bound attribute, not %@", [property]), typeof property === 'string');

    var value = Em.get(ctx, property);

    Ember.assert(fmt("Attributes must be numbers, strings or booleans, not %@", [value]), value == null || typeof value === 'number' || typeof value === 'string' || typeof value === 'boolean');

    var observer, invoker;

    observer = function observer() {
      var result = Em.get(ctx, property);

      Ember.assert(fmt("Attributes must be numbers, strings or booleans, not %@", [result]), result == null || typeof result === 'number' || typeof result === 'string' || typeof result === 'boolean');

      var elem = view.$("[data-bindAttr-" + dataId + "='" + dataId + "']");

      // If we aren't able to find the element, it means the element
      // to which we were bound has been removed from the view.
      // In that case, we can assume the template has been re-rendered
      // and we need to clean up the observer.
      if (elem.length === 0) {
        Ember.removeObserver(ctx, property, invoker);
        return;
      }

			elem = elem[0];

      var currentValue = elem.getAttribute(attr);

      if (currentValue !== result) {
				elem.setAttribute(attr, result);
      }
    };

    invoker = function() {
      Ember.run.once(observer);
    };

    // Add an observer to the view for when the property changes.
    // When the observer fires, find the element using the
    // unique data id and update the attribute to the new value.
    Ember.addObserver(ctx, property, invoker);

    // Use the attribute's name as the value when it is YES
    if (value === true) {
      value = attr;
    }

    // Do not add the attribute when the value is false
    if (value !== false) {
			if(typeof value === 'number'){
      	ret.push(attr+'="'+value+'"');
			} else {
      	ret.push(attr+'="'+value+'"');
			}
    }
  }, this);

  // Add the unique identifier
  ret.push('data-bindAttr-' + dataId + '="' + dataId + '"');
  return new Ember.Handlebars.SafeString(ret.join(' '));
});

/****************************************************
* Bind an attribute to a specific style tag
****************************************************/

Ember.Handlebars.registerHelper('bindStyle', function(options) {
  var fmt = Ember.String.fmt;
  var attrs = options.hash;

  Ember.assert("You must specify at least one hash argument to bindStyle", !!Ember.keys(attrs).length);

  var view = options.data.view;
  var ret = [];
  var ctx = this;

  // Generate a unique id for this element. This will be added as a
  // data attribute to the element so it can be looked up when
  // the bound property changes.
  var dataId = ++jQuery.uuid;

  var attrKeys = Ember.keys(attrs);

  // For each attribute passed, create an observer and emit the
  // current value of the property as an attribute.
  attrKeys.forEach(function(attr) {
    var property = attrs[attr];

    Ember.assert(fmt("You must provide a String for a bound attribute, not %@", [property]), typeof property === 'string');

    var value = Em.get(ctx, property);

    Ember.assert(fmt("Attributes must be numbers, strings or booleans, not %@", [value]), value == null || typeof value === 'number' || typeof value === 'string' || typeof value === 'boolean');

    var observer, invoker;

    observer = function observer() {
      var result = Em.get(ctx, property);

      Ember.assert(fmt("Attributes must be numbers, strings or booleans, not %@", [result]), result == null || typeof result === 'number' || typeof result === 'string' || typeof result === 'boolean');

      var elem = view.$("[data-bindAttr-" + dataId + "='" + dataId + "']");

      // If we aren't able to find the element, it means the element
      // to which we were bound has been removed from the view.
      // In that case, we can assume the template has been re-rendered
      // and we need to clean up the observer.
      if (elem.length === 0) {
        Ember.removeObserver(ctx, property, invoker);
        return;
      }

      var currentValue = elem.css(attr);

      if (currentValue !== result) {
        elem.css(attr, result);
      }
    };

    invoker = function() {
      Ember.run.once(observer);
    };

    // Add an observer to the view for when the property changes.
    // When the observer fires, find the element using the
    // unique data id and update the attribute to the new value.
    Ember.addObserver(ctx, property, invoker);

    // Use the attribute's name as the value when it is YES
    if (value === true) {
      value = attr;
    }

    // Do not add the attribute when the value is false
    if (value !== false) {
			if(typeof value === 'number'){
      	ret.push('style="'+attr+':'+value+'px;"');
			} else {
      	ret.push('style="'+attr+':'+value+';"');
			}
    }
  }, this);

  // Add the unique identifier
  ret.push('data-bindAttr-' + dataId + '="' + dataId + '"');
  return new Ember.Handlebars.SafeString(ret.join(' '));
});
