// ------------------- //
// Events "Controller" //
// ------------------- //

var events_controller = function(app) {

  this.get('#/events', function(context) {
    this.get_page("/events.json", context, 'events');
  	context.app.swap('');
  	this.partial('haml/events.haml');
  });

};