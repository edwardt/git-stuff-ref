// ---------------- //
// Log "Controller" //
// ---------------- //

var log_controller = function(app) {

  this.get('#/log', function(context) {
    this.get_page("/apps.json?token=" + Sammy.current_user["token"], 'apps');
  });

  this.get('#/log/:name', function(context) {
    name = this.params['name']
    this.get_page("/apps/" + name + "/bee_logs.json?token=" + Sammy.current_user["token"], 'bee_log');
    this.template = "log/show";
  });

};


