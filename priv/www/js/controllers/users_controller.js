//------------------//
// Users Controller //
//------------------//

var users_controller = function(app) {

  this.get("#/users", function(context) {
    this.get_page("/users.json", "users");
    this.users_list = [];
    for(name in this.users) {
      var user = {name: name, level: this.users[name].level};
      this.users_list.push(user);
    }
  });

  this.get("#/profile", function(context) {
    var email = Sammy.current_user["user"];
    this.get_page("/users/" + email + ".json", "user");
    this.get_page("/users/" + email + "/apps.json", "apps");
    this.template = "users/show";
    console.log(this.user);
    console.log(this.apps);
  });

}
