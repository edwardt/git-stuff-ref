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

  this.get("#/account", function(context) {
    var email = Sammy.current_user["user"];
    this.get_page("/users/" + email + ".json", "user");
    this.get_page("/users/" + email + "/apps.json", "apps");
    this.template = "users/show";

    // Either we need this dumb thing, or I don't get Sammy/magic js haml
    this.error = false; 
  });

  this.post("#/account/pubkeys", function(context) {
    var email = Sammy.current_user["user"];
    this.post_page("/users/"+email+"/pubkeys.json",
                   {"pubkey":this.params["pubkey"],
                    "token" :Sammy.current_user["token"]},
                   context, "user");

    if(context['error']) {
      this.error = true;
      this.template = "users/show";
    } else {
      this.redirect('#/account');
    }

  });

  this.post("#/account/pubkeys", function(context) {
    var email = Sammy.current_user["user"];
    this.post_page("/users/"+email+"/pubkeys.json",
                   {"pubkey":params["pubkey"]},
                   context, "user");


  });

}
