Configuration
===

The config file currently expects a yaml format. These will be converted to
a proplist of erlang tuples.

Beehive searches for config files at ~/.beehive.conf and
/etc/beehive.conf as well.

## Valid Config Variables

### home

Sets the base directory for the running beehive instance, which will
include the DB, running apps and log files.  The default value is
/var/lib/beehive .

### code_root

Used by git post-receive hooks to determine how to call restart-app
scripts.  Git hooks won't work properly without it.

### routing_param

Defaults to 'Host', which means that the router will use the first
subdomain of the given url to handle app routing.

Alternate value, provided by routing_param is 'subdirectory',
which will instead expect app names to appear as the first component
of the path in the url.  http://domain.com/<app-name>/path

### dashboard_port

Defaults to 4999.  Port that the dashboard app will be listening on.

### client_port

Defaults to 8080.  Port that the router will be listening on.

### domain

Defaults to `hostname -f`.  Used by router to parse out subodomains
for app picking.

### bee_strategy

bee_strategy is used to determine which node to start a new bee on.
Currently defaults to 'random'.

### debug
If debug=true, beehive will increase logging output in console/log files.


## Environment Variables

Beehive will use these values if they're found in the
environment. Beehive config expects that env variables will be upper
case and start with BEEHIVE_, so the 'home' config key will be
overridden by a BEEHIVE_HOME env variable.

