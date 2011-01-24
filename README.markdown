Beehive
===

## What is Beehive?

  * Distributed router
  * Application deployment tool
  * Written in erlang, c and ruby
  * Scalable
  * Application load-balancer
  * Template-able
  * Configurable
  * And much more

## Dependencies ##

   * Erlang  - Development has been happening on R14B also runs on R14A and R13B
   * Ruby    - 1.8.7 or 1.9.2
   * rubygems- rack, thin (for tests and prodcution)
   * rubygems- isolate, bundler (if any prod apps depend upon them)
   * git     - 1.7.x

## Get Started ##

    ./start_dev.sh

This will start beehive on your local machine with the root
/tmp/beehive. If you want to use a different directory, (such as
something less transient like ~/beehive_data) run:

    export BEEHIVE_HOME=~/beehive_data

When starting beehive in a non-dev mode, the beehive root will default
to /var/lib/beehive.

## Running tests ##

     make test

**Make sure you have all dependencies installed.** Various tests
depend up them, and tests likely won't complete properly without them.
You'll get some especially confusing output if you're missing thin and
rack.

How it works
===

The incredibly basic architecture diagram of beehive looks like:

    Distributed Routing layer
    ----------------------------
      |         |         |    
    Backend   Backend   Backend
      |         |         |    
    Storage   Storage   Storage
    ----------------------------

The distributed routing layer, written in erlang uses
[Mnesia](http://ftp.sunet.se/pub//lang/erlang/doc/apps/mnesia/index.html),
the distributed database management system intelligently routes
requests across the bees. The router currently can handle http
requests. Because Beehive was written with the intention of being
extensible it can be extensible to other protocols.

It handles pending connections seamlessly and allows for streaming
connections. It also keeps track of statistical data available through
a web interface. The router itself has a RESTful interface for adding
bees, which don't even need to sit inside the Beehive network. This
can be useful for putting the router in front of a personal cluster
(such as [Eucalyptus](http://www.eucalyptus.com/)) and expanding to
the cloud environment (such as [EC2](http://aws.amazon.com/ec2/))
without having to change a line of code.

Beehive keeps track of the available bees for the known applications. 

Beehive has an event system that allows for notifications along the
system in a nonblocking manner. This way system events, statistic
gathering log events can all be handled without affecting the
performance of the router, which is tuned for speed.

---

For more information, see [more docs](http://github.com/auser/beehive/tree/master/docs)

---

## Thanks

  * Mad props for the super smart folks at [Heroku](http://heroku.com) for their sweet architecture ideas and for providing such a rad interface and an unmatched user experience.
  * [Daniel Fischer](http://www.danielfischer.com/) for his web design help
  * AT&T for their support of the project
  * Beehive is a part of the [Poolparty](http://poolpartyrb.com) project suite.

### Note on Patches/Pull Requests
 
  * Fork the project.
  * Make your feature addition or bug fix.
  * Add tests for it. This is important so I don't break it in a
    future version unintentionally.
  * Commit, do not mess with rakefile, version, or history.
    (if you want to have your own version, that is fine but
     bump version in a commit by itself I can ignore when I pull)
  * Send me a pull request. Bonus points for topic branches.

### Copyright

Copyright (c) 2010 Ari Lerner. See LICENSE for details.
