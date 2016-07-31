maurs
=====

AUR package helper written in Erlang. This development repo is now deprecated; I originally thought that using Erlang for this project would be an awesome choice, and it was. However, I disliked the way it interfaces to the OS and ultimately deemed it a poor choice and the VM ultimately got in the way of making this a suitable choice to provide a convenient interface to a system administration tool for the end user.

I may or may not decide to take a stab at it again, but for now I'm marking this repo as deprecated, and keeping the code for archival purposes.

Building
--------

    $ rebar3 compile

Distribution
------------

    $ rebar3 as prod release

_or_

    $ rebar3 as prod tar
