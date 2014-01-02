eppi
====

eppi — Erlang based Python Package Index (pypi).

Features
========

  * cluster support
  * high performance
  * easy installation

Installation
============

    $ sudo apt-get install erlang git -y
    
    $ git clone git://github.com/rebar/rebar.git
    $ cd rebar
    $ ./bootstrap
    $ cd ..

    $ git clone git@github.com:ekalinin/eppi.git
    $ cd eppi
    $ cp ../rebar/rebar .
    $ make install

Usage
=====

Run a single node
-----------------

    # run with default config
    $ make run

    # run with another config file
    $ make run config_file=/some/path/to/eppi.config

Run a cluster
-------------

    # run a first node
    user@host01:~/eppi $ make run-node cookie=1596ba69881a0aac48bfe7806b5b

    # run a second node on another server
    user@host02:~/eppi $ make run-node cookie=1596ba69881a0aac48bfe7806b5b
    (eppi@host02)1> eppi:connect('eppi@host01').


Coniguration
============

Default config file is: `eppi.config`. It is in the root of the git repo.
Main options:

  * `pypi_url` — main pypi server (default: `pypi.python.org/simple/`)
  * `packages_dir` — directory with packages (default: `./packages`)
  * `http_port` — http port (default: `7890`)

Dependency
==========

  * erlang
  * cowboy
  * erlydtl
  * lager
