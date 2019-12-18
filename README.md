# SOCKETSERVER

A UNIX TCP socket client/server that manages socket connections and facilitates sending byte arrays through them.

The program is called `ocs` which stands for OCaml sockets.

# Building the project

make sure you have `dune` installed via `opam` and run the following to compile the project:

```
dune build @install
```

# User Guide

First run the core process that manages tcp sockets
```
ocs core
```

optionally you can run it in the background if you only have access to one shell
```
ocs core &
```

Then open a server
```
ocs open 8888
```

Check that the connection was created
```
ocs ls
```

Then on another machine after running `ocs core` connect to the server
```
ocs connect 192.168.1.232:8888
```

Send a string
```
ocs sendstring 0 hello
```

Cleanup after you are done (kill core and close file descriptors)
```
ocs exit
```

# Developer Guide

Each command execution is a process. Running `ocs core` runs the main loop that listens
to commands via a unix domain socket and tcp sockets it is currently connected to.

User input validation is done prior to sending data to core. If validation requires
knowledge of tcp connections it will be done in core.

Each command is stored in `commands.ml` in `all_commands` list. Besides strings for
help text, it also has:

* `cmd_func` which is a function invoked on the command process
* `core_func` which is a function core runs with bytes recieved via unix domain socket

The main loop for core is as follows:

1. The core first checks its unix domain socket for any messages and invokes `core_func`
2. If exit flag is true, cleanup and exit
3. Otherwise check tcp connections for any messages
4. Repeat step 1

The framing protocol used in both Unix domain sockets and tcp sockets are the same and
they both collate all data into a single `Bytes` object. Thus data communication is
limited to max `Bytes` length.

In the layer beneath the framing protocol, for Unix domain sockets is another protocol
first specifying command type via an integer in the first byte, and depending on the byte
a different protocol for the remaining of the data.

For tcp sockets the first byte specifies the type of message. Type 0 are `ACK` messages.
Type 1 are `string` messages. Thus it is extensible to interpret other types of data in 
the future.