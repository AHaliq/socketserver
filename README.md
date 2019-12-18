# SOCKETSERVER

A UNIX TCP socket client/server that manages socket connections and facilitates sending byte arrays through them.

The program is called `ocs` which stands for OCaml sockets.

# Building the project

make sure you have `dune` installed via `opam` and run the following to compile the project:

```
dune build @install
```

# Example use
First run the core process that manages tcp sockets
```
ocs core
```

optionally you can run it in the foreground if you only have access to one shell
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
ocs sendstring 1 hello
```

Cleanup after you are done (kill core and close file descriptors)
```
ocs exit
```