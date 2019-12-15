# SOCKETSERVER

A UNIX TCP socket client/server that manages socket connections and facilitates sending byte arrays through them.

The program is called `ocs` which stands for OCaml sockets.

# Design

The main process on init starts a new thread and then goes blocking awaiting user input. The thread however will call non blocking `recv()` on all sockets until main process sets exit flag to true.

The user is able to create connections via two commands

* `open <port>` which starts a socket server open to recieve one client
* `connect <ip:port>` which connects to an existing socket server with the given address

With active connections, users can send strings converted to byte array / `char list`. The byte array will be divided into frames and headers of payload length in the frame and end of message flag are added. This is to enable arbitarily long byte arrays to be sent over the socket divided into arbitarily many packets. Since TCP ensures order theres no need to keep track of it ourselves, an end of message flag suffices.

The beginning of a payload contains an MD5 hash to identify the message. When messages are sent a hashtable entry of key the MD5 hash and value current time will be added. Each connection has their own hashtable.

When a new message is recieved, an empty message with the payload of the same hash will be sent back as an ACK message. On recieving an ACK message, the hash will be checked in the hashtable and if the entry exists, calculate the time difference from the value and that will be the roundtrip time for sending the message.

# Running the project

make sure you have `dune` installed via `opam`.
```
dune build @install
dune exec ocs
```