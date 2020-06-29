# Datalake Interface

This library provides the infrastructure necessary to interface between the web
frontend and the backend. Any functionality that the web frontend might need
(as compiled JavaScript) lives here; it is therefore crucial that this library
is buidable with `ghcjs`. The rest of the functionality (shared by the command
line tool and the server) lives in the `datalake-backend` library. 
