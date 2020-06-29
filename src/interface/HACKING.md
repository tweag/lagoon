# Lagoon Backend-Frontend Interface

The `lagoon-interface` library defines the API between the Lagoon backend
functionality (server and ingest tools) on the one hand and the web frontend
on the other.

## Getting Started

The `lagoon-interface` library right now is quite a simple library, exporting
only a single module
[`Lagoon.Interface`](src/Lagoon/Interface.hs), which
re-exports the rest of the library's functionality. This functionality consists of

* Some utility classes (right now just [`Lagoon.Interface.Pretty`](src/Interface/Pretty.hs))
* Some newtype wrappers around database-related values ([`Lagoon.Interface.DB`](src/Lagoon/Interface/DB.hs))
* Type definitions of values that get shared between the frontend and backend,
  the most important of which is [`Lagoon.Interface.SourceInfo`](src/Lagoon/Interface/SourceInfo.hs); that module would be a good starting point for exploring the `lagoon-interface` library.
