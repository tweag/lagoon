# Datalake Backend-Frontend Interface

The `datalake-interface` library defines the API between the Datalake backend
functionality (server and ingest tools) on the one hand and the web frontend
on the other.

## Getting Started

The `datalake-interface` library right now is quite a simple library, exporting
only a single module
[`Pfizer.Datalake.Interface`](src/Pfizer/Datalake/Interface.hs), which
re-exports the rest of the library's functionality. This functionality consists of

* Some utility classes (right now just [`Pfizer.Datalake.Interface.Pretty`](src/Pfizer/Datalake/Interface/Pretty.hs))
* Some newtype wrappers around database-related values ([`Pfizer.Datalake.Interface.DB`](src/Pfizer/Datalake/Interface/DB.hs))
* Type definitions of values that get shared between the frontend and backend,
  the most important of which is [`Pfizer.Datalake.Interface.SourceInfo`](src/Pfizer/Datalake/Interface/SourceInfo.hs); that module would be a good starting point for exploring the `datalake-interface` library.
