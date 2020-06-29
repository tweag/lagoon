# Datalake Backend-Frontend Interface

The `datalake-interface` library defines the API between the Datalake backend
functionality (server and ingest tools) on the one hand and the web frontend
on the other.

In general of course the more functionality we can share between the frontend
and the backend the better. Right now the `datalake-interface` library is
rather small however since the Datalake web frontend is a relatively recent
addition to the Datalake project and we only recently started splitting out
functionality of the `datalake-backend` library into the `datalake-interface`
one.

The reason for not sharing the entire `datalake-backend` library wholesale
between frontend and backend is library dependencies: we don't want to bloat the
frontend (written in Haskell and then compiled into JavaScript) too much with
unnecessary dependencies, and some dependencies simply are not available in
JavaScript (most notably `postgresql-simple`).

## Getting Started

The `datalake-interface` library right now is quite a simple library, exporting
only a single module
[`Pfizer.Datalake.Interface`](src/Pfizer/Datalake/Interface.hs), which
re-exports the rest of the library's functionality. This functionality consists of

* Some utility classes (right now just [`Pfizer.Datalake.Interface.Pretty`](src/Pfizer/Datalake/Interface/Pretty.hs))
* Some newtype wrappers around database-related values ([`Pfizer.Datalake.Interface.DB`](src/Pfizer/Datalake/Interface/DB.hs))
* Type definitions of values that get shared between the frontend and backend,
  the most important of which is [`Pfizer.Datalake.Interface.SourceInfo`](src/Pfizer/Datalake/Interface/SourceInfo.hs); that module would be a good starting point for exploring the `datalake-interface` library.
