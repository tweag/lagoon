# nix

Nix packaging for datalake. Please use the [default.nix](../default.nix) in the base repo directory when building.

Core application derivations are defined as overlays in [overlays.nix](overlays.nix). Integration tests and supporting scripts are packaged in [pfizer/datalake](./pfizer/datalake).
