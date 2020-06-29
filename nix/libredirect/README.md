# libredirect

This redirects `fopen` syscalls based on the `NIX_REDIRECTS`
environment variable. This is necessary because some Haskell
library makes calls to `/etc/protocols`, which doesn't exist in
the sandbox.
