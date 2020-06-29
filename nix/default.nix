let
  fetch = import ./fetch.nix;

  # Forces the first argument fully before returning the second one. Only works
  # if the first argument is a derivation that evaluates to a file.
  seqDerivation = x: y: builtins.seq (builtins.readFile "${x}") y;

in { nixpkgs ? fetch "nixpkgs" }: import nixpkgs {
  config = { allowUnfree = true; };
  overlays = import ./overlays.nix { inherit fetch; };
}
