{
  description = "OCaml environment setup";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    fu.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, fu }: fu.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in
    with pkgs;
    {
      formatter = nixpkgs-fmt;

      devShells.default = mkShell {
        packages = [
          ocamlPackages.ocaml-lsp
          ocamlPackages.utop
          ocamlformat
        ];
      };
    }
  );
}
