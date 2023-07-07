{
  description = "ICFPC 2023";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    ocaml-overlay = {
      url = "github:nix-ocaml/nix-overlays";
      #url = "github:nix-ocaml/nix-overlays?ref=anmonteiro/try-to-fix-concurrency-build-ocaml-5";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, ocaml-overlay }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ ocaml-overlay.overlays.default ]; };
        ocamlDeps = with pkgs.ocaml-ng.ocamlPackages_5_1; [
          ocaml
          ocaml-lsp
          #ocamlformat
          dune
          utop
          ocp-indent
          ocamlgraph
          zarith
          yojson
          ppx_yojson_conv
          angstrom
          # # qcheck
          # ppxlib
          # ppx_deriving
          # # odoc
          # core_kernel
        ];
      in
        {
          devShell = pkgs.mkShell {
            buildInputs = ocamlDeps ++ [ pkgs.jq ];
          };
        }
    );
}
