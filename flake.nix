{
  description = "Stack development and CI environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  outputs = { nixpkgs, ... }: {
    devShells = nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-darwin" ] (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        nativeLibraries = [
          pkgs.gmp
          pkgs.zlib
        ];

        # Use the flake's compiler without changing Stack for non-Nix users.
        # Based on https://docs.haskellstack.org/en/stable/topics/nix_integration/
        stack = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          nativeBuildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "--no-nix --system-ghc --no-install-ghc"
          '';
        };
      in
      {
        default = pkgs.mkShell {
          packages = [
            stack
            pkgs.haskell.compiler.ghc9103 # Must match the Stack snapshot.
            pkgs.haskellPackages.hpc-codecov
          ];
          buildInputs = nativeLibraries;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeLibraries;
          # Keep build products separate from non-Nix Stack builds.
          STACK_PLATFORM_VARIANT = "nix";
        };
      }
    );
  };
}
