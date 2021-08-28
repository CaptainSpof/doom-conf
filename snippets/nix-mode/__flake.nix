{
  description = "${1:'something, something…'}";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.$\{system\};
    in {
      devShell = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          ${0:`deps goes here`}
        ];

        shellHook = ''
                  echo "❄"
                '';
      };
    });
}
