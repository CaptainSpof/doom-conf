{
  description = "${1:something, something…}";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let
      name = "${2:project name}";
      pkgs = nixpkgs.legacyPackages.$\{system\};
    in {
      devShell = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          ${3:# deps goes here}
        ];

        shellHook = ''
          ${0:\${pkgs.figlet\}/bin/figlet "\${name\}" | \${pkgs.lolcat\}/bin/lolcat}
        '';
      };
    });
}
