{
  description = "skillx";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      withTests = pkgs: pkgs.haskellPackages.callPackage ./skillx.nix { };

      skillxFor = pkgs:
        let
          lib = pkgs.haskell.lib;
        in
        lib.justStaticExecutables (lib.dontHaddock (lib.dontCheck (withTests pkgs)));
    in
    {
      packages = forAllSystems (pkgs: {
        skillx = skillxFor pkgs;
        default = skillxFor pkgs;
      });

      apps = forAllSystems (pkgs: {
        default = {
          type = "app";
          program = "${skillxFor pkgs}/bin/skillx";
        };
      });

      checks = forAllSystems (pkgs: {
        tests = withTests pkgs;
        format = pkgs.runCommand "skillx-format-check"
          {
            nativeBuildInputs = [ pkgs.haskellPackages.ormolu ];
            src = ./.;
          }
          ''
            cd $src
            ormolu --mode check $(find app src test -name '*.hs' | sort)
            touch $out
          '';
        module =
          let
            lib = nixpkgs.lib;
            linux = self.moduleEvals.x86_64-linux;
            mac = self.moduleEvals.aarch64-darwin;
            expect = what: ok: lib.assertMsg ok "module check failed: ${what}";
            ok =
              expect "writes skillx/skillx.toml on both darwin and linux" (mac.hasConfigFile && linux.hasConfigFile)
              && expect "links enabled harness plugin directories" (
                mac.homeFiles == [ ".claude/plugins/dev-skills" ".gemini/config/plugins/dev-skills" ]
                && linux.homeFiles == mac.homeFiles
              )
              && expect "manifest holds tiers, places and capabilities" (
                mac.manifest.name == "dev-skills"
                && mac.manifest.always == [ "core-rules" ]
                && mac.manifest.entry == [ "rust-dev" ]
                && mac.manifest.available-mcp == [ "akyuu" ]
                && mac.manifest.places."~/devel/garden-app".entry == [ "garden-deploy" ]
              );
          in
          assert ok;
          pkgs.runCommand "skillx-module-check" { } "touch $out";
      });

      moduleEvals = nixpkgs.lib.genAttrs systems (system: import ./nix/module-check.nix { inherit self nixpkgs system; });

      overlays.default = final: _prev: {
        skillx = skillxFor final;
      };

      homeModules.default = import ./nix/home-module.nix self;

      devShells = forAllSystems (pkgs: {
        default = pkgs.haskellPackages.shellFor {
          packages = _: [ (withTests pkgs) ];
          nativeBuildInputs = with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
            ormolu
          ];
        };
      });
    };
}
