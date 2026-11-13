# Pure evaluation check of the home-manager module without home-manager.
{ self, nixpkgs, system }:
let
  pkgs = nixpkgs.legacyPackages.${system};
  lib = pkgs.lib;
  stub = lib.mkOption {
    type = lib.types.lazyAttrsOf lib.types.anything;
    default = { };
  };
  evalWith = skillxCfg:
    lib.evalModules {
      specialArgs = { inherit pkgs lib; };
      modules = [
        (import ./home-module.nix self)
        {
          options = {
            home.homeDirectory = lib.mkOption { type = lib.types.str; };
            home.packages = lib.mkOption {
              type = lib.types.listOf lib.types.package;
              default = [ ];
            };
            home.file = stub;
            xdg.configFile = stub;
          };
          config = {
            home.homeDirectory = "/home/test";
            programs.skillx = skillxCfg;
          };
        }
      ];
    };
  evaluated = evalWith {
    enable = true;
    name = "dev-skills";
    version = "0.2.0";
    sources.local = /store/skills;
    always = [ "core-rules" ];
    entry = [ "rust-dev" ];
    availableBins = [ "cargo" ];
    mcp.akyuu = {
      command = "akyuu";
      args = [ "mcp" ];
    };
    hooks.akyuu-context = {
      event = "SessionStart";
      command = "akyuu context --page 1";
      inject = true;
    };
    places."~/devel/garden-app".entry = [ "garden-deploy" ];
    targets.claudeCode.enable = true;
    targets.antigravity.enable = true;
  };
  c = evaluated.config;
in
{
  inherit system;
  hasConfigFile = c.xdg.configFile ? "skillx/skillx.toml";
  homeFiles = lib.sort (a: b: a < b) (builtins.attrNames c.home.file);
  manifest = c.programs.skillx.generatedManifest;
}
