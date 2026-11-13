# A home-manager module for skillx.
#
#   imports = [ inputs.skillx.homeModules.default ];
#   programs.skillx = {
#     enable = true;
#     sources.local = ./skills;
#     always = [ "core-rules" ];
#     entry = [ "rust-dev" ];
#     targets.claudeCode.enable = true;
#     targets.antigravity.enable = true;
#   };
#
# It installs the `skillx` binary, writes `~/.config/skillx/skillx.toml`, and
# compiles each enabled target harness into the Nix store with `skillx build`
# so broken links or missing roots fail at build time.
self:
{ config, lib, pkgs, ... }:
let
  cfg = config.programs.skillx;
  inherit (lib)
    filterAttrs
    literalExpression
    mapAttrs
    mapAttrs'
    mkEnableOption
    mkIf
    mkOption
    nameValuePair
    optionalAttrs
    optionalString
    types
    ;
  system = pkgs.stdenv.hostPlatform.system;
  toml = pkgs.formats.toml { };

  nameType = types.strMatching "[a-z0-9][a-z0-9._-]{0,63}";

  placeType = types.submodule {
    options = {
      always = mkOption {
        type = types.listOf nameType;
        default = [ ];
        description = "Tier 0 always-on rules active in this directory.";
      };
      entry = mkOption {
        type = types.listOf nameType;
        default = [ ];
        description = "Tier 1 entry-point skills active in this directory.";
      };
    };
  };

  mcpType = types.submodule {
    options = {
      command = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Executable command for a stdio MCP server.";
      };
      args = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Command-line arguments for a stdio MCP server.";
      };
      env = mkOption {
        type = types.attrsOf types.str;
        default = { };
        description = "Environment variables for a stdio MCP server.";
      };
      url = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Endpoint URL for a Streamable HTTP MCP server.";
      };
    };
  };

  hookType = types.submodule {
    options = {
      event = mkOption {
        type = types.enum [
          "SessionStart"
          "PreToolUse"
          "PostToolUse"
          "PreInvocation"
          "UserPromptSubmit"
          "PostInvocation"
          "Stop"
        ];
        description = "Lifecycle event when the hook fires.";
      };
      command = mkOption {
        type = types.str;
        description = "Shell command to execute.";
      };
      matcher = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Optional tool matcher regex for PreToolUse and PostToolUse.";
      };
      inject = mkOption {
        type = types.bool;
        default = false;
        description = "Whether stdout is plain text to inject as a system message.";
      };
      timeout = mkOption {
        type = types.nullOr types.ints.positive;
        default = null;
        description = "Optional execution timeout in seconds.";
      };
    };
  };

  targetSubmodule = defaultDir:
    types.submodule {
      options = {
        enable = mkEnableOption "compiling and linking this harness target";
        targetDir = mkOption {
          type = types.str;
          default = defaultDir;
          description = "Path relative to the home directory where the compiled tree is linked.";
        };
      };
    };

  placeToml = p:
    optionalAttrs (p.always != [ ]) { inherit (p) always; }
    // optionalAttrs (p.entry != [ ]) { inherit (p) entry; };

  mcpToml = m:
    optionalAttrs (m.command != null) { inherit (m) command; }
    // optionalAttrs (m.args != [ ]) { inherit (m) args; }
    // optionalAttrs (m.env != { }) { inherit (m) env; }
    // optionalAttrs (m.url != null) { inherit (m) url; };

  hookToml = h:
    {
      inherit (h) event command;
    }
    // optionalAttrs (h.matcher != null) { inherit (h) matcher; }
    // optionalAttrs h.inject { inherit (h) inject; }
    // optionalAttrs (h.timeout != null) { inherit (h) timeout; };

  sourceToml = path: { path = toString path; };

  manifestAttr =
    lib.recursiveUpdate
      ( {
          inherit (cfg) name version always entry;
        }
        // optionalAttrs (cfg.availableMcp != null) { available-mcp = cfg.availableMcp; }
        // optionalAttrs (cfg.availableBins != null) { available-bins = cfg.availableBins; }
        // optionalAttrs (cfg.places != { }) { places = mapAttrs (_: placeToml) cfg.places; }
        // optionalAttrs (cfg.sources != { }) { sources = mapAttrs (_: sourceToml) cfg.sources; }
        // optionalAttrs (cfg.mcp != { }) { mcp = mapAttrs (_: mcpToml) cfg.mcp; }
        // optionalAttrs (cfg.hooks != { }) { hooks = mapAttrs (_: hookToml) cfg.hooks; }
      )
      cfg.settings;

  manifestFile = toml.generate "skillx.toml" manifestAttr;

  buildHarness = harness:
    pkgs.runCommand "skillx-${cfg.name}-${harness}"
      {
        nativeBuildInputs = [ cfg.package ];
      }
      ''
        skillx build \
          --manifest ${manifestFile} \
          --harness ${harness} \
          ${optionalString cfg.lenient "--lenient"} \
          --out "$out"
      '';

  targetDefs = {
    claudeCode = {
      harness = "claude-code";
      inherit (cfg.targets.claudeCode) enable targetDir;
    };
    antigravity = {
      harness = "antigravity";
      inherit (cfg.targets.antigravity) enable targetDir;
    };
    openCode = {
      harness = "opencode";
      inherit (cfg.targets.openCode) enable targetDir;
    };
    codex = {
      harness = "codex";
      inherit (cfg.targets.codex) enable targetDir;
    };
  };

  enabledTargets = filterAttrs (_: t: t.enable) targetDefs;

  compiledTrees = mapAttrs (_: t: buildHarness t.harness) enabledTargets;
in
{
  options.programs.skillx = {
    enable = mkEnableOption "skillx, a declarative multi-harness skill graph compiler";

    package = mkOption {
      type = types.package;
      default = self.packages.${system}.skillx;
      defaultText = "skillx from the skillx flake";
      description = "The skillx package.";
    };

    name = mkOption {
      type = types.str;
      default = "skillx-bundle";
      description = "Plugin or bundle name written to plugin manifests.";
    };

    version = mkOption {
      type = types.str;
      default = "0.1.0";
      description = "Plugin or bundle version written to plugin manifests.";
    };

    sources = mkOption {
      type = types.attrsOf types.path;
      default = { };
      example = literalExpression "{ local = ./skills; }";
      description = "Skill store directories, keyed by source name.";
    };

    always = mkOption {
      type = types.listOf nameType;
      default = [ ];
      example = [ "core-rules" ];
      description = "Tier 0 skills emitted as always-on rules.";
    };

    entry = mkOption {
      type = types.listOf nameType;
      default = [ ];
      example = [ "rust-dev" "git-finish" ];
      description = "Tier 1 top-level entry skills exposed in the harness system prompt.";
    };

    places = mkOption {
      type = types.attrsOf placeType;
      default = { };
      example = literalExpression ''
        {
          "~/devel/garden-app".entry = [ "garden-deploy" ];
        }
      '';
      description = "Directory-scoped Tier 0 and Tier 1 roots.";
    };

    mcp = mkOption {
      type = types.attrsOf mcpType;
      default = { };
      example = literalExpression ''
        {
          akyuu = {
            command = "akyuu";
            args = [ "mcp" ];
          };
        }
      '';
      description = "MCP servers declared in the manifest and emitted when required by reachable skills.";
    };

    hooks = mkOption {
      type = types.attrsOf hookType;
      default = { };
      example = literalExpression ''
        {
          akyuu-context = {
            event = "SessionStart";
            command = "akyuu context --page 1";
            inject = true;
          };
        }
      '';
      description = "Lifecycle hooks declared in the manifest and emitted when required by reachable skills.";
    };

    availableMcp = mkOption {
      type = types.nullOr (types.listOf nameType);
      default = null;
      example = [ "akyuu" ];
      description = "MCP servers available in the environment, checked at build time when non-null.";
    };

    availableBins = mkOption {
      type = types.nullOr (types.listOf nameType);
      default = null;
      example = [ "cargo" "git" ];
      description = "CLI binaries available on PATH, checked at build time when non-null.";
    };

    lenient = mkOption {
      type = types.bool;
      default = false;
      description = "Accept non-canonical SKILL.md header ordering when compiling.";
    };

    settings = mkOption {
      type = toml.type;
      default = { };
      description = "Extra settings merged into ~/.config/skillx/skillx.toml.";
    };

    targets = {
      claudeCode = mkOption {
        type = targetSubmodule ".claude/plugins/${cfg.name}";
        default = { };
        description = "Claude Code plugin output settings.";
      };
      antigravity = mkOption {
        type = targetSubmodule ".gemini/config/plugins/${cfg.name}";
        default = { };
        description = "Antigravity plugin output settings.";
      };
      openCode = mkOption {
        type = targetSubmodule ".config/opencode/plugins/${cfg.name}";
        default = { };
        description = "OpenCode output settings.";
      };
      codex = mkOption {
        type = targetSubmodule ".codex/plugins/${cfg.name}";
        default = { };
        description = "Codex output settings.";
      };
    };

    generatedManifest = mkOption {
      type = types.attrs;
      internal = true;
      readOnly = true;
      default = manifestAttr;
      description = "Attrset written to skillx.toml (used by module checks).";
    };

    compiledPackages = mkOption {
      type = types.attrsOf types.package;
      internal = true;
      readOnly = true;
      default = compiledTrees;
      description = "Compiled store derivations for each enabled harness.";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    xdg.configFile."skillx/skillx.toml".source = manifestFile;

    home.file = mapAttrs' (
      key: t:
      nameValuePair t.targetDir {
        source = compiledTrees.${key};
      }
    ) enabledTargets;
  };
}
