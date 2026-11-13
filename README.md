# skillx

`skillx` is a declarative multi-harness skill graph compiler and manager written in Haskell.

It treats agent rules, entry-point skills, and sub-skills as a single procedural node type (`SKILL.md`) organised in a directed graph, and compiles tiered plugin trees for **Claude Code**, **Antigravity**, **OpenCode**, and **Codex**.

## Disclosure Tiers

Instead of dumping every installed skill into the root system prompt on every turn, `skillx` separates **authoring** from **disclosure tier**:

- **Tier 0 (`always`)**: Full body is active on every turn (`rules/<name>.md` for Claude Code and Antigravity; `AGENTS.md` for OpenCode and Codex).
- **Tier 1 (`entry`)**: `name` and `description` are exposed in the harness's top-level `skills/<name>/SKILL.md`.
- **Tier 2 (`leaf`, by reachability)**: Any skill not in `always` or `entry` that is reachable via frontmatter `links` from a Tier 0 or Tier 1 root is compiled to `_nodes/<name>/SKILL.md`. It costs zero tokens in the root system prompt and is unfolded via the generated `## Linked skills` routing footer in the skills that link to it.
- **Directory-scoped (`[places."<dir>"]`)**: Additional Tier 0 and Tier 1 roots activated when compiling for a specific working directory (`--at <dir>`).

## Canonical `SKILL.md` Format

```markdown
---
name: rust-dev
description: Rust conventions and entry point to the other Rust skills.
links: rust-cargo, rust-debug
requires-mcp: akyuu
requires-hook: akyuu-context
requires-bin: cargo, rustc
---

Body of the skill in Markdown.
```

At compile time, `skillx` strips `links`, `requires-mcp`, `requires-hook`, and `requires-bin` from the emitted harness `SKILL.md` frontmatter, appends a relocatable `## Linked skills` routing section with each target skill's description and relative path, and emits only the MCP servers and lifecycle hooks required by the active skill closure.

## Manifest (`skillx.toml`)

```toml
name = "my-skills"
version = "0.1.0"
always = ["core-rules"]
entry = ["rust-dev"]
available-bins = ["cargo", "rustc", "git"]

[places."~/devel/garden-app"]
entry = ["garden-deploy"]

[sources.local]
path = "./skills"

[mcp.akyuu]
command = "akyuu"
args = ["mcp"]

[hooks.akyuu-context]
event = "SessionStart"
command = "akyuu context --page 1"
inject = true
```

## Commands

```sh
skillx check                          # Verify roots, links, cycles, MCPs and binaries
skillx graph                          # Show resolved tiers and outgoing links
skillx build -t claude-code -o dist   # Compile a plugin tree for a target harness
skillx fmt skills                     # Normalise SKILL.md headers into canonical form
```
