{-# LANGUAGE LambdaCase #-}

-- | Pure static verification of a skill store and manifest.
--
-- 'check' inspects the skill graph, the manifest roots and the declared
-- environment capabilities, returning a sorted list of 'Finding' values.
module Skillx.Check
  ( -- * Findings
    Severity (..),
    FindingKind (..),
    Finding (..),
    severityText,
    kindText,

    -- * Verification
    check,
    hasProblems,
  )
where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Skillx.Graph (Graph)
import Skillx.Graph qualified as Graph
import Skillx.Manifest
  ( Manifest (manAlways, manAvailableBins, manEntry, manPlaces),
    PlaceRules (prAlways, prEntry),
    allRoots,
    knownHooks,
    knownMcp,
  )
import Skillx.Name (Name, nameText)
import Skillx.Skill
  ( Skill (skillLinks, skillRequiresBin, skillRequiresHook, skillRequiresMcp),
    skillGraph,
  )

-- | How urgent a finding is. 'Problem' sorts before 'Suggestion'.
data Severity
  = -- | A broken link, missing root, or unsatisfied capability requirement.
    Problem
  | -- | An unreachable skill, cycle, or redundant tier assignment.
    Suggestion
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The category of a 'Finding'.
data FindingKind
  = -- | A root named in @always@, @entry@ or @places@ is not in the store.
    MissingRoot
  | -- | A skill links to one or more skills that are not in the store.
    DanglingLink
  | -- | A reachable skill requires MCP servers not in @[mcp]@ or
    -- @available-mcp@.
    MissingMcp
  | -- | A reachable skill requires hooks not in @[hooks]@.
    MissingHook
  | -- | A reachable skill requires binaries not in @available-bins@.
    MissingBin
  | -- | A skill participates in a directed link cycle.
    CycleMember
  | -- | A skill in the store is not reachable from any manifest root.
    Unreachable
  | -- | A skill is listed in both @always@ and @entry@ in the same scope.
    DuplicateTierRoot
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | One diagnostic finding on a skill.
data Finding = Finding
  { findingSeverity :: Severity,
    findingKind :: FindingKind,
    findingSubject :: Name,
    findingRelated :: [Name],
    findingDetail :: Text
  }
  deriving (Eq, Ord, Show)

-- | Render a 'Severity' as lowercase text.
severityText :: Severity -> Text
severityText = \case
  Problem -> "problem"
  Suggestion -> "suggestion"

-- | Render a 'FindingKind' as kebab-case text.
kindText :: FindingKind -> Text
kindText = \case
  MissingRoot -> "missing-root"
  DanglingLink -> "dangling-link"
  MissingMcp -> "missing-mcp"
  MissingHook -> "missing-hook"
  MissingBin -> "missing-bin"
  CycleMember -> "cycle"
  Unreachable -> "unreachable"
  DuplicateTierRoot -> "duplicate-tier-root"

-- | Whether a list of findings contains at least one 'Problem'.
hasProblems :: [Finding] -> Bool
hasProblems = any ((== Problem) . findingSeverity)

-- | Run all static checks over a store of skills and a manifest.
check :: Map Name Skill -> Manifest -> [Finding]
check store manifest =
  sortOn
    (\f -> (findingSeverity f, findingKind f, findingSubject f))
    ( missingRootFindings
        ++ danglingFindings
        ++ mcpFindings
        ++ hookFindings
        ++ binFindings
        ++ cycleFindings
        ++ unreachableFindings
        ++ dupTierFindings
    )
  where
    graph :: Graph
    graph = foldMap skillGraph (Map.elems store)

    storeNames :: Set Name
    storeNames = Map.keysSet store

    roots :: Set Name
    roots = allRoots manifest

    existingRoots :: Set Name
    existingRoots = roots `Set.intersection` storeNames

    reached :: Set Name
    reached = Graph.reachable existingRoots graph `Set.intersection` storeNames

    missingRootFindings :: [Finding]
    missingRootFindings =
      [ Finding
          { findingSeverity = Problem,
            findingKind = MissingRoot,
            findingSubject = r,
            findingRelated = [],
            findingDetail = "manifest root " <> nameText r <> " is not in the store"
          }
      | r <- Set.toAscList (roots `Set.difference` storeNames)
      ]

    danglingFindings :: [Finding]
    danglingFindings =
      [ Finding
          { findingSeverity = Problem,
            findingKind = DanglingLink,
            findingSubject = name,
            findingRelated = Set.toAscList missing,
            findingDetail =
              nameText name
                <> " links to missing skill(s): "
                <> T.intercalate ", " (map nameText (Set.toAscList missing))
          }
      | (name, s) <- Map.toAscList store,
        let missing = skillLinks s `Set.difference` storeNames,
        not (Set.null missing)
      ]

    mcpFindings :: [Finding]
    mcpFindings = case knownMcp manifest of
      Nothing -> []
      Just avail ->
        [ Finding
            { findingSeverity = Problem,
              findingKind = MissingMcp,
              findingSubject = name,
              findingRelated = Set.toAscList missing,
              findingDetail =
                nameText name
                  <> " requires unavailable MCP server(s): "
                  <> T.intercalate ", " (map nameText (Set.toAscList missing))
            }
        | name <- Set.toAscList reached,
          Just s <- [Map.lookup name store],
          let missing = skillRequiresMcp s `Set.difference` avail,
          not (Set.null missing)
        ]

    hookFindings :: [Finding]
    hookFindings =
      let avail = knownHooks manifest
       in [ Finding
              { findingSeverity = Problem,
                findingKind = MissingHook,
                findingSubject = name,
                findingRelated = Set.toAscList missing,
                findingDetail =
                  nameText name
                    <> " requires unconfigured hook(s): "
                    <> T.intercalate ", " (map nameText (Set.toAscList missing))
              }
          | name <- Set.toAscList reached,
            Just s <- [Map.lookup name store],
            let missing = skillRequiresHook s `Set.difference` avail,
            not (Set.null missing)
          ]

    binFindings :: [Finding]
    binFindings = case manAvailableBins manifest of
      Nothing -> []
      Just avail ->
        [ Finding
            { findingSeverity = Problem,
              findingKind = MissingBin,
              findingSubject = name,
              findingRelated = Set.toAscList missing,
              findingDetail =
                nameText name
                  <> " requires unavailable binary/binaries: "
                  <> T.intercalate ", " (map nameText (Set.toAscList missing))
            }
        | name <- Set.toAscList reached,
          Just s <- [Map.lookup name store],
          let missing = skillRequiresBin s `Set.difference` avail,
          not (Set.null missing)
        ]

    cycleFindings :: [Finding]
    cycleFindings =
      [ Finding
          { findingSeverity = Suggestion,
            findingKind = CycleMember,
            findingSubject = n,
            findingRelated = [],
            findingDetail = nameText n <> " participates in a link cycle"
          }
      | n <- Set.toAscList (Graph.cycles graph)
      ]

    unreachableFindings :: [Finding]
    unreachableFindings =
      [ Finding
          { findingSeverity = Suggestion,
            findingKind = Unreachable,
            findingSubject = n,
            findingRelated = [],
            findingDetail = nameText n <> " is not reachable from any manifest root"
          }
      | n <- Set.toAscList (storeNames `Set.difference` reached)
      ]

    dupTierFindings :: [Finding]
    dupTierFindings =
      let globalDups =
            Set.fromList (manAlways manifest)
              `Set.intersection` Set.fromList (manEntry manifest)
          placeDups =
            foldMap
              (\pr -> Set.fromList (prAlways pr) `Set.intersection` Set.fromList (prEntry pr))
              (Map.elems (manPlaces manifest))
       in [ Finding
              { findingSeverity = Suggestion,
                findingKind = DuplicateTierRoot,
                findingSubject = n,
                findingRelated = [],
                findingDetail = nameText n <> " is listed in both always and entry"
              }
          | n <- Set.toAscList (globalDups <> placeDups)
          ]
