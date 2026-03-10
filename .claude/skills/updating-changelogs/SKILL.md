---
name: updating-changelogs
description: Updates changelogs and bumps versions across multiple packages in a monorepo branch. Use when releasing features, preparing PRs, resolving changelog/version merge conflicts, or when user says "update changelog", "bump version", "prepare release", "version bump", or "resolve conflicts".
---

# Updating Changelogs

Update changelogs and versions across multiple packages after implementing a feature branch.

## Quick Start

```bash
# 1. Get today's date base (MANDATORY - never type manually)
date +"%Y.%-m.%-d"   # Output: 2026.1.17 (no leading zeros)

# 2. Check existing latest version to avoid collisions
grep -m1 '^## ' <package>/ChangeLog.md
# If latest is already today's date (e.g., 2026.1.17.0), use 2026.1.17.1

# 3. Find changed packages
git diff origin/master...HEAD --name-only | cut -d/ -f1-2 | sort | uniq

# 4. Find ALL changelog files (case-insensitive)
find . -maxdepth 4 -iname '*changelog*' -type f 2>/dev/null | grep -v node_modules

# 5. Update each changelog + version, then commit
```

## 🚨 Critical Rules

### Date Format — NEVER Leading Zeros
```bash
date +"%Y.%-m.%-d"   # ALWAYS use this command for the base
```
- ✅ `2026.1.17.0` ✅ `2026.1.5.0`
- ❌ `2026.01.17.0` ❌ `2026.1.07.0`

### Version Collision — ALWAYS Check Before Bumping
```bash
# Check latest version BEFORE choosing the new one
grep -m1 '^## ' <package>/ChangeLog.md
```
- If latest is `2026.1.17.0` and today is `2026.1.17` → use `2026.1.17.1`
- If latest is `2026.1.17.1` and today is `2026.1.17` → use `2026.1.17.2`
- Never blindly append `.0` — always check for collisions first

### Content Quality — Feature-First, Not Commit-First

**BEFORE writing, read the specs/proposal** (openspec, PRD, issue) to understand what the PR delivers as a whole. Individual commits are implementation details — the changelog captures the *feature*.

**Two-layer structure:**
1. **Feature headline** — What capability does the user/developer gain? Write this FIRST.
2. **Supporting details** — Only list specifics that help someone decide if this version matters to them.

```markdown
## 2026.2.18.0 -- 13.7.0

### Added

- API Refresh Token management page: list, create, reveal, copy, delete tokens
  with inline panels and search. ADT state machine architecture.

### Infrastructure

- `UseStateEffect` module: reusable hook for state-driven async effects with
  automatic cancellation and optional key-identity tracking.
```

**Anti-pattern — commit-log regurgitation:**
```markdown
## 2026.2.18.0 -- 13.7.0
- Extract useStateEffect into standalone module
- Add optional ~toDepKey parameter
- Move Generate New button to left side
- Disable delete buttons during Revealing state
```
This tells you *what changed in code* but not *what the PR delivers*. A reader can't tell this is a brand new feature page.

**Rule of thumb:** If your entry reads like `git log --oneline`, you're writing at the wrong level. Read the specs, then write what matters to someone upgrading.

Omit: test additions, pure refactors, minor fixes (unless user-facing).

### 🚨 Special Cases (Memorize These)

| Package | Changelog Path | Casing | Format |
|---------|---------------|--------|--------|
| `onping2.0/` | `onping2.0/ChangeLog.md` (ROOT, not nested) | `ChangeLog.md` | `## YYYY.M.D.patch` |
| `re-react-onping-frontend/` | `re-react-onping-frontend/Changelog.md` | `Changelog.md` (lowercase L) | `## YYYY.M.D -- semver` |

Wrong casing = file IGNORED by CI checks.

## Workflow

### 1. Understand the Feature (DO THIS FIRST)
```bash
# Read specs/proposal to understand WHAT this PR delivers
ls openspec/changes/*/proposal.md openspec/changes/*/design.md 2>/dev/null
# Then check which packages changed
git diff origin/master...HEAD --name-only | cut -d/ -f1-2 | sort | uniq
# Then scan commits for supporting details
git log --oneline origin/master..HEAD -- <package>/
```

**Write the feature headline from the specs, not from the commit log.**

### 2. Match Existing Format
```bash
head -20 <package>/ChangeLog.md
```

### 3. Determine Version

**ALWAYS check the latest version first:**
```bash
grep -m1 '^## ' <package>/ChangeLog.md
```

| Type | Format | Bump Rule | Example |
|------|--------|-----------|---------|
| Date-versioned | `YYYY.M.D.patch` | Today's date, increment patch if date taken | `2026.1.17.0` → `2026.1.17.1` |
| Haskell 4-part semver | `A.B.C.D` | Patch: bump 3rd (`C`); Feature: bump 2nd (`B`) | `9.128.0.0` → `9.128.1.0` (patch) |
| Semver breaking | Major bump | | `1.10.0` → `2.0.0` |
| NPM | Semver | Feature: minor; Fix: patch | `12.54.2` → `12.55.0` |

### 4. Write Entry
```markdown
## <version>
* BREAKING: <summary> (if any)
* Add <feature>
* Fix <bug>
```

### 5. Update Version Files
```bash
# Cabal
sed -i 's/^version:.*$/version:        2026.1.17.0/' package.cabal

# package.json
sed -i 's/"version": "[^"]*"/"version": "12.55.0"/' package.json
```

### 6. Verify (MANDATORY)
```bash
# Packages with changes
git diff origin/master...HEAD --name-only | cut -d/ -f1-2 | sort | uniq

# Changelog updates staged
git diff --cached --name-only | grep -iE 'changelog'
```

**Checklist**:
- [ ] Every changed package has changelog entry
- [ ] `onping2.0/ChangeLog.md` (root level)
- [ ] `re-react-onping-frontend/Changelog.md` (note casing!)
- [ ] Versions match in changelog and `.cabal`/`package.json`

### 7. Commit
```
chore: update changelogs and bump versions for <feature>

BREAKING: package-a 1.0→2.0 (<reason>)
- package-b: <summary>
- package-c: <summary>
```

## Anti-Patterns

### ❌ Leading Zeros
```
## 2026.01.17.0  # WRONG
## 2026.1.17.0   # CORRECT
```

### ❌ Verbose Entries (>5 lines)
Condense related changes. Nobody reads 10+ line entries.

### ❌ Missed Packages
```bash
onping2.0/ChangeLog.md              # ROOT level, not nested
re-react-onping-frontend/Changelog.md  # Note: Changelog.md not ChangeLog.md
```

### ❌ Manual Dates
Always use `date +"%Y.%-m.%-d"` — never type from memory.

### ❌ Blind `.0` Suffix
Always check existing latest version. If today's `.0` already exists, use `.1`, `.2`, etc.

## Cherry-Pick & Merge Conflict Resolution

When cherry-picking or merging introduces changelog/version conflicts:

### Strategy
1. **Keep ALL changelog entries from both sides** in chronological order (newest first)
2. **Bump to a new version** at the top — don't just pick one side's version
3. **Remove the cherry-pick's duplicate entries** if they already appear under a different version in HEAD
4. **`.cabal` versions always match the top changelog entry**

### Steps
```bash
# 1. Identify conflicted files
git diff --name-only --diff-filter=U

# 2. For each changelog: resolve by keeping both sides' entries, add new entry on top
#    - HEAD entries stay as-is (they're newer)
#    - Cherry-pick entries slot in chronologically below HEAD's
#    - Add a NEW top entry with bumped version describing what's being re-added

# 3. For each .cabal/package.json: keep HEAD's version, then bump it to match new changelog top

# 4. Verify no conflict markers remain
grep -rn '<<<<<<\|>>>>>>>' <file>

# 5. Verify version sync
grep -m1 '^## ' <package>/ChangeLog.md
grep '^version:' <package>/*.cabal
```

### Example
```
Before (conflict in ChangeLog.md):
  <<<<<<< HEAD
  ## 9.128.0.0
  * Feature X
  =======
  >>>>>>> 247d901 (cherry-pick)
  ## 9.125.0.0
  * OAuth metadata

After (resolved):
  ## 9.128.1.0                          <-- NEW bumped version
  * Re-add OAuth metadata endpoints (cherry-pick)
  ## 9.128.0.0                          <-- HEAD entries preserved
  * Feature X
  ## 9.125.0.0                          <-- original entry in history
  * OAuth metadata

Before (conflict in .cabal):
  <<<<<<< HEAD
  version:        9.128.0.0
  =======
  version:        9.125.0.0
  >>>>>>> 247d901 (cherry-pick)

After (resolved — matches new changelog top):
  version:        9.128.1.0
```

## Quick Reference

```bash
# Today's date base
date +"%Y.%-m.%-d"

# Check latest version (to avoid collisions)
grep -m1 '^## ' <package>/ChangeLog.md

# Changed packages
git diff origin/master...HEAD --name-only | cut -d/ -f1-2 | sort | uniq

# All changelogs
find . -maxdepth 4 -iname '*changelog*' -type f 2>/dev/null | grep -v node_modules

# Package history
git log --oneline origin/master..HEAD -- <package>/

# Verify versions match
grep "^## " */ChangeLog.md
grep "^version:" */*.cabal
```
