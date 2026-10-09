# Arc01 Slice01 execution record

**Assignment:** `project099-site-ia/arc01-evidence-baseline/slice01-baseline-intake/cc-prompt.md`
**Planning root:** `/Users/oubiwann/lab/lfe/lfe/.worktrees/planning`
**Intake head/branch:** `e8f4c394df1c5a902086fcb45f3249a52bc6c453` / `planning`
**Prompt opening commit/digest:** `e8f4c394df1c5a902086fcb45f3249a52bc6c453` / `ac2b9a6c59bc318167e39103ec1e1ff1190efef9fd056e63973c4ba15890aeee`
**Mode/workflow:** Expedited / Two-Contributor (CDC + CC). CC evidence remains attested pending independent CDC review.

## Intake and contract readback

I read the issued prompt, the slice plan and ledger, project `AGENTS.md`, the project v1.1 required sections, and the Arc01 plan/ledger. The prompt is the opening assignment at `e8f4c39`; its working bytes equal the committed prompt bytes. All five inherited first-steps files equal both the prompt's expected SHA-256 and the corresponding blobs at planning commit `09bae67ee3ffb5b2365e3dd985b3eb16595a15f0`. Exact byte counts, line counts, and digests are recorded in `baseline.json`.

The assignment is evidence-only. It authorizes these four artifact files, the slice ledger's evidence cells, and the actual closing report. It does not authorize source changes, live URL checks, builds, installs, container activity, ticket edits, publication, or a compatibility/IA decision. The parent contract preserves `/docs` as a hand-curated gateway; the generated-reference format and location remain unconfirmed. Expedited Mode requires a scoped commit but preserves every evidence and review gate. The ledger guide requires doer evidence to remain `attested`; self-review does not turn it into reproduced evidence. Accordingly, all eight slice rows remain open for CDC.

The established distinction is carried throughout: saved report rows are inherited claims with their original dates and qualifications; local working source is a separate witness; a source URL is not an HTTP observation; a heading-only extraction is not proof of absent rendered text; a local HEAD or a `current` label is not deployment provenance or support policy. The old release-history document contains dates, inferences, and recommendations as of its own stated period. It is historical evidence only here. No technical failure packet is present in the inherited handoff; Arc01 Slice04 retains that missing-input gate.

## Required reading and query coverage

All text was read in full or in the exact required sections below. Output limits were handled by contiguous line chunks; no heading-only, hash-only, or second-context read was substituted for required text.

| Required item | Actual extent read |
|---|---|
| Issued `cc-prompt.md` | Full, lines 1–257 (19,381 bytes); opening object and working-file SHA-256 rechecked |
| `slice-plan.md`, slice `ledger.md`, `AGENTS.md` | Full; 147, 18, and 24 lines respectively |
| `project-plan.md` v1.1 | Required sections: Inputs/decisions 54–86; locations 87–118; DoD/boundaries 119–160; Arc01 186–200; workflow/authority 388–432; expansion 433–452; current state 453–463 |
| Arc01 `arc-plan.md` and `ledger.md` | Full, 141 and 18 lines |
| `HANDOFF.md`, work classification | Full, 32 and 13 lines |
| LFE investigation | Full, 371 lines, read in contiguous ranges 1–190, 191–235, 236–280, and 281–371 |
| Erlang investigation | Full, 479 lines; contiguous report ranges 1–100, 101–165, 166–230, 231–300, 301–334, 335–400, and 401–479 |
| IA proposal Draft 0.1 | Required sections: opening through §1 (1–37), §10 decisions (324–342), §11 references (343–352) |
| LFE §3.3 content families | Full table and adjoining qualification, lines 79–100 |
| Cobalt `SKILL.md` | Full, 604 lines, chunks 1–120, 121–240, 241–420, 421–604 |
| Cobalt guide | CB-19 lines 627–714; CB-21 lines 719–744; CB-32 lines 1138–1189; Summary Table 1190–1232 |
| Work-verification guides: ledger discipline, evidence strength, row closure | Full, 96, 72, and 81 lines |
| Project-management wayfinder | Notes for Codex and Expedited Mode, lines 1–122 |
| Testing validation gates | Gate Selection, Proportionate Proof, and Gate Evidence, lines 1–50 and 100–107 |
| Website README and seven named data/page/config witnesses | Full: README 1–77, `_cobalt.yml` 1–6, `site.yml` 1–5, `docs.html` 1–17, `buildit.yml` 1–57, `pagefind.yml` 1–7, release history 1–110 |
| `tools/lfesite/src/cmd/build.rs` | Required `pub fn run` section, lines 17–44 of 453; full file fingerprinted, rest not claimed read |

At pre-edit intake, `git -C .worktrees/planning status --short --branch` returned only `## planning`, with no path entries. The planning root, HEAD, branch, prompt opening identity, and status are thus captured; the required per-source status projections use porcelain. The complete repository command/exits and porcelain path projections are in `baseline.json`. The exact website command `git -C /Users/oubiwann/lab/lfe/lfe.github.io ls-files -z -- src` returned 385 unique paths, all preserved in that JSON. Priority partitions (`posts`, `templates`, `data`, `pages`, `other`) contain 124, 27, 12, 35, and 187 paths; uniqueness, disjointness, and exhaustive union hold. Untracked, ignored, generated, and out-of-`src/` paths are explicitly excluded from this population.

All eight repository roots passed `rev-parse --show-toplevel` and `rev-parse HEAD`; all had a branch name; all status queries exited 0. The exact HEADs, branch names, status lines, and command/exit pairs are in `baseline.json`. Website dirty build tools and verification brief, plus unrelated untracked files in LFE/manual/casting/rebar3_lfe, are preserved and not attributed to this work.

## Applied guidance and source observations

- **Cobalt CB-19:** read the real config; it sets `source: "src"` and `destination: "./site"`. This does not prove which files publish.
- **Cobalt CB-21:** read the real shared YAML data sources. `site.yml` labels 2.2.0 current, 2.1.5 prior, `develop` prerelease; these labels do not define compatibility/support.
- **Cobalt CB-32:** kept source, generated output, and publication boundaries visible. No build, publish, layout repair, or source edit occurred.
- **Evidence strength/row closure:** raw inputs and current witnesses have distinct provenance; ledger rows are attested/open, not independently accepted.
- **Testing Proportionate Proof:** exact data projections and focused semantic comparisons cover this unchanged-source evidence contract. No new validator product or source-behavior test was warranted; source builds/tests were intentionally outside scope.

The four links in `src/_data/home/buildit.yml` are recorded as source seeds. Their match to inherited report references does not reproduce an HTTP result. The report's heading-only manual cases retain their source/rendered verification qualification and route to Arc01 Slice03. The local `/docs` file currently points to `/use`; the project decision still defines `/docs` as the curated gateway. The local release-history prose is dated/inferred and makes no present support or deployment claim.

## Commands, failures, and validation

The capture commands were read-only Git queries: for each of the eight paths, `git -C <root> rev-parse --show-toplevel`, `git -C <root> rev-parse HEAD`, `git -C <root> symbolic-ref --short -q HEAD`, and `git -C <root> status --porcelain=v1 -uall`. Website population command: `git -C /Users/oubiwann/lab/lfe/lfe.github.io ls-files -z -- src`. Witness qualification used `git -C <website> diff --quiet HEAD -- <path>` plus a HEAD blob hash comparison. All returned expected success/change codes; the `build.rs` diff returned 1 and is recorded as `differs_from_head: true`.

The first inline extraction attempt exited with `FileNotFoundError` because it joined the project root with a prompt path that already included the project directory. It wrote no files and extracted no records. The corrected command resolved the commit path and worktree-relative file separately; it exited 0 and produced the two JSON artifacts. The parser located report sections by heading, retained only ID-matching Markdown rows, trimmed table-cell padding, extracted numbered question text, and assigned pinned line locators. Sorting follows report/ID order. A separate comparison reread the pinned Markdown tables/lists and checked every raw cell/text/locator against the JSON. No input mismatch or access failure occurred. A preliminary Markdown-link check ran before `closing-report.md` existed and stopped on that planned summary link; the report was created and the final link check is recorded below. Two trailing-whitespace normalization passes joined `artifacts/closing-report.md` under the artifact directory; each stopped after processing earlier files. The corrected pass used the actual slice-root `closing-report.md` path and normalized all six authorized outputs. A preliminary final-preservation comparison joined project-relative input paths under the slice directory and failed on `artifacts/first-steps/HANDOFF.md`; it made no changes. The corrected project-root comparison passed for all eight source-root snapshots, five inherited inputs, and eight source witnesses.

Performed checks:

- Preserved packet: five working files equal pinned Git objects and expected SHA-256 values before extraction.
- Populations: 15 LFE + 14 Erlang assessments; 8 + 8 questions; 43 + 61 source IDs. IDs are namespaced and ordered without gaps/duplicates. Each source row retains its raw ID/label/destination cells; compound cells were not split.
- Focused negative control through the same exact-set predicate: an omitted expected ID was rejected; a duplicate ID was rejected. Both are in-memory cases and changed no files.
- Tracked `src/`: the stored path set was compared with a fresh `git ls-files -z -- src` query; recomputed partitions were unique, disjoint, and exhaustive.
- Witnesses: working SHA-256 values recorded for all eight files; seven full files match website HEAD. The full `build.rs` working file differs from HEAD, while its required function section was read from current working bytes.
- JSON parsing, source raw-cell/locator comparisons, local Markdown-link checks, whitespace checks, and `git diff --check`: results are recorded in the closing report after the generated packet was validated.
- Site builds, Rust tests, live URL probes, book rendering, and container checks: intentionally unrun/outside the assignment.

## Re-entry and limits

This intake does not verify live URLs, the complete site route set, deployed source provenance, search effectiveness, rendered chapter completeness, channel activity, or any compatibility combination. Arc01 Slice02 owns the declared local content/route/anchor inventory; Slice03 owns targeted endpoint and source/rendered readiness checks; Slice04 needs the actual technical-failure/environment packet; Slice05 reconciles the accepted inputs and handoffs. Independent CDC reproduction remains required before any row is accepted.

## Iteration01 correction intake and evidence — 9 October 2026

**Active assignment:** `arc01-evidence-baseline/slice01-baseline-intake/cc-prompt-iteration01.md`.
Its exact-path Git history resolves to opening commit
`7f2b47f24873211c3eca66702b5a1e81ccec4897`; SHA-256 of the committed/working
prompt is `f3160c0c43533cf8fd93686cc53665cfb00e7ce0d94a275780734be4cfebc7b2`.
Planning was at that same HEAD on branch `planning` at correction intake. This
active assignment is distinct from the original snapshot identity retained in
the JSON: that `assignment` identifies `cc-prompt.md`, while `captured_at`
and `planning` remain the original 2026-10-09T04:45:49Z snapshot metadata and
e8f4c39 opening state. The iteration assignment and its intake belong here and
in the dated close section; no snapshot was recaptured or relabelled.

### Required reading and data coverage

| Required item | Extent read / query coverage |
|---|---|
| Iteration01 prompt | Full, lines 1–138; current committed opening at 7f2b47f |
| `slice-plan.md`, `ledger.md`, `cdc-verification.md`, `AGENTS.md` | Full current committed opening: 178, 21, 119, and 24 lines |
| Original `cc-prompt.md` | Required sections: Assignment and authority (1–64); Binding scope and preservation (65–137); Evidence representation and construction (138–194); Applied guidance and checks (195–225); Write allowlist, commit and return (226–257); opening e8f4c39 |
| `project-plan.md` | Workflow, authority, and review (390–412); Current state (455–463) |
| Arc01 `arc-plan.md` | Capability and authority (16–28); Scope and boundaries (29–50); Slice breakdown (51–68); Workflow and current assignment (119–139) |
| `baseline-summary.md`, `execution.md`, `closing-report.md` | Full current files compared with ddeecb31: 38, 72, and 55 lines |
| JSON data | Full parsed objects, every top-level key/type and complete value comparison with ddeecb31 for both JSON files |
| Work-verification guide | Separation Rule; Assigned Reviewer Slice Verification; Evidence During Correction Review |
| Project-management wayfinder | Notes for Codex and Expedited Mode |
| Testing guide | Gate Selection, Proportionate Proof, Gate Evidence |
| Five packet inputs and eight repository roots/eight witnesses | Full digest and Git projection comparisons, no new content investigation |

The baseline candidate read used `git show ddeecb31:<worktree-relative-artifact-path>`;
both current parsed objects were exactly equal to the candidate before repair.
The source catalogue was queried at `baseline.json#/source_catalogue` (43 LFE,
61 Erlang); assessments/questions were queried in `findings.json` (29 and 16).
No inherited evidence row was re-extracted or rewritten.

### Correction, checks, failures, and results

R1 was applied with a Python standard-library JSON transformation: the original
nested baseline assignment's `prompt_path`, `captured_at`, and `planning` were
moved to the required top-level `assignment`, `captured_at`, and `planning`
fields. The resulting ordered top-level keys are exactly `assignment`,
`captured_at`, `planning`, `inputs`, `repositories`, `tracked_src`,
`source_witnesses`, and `source_catalogue`. All five non-metadata baseline
payload values compare equal as complete parsed values to ddeecb31. In
`findings.json`, only `assignment` changed to the same original project-relative
prompt path; `assessments` and `questions` compare completely equal to the
predecessor. Both assignment values now equal
`arc01-evidence-baseline/slice01-baseline-intake/cc-prompt.md`.

The same ad hoc shape/path predicate was applied to the predecessor and repaired
baseline: the predecessor fails because its assignment is nested and prefixed;
the repaired object passes with the required top-level timestamp/planning values.
The predecessor findings assignment fails the path predicate and the repaired
findings object passes. The predecessor is the actual negative control; no
synthetic mutation suite was run. Totals remain 29 assessments, 16 questions,
104 source-register objects (43/61), and 385 tracked website paths partitioned
124/27/12/35/187. Complete-value equality, not these counts alone, was checked.

R2 was corrected in the active ledger evidence and summary handoff. Source
register provenance now resolves to `artifacts/baseline.json#/source_catalogue`;
assessment/question provenance resolves to `artifacts/findings.json`; the
canonical report resolves to slice-root `closing-report.md`. The initial close
section remains labelled historical, including its failed initial pointer, and
this correction section records the current paths.

Preservation and drift commands/results:

- For each of five `baseline.json` input paths, SHA-256 of current working bytes
  equals the original recorded digest and SHA-256 of
  `git show 09bae67:project099-site-ia/<path>`; recorded byte and line counts
  also match. All five pass.
- For all eight baseline repository roots, reran `rev-parse --show-toplevel`,
  `rev-parse HEAD`, `symbolic-ref --short -q HEAD`, and
  `status --porcelain=v1 -uall`. Every root, HEAD, branch, and complete status
  path projection matches the original baseline. The website remains the same
  dirty checkout with 15 status entries; no concurrent source drift was found.
- Recomputed full-file SHA-256 for all eight website witnesses and compared
  each with both its recorded working digest and recorded HEAD blob. All working
  digests match; seven match HEAD and `tools/lfesite/src/cmd/build.rs` remains
  the one recorded working/HEAD difference. Source checkout files were not
  modified.
- Both issued prompts, CDC verification, slice/arc/project plans, and AGENTS
  remain unchanged. The initial prompt remains at its e8f4c39 opening; the
  iteration prompt remains at its 7f2b47f opening.

Two preliminary read-only input-object queries failed due to path construction:
one used a project-relative object path without the repository's
`project099-site-ia/` prefix; the next incorrectly tried that prefix after the
`-C` root had already been set to the project directory. Both returned Git exit
128 and changed no files. The corrected command used the planning worktree root
and `09bae67:project099-site-ia/<input-path>`; all five comparisons passed. A
first aggregate status-comparison display marked the website line array false
due to a comparison-script projection mismatch; direct exact-list comparison
then confirmed all 15 recorded/current status lines match. No source change or
baseline rewrite resulted from these preliminary attempts.

Validation at correction close: both JSON files parse with Python's standard
`json` parser; the explicit repair predicates pass and their predecessor
controls fail; the five input hashes, eight root projections, eight witness
hashes, full JSON preservation comparisons, counts, and corrected references
pass. Local Markdown links in changed Markdown, trailing whitespace, final
newlines, exact changed-path allowlist, and `git diff --check` are checked after
all edits. No build, network check, install, source test, or new content
investigation was run; these remain outside this metadata/reference correction.
