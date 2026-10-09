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
