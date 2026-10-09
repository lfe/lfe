# CC assignment — Arc01 Slice01 baseline intake

## Assignment and authority

Execute **evidence-only work**, initial pass, for `project099-site-ia` /
`arc01-evidence-baseline` / `slice01-baseline-intake`. This prompt's exact
project-relative path is:

`arc01-evidence-baseline/slice01-baseline-intake/cc-prompt.md`

Predecessor: none. Author/reviewer: CDC (Sofie in the desktop planning chat).
Duncan approved the roadmap, requested this opening, and enabled Expedited Mode
on 8 October 2026. Execute the assignment and return your proposed-done evidence
through the Operator. Do not stop after a readback if preflight is coherent.

Project root `P` below means the literal path
`/Users/oubiwann/lab/lfe/lfe/.worktrees/planning/project099-site-ia`.
Slice root `D` means `P/arc01-evidence-baseline/slice01-baseline-intake`.
These abbreviations are document notation, not environment variables to repurpose.
Run planning commands in `/Users/oubiwann/lab/lfe/lfe/.worktrees/planning`.

Match this assignment to the slice's Assignment history. Resolve its opening
commit with `git log -1 --format=%H -- project099-site-ia/arc01-evidence-baseline/slice01-baseline-intake/cc-prompt.md`;
record that revision, prompt digest and current planning HEAD at intake. Parent
plan version is 1.1; arc and slice versions are 1.0. The committed opening packet
governs this assignment; material later conflicts go to CDC, not silent selection.

## Required reading and data-query manifest

All paths below are real local paths, with `P`/`D` expanded as defined above.
Read before dependent extraction/writing. Retain full extents and source states
in `D/artifacts/execution.md`; recover truncated outputs in contiguous chunks.

| Order | Exact item / state | Scope | Purpose |
| --- | --- | --- | --- |
| 1 | This prompt, `D/slice-plan.md`, `D/ledger.md`, `P/AGENTS.md` — opening packet | Required-full | Assignment, shapes, acceptance, write/commit boundaries |
| 2 | `P/project-plan.md` v1.1 | Required-section: Inputs/established decisions; Planning/implementation locations; DoD/boundaries; Arc01 subsection; Workflow/authority/review; Expansion/findings/maintenance; Current state | Project authority, scope and review/mode gates |
| 3 | `P/arc01-evidence-baseline/arc-plan.md` and its `ledger.md` v1.0 | Required-full | Arc capability, slice boundaries and composition |
| 4 | `P/artifacts/first-steps/HANDOFF.md` and `lfe-work-classification.md` at `09bae67ee3ffb5b2365e3dd985b3eb16595a15f0` | Required-full | Settled decisions and seven work groups |
| 5 | Same directory's `lfe-io-ia-investigation.md` and `erlang-org-ia-investigation.md`, same pinned commit | Required-full, including source registers/qualifications | Complete inherited assessment/question/source populations and interpretation context |
| 6 | Same directory's `lfe-ia-content-proposal.md`, same pinned commit | Required-section: opening through §1; §10 Review decisions and dependencies; §11 Evidence references | Proposal/decision boundary; remainder reference-only |
| 7 | `/Users/oubiwann/.agents/skills/cobalt-guidelines/SKILL.md` | Required-full | Source/config/data conventions; follow applicable guide routing |
| 8 | `/Users/oubiwann/.agents/skills/cobalt-guidelines/guides/01-cobalt-static-site-generation.md` | Required-section: CB-19, CB-21, CB-32 and Summary Table | Distinguish source config/shared data from generated output; no publishing/build here |
| 9 | `/Users/oubiwann/.agents/skills/collaboration-framework/knowledge/work-verification/guides/01-ledger-discipline.md`, `02-evidence-strength.md` and `03-row-closure.md` | Required-full | Ledger intake, honest proposed-done evidence and row walk |
| 10 | `/Users/oubiwann/.agents/skills/collaboration-framework/knowledge/project-management/guides/README.md` | Required-section: Notes for Codex; Expedited Mode | Selected workflow/mode and preserved gates |
| 11 | `/Users/oubiwann/.agents/skills/collaboration-framework/knowledge/testing/guides/03-validation-gates.md` | Required-section: Gate Selection; Proportionate Proof; Gate Evidence | Bounded real checks; no invented validator product |
| 12 | Website root `/Users/oubiwann/lab/lfe/lfe.github.io`: `README.md`, `_cobalt.yml`, `src/_data/site.yml`, `src/docs.html`, `src/_data/home/buildit.yml`, `pagefind.yml`, `docs/lfe-erlang-release-history.md` | Required-full for each, current working bytes qualified against HEAD | Local witnesses, including dated/inferred claims, not inherited live conclusions |
| 13 | Website `tools/lfesite/src/cmd/build.rs` | Required-section: `pub fn run` through its closing brace before `generate_sitemap` | Actual wrapper stages only; full file is fingerprinted, not claimed read |
| 14 | Planning root and the eight repositories below | Required-data: complete Git metadata/status projection; complete website tracked-src filename query and partition | Exact populations; source state and dirty/generated boundaries |

Existing framework/AGENTS reading obligations still apply. No arbitrary historical
links become current instructions. Conditional source detail: if a witness read
requires a further local definition to interpret its claimed role, read/cite that
exact dependency before the claim; otherwise flag the inference as unresolved.
Do not follow live URLs during this slice. No Rust API design or source edits are
assigned; broader code/tooling reads are unnecessary for this bounded evidence task.

Preflight: load required text, execute/inspect full declared data projections,
record actual text extents separately from query coverage, and write a concise
source-cited contract readback. Explain why inherited 404s, heading-only chapters,
and release-history prose cannot become fresh verified/support claims. Hashes,
headings, another context's read or command exit zero are not full-text reading.
Missing primary inputs/conflicting authority block dependent work until resolved.

## Binding scope and preservation

Outputs are only the four named artifact files, slice `ledger.md` and actual
`closing-report.md`. Do not edit this issued prompt, slice plan, arc/project
contracts, inherited packet, or any source repository. No browsing, builds,
installs, container pulls, repair, tickets, messages or publication. Do not stage
unrelated files. A runtime failure packet is absent from the inherited handoff;
retain the gap for Slice04/Arc02 rather than fabricate reproduction.

Hash all five inherited files before extraction and at return. Expected SHA-256:

| File in `P/artifacts/first-steps/` | SHA-256 |
| --- | --- |
| `HANDOFF.md` | `3585b067463d21c5b7c3938202682905e34655b2a3832b711db06cec4f01205c` |
| `erlang-org-ia-investigation.md` | `1f43d24af3206346f2217f0a15e6d7e5eab8c4f94e0e8626f4d47217250e06da` |
| `lfe-ia-content-proposal.md` | `897bd06c798af9fa03c6314fffc940e49d8459cf890f0334925816fe0f9d0852` |
| `lfe-io-ia-investigation.md` | `0e922938b46e99501b8f81f978f193c2a529004f4a52e67b94a503afb5ecc2ca` |
| `lfe-work-classification.md` | `30b4ebc7ea5ebacf0e1f6f3171080f42ca3c06cd44d37b762e35a8c2ed889ecb` |

An unexpected packet digest blocks its dependent extraction; compare the pinned
object and report the discrepancy to CDC. Do not update expected hashes to fit.
Capture current source drift separately from the author baseline: website HEAD
`036e231b09c48966da26d2227bccb4fa3b295df9` with dirty Makefile/build tools
and untracked verification brief; language HEAD
`dae7489ebf9588e5f746a34cd4f8ff83d1469eef` with untracked `rebar.lock`.
Concurrent source changes are recorded, not repaired/reverted or attributed to you.

## Inspection sequence

1. Complete preflight and preservation hashes. Capture planning root/HEAD/branch
   and `git status --porcelain=v1 -uall`. Create only the allowed evidence home.
2. Attempt read-only `git rev-parse --show-toplevel`, `git rev-parse HEAD`,
   `git symbolic-ref --short -q HEAD` (exit 1 can mean detached), and
   `git status --porcelain=v1 -uall` for each of these exact roots:
   `/Users/oubiwann/lab/lfe/lfe`, `lfe.github.io`, `lfe-manual`, `lfe-tutorial`,
   `lfe-casting-spels`, `lfe-sicp`, `rebar3_lfe`, and `docs`, all with the
   `/Users/oubiwann/lab/lfe/` prefix. Preserve command exits and failures.
   Primary language/website/planning access is required; inaccessible related
   roots receive `unavailable`/`not-git` records and concrete follow-ups.
3. In the website root, query `git ls-files -z -- src`. Decode losslessly to
   sorted repository-relative paths. Partition in this priority order:
   `posts` = `src/posts/**`; `templates` = `src/_layouts/**` or `src/_includes/**`;
   `data` = `src/_data/**`; `pages` = remaining `.md`, `.html`, `.liquid` files
   without any dot/underscore-prefixed path segment; `other` = remainder.
   Require unique paths and a disjoint/exhaustive union. Ignored/generated output
   and untracked files are not this population; record that exclusion explicitly.
   This is filename coverage, not proof that every file publishes or every URL exists.
4. Read the manifest's source witnesses and fingerprint all eight complete files.
   Record working-file SHA-256 plus whether tracked bytes differ from the captured
   HEAD (use read-only `git diff HEAD -- <path>`/blob comparison; retain exits).
   The `build.rs` body outside `run` need not be read to hash it. Do not infer
   deployment provenance from local HEAD or latest/current support from labels.
5. Extract LFE §8 / Erlang §11 assessment tables, LFE §11 / Erlang §13 numbered
   questions, and LFE §12 / Erlang §14 source-register tables into the shapes
   below. Parse only the specified tables/lists; preserve IDs/raw cells/text.
   Read full surrounding reports before interpretation. Expected baseline is
   15+14 assessments, 8+8 questions, and 43+61 source IDs. Derive actual sets
   from inputs; if these disagree, investigate parsing/identity and retain the
   discrepancy instead of adding invented rows or dropping awkward ones.
6. Assign each inherited record a concrete downstream arc/next check and retain
   its limitations. Owner/next-check routing is a proposed handoff inside the
   approved roadmap, not authority to expand another arc or resolve its policy.
   Document the 11 content families in LFE §3.3 and seven work
   groups, four Build It URL seeds, `/docs` state/established intent, publication
   identity uncertainty, and the missing technical-failure packet. Explain the
   older release-history reference's status without selecting its recommendations.
7. Validate exact sets, literal cells, locators, JSON/links and preservation;
   self-review semantic boundaries. Fill slice row evidence at `attested` strength
   and leave proposed-complete rows open. Produce actual closing report with the
   row walk, artifacts, commands, limits, What Worked, and Bubble-up to Arc01.
8. Commit only the six exact paths below under Expedited Mode and return the
   commit, assignment identity and proposed-done packet to CDC via Duncan.

## Evidence representation and construction

Use UTF-8 JSON with deterministic ID/path sorting. Private extraction helpers
or ad hoc Python commands are local discretion; retain their commands/results
in `execution.md`. No helper/validator is a new committed deliverable. Preserve
raw source content while putting interpretation in separate fields.

`baseline.json` top-level keys:

- `assignment`: exact project-relative prompt path; `captured_at`: actual time;
  `planning`: root, HEAD, branch/detached state, prompt opening commit/digest.
- `inputs`: five objects `{path, commit, sha256, bytes, lines}` using project-relative
  paths and the pinned packet commit. SHA-256 identifies bytes, not comprehension.
- `repositories`: eight objects `{id, root, availability, head, branch, status,
  commands, limits}`. `status` preserves the observed path/status projection;
  `commands` records command/exit pairs; unknown values are null, not invented.
- `tracked_src`: `{paths, partitions, counts, exclusions}`; partitions have the
  five exact keys above and path arrays, with counts derived from those arrays.
- `source_witnesses`: eight objects `{path, head, working_sha256, differs_from_head,
  read_extent, limits}`; paths are website-relative, read_extent matches the manifest.
- `source_catalogue`: `{lfe: [...], erlang: [...]}`. Each object is
  `{id, raw_cells, locator}`. `raw_cells` includes the source-table ID, label,
  literal destination cell; trim table padding only. Do not split/normalize
  compound URL cells into pretend independent checks.

`findings.json` keys `assignment`, `assessments`, `questions`:

- Assessment object: `{id, report, original_id, raw_cells, locator, report_date,
  evidence_state, kind, owner_arc, next_check, limits}`. IDs are `LFE-F01`…`LFE-F15`
  and `ERLANG-F01`…`ERLANG-F14`; report is `lfe`/`erlang`; raw_cells retains the
  complete original table row excluding delimiters. `evidence_state` is always
  `inherited-report` in this slice. kind describes `strength`, `gap`, `opportunity`
  or `mixed` with support from the original text; it is not an imposed bug severity.
- Question object: `{id, report, original_number, raw_text, locator, report_date,
  evidence_state, owner_arc, next_check, limits}`. IDs are `LFE-Q01`…`LFE-Q08`
  and `ERLANG-Q01`…`ERLANG-Q08`; raw_text preserves the numbered question's wording;
  evidence_state is `unresolved-in-report`. No question is answered by extraction.
- Every locator is `{path, commit, line_start, line_end}` into one pinned input;
  the path resolves from project root. Every next_check is actionable and owned
  by a named future slice/arc, not “later”. Do not force missing evidence into
  a predetermined finding. New local observations belong in the summary with
  their current witness/date, not inside an invented inherited row.

Source-grounded example: `LFE-F02` resolves to the assessment row at line 254
of `artifacts/first-steps/lfe-io-ia-investigation.md` in the pinned input. Its
raw cells describe four build destinations reported as 404s and the report's
confidence/consequence. Carry it as `inherited-report`, owner `Arc08`, next
check `Arc01 Slice03 endpoint checks using Slice02 inventory`. The live claim
remains qualified by the report's §2.3. The current local
`src/_data/home/buildit.yml` item link URLs are separate source witnesses; their
matching paths do **not** re-prove the HTTP result. Manual chapter findings
similarly retain the source/rendered verification requirement.

The source registers' `L25`–`L28` cells are real destinations for that example;
IDs in the two registers stay in their own report namespace. A reference must
resolve to an atomic source object, not only a count or a summary label.

## Applied guidance and checks

| Guidance actually applied | Decision / reason | Proof and limits |
| --- | --- | --- |
| Cobalt CB-19 and CB-21 | Read actual config and data sources rather than assuming default paths or central authority | Config/data witnesses plus exact tracked-source population; no output-route claim |
| Cobalt CB-32 | Keep source/generation/draft boundaries visible; no publishing or opportunistic layout repair | Recorded exclusions and absence of source mutations |
| Framework evidence strength / row closure | Inherited observations and CC output are not independently verified live facts | provenance fields, source citations, open rows with attested evidence |
| Testing Proportionate Proof | Use exact queries, set/cell comparisons, bounded semantic checks and links | No site build or new validator product for unchanged source |
| Prompt-author intake contract | Full required text plus full declared projections before dependent edits | Actual extents/query results/readback in the allowed execution record |

Check JSON with Python's standard `json` parser (e.g. `python3 -m json.tool <file>`)
and run ad hoc exact-set/raw-cell/partition comparisons against the original
reports and Git query, not against a second call to your own extraction alone.
Recompute all input hashes. Check local links in created Markdown and every
JSON locator against the actual pinned files/line extents. Validate evidence
distinctions by reading the four build-target and chapter-readiness witnesses.

The S1-01…S1-08 ledger maps each criterion to its check. Important wrong results
these checks must reject: omitting/duplicating an inherited ID while keeping the
same total; mixing report namespaces; inventing an HTTP result from source text;
normalizing compound source cells into unique-URL claims; partition overlap;
promoting historical setup/banner text into support policy; rewriting an input;
claiming a pending question was answered. If using a set-check predicate, exercise
one omitted/duplicate-ID in-memory negative case through that same predicate and
retain the rejection. Do not build a broad mutation suite around incidental checks.

Record actual commands/exits/coverage, including failed or intentionally unrun
checks. Source tests/builds are outside this assignment because no source behavior
changes. Markdown/JSON structural checks are not semantic or independent acceptance.
Use `git diff --check` plus explicit whitespace/parse checks for untracked outputs.

## Write allowlist, commit and return

Only these paths, relative to the planning worktree, may change:

1. `project099-site-ia/arc01-evidence-baseline/slice01-baseline-intake/artifacts/baseline.json`
2. `project099-site-ia/arc01-evidence-baseline/slice01-baseline-intake/artifacts/findings.json`
3. `project099-site-ia/arc01-evidence-baseline/slice01-baseline-intake/artifacts/baseline-summary.md`
4. `project099-site-ia/arc01-evidence-baseline/slice01-baseline-intake/artifacts/execution.md`
5. `project099-site-ia/arc01-evidence-baseline/slice01-baseline-intake/ledger.md`
6. `project099-site-ia/arc01-evidence-baseline/slice01-baseline-intake/closing-report.md`

Stage only these explicit files and commit with `git commit --only` and the same
six literal paths; do not use `git add .`, `git add -A`, or an unbounded directory
path. Suggested subject: `Record Project099 Arc01 Slice01 baseline intake`.
No push. A commit records delivery, not CDC acceptance. A close report cannot know
its own commit hash in advance: identify the source/input states in the file and
return the actual delivery commit in your final message, without rewriting the
issued prompt or adding an unrelated metadata artifact.

Stop affected work for packet identity mismatch, primary root/required-reading
failure, contradictory contracts, consequential scope/acceptance changes, or
unexpected needed source mutation. Retain partial evidence, exact attempts and
owner/re-entry conditions; unaffected assigned work may continue. Routine source
drift receives a fresh scoped snapshot and explicit notes, not fabricated immutability.

Return: exact assignment and commit, six changed paths, S1-01…S1-08 proposed
dispositions with evidence pointers, actual checks/totals/limits, missing inputs,
and Bubble-up to Arc01. Include `execution.md` intake/readback and the artifact
inventory. Keep independent acceptance pending for CDC. Do not create
`cdc-verification.md` or mark this arc/project closed. After CDC acceptance,
Expedited Mode opens the next permitted slice automatically; that advancement is
CDC's work, not permission for CC to execute Slice02 from this prompt.
