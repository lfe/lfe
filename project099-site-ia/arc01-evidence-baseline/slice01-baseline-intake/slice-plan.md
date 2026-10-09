---
project: project099-site-ia
arc: arc01-evidence-baseline
slice: slice01-baseline-intake
version: "1.2"
date: 2026-10-09
status: closed
depends-on: []
workflow: two-contributor
mode: expedited
---

# Slice01 — Baseline provenance and inherited finding intake

## Goal and authority

Give Arc01 a reproducible starting population: the unchanged first-steps packet,
the actual local repository/source state, and all saved assessment rows and
numbered unresolved questions, with explicit provenance and next-check ownership.
This is an evidence-only slice. Duncan's roadmap approval and Arc01/Slice01
opening request authorize this work; no public architecture/policy is selected.

Parent contracts: [project-plan.md](../../project-plan.md) v1.4,
[arc-plan.md](../arc-plan.md) v1.2, and this sibling [ledger.md](ledger.md).
Was: project v1.1 and arc v1.0 at opening; acceptance criteria are unchanged.
Current assignment: [cc-prompt-iteration01.md](cc-prompt-iteration01.md), first correction pass.
Was: [cc-prompt.md](cc-prompt.md), initial pass; returned at
`ddeecb31abb454d5619f74a178c3d0121024a35e` and reviewed on 9 October 2026.
[CDC verification](cdc-verification.md) accepts S1-01/S1-03/S1-05/S1-06;
Was: R1 metadata shape/identity and R2 evidence pointers kept four rows open.
CDC accepted the correction at 9111268 on 9 October 2026; all eight rows are done.
CDC is Sofie in
the desktop planning chat; CC executes in a separate Operator-selected context.
The prompt is issued to Duncan for relay, not automatically dispatched.

## In scope

1. Hash/identify all five inherited Markdown files without modifying them.
2. Record planning state plus eight explicitly named repository roots, HEADs,
   branch identities, dirty/untracked path status and failures/availability.
3. Query the website's complete tracked `src/` filename population and partition
   it without reading every file or claiming an output-route census.
4. Read the bounded source witnesses named in the prompt; record their exact
   digests and qualified local observations relevant to subsequent inventory.
5. Carry forward both saved assessment tables, source registers, and numbered
   unresolved-question lists. Retain raw text, scope, dates, confidence/limits,
   and distinctions between strengths, defects, opportunities and unknowns.
6. Reconcile the saved content families and seven work groups into a concise
   coverage/next-check handoff, preserving the established `/docs` decision.

## Out of scope

No live browsing, HTTP probing, website/compatibility builds, Cobalt rendering,
container pulls, dependency installation, source repair, complete book inspection,
route/anchor derivation, ticket edits, messages, or publication. Do not treat
local release-history prose as compatibility policy or deployed source identity.
No new reusable validator, schema package or product metadata system is requested.

## Outputs and representation

Durable outputs belong in this slice's `artifacts/` directory. CC creates:

| File | Content / authoritative role |
| --- | --- |
| `artifacts/baseline.json` | Input identities, planning/repository snapshots, tracked-source filename partition, selected witness identities, and literal inherited source-register cells |
| `artifacts/findings.json` | Every inherited assessment and numbered unresolved question with stable report-qualified IDs, raw source text, provenance, limits and a concrete next-check owner |
| `artifacts/baseline-summary.md` | Human-readable coverage, source-versus-publication boundaries, local observations, practical-route seeds and handoff to Slices02–04 / consuming arcs |
| `artifacts/execution.md` | Actual reading/query coverage and contract readback; commands, failures/unrun checks, validation totals, self-review and before/after preservation evidence |

JSON field shapes and exact inspection procedures are binding in the prompt's
**Evidence representation and construction** section. JSON is a slice evidence
format only. Field names do not establish a production compatibility schema.
CC additionally updates row evidence in this slice's `ledger.md` and writes
`closing-report.md` only after attempting the assigned work. No independent
verification file or parent-plan/ledger edit belongs to CC's write scope.

## Verification approach and exit criteria

Use read-only Git queries, SHA-256, JSON parsing/set comparisons, local link
checks and actual witness inspection. Derive the complete input populations
from the pinned reports/queries; reconcile against the expected 5 packet files,
8 repository attempts, 15+14 assessments, 8+8 questions, and 43+61 source IDs.
Counts are not semantic proof: each ID must resolve to the correct report,
source location and literal cells, with no inherited claim relabelled live.

Check disjoint/exhaustive filename partitions and uniqueness, not only totals.
Inspect the four Build It URLs and the heading-only chapter uncertainty as
semantic negative cases: a source URL does not prove an HTTP response; extraction
does not prove absence of rendered content. Do not invent a validator deliverable.
No website `make build`, `make check` or Rust tests are relevant to this unchanged
source/evidence contract; document them as intentionally unrun/outside this slice.

Exit requires all assigned outputs, a row-by-row proposed disposition and
source-cited evidence, scoped commit, honest bubble-up, and independent CDC
review before acceptance. CC does not mark `done` based on its own attestation;
leave proposed-complete rows open with attested evidence until CDC reproduces them.
Deferral/no-op requires the framework's reason/owner/re-entry or rationale.

## Preservation and drift

The first-steps packet is immutable at planning commit
`09bae67ee3ffb5b2365e3dd985b3eb16595a15f0`; the prompt lists its hashes.
Stop dependent extraction if identity changes; do not silently repin it.
Capture current source state separately from the author snapshot. Source HEAD
or concurrent dirty-state drift is not permission to alter another checkout;
qualify changed observations and report materially conflicting inputs to CDC.
Primary planning, language, and website roots must be accessible. A missing
related book/tooling root gets a retained failure/owner, not a fabricated HEAD.

At opening website HEAD is `036e231b09c48966da26d2227bccb4fa3b295df9`
with dirty build tools and an untracked verification brief; language HEAD is
`dae7489ebf9588e5f746a34cd4f8ff83d1469eef` with untracked `rebar.lock`.
Source writes are prohibited. Only the six paths in the prompt's commit list
may be created/updated by CC. Parent contracts and issued prompt remain unchanged.

## Author readiness

CDC inspected the source roots/status, Cobalt config, data conventions, redirect,
build-stage implementation, source-file population and dated release-history
reference. The inherited reports were read in full during handoff intake;
assessment/question/source-register extents were checked again for this assignment.

| Readiness dimension | Concrete basis |
| --- | --- |
| Source-grounded | Actual website/language heads and dirty status; verified eight roots; named witnesses and report sections in prompt |
| Design-complete | Evidence-only scope; immutable packet; strict ID/representation rules; source drift/partial failure handling; no policy conclusions prescribed |
| Guideline-applied | Cobalt CB-19/21 distinguish config/data from output; framework evidence-strength and prompt-intake rules; testing Proportionate Proof selects bounded real checks |
| Executable | Ordered capture/extract/reconcile procedure, resolved JSON fields, actual file/section reads, exact output and commit paths |
| Falsifiable | Exact ID sets reject omission/duplication; raw-cell comparison rejects rewriting; partition checks reject overlap/exclusion; provenance review rejects live/unsupported claims |
| Coherent/portable | Resolvable local paths and source states; bounded reading manifest; separate text and data-query coverage; expected packet fits one context with substantial recovery headroom |

This is author self-review, not independent acceptance. The required packet
includes both reports in full (about 20k tokens), small parent-contract sections,
the three current slice documents, focused source/guide reads, and filename/status
queries. It deliberately avoids full book bodies, whole compiler/tool code and
the full IA proposal. An executing context unable to load the packet must report
that constraint; it must not skim required-full inputs.

## Assignment history

| Assignment | Issued | Predecessor | Reason | Disposition |
| --- | --- | --- | --- | --- |
| `cc-prompt.md` — initial pass | 2026-10-08 | None | Open the approved arc's first bounded evidence slice | Executed at ddeecb31; reviewed 2026-10-09; correction required, preserved |
| `cc-prompt-iteration01.md` — first correction | 2026-10-09 | `cc-prompt.md` | Repair R1 metadata representation and R2 evidence references without redoing accepted intake | Executed at 9d3a7e1 + 9111268; accepted by CDC 2026-10-09 |

The prompt's opening commit is resolved from Git history at intake; record its
commit and SHA-256 plus the current planning HEAD in `artifacts/execution.md`.
The initial execution and review are retained; this supersedes the opening
statement that no execution/close/review had occurred. Slice closure is accepted in the iteration01 CDC record. Follow-up prompts get new numbered paths;
the current prompt is preserved after issuance.

## Correction author readiness — iteration01

CDC inspected both delivered JSON objects, all six changed files, and the issued
representation against ddeecb31. The original content populations, hashes,
partitions, locators and local witnesses were independently reproduced.

| Dimension | Basis |
| --- | --- |
| Source-grounded | R1/R2 cite actual metadata paths and incorrect evidence pointers in the reviewed candidate |
| Design-complete | Original snapshot identity/time retained; metadata moves are lossless; only current references and dated return evidence change |
| Guideline-applied | Prompt-authoring correction rule and work-verification retention/separation rule bound repairs; testing Proportionate Proof selects whole-value comparisons |
| Executable | Six-path allowlist, exact field moves, predecessor comparison, preserved attempt sections and row states in iteration01 |
| Falsifiable | Actual predecessor fails required shape/path; corrected values must pass and preserve every unaffected payload |
| Coherent/portable | Current assignment and pending gate agree across plans; bounded manifest separates metadata queries from optional historical investigation rereads |

This is author readiness, not acceptance of a repair. The required correction
packet is small enough for one context with recovery headroom; the complete
850-line inherited reports are data inputs for retained evidence, not a new
required-full investigation for this metadata/reference repair. No scope or
criterion change is proposed.

## Version History

| Version | Date | Origin | Change |
| --- | --- | --- | --- |
| 1.0 | 2026-10-08 | Arc01 opening, authorized by Duncan | Initial provenance/finding-intake contract, artifact shapes, verification and preserved CC assignment |
| 1.1 | 2026-10-09 | Initial CC delivery and CDC review | Record partial acceptance, preserve failed initial attempt, and issue bounded metadata/reference correction as iteration01; no criterion or scope change |
| 1.2 | 2026-10-09 | Iteration01 CDC review | Close all eight rows after lossless metadata/reference correction; preserve initial failed review and qualify concurrent LFE CI drift; no criterion change |
