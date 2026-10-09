---
project: project099-site-ia
arc: arc01-evidence-baseline
version: "1.0"
date: 2026-10-08
status: active
depends-on: []
blocks: []
related: [Arc02, Arc03, Arc04, Arc05, Arc06, Arc11, Arc12]
workflow: two-contributor
mode: expedited
---

# Arc01 — Evidence baseline

## Capability and authority

Deliver an explicit inventory and evidence baseline for the project's technical
and content estate, as specified by [project-plan.md](../project-plan.md), Arc01.
Duncan approved that roadmap and requested opening Arc01 and Slice01 on
8 October 2026. This arc decomposes its approved capability; it does not approve
the future IA proposal, support policy, release model, or design system.

The existing investigations already establish a useful dated structural baseline.
Preserve them and their limitations. Extend coverage with exact source identities,
inventories, targeted checks, and a reconciled finding/ownership register rather
than commissioning the same audits again.

## Scope and boundaries

Include the saved reports' known findings and unanswered questions; the local
site/source/tooling/book repositories; website content, routes, anchors,
promotions, resources and legacy entrances; representative external destination
and chapter-readiness checks; release/container/source facts; technical failure
intake; and handoffs to the other arcs. Record what the inspected population
contains and excludes, with exact denominators.

Local repository content, inherited report observations, live observations,
source-declared mechanisms, interpretation, and unknowns remain distinct.
The local website HEAD is not proof of the deployed commit. Heading-only text
retrieval is not proof of an empty chapter. A version banner is not a support
matrix. Separate the generated reference from the established curated `/docs`.

This arc investigates and records evidence. It does not repair websites/books,
run compatibility experiments, pull containers, change branches/tags/releases,
choose support promises or production schemas, replace the design system, or
send messages/edit tickets. Later slices can name bounded read-only network or
rendering checks with their prerequisites. Source builds that mutate the user's
checkouts require an isolated, explicit method in the relevant slice contract.

## Slice breakdown

Only Slice01 is opened now. Later descriptions define capabilities and inputs;
their full open sets are prepared immediately before assignment.

| Slice / eventual directory | Delivered piece | Inputs / dependencies | Status |
| --- | --- | --- | --- |
| Slice01 — `slice01-baseline-intake` | Preserved-input provenance, local repository/source map, and complete reconciliation of the inherited assessment/question registers | Saved first-steps packet, approved parent plan, read-only local metadata and bounded source witnesses | Ready for CC; not executed |
| Slice02 — `slice02-local-inventory` | Complete inventory of the agreed local site content, route/anchor declarations, resource promotions, and legacy-source entrances | Accepted Slice01 provenance/populations; inspect actual Cobalt/wrapper semantics before claiming output routes | Planned |
| Slice03 — `slice03-resource-verification` | Targeted external destination and source/rendered readiness checks with coverage limits | Accepted Slice02 destinations; Slice01 inherited findings; read-only retrieval/rendering method defined at opening | Planned |
| Slice04 — `slice04-technical-intake` | Technical evidence packet for release/container/environment questions and the reported runtime failure | Slice01 repository/provenance map; Slice03 relevant observations; actual Operator-provided failure context where available | Planned |
| Slice05 — `slice05-baseline-reconciliation` | Reconciled evidence/coverage/finding register and concrete handoffs to consuming arcs | Accepted Slices01–04 and their explicit gaps/dispositions | Planned |

Slice01 intentionally performs no live re-audit, exhaustive route derivation,
book-body census, or technical reproduction. Slice02 owns source-route coverage;
Slice03 owns endpoint/readiness evidence; Slice04 obtains reproducible technical
inputs without deciding the compatibility policy that belongs to Arc02.

The external population is derived from the agreed inventory, with each check's
scope explicit. The complete erlang.org URL estate, all historical module pages,
every community service, and full book correctness are not implicitly claimed.
Important inaccessible items retain their attempts, missing inputs, and owners.

## Inputs and source reconnaissance

The preserved first-steps files are committed on `planning` at
`09bae67ee3ffb5b2365e3dd985b3eb16595a15f0`. Slice01 specifies their expected
digests. The LFE report has 15 assessment rows and 8 numbered open questions;
the Erlang report has 14 assessment rows and 8 numbered unresolved questions.
Source registers contain 43 LFE IDs and 61 Erlang IDs; an ID can describe a
compound destination and is not necessarily one unique URL or completed check.
Counts are identity/coverage checks, not validation of the reports' live claims.

At opening, website HEAD is `036e231b09c48966da26d2227bccb4fa3b295df9`
with unrelated dirty build-tool changes; language HEAD is
`dae7489ebf9588e5f746a34cd4f8ff83d1469eef` with untracked `rebar.lock`.
Other existing local repositories include `lfe-manual`, `lfe-tutorial`,
`lfe-casting-spels`, `lfe-sicp`, `rebar3_lfe`, and `docs` under
`/Users/oubiwann/lab/lfe/`. Capture current HEAD/status separately per checkout.
Do not imply local book sources are the published snapshots.

The site uses `_cobalt.yml` (`source: src`, `destination: ./site`), shared
`src/_data/`, layouts/includes, and the `lfesite` wrapper. `src/docs.html`
redirects to Use; `src/_data/site.yml` contains download labels; Home's four
build URLs live in `src/_data/home/buildit.yml`. The older
`docs/lfe-erlang-release-history.md` contains dated/inferred claims that need
careful authority treatment. Its presence does not supersede the saved audits
or establish current support policy. Preserve unrelated work in all checkouts.

## Verification and composition

The sibling [ledger.md](ledger.md) holds child-close, composition, and finding
disposition rows. At arc close, demonstrate that the inventory populations,
source witnesses, results and unresolved findings join into usable evidence
for the project. Closed child slices alone do not prove that join.

Verify stable IDs and links, source/observation dates and identities, exact
coverage including failed/blocked/unrun checks, source-versus-rendered limits,
and a concrete destination/owner/next check for every open finding. Spot checks
can establish interpretation; population checks must cover the declared set.
Retain scoped unresolved conditions rather than inventing evidence.

The runtime failure has no actual stacktrace/environment packet in the inherited
handoff. Slice04 must request or locate it and record its state. If unavailable,
the missing evidence remains an explicitly owned Arc02 entry condition; it
cannot be claimed reproduced. CDC/Duncan disposition any material inability
to deliver the arc capability before closure. Unaffected inventory work can proceed.

## Workflow and current assignment

CDC: Sofie in this desktop planning chat. CC: a separate contributor context
selected by Duncan; no automatic dispatch is implied. Reviewer: CDC, independent
of CC's evidence production. Arc composition gate: Operator or fresh independent
context, attributed at actual closure. No CRC is selected.

Expedited Mode applies as recorded in the project plan. CC commits only the
explicitly authorized slice paths before returning proposed-done evidence;
CDC commits planning and review changes. After independent Slice01 acceptance,
prepare/open Slice02 immediately unless an unresolved gate prevents it. Do not
fabricate closure, relax evidence, or change an issued prompt to advance faster.

Current assignment:
`slice01-baseline-intake/cc-prompt.md`, initial pass, issued to the Operator
on 8 October 2026. Supporting outputs default to that slice's `artifacts/`.
The inherited `../artifacts/first-steps/` packet stays in place.

## Version History

| Version | Date | Origin | Change and rationale |
| --- | --- | --- | --- |
| 1.0 | 2026-10-08 | Duncan's approved project roadmap and explicit Arc01/Slice01 opening request | Open the evidence baseline with five bounded slices; separate inherited provenance from route census, live/resource verification, technical intake and reconciliation; issue Slice01 under Expedited Mode |
