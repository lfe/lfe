---
project: project099-site-ia
arc: arc01-evidence-baseline
version: "1.25"
date: 2026-10-19
status: active
depends-on: []
blocks: []
related: [Arc02, Arc03, Arc04, Arc05, Arc06, Arc11, Arc12]
workflow: two-contributor
mode: autonomous (includes expedited)
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

Historical opening status (superseded by current Slice03 review below):
Slice01 is accepted and Slice02 remains open for receipt-only iteration05;
S2-01–06 are accepted, S2-07/08 remain open. Was: three done, five open at iteration04 issue.
Was: Slice02 initial assignment opened after Slice01 acceptance; before that,
only Slice01 opened.
Later descriptions define capabilities and inputs;
their full open sets are prepared immediately before assignment.

| Slice / eventual directory | Delivered piece | Inputs / dependencies | Status |
| --- | --- | --- | --- |
| Slice01 — `slice01-baseline-intake` | Preserved-input provenance, local repository/source map, and complete reconciliation of the inherited assessment/question registers | Saved first-steps packet, approved parent plan, read-only local metadata and bounded source witnesses | Closed; all eight rows independently accepted at 9111268; initial failed review preserved |
| Slice02 — `slice02-local-inventory` | Complete inventory of the agreed local site content, route/anchor declarations, resource promotions, and legacy-source entrances | Accepted Slice01 provenance/populations; inspect actual Cobalt/wrapper semantics before claiming output routes | Closed; all8 rows independently accepted after authorized iteration06; retrospective complete |
| Slice03 — `slice03-resource-verification` | Targeted external destination and source/rendered readiness checks with coverage limits | Accepted Slice02 destinations; Slice01 inherited findings; read-only retrieval/rendering method defined at opening | Closed; all8 rows independently accepted after iteration02;40/40 consumed, retrospective complete |
| Slice04 — `slice04-technical-intake` | Technical evidence packet for release/container/environment questions and the reported runtime failure | Slice01 repository/provenance map; Slice03 relevant observations; actual Operator-provided failure context where available | Closed; all8 rows independently accepted after iteration02, retrospective complete |
| Slice05 — `slice05-baseline-reconciliation` | Reconciled evidence/coverage/finding register and concrete handoffs to consuming arcs | Accepted Slices01–04 and their explicit gaps/dispositions | Closed;all8 rows independently accepted after03,retrospective complete |

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

## Independent composition review assignment — 19 October 2026

This is the durable fresh-review assignment for the assembled
[closing-report.md](closing-report.md) and [CDC retrospective](cdc-retrospective.md),
not a CC implementation prompt or pre-filled favorable verdict. Its creation is
the commit containing arc-plan v1.25/project-plan v1.27, based on clean child-close
`fb4605d66a73c406c47f4f376d2454ffa68004ab`. Resolve that actual creation object from
Git before acknowledgment. This complete section ends at the next level-two
heading;its last substantive sentence is the stop condition below.

**Roles and authority.** Implementing assembler is CDC `/root/autonomous_cdc`;
fresh independent reviewer `/root/autonomous_cdc/arc01_reviewer` uses explicit
OpenAI Codex `gpt-6.1-sol` / reasoning `high`, selected through the collaboration
tool with no fallback. These are Duncan's19 October approved CDC-role settings;
backend identity remains unreported. Reviewer owns the verdict and actual checks,
not merely a countersignature of CDC's assertions. A separate context is required
because CDC assembled substantive arc composition;this is the default two-party
arc gate,not a standing CRC or staffing for available capacity. CC contexts are
idle/read-only. Root is read-only transport/Operator liaison.

**Scope and gate.** Review all9 original arc criteria and the actual five-child
composition/current project bubble-up. All40 child rows are independently done;
A1-06–09 and P-01 stay open pending your outcome. Arc01's useful intermediate
capability is evidence/coverage/owned next actions,not repaired reader journeys,
runtime resolution,support/IA adoption or artifacts. The original missing-runtime
contingency remains legitimate only with honest Operator/Arc02 ownership. The
review must test actual composition,not infer it from closed child files.

**Stable baseline and ownership.** Repository/worktree is
`/Users/oubiwann/lab/lfe/lfe/.worktrees/planning`, branch `planning`. The seven
original JSON registers plus four saved CI inputs are pinned to
`138543bf9f035a01a1b605ec155405d22a5f9c43`;Slice02's three inventories additionally
remain byte-equal `2112713d5673f186e7a6d00a4ee4f2d925d56794`. Current Slice05 JSON
is unchanged since `8d6304bdb6056eff87978f89da5e660383a7e111`;summary is unchanged
since initial `338176f883ed0a893b70de68d26ae8867b6f846c`. All issued slice prompts
and accepted child data/reviews/history remain immutable.
CDC owns planning/index through creation/read-only intake. First acknowledge
actual complete assignment/current authority,clean creation state,settings,
coherence and any unresolved input. Do not mutate or start dependent review until
CDC supplies a separate parent-only committed ownership handoff. Then load that
entire changed short ownership paragraph from the actual handoff object,through
its final release sentence. Arc plan/assembly/retrospective remain creation-fixed.
During review you alone own planning/index;CDC/root suspend mutable scans/edits.

**Required text, loaded losslessly before dependent judgment.** Read actual bodies,
not hashes/counts or a continuation summary. Bound outputs to recover truncation.
Use complete files/heading-to-next-heading sections,including final substantive
lines;labels cannot substitute for delivered text. Record current identities,
actual loaded extents and source-cited contract readback. After compaction recover
operative text actually lost;honestly separate loaded-now from earlier unknowns.
No numeric tail guessed from a remembered predecessor is a complete-section test.

- Installed `collaboration-framework/SKILL.md` v3.0.1,whole574-line body under
  `/Users/oubiwann/Library/Application Support/ChatGPT-Profiles/personal/codex-home/skills/`;
  bundled `knowledge/` is authority. Load posture guides01–03 full;engineering-
  methodology Notes for Codex through full Two-Contributor section,and full
  Autonomous Mode section through its next same-level heading. Existing3.0.0
  provenance is historical;the resolved metadata-only3.0.1 change does not alter
  completed old reads. Legacy standalone routes in old issued prompts remain
  history,not a reason to rewrite those prompts or omit current required text.
- Bundled PM guides README first,05-closing-arcs full,and02-canonical-planning-
  worktree complete sections Existing records and supporting evidence,
  Verification records by workflow and scale,and Retrospective records. Bundled
  work-verification guides01–05 full;coordination01–03 full;stewardship full.
  Load separate `cobalt-guidelines/SKILL.md` and its relevant source/output
  validation/deployment guidance for interpreting existing declarations;no build,
  generated-output or publication proof is implied. This is review,not a new
  implementation prompt or a new domain investigation.
- Project `AGENTS.md` full;project-plan metadata/opening and all complete sections
  Purpose through Workflow/authority/review (includes DoD,all12 arc descriptions),
  Autonomous preflight,Current state's latest composition paragraph and latest
  ownership handoff,plus current Version History entries relevant to this close.
  Older issue/handoff paragraphs are historical reference,not current ownership.
  Read whole current project/arc ledgers,whole arc plan,closing report and arc
  retrospective. This exact assignment is part of the required whole arc plan.
- All five current child ledgers and metadata/capability/verification/status
  contract sections of their slice plans;each final independent acceptance
  section linked by the arc closing report in full,plus any earlier evidence
  section retained for a claim. Inspect child handoff/inventory/technical summaries
  full and retrospectives02–05 full for synthesis applicability. Slice01 pre-mode
  retrospective must not be invented. Old prompts/failed returns are reference
  for a concrete affected provenance claim,not blanket fresh intake obligations.

**Required data and independently reproduced composition.** Execute complete
whole-population predicates with complete result/failure projections;do not dump
multi-megabyte JSON bodies into context. Inspect actual schemas before selecting
fields. Full rows/semantic projections may be delivered in bounded lossless batches.
The assembled72-row current register is the consumer;original source registers
and saved records are the oracle,not copied counts from this assignment.

1. Inspect actual diffs from fb4605d through creation;check allowlist and all five
   current eight-row child ledgers/closed plans. Verify original inputs/pins and
   source metadata/populations/fingerprints with qualified working-vs-HEAD modes.
   Reuse the successful preservation prefix of the incidental
   `/tmp/project099-cdc-iteration06-review.py` before its obsolete `allow=[D/`
   tail,after inspecting that code. No tag query/intake search is part of this.
   Establish current protection of eight roots/eight populations/2,839 witnesses/
   eight pins,three frozen JSON bytes and seven accepted original registers.
   Reconcile Slice01 master/dirty-lock epoch with later clean develop0aa820e via
   the actual qualified Slice01 review;do not restore or claim unrecorded bytes.
2. Independently join the complete104 namespaced catalogue IDs,29 assessments+
   16 questions,three gaps,11 families,six R03 findings and seven T04 findings to
   exactly72 consumer obligations. Compare every original meaning/limit/date/
   locator,then inspect the full current owner/disposition/next-evidence/re-entry
   projection for all72. Include all11 family typed reference arrays,1,872 nested
   members/31 direct memberships,all168 JSON pointers,and the13 direct+3 explicit
   Slice04-only witness authority boundary. No omitted field or namespace may be
   inferred valid from a count. Resolve the aggregate gap's five concern owners
   and T04-F05's three actual consumed surface handoffs,not generic owner prose.
3. Reproduce source/resource/route interactions:all1,447 occurrences and exact804
   group union,159 source unit/route pairs,135 fragment links,20 selected literal
   URLs/source joins/32 locators/observation fields,40 consumed slots/zero left,
   16 technical claims/102 unique original local tags and ten equally P0 pending
   OTP22–26 × Linux/macOS targets. Preserve blocked/inaccessible/unrun states.
   Inspect all3 practical/technical traces as usable decisions with original
   source/observer/date/limit semantics,not merely referential integrity.
4. Source-ground the BuildIt occurrence/website line17 versus inherited L25,
   manualR03-17 and three complete one-line source witnesses using accepted
   saved bodies/hashes/review evidence,and runtime-packet absence versus saved
   CI build/install/run semantics. Do not revisit a URL/browser/source body as
   new inquiry. Whole protected-byte comparisons are preservation,not renewed
   content investigation. Saved CI source and original result projections remain
   available locally;the16/eight/one budget is technical intake,not CI validation.
5. Inspect/reuse `/tmp/project099-cdc-arc01-composition.py` and
   `/tmp/project099-cdc-slice05-pointers.py` as incidental assembler methods;
   independently reason about or strengthen their predicates and negative
   controls. A successful doer helper is evidence access,not your verdict.
   Trace source meanings,actual consumed output and project capability together.
   Record fresh checks separately from retained independent child proof,with
   original candidate/date/method and concrete unchanged-input applicability.
   Do not rerun obsolete candidate-specific helpers or substitute summary trust.

**Prohibitions and honest limits.** All20 primary+20 independent resource slots
and exact16-file/eight metadata-group/one combined project+chat intake are fully
consumed. There is no HTTP/browser/retry/tag/runtime/build/generation/network
pull/install allowance,including for arc review. No source mutation,branch
switch,repair,push,merge,publish,ticket,external message or new investigation.
Preserve dirty/untracked sources,including unknown crash/log content. If a check
cannot establish a criterion within current evidence/authority,leave that gate
pending and return the concrete missing proof;never expand or soften the contract.

**Return and write allowlist.** Only append your actually performed attributed
independent composition gate to this arc's `closing-report.md`;do not repair or
rewrite assembler content,plans,ledgers,retrospectives or child files. The normal
Two-Contributor arc gate lives in that report under PM02;do not invent CRC records.
Your section must name actual reviewer/implementer contexts,selected settings,
exact creation and execution/candidate revisions,complete required text/data
coverage,source state,diff/allowlist checks,fresh methods/results and retained
proof applicability,all9-row assessment,semantic composition/bubble-up verdict,
concrete severity/location findings and blocked/unrun/remaining Operator gates.
Use a distinct local exact-path commit,then inspect committed state and report
hash/tree/clean status/no active tool;explicitly return ownership to CDC. A
failure needs planned remediation/appropriate decision,not an assembly repair in
the reviewer context. Your actual independent judgment may accept or reject.
CDC reconciles status only after your return. Stop at the Arc01 endpoint;do not
open Arc02 or commission any further inquiry.

## All five slices accepted; formal composition next — 19 October 2026

Slice05 all8 rows independently pass at1d44a8ee +3ab8492d;retrospective complete.
All five children are closed;A1-01–05 are done. CDC owns planning/index and will
assemble the actual whole-baseline composition and project bubble-up for a
fresh approved SolHigh independent gate. Child acceptance alone is insufficient.
No new inquiry;stop before Arc02. Earlier snapshots remain historical.

## Slice05 iteration02 review — 19 October 2026

Seven rows accepted;R5-04 canonical report content passes at719c3e4 +e7f5d0ee.
OnlyS5-08 remains forR5-05 exact complete current-status loading and truthful
receipt/classification. Third normal refinement freezes all data/accepted rows;
no new inquiry or criterion change. CDC owns planning pending acknowledgment;
Arc01 composition and retrospectives remain pending,Arc02 unopened.

## Slice05 iteration01 review — 19 October 2026

Seven rows accepted ate611be5 +8d6304b;one canonical reporting row remains for
R5-04. Distinct normal iteration02 freezes all substantive data and accepted
rows. CDC owns planning pending acknowledged transfer. No new inquiry or plan
criterion change;composition/retrospective and Arc01 endpoint gates persist.
Older snapshots remain historical.

## Slice05 current review — 19 October 2026

Independent review of338176f +17e72267 accepts five rows and retains complete
source/meaning/slot/target proof. S5-04/07/08 await first normal refinement for
actual current required prose recovery,original Notes restoration and one gap's
concern-specific handoffs. CDC owns planning pending acknowledged transfer;
composition remains gated. No source/inquiry allowance or criterion change.
Older opening/handoff snapshots are preserved history.

## Slice05 current opening — 19 October 2026

The complete slice05-baseline-reconciliation open set composes existing accepted
inputs:72 obligation rows,20 resource joins,16 technical claims/102 tags and ten
inherited P0 targets,with full source/observer/date/scope limits. All primitive
joins were source-grounded before issuance,including13 direct+3 Slice04-only
witness authorities. No new source/endpoint/tag/intake/runtime investigation.
Fresh LunaHigh CC05 will acknowledge before parent-only ownership transfer;
SolHigh CDC owns planning now. The slice plan remains stable through execution.
Arc01 composition needs a fresh independent SolHigh reviewer after acceptance;
stop before Arc02. Prior Slice04 snapshots below remain historical.

## Slice04 accepted; planned Slice05 next — 19 October 2026

Independent CDC accepts all8 rows at580f256 +e4fac583, with retrospective complete.
Exact16 witnesses/eight metadata groups/one combined intake are preserved with
source-only meanings,102 local tags and seven owned technical gaps. Runtime
packet absence is an owned Arc02/Operator input disposition. CDC owns planning
after clean release; planned Slice05 finite reconciliation may open after actual
author readiness. No renewed source/endpoint/tag/search/runtime work or Arc02.
The earlier issue/review/handoff snapshots below remain historical.

## Slice04 current review — 19 October 2026

Iteration01 accepts6 rows;S4-07/08 remain for truthful current intake/receiptR4-04.
All substantive data/owners pass. Distinct iteration02 preserves JSON/summary and
all original observations, correcting only existing receipt/Evidence/return prose.
CDC owns planning pending acknowledged parent-plan handoff; Slice05/composition
remain gated. No source/budget/criterion change; earlier review snapshots historical.

## Slice04 initial review — 19 October 2026

Initial a0fc7b8e +bbcf9303 independently accepts3 child rows;5 remain for
R4-01–03 source/ownership/receipt correction. Current distinct iteration01 keeps
all original16/eight/one observations and passed proof; no new inquiry allowance.
CDC owns planning pending acknowledged handoff. Slice05/composition remain gated.
The opening facts below remain historical; no structural or criterion change.

## Slice04 current opening — 19 October 2026

The complete slice04-technical-intake open set preserves the approved exact16
witnesses/eight metadata groups/one combined intake. CDC's single author-readiness
pass and root's chat component are already captured; CC analyzes original results,
without repeating tags/search or expanding the source population. All16 witnesses
are HEAD-equal. Actual runtime packet is absent within the searched scope; saved
CI compile background and uninspected workbench filenames remain distinct.
The existing missing-input contingency supplies Arc02/Operator ownership and
re-entry fields; no policy, reproduction or source repair is inferred.
Fresh LunaHigh CC04 is planned due observed CC03 recovery, with SolHigh CDC review.
CDC owns planning/index pending acknowledged handoff. Slice05/composition remain
open; all40 Slice03 checks stay consumed and Arc02 stays unopened.

## Slice03 accepted; approved Slice04 next — 19 October 2026

Independent CDC accepts all8 rows atc92041e7 +2278325f and completes the
retrospective. All40 checks are consumed; no extra request or body inquiry.
CDC owns planning after clean release; approved Slice04 may be opened with
its exact16/eight-group/one-search limits. Arc composition remains open and
Arc02 unopened. Older assignment/review snapshots below are historical.

Current after iteration01 review:d16aa934 +335b5aa accepts7 child rows;
S3-05 remains open for two residual JSON wording fields. Distinct
`slice03-resource-verification/cc-prompt-iteration02.md` permits only those
strings and necessary local receipts, retaining passed proof and40/40 cap.
CDC owns planning pending acknowledged handoff. Prior initial-review/opening
snapshots below are historical; no scope/sequence/criterion change.

Initial return0d2e09fd +5a3424cc is independently reviewed: S3-02/03/04
accepted;5 rows open for R3-01–04 locator/intake/coverage/Notes corrections.
All20 primary and20 independent slots are consumed; no additional HTTP/body
check is authorized. Current preserved assignment is
`slice03-resource-verification/cc-prompt-iteration01.md`, first local refinement.
CDC owns planning pending actual acknowledged handoff; role models unchanged.
The following original opening facts are retained as historical state.

The complete open set at slice03-resource-verification/ fixes20 destinations,
one CC primary and one reserved CDC browser/source check each, max40 including
blocked attempts. No check is executed at authoring. Fresh CC
`/root/autonomous_cdc/cc_slice03`, explicit gpt-6-luna/high, must acknowledge
the committed handoff; CDC gpt-6.1-sol/high owns mutations until then. Scope,
source-only preservation and actual browser-vs-extraction limits are explicit
in the slice plan/prompt. Slice04 remains approved but unopened behind its
actual dependency; Slice05/composition and Arc01 endpoint remain unchanged.

## Slice02 accepted; next approved work — 19 October 2026

Independent CDC review accepts all8 Slice02 rows at6ab32bd2 +96f54f37;
its retrospective is complete. R16/R17 failed history and the exact one-extra
authorization stay preserved. No seventh pass. CDC owns planning after clean
operational recovery; final post-receipt proof is CDC-reproduced, not a CC run.
No structural scope/criterion change is needed. The approved bounded Slice03
may now receive its complete open set; Slice04 remains behind its own actual
dependencies. Arc composition remains open; stop before Arc02. The older
issue/gate paragraphs below are historical and superseded by this verdict.

## One extra correction authorized — 19 October 2026

After checkpoint `99b0e6d`, Duncan explicitly selected “Authorize one extra
correction (Recommended)”. Exactly one additional Slice02 receipt/intake
correction is authorized, using a distinct iteration06 prompt and unchanged
Luna High CC / Sol High CDC settings. No reset, seventh pass, new source work
or acceptance amendment. Six accepted rows remain; S2-07/08 await independent
review. Existing CC acknowledged context coherence/headroom and will receive
a serialized handoff after author readiness. The following exhausted fifth-pass
state is preserved history; investigation budgets remain approved but gated.

## Current review gate and bounded investigation approval — 19 October 2026

Iteration05 at `1ffaba0` + `d0f6cae` repairs R16, but R17 current-mode
intake remains unestablished; six Slice02 rows accepted, S2-07/08 open.
CDC owns planning again; the fifth-pass budget is exhausted. Duncan must
authorize any further correction or choose to stop. No sixth prompt exists.
Slice03 remains gated despite the following separate investigation approval.

Duncan answered **“Approve both bounded passes”** on 19 October, in response
to the exact proposal relayed by root. This replaces the earlier pending
concrete-investigation-agreement state, without changing capability/criteria:

- Slice03: at most20 distinct already source-registered destinations. Priority:
  four Build It targets, four book entry targets, then registered legacy
  entrances, then inherited specifically flagged inaccessible/heading-only
  targets until the cap. Deduplicate literal destinations and retain excluded/
  unselected/failed/blocked coverage. At most one primary retrieval and one
  browser/source cross-check per selected destination, max40 checks total.
  Allocate at most20 primary checks to CC and at most20 second checks to CDC
  independent verification; failed/blocked attempts consume their slot. No
  implicit retry or arc-review HTTP allowance, full-site/chapter audit, build
  or generation. Output informs promoted-resource readiness for Arc04/08/10/11.
- Slice04: one read-only pass of exactly16 named witnesses: website
  README.md/_cobalt.yml; each of manual/tutorial/casting-spels/sicp
  README.md/book.toml; LFE README.md/Dockerfile; rebar3_lfe
  README.md/rebar.config; legacy docs README.md/docs/index.html. At most
  one HEAD/branch/status/tag projection group per each of eight repos; one
  bounded search of existing saved project intake/current chat summaries
  for the reported failure packet. No runtime reproduction, pulls, installs,
  builds or compatibility experiment. If absent, stop that claim with exact
  missing environment/failure inputs, Arc02 owner and re-entry condition.

Stop at these caps and report unchecked/blocked outcomes; extensions require
Duncan's decision. Both investigations await their actual slice gates and
complete source-grounded open sets. Slice05/composition and the Arc01 endpoint
remain unchanged. Earlier mode/preflight status paragraphs are historical.

## Autonomous execution — 19 October 2026

The [project preflight](../project-plan.md#autonomous-mode-preflight--19-october-2026)
is the current authority/settings/source/ownership entrypoint. It supersedes
older manual relay/no-dispatch language below for Arc01 only. CDC
`/root/autonomous_cdc` acknowledges the assignment and owns planning mutations;
CC will be a separate explicitly selected `gpt-6-luna` / `high` context, with
CDC `gpt-6.1-sol` / `high` independently reviewing. Current prompt creation
`21c0268` and SHA remain unchanged; actual execution-intake state will be
recorded at handoff. All ledger criteria, six accepted/two open Slice02 rows,
R16 scope and the exhausted-at-failure fifth-pass limit remain binding.
Sources and Project AGENTS remain unchanged at preflight; all 2,839 frozen
file fingerprints and eight input pins are preserved. No JSON repin.
Slices03/04 await a concrete investigation agreement before commissioning;
this leaves already-agreed iteration05 unaffected. Arc01 closure requires
independent composition and CDC retrospectives; stop before Arc02.

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
`slice02-local-inventory/cc-prompt-iteration05.md`, receipt correction, issued
through the Operator on 9 October 2026. Was: iterations04/03/02/01 and initial
`cc-prompt.md`. CDC reviewed e9dc003 + 2112713: all 114 structural predicates,
6,940 locators and 133 author/preservation predicates pass. S2-02/04/06 are
accepted, retaining S2-01/03/05; six rows done and two open. R16 is confined to
precise CDC-review source revision and complete Workflow/Current state reading
receipts with their scoped return. The [review](slice02-local-inventory/cdc-verification.md)
and all candidates/prompts are preserved. No capability, sequencing or criterion
change is required. Slice03 stays behind the existing acceptance gate. This
fifth refinement exhausts the existing fix-loop budget if it fails to converge;
any further assignment requires an explicit Operator decision or authorized
re-scoping. Slice01's correction at 9d3a7e1 + 9111268 is
independently accepted; all eight rows are done. Was: Slice01 iteration01
active with four open rows. Both Slice01 prompts and the initial failed review
remain preserved. Its bubble-up requires no capability, scope, sequence or
criterion amendment. Slice02's complete open set details its existing capability
and preserves source/output and related-entrance coverage limits. Later slices
remain planned; arc composition is open. Supporting outputs belong to the
owning slice's `artifacts/`.
The inherited `../artifacts/first-steps/` packet stays in place.

## Version History

| Version | Date | Origin | Change and rationale |
| --- | --- | --- | --- |
| 1.0 | 2026-10-08 | Duncan's approved project roadmap and explicit Arc01/Slice01 opening request | Open the evidence baseline with five bounded slices; separate inherited provenance from route census, live/resource verification, technical intake and reconciliation; issue Slice01 under Expedited Mode |
| 1.1 | 2026-10-09 | Slice01 initial delivery / CDC review | Update current assignment and partial acceptance; record R1/R2 corrective pass with Slice02 held at its existing dependency; no structural amendment |
| 1.2 | 2026-10-09 | Slice01 iteration01 CDC acceptance | Close the accepted child and open planned Slice02 under Expedited Mode; record lossless correction and qualified concurrent CI drift; no structural amendment |
| 1.3 | 2026-10-09 | Slice02 initial delivery / CDC review | Record R1–R6 correction and retained reproduced subclaims; issue iteration01 without adopting source-extraction deferrals or changing slice ownership; no structural amendment |
| 1.4 | 2026-10-09 | Slice02 iteration01 / CDC review | Accept S2-01 and retain repaired subclaims; issue iteration02 for R7–R10 with seven child rows open and Slice03 gated; no structural or criterion amendment |
| 1.5 | 2026-10-09 | Slice02 iteration02 / CDC review | Retain S2-01 and valid repaired subclaims; issue bounded iteration03 for R11–R13 with seven child rows open and Slice03 gated; no structural or criterion amendment |
| 1.6 | 2026-10-09 | Slice02 iteration03 / CDC review | Accept S2-03/S2-05, retain S2-01; issue bounded iteration04 for R14–R15 with five child rows open and Slice03 gated; no structural or criterion amendment |
| 1.7 | 2026-10-09 | Slice02 iteration04 / CDC review | Accept S2-02/04/06 and retain S2-01/03/05; issue receipt-only iteration05 for R16 with two rows open and Slice03 gated; no structural or criterion amendment |

| 1.8 | 2026-10-19 | Duncan’s Autonomous Mode selection / current Slice02 handoff | Link compact project preflight, record actual CDC context and explicit role settings, ownership and unchanged R16/iteration/independent gates; later investigations await bounded agreement; no capability or criterion amendment |

| 1.9 | 2026-10-19 | Slice02 Iteration05 CDC review and Duncan’s bounded-pass approval | Preserve repaired R16/six accepted rows, hold R17 at exhausted iteration gate, and record approved-but-gated Slice03 max20/max40 and Slice04 exact16/eight query-group/one-search limits; no capability or criterion amendment |

| 1.10 | 2026-10-19 | Duncan’s explicit bounded extra-pass authorization | Permit one distinct iteration06 for current intake/receipt correction, preserving history, accepted evidence and downstream gates; no seventh or reset |

| 1.11 | 2026-10-19 | Slice02 Iteration06 CDC acceptance | Close the8-row child and retrospective, retain failed history/limits and open the approved Slice03 planning gate; no structural or criterion amendment |

| 1.12 | 2026-10-19 | Slice02 accepted close / bounded investigation agreement | Open Slice03 with exact selection, budget allocation, method and fresh CC rationale; no structural or criterion change |

| 1.13 | 2026-10-19 | Slice03 initial independent review | Retain all20 primary and20 independent observations, accept three rows and require local R3-01–04 return correction;40-check budget exhausted, no-network iteration01 and later acceptance gates retained |

| 1.14 | 2026-10-19 | Slice03 iteration01 independent review | Accept seven rows, retain full passed proof and issue second local refinement for two consumed-data wording strings; no scope or check-budget change |

| 1.15 | 2026-10-19 | Slice03 iteration02 independent acceptance | Close8 child rows/retrospective and release approved Slice04 planning; retain40-check limit and all source/rendered/provenance boundaries |

| 1.16 | 2026-10-19 | Approved Slice04 author readiness after Slice03 close | Open original16-witness/eight-group/combined-intake analysis, explicit missing-runtime disposition and fresh-context rationale; no scope or criterion change |

| 1.17 | 2026-10-19 | Slice04 initial independent review | Accept3 child rows and require bounded local R4-01–03 correction, preserving original observations and later gates |

| 1.18 | 2026-10-19 | Slice04 Iteration01 independent review | Accept6 child rows and issue narrowR4-04 intake/receipt correction, preserving all substantive evidence |

| 1.19 | 2026-10-19 | Slice04 Iteration02 independent acceptance | Close8 child rows/retrospective, preserve bounded technical facts and missing inputs, release planned Slice05 reconciliation gate |

| 1.20 | 2026-10-19 | Slices01–04 accepted and finite Slice05 readiness | Open planned whole-evidence/ownership reconciliation without renewed investigation,retaining all capability/criteria and composition gate |

| 1.21 | 2026-10-19 | Slice05 initial CDC review | Accept five child rows and require first local R5-01–03 refinement,retaining passed proof and all arc gates |

| 1.22 | 2026-10-19 | Slice05 iteration01 CDC review | Preserve seven accepted rows and require final report/bubble-up completion,without scope or criterion change |

| 1.23 | 2026-10-19 | Slice05 iteration02 CDC review | Retain report/data proof and require third narrow current-status/receipt correction,without new inquiry or gate change |

| 1.24 | 2026-10-19 | Slice05 iteration03 independent acceptance | Close final child/retrospective and release formal arc composition,retaining all scope/independent/Operator gates |

| 1.25 | 2026-10-19 | Actual five-child composition assembly | Add formal close/retrospective and durable fresh SolHigh independent gate assignment,with explicit source/meaning checks,serialized ownership and unchanged Arc01 endpoint |
