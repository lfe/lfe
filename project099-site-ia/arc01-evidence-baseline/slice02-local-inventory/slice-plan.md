---
project: project099-site-ia
arc: arc01-evidence-baseline
slice: slice02-local-inventory
version: "1.4"
date: 2026-10-09
status: active
depends-on: [slice01-baseline-intake]
workflow: two-contributor
mode: expedited
---

# Slice02 — Local content and declaration inventory

## Goal, authority and current assignment

Deliver Arc01's planned complete inventory of agreed local website content,
route/anchor declarations, resource promotions and legacy-source entrances.
[Slice01](../slice01-baseline-intake/cdc-verification.md) is independently
accepted at 9111268, all eight rows done. Duncan's approved roadmap and
Expedited Mode authorize this next slice. Governing parents are
[project-plan.md](../../project-plan.md) v1.8 and [arc-plan.md](../arc-plan.md)
v1.6. The original eight-column sibling ledger at b5a7c50 defines the unchanged
acceptance contract; iteration01 restores that table, preserving initial proposals.

Current assignment: [cc-prompt-iteration04.md](cc-prompt-iteration04.md),
correction R14–R15, issued through Duncan to a separate CC context. Iteration03
at fb83798b + 6a43ab2 was reviewed: all 114 structural predicates and 6,844
source locators pass; all 61 rich-input edges and current branch/source
qualifications are independently reproduced. S2-01 remains done; S2-03 and
S2-05 are now accepted for their declared source scopes. Five rows remain open
for the author aggregate's omitted real source fields/alias-role inconsistency
and precise required-intake/validation receipts. No source drift was found.
Was: [cc-prompt-iteration03.md](cc-prompt-iteration03.md), R11–R13; iterations02/01
and [cc-prompt.md](cc-prompt.md). Prior attempts and successful extraction,
metadata, reference, family, mechanism and entrance subclaims remain preserved
in [cdc-verification.md](cdc-verification.md). No source-extraction deferral,
output/live acceptance or criterion amendment is adopted. Slice03 remains gated.
CDC is Sofie in the desktop planning chat.
No automatic dispatch or CRC role is selected. Issued prompts stay immutable.

## Scope and evidence boundaries

The authoritative website population is every tracked `src/` path in
`/Users/oubiwann/lab/lfe/lfe.github.io`, plus named build/config witnesses.
Slice01 supplies the initial exact 385-path denominator (124 posts, 27
templates, 12 data files, 35 pages, 187 other). Re-query it and explain drift;
do not equate filenames, scanner membership or declared permalinks with a
rendered or deployed route. Inspect all page/post metadata, markup declarations,
shared data and template consumers, including drafts, pagination/feed rules,
raw HTML relocation files and repeated promotions. Record explicit IDs/names,
Markdown heading declarations, fragment references and dynamic expressions.
An inferred heading slug is not an observed HTML anchor.

Related-source inventory covers all tracked book `src/` filenames plus each
book's README/config/SUMMARY entrances; all LFE `doc/`, rebar3_lfe `docs/`,
and legacy docs `docs/` filenames; the legacy root/current/dev/version index
entrances. These populations are explicit filename/entrance inventories,
not a semantic audit of every book chapter or historical generated HTML body.
The four practical Build It targets, resource catalogue and old documentation
generations must remain traceable even when no local target exists.

No source repair, installs, checkout changes, network/HTTP checks, rendering,
wrapper execution, builds, compatibility tests, ticket edits or publication.
The established curated `/docs` decision remains distinct from the currently
copied `docs.html` relocation source. IA, migration destinations, support
policy and download storage are unresolved decisions belonging to later arcs.
Slice03 owns endpoint/source/rendered readiness evidence; Slice04 owns runtime
failure intake; Slice05 reconciles the registers. Preserve all inherited IDs.

## Outputs and execution design

Supporting outputs belong in this slice's `artifacts/`:

| Output | Authoritative content |
| --- | --- |
| `inventory.json` | Assignment/intake identities; input digests; eight root projections; complete declared file populations, website partitions, source fingerprints and mechanism witnesses |
| `route-anchor-register.json` | Website content metadata and route declarations/rules; raw HTML relocations; explicit anchors, heading declarations and fragment references with exact source locators and uncertainty |
| `resource-register.json` | Source link/promotional occurrences, shared-data-to-template consumers, exact literal destination groups, book/tool/language/legacy entrances and owned gaps |
| `inventory-summary.md` | Readable coverage, eleven-family reconciliation, consequential source observations and next-check handoff |
| `execution.md` | Required reads/data queries/readback, methods, actual attempts/results, controls, preservation and exclusions |

CC updates only this slice's ledger evidence and actual `closing-report.md`
in addition to those five artifacts. No close or verification is pre-created.
The prompt's **Representation**, **Recipe**, and **Checks** sections prescribe
the binding data fields, population queries, parsing/inspection boundaries,
failure handling and proof. JSON is evidence packaging, not a product schema.
Private ad hoc query mechanics are local discretion; no maintained validator
or extraction library is a requested deliverable.

## Verification and exit

Reproduce whole-set membership, uniqueness and partition union using independent
Git queries; match metadata and declaration occurrences to original bytes and
line/key/occurrence locators. Inspect parser residuals and representative hard
cases, rather than claiming a regex enumerated all Markdown semantics. Distinguish
examples/assets/technical links and promotions, literal versus dynamic targets,
source-declared versus inferred routes, and unresolved implicit heading IDs.
Complete declared-population coverage does not imply every target is valid.

Verify all source/data/template ownership joins, each of eleven inherited
families, four Build It seeds, book TOCs and eight legacy index entrances.
Keep blockers and unrun checks with owners and re-entry conditions. Run exact
set negative controls, JSON/local-link/locator and whitespace checks, full
before/after input/source preservation, then commit only the seven named paths.
CC leaves proposed-complete rows open/attested. Exit requires CDC independent
acceptance, full eight-row walk and honest Arc01 bubble-up. Arc composition
does not close with this slice.

## Author reconnaissance and readiness

Website remains main at `036e231b09c48966da26d2227bccb4fa3b295df9` with
14 modified Makefile/tool files and one untracked brief. LFE is now clean
develop at `0aa820e93bb592a821fe0a50b1db7e8926e56b30` after Duncan's
separate CI update; Slice01's older snapshot is retained. Capture all eight
roots again at CC intake, preserving concurrent work.

CDC inspected config/README/Cargo manifest, page/data/template examples,
raw relocation files, wrapper `build::run`, `generate_sitemap`, `run_cobalt`,
prerender traversal/field mapping and blog resolution/scan/write sites.
Prerender writes data siblings; blog resolution writes generated data and can
assign cover metadata to posts. The wrapper must not run on these checkouts.
The sitemap guesses paths from Markdown stems and depth, separately from
Cobalt's permalink declarations. Pagination/feed sources require rule records,
not a guessed count of rendered pages. All 124 post files declare permalinks;
two declare drafts. These are author observations to recheck, not prescribed
CC conclusions.

Installed Cobalt reports `cobalt-bin 0.20.4`. CDC executed its read-only
`debug config` and unqualified `debug files`: 346 scanner members, excluding
the 39 tracked data/templates, with no extra member. This is scanner evidence
only. Qualified `debug files pages/posts/assets` each exit 1 with
`Error: TODO Re-implement`; help has no `--trace` switch. Skill CB-25 examples
therefore require version qualification. Do not repeatedly run those failing
examples or substitute debug membership for output-route validation. Raw
config output starts `ce: ./src`; preserve actual output if cited, compare the
real `_cobalt.yml`, and do not silently repair the display or infer its cause.

CDC queried related tracked topology: four mdBook configs/SUMMARYs, language
`doc/`, tooling `docs/`, and legacy docs root plus current/dev/v1.2/v0.10/
v0.9/v0.8/v0.7 index entrances. No body-completeness claim was made.

| Readiness dimension | Basis |
| --- | --- |
| Source-grounded | Actual source identities, 385-path membership, metadata counts, wrapper symbols, debug command results and related repository topology inspected |
| Design-complete | Evidence-only populations and constructs fixed; source declarations separated from emitted routes; parser uncertainty and drift have explicit owners/stop rules |
| Guideline-applied | Cobalt CB-06/08/09/19/21/23/25/32 applied to metadata/defaults, data joins, pagination and version-qualified debug evidence; testing Proportionate Proof selects queries and source review |
| Executable | Ordered capture, scan, classify, join, reconcile and validation recipe; three authoritative JSON roles with stable occurrence locators |
| Falsifiable | Independent membership/cell/locator comparisons and omission/duplication controls; source URL/heading/scanner evidence cannot pass as HTTP/rendered proof |
| Coherent/portable | Full current contracts plus bounded source witnesses; full projections for larger corpora; no build/install dependency or machine-specific generated-site claim |

The required website page/template/data text is about 130 KB; related README/
config witnesses and bounded wrapper sections add modest prose. Post bodies,
large book TOCs and related filename estates use complete declared projections,
not required-full body reading. A roughly 45–65k-token intake/query budget leaves
substantial implementation and correction headroom in a 200k context. Check
actual executing capacity before dependent work; report inability rather than
skim, omit populations or silently re-slice. Author readiness is self-review,
not independent acceptance of CC output.

## Assignment history

| Assignment | Issued | Predecessor | Reason | Disposition |
| --- | --- | --- | --- | --- |
| `cc-prompt.md` — initial pass | 2026-10-09 | None; accepted Slice01 is input | Open Arc01's planned local inventory after the Slice01 gate | Returned at b43acb7 + d629b7a; CDC requires R1–R6; no complete row accepted |
| `cc-prompt-iteration01.md` — correction | 2026-10-09 | `cc-prompt.md`; b43acb7 + d629b7a | Restore contract; finish extraction and accurate witness/consumer/label/family/coverage evidence | Returned at 30b58c3 + 1b9ab34; CDC accepts S2-01 and retains repaired subclaims; R7–R10 remain |
| `cc-prompt-iteration02.md` — correction | 2026-10-09 | `cc-prompt-iteration01.md`; 30b58c3 + 1b9ab34 | Correct scalar scope/rich extraction, exact consumer/generated associations, semantic family/entrance joins and calibrated proof | Returned at 89f1ebb3 + cc401dd + ecc4993; CDC retains repaired extraction and S2-01; R11–R13 remain |
| `cc-prompt-iteration03.md` — correction | 2026-10-09 | `cc-prompt-iteration02.md`; 89f1ebb3 + cc401dd + ecc4993 | Reconcile copied derived coverage/qualifiers, real prerender roles and author aliases/branches, and current honest proof | Returned at fb83798b + 6a43ab2; CDC retains S2-01, accepts S2-03/S2-05; narrow R14–R15 remain |
| `cc-prompt-iteration04.md` — correction | 2026-10-09 | `cc-prompt-iteration03.md`; fb83798b + 6a43ab2 | Complete author aggregate source-field roles and precise current intake/proof receipts; preserve verified joins | Active; fourth corrective refinement issued through Operator for CC implementation and independent CDC return |

Record this prompt's opening Git object and digest at intake. Follow-ups get
new slice-root iteration paths; do not overwrite this issued assignment.

### Iteration01 author readiness

The current prompt carries the source-grounded R1–R6 construction, immutable
versus derived-value boundary, reading/query manifest, representative rich-YAML
consumer example, real regression oracles and exact seven-path return. CDC
reproduced 98 structural checks plus complete metadata/TOC target projections
and inspected affected semantic witnesses. The correction preserves all original
scope/criteria and source states; no structural amendment is required. Reading
budget leaves implementation/recovery headroom, with unavailable inputs or
material source drift held explicitly rather than silently reducing coverage.
This readiness record is distinct from independent acceptance of CC output.

### Iteration02 author readiness

The active prompt supplies source-inspected scalar-local reference construction,
exact alias/branch/generated ownership, semantic membership reasons and real
negative controls. CDC independently replays 109 structural checks and all 966
SUMMARY projections, queries twelve data files and inspects cited complete
template/mechanism contexts. Original scope/criteria and immutable inputs remain
fixed; evidence errors are corrected by CC rather than by the independent reviewer.
The bounded manifest and 45–70k-token intake budget leave correction headroom.
Unavailable inputs/drift block affected joins; no policy/architecture choice or
structural amendment is required. Readiness is separate from CC acceptance.

### Iteration03 author readiness

CDC freshly replays eight root/population/all-file/pin projections, all 966 TOC
comparisons, 827 ordinary Markdown/426 HTML nodes, 38 rich Markdown/39 rich HTML
nodes and 97 scoped reference joins. Complete 61-input versus consumer-role
comparison finds 26 missing roles; exact source context identifies stale copied
counts, two legacy plus five secondary author source qualifiers and misnamed
fallback witnesses. The recipe specifies precise current repairs, actual source
keys/aliases/branches, negative controls and immutable-versus-derived boundaries.
Cobalt CB-13/14/16/18/21 and the prompt-authoring/proportionate-proof/retention
contracts are applied. The current manifest bounds prose to affected contexts
and complete data projections, with 25–40k-token intake/query budget plus recovery
headroom; no material source assumption or scope/architecture decision is open.
Retained valid extraction is checked for input/value equality, not repeated for
ceremony. No criterion amendment or artifact repair by CDC; readiness remains
separate from independent acceptance of CC's forthcoming return.

### Iteration04 author readiness

CDC has inspected the complete authors YAML and five author layouts, current
consumer roles/aliases and all 61 rich-input edge joins. The independent source
walk identifies fourteen omitted direct leaf fields plus actual links-container
accesses; author-map keys and Liquid aliases remain separate concepts. Null
values/empty links are present fields, not missing records. The worked recipe
binds to all six source authors instead of only the five with rich mirrors.
Current receipt inspection identifies 1–239 versus a 266-line required-full
prompt, without inferring unread text or reconstructing past attendance.
Author readiness follows the prompt-authoring, proportionate-proof, retention,
Cobalt CB-13/14/16/18/21 and unchanged ledger contracts. Required intake is
bounded to current contract/receipt contexts, the author source/consumer census
and preservation projections; unaffected extraction is retained by exact
input/value equality. Expected intake/query budget is 20–30k tokens plus repair
headroom, using lossless chunks. No architecture, scope or criterion decision
is unresolved. CDC repairs no CC artifacts; readiness is not acceptance.

## Version History

| Version | Date | Origin | Change |
| --- | --- | --- | --- |
| 1.0 | 2026-10-09 | Slice01 iteration01 CDC acceptance; approved Arc01 Slice02 and Expedited Mode | Open the source inventory with complete declared populations, qualified route/anchor evidence, resource/legacy joins and eight acceptance rows |
| 1.1 | 2026-10-09 | Slice02 initial delivery / CDC review | Record R1–R6 and eight open rows; preserve valid subclaims, initial prompt/proposals and source scope; issue correction iteration01 with explicit construction/proof; no acceptance amendment |
| 1.2 | 2026-10-09 | Slice02 iteration01 / CDC review | Accept S2-01; preserve valid restored contract/provenance/rules/metadata/SUMMARY subclaims; issue iteration02 for R7–R10 with seven rows open; no acceptance amendment |
| 1.3 | 2026-10-09 | Slice02 iteration02 / CDC review | Retain S2-01 and valid extraction/consumer/family subclaims; issue bounded iteration03 for R11–R13 copied coverage/qualification, roles/branches and calibrated proof; seven rows open; no criterion amendment |
| 1.4 | 2026-10-09 | Slice02 iteration03 / CDC review | Accept S2-03/S2-05 and retain S2-01; issue bounded iteration04 for R14 author aggregate and R15 precise intake/proof receipts; five rows open; unchanged criteria |
