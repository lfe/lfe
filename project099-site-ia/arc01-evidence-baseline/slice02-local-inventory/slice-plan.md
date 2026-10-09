---
project: project099-site-ia
arc: arc01-evidence-baseline
slice: slice02-local-inventory
version: "1.1"
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
[project-plan.md](../../project-plan.md) v1.5 and [arc-plan.md](../arc-plan.md)
v1.3. The original eight-column sibling ledger at b5a7c50 defines the unchanged
acceptance contract; CC's initial replacement table requires R1 restoration.

Current assignment: [cc-prompt-iteration01.md](cc-prompt-iteration01.md),
correction R1–R6, issued through Duncan to a separate CC context. Was:
[cc-prompt.md](cc-prompt.md), initial pass. CDC reviewed b43acb7 + d629b7a in
[cdc-verification.md](cdc-verification.md): valid source census/metadata
subclaims are recorded, all eight rows remain open and proposed S2-03/S2-04
source-extraction deferrals are not accepted. Slice03 remains planned.
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
| `cc-prompt-iteration01.md` — correction | 2026-10-09 | `cc-prompt.md`; b43acb7 + d629b7a | Restore contract; finish extraction and accurate witness/consumer/label/family/coverage evidence | Active; issued through Operator for CC implementation and independent CDC return |

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

## Version History

| Version | Date | Origin | Change |
| --- | --- | --- | --- |
| 1.0 | 2026-10-09 | Slice01 iteration01 CDC acceptance; approved Arc01 Slice02 and Expedited Mode | Open the source inventory with complete declared populations, qualified route/anchor evidence, resource/legacy joins and eight acceptance rows |
| 1.1 | 2026-10-09 | Slice02 initial delivery / CDC review | Record R1–R6 and eight open rows; preserve valid subclaims, initial prompt/proposals and source scope; issue correction iteration01 with explicit construction/proof; no acceptance amendment |
