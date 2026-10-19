---
project: project099-site-ia
title: LFE site information architecture and supporting foundations
version: "1.10"
date: 2026-10-19
status: active
depends-on: []
blocks: []
related:
  - /Users/oubiwann/lab/lfe/lfe
  - /Users/oubiwann/lab/lfe/lfe.github.io
  - artifacts/first-steps/HANDOFF.md
  - artifacts/first-steps/lfe-work-classification.md
workflow: two-contributor
mode: autonomous (includes expedited)
---

# Project 099 — LFE site IA

**Approved roadmap, v1.9 — Arc01 active; historical-download priority recorded.** Duncan approved the initial roadmap
in this chat and requested Arc01, its first slice, and Expedited Mode on
8 October 2026. This supersedes v1.0's draft-for-review status. The plan covers the
full body of work described in the handoff, including the compatibility,
distribution, design, and validation tracks that support the website. Those
tracks keep their own decisions and evidence; the IA proposal does not decide
them. The current assignment is Slice02 correction iteration05 after CDC review
of iteration04; S2-01–06 are accepted and two receipt/return rows remain open. Was: Slice02 initial local inventory after independent
Slice01 acceptance; before that, a bounded Slice01 metadata/reference correction following
the initial delivery; both attempts and their review evidence remain preserved. No website or compiler implementation is
authorized by the current evidence work.

Project expansion is expected and is not considered scope creep. This is a
living roadmap: discoveries and discussion can add, split, combine, or reorder
arcs. Changes are recorded so that expansion is visible and existing obligations
are preserved. Arc numbers identify work; they are neither priorities nor dates.

## Purpose and intended outcome

Make LFE's public information estate dependable for people trying the language,
learning it, building projects, choosing releases, finding reference material,
and participating in its community. Preserve LFE's Lisp/BEAM identity, books,
talks, examples, and history while making each promoted route deliver the help
its label promises.

The project joins three kinds of work: establishing technical facts and policy;
deciding the architecture, content, and visual experience; and implementing and
validating those decisions. A working link alone does not establish a useful
guide, and a source build alone does not establish runtime compatibility.

The approved project boundary covers **all seven work groups** in
the [classification](artifacts/first-steps/lfe-work-classification.md), not only
group 4. The [IA/content proposal](artifacts/first-steps/lfe-ia-content-proposal.md)
remains the group-4 design input. Approval of the roadmap establishes the work
tracks; the specific IA, design, compatibility, and distribution choices remain
subject to their recorded decision gates.

## Inputs, established decisions, and open choices

The preserved starting packet is:

- [Continuation handoff](artifacts/first-steps/HANDOFF.md).
- [Work classification](artifacts/first-steps/lfe-work-classification.md).
- [Erlang IA investigation](artifacts/first-steps/erlang-org-ia-investigation.md).
- [LFE IA investigation](artifacts/first-steps/lfe-io-ia-investigation.md).
- [IA and content proposal, Draft 0.1](artifacts/first-steps/lfe-ia-content-proposal.md).

The investigations are dated 8 October 2026. They supply observations,
assessments, source registers, and explicit retrieval limitations. Use them as
the baseline and extend the unanswered areas; do not restart the investigations
or rewrite historical observations to describe a future site.

**Established:** `/docs` is the hand-curated documentation gateway. It will
link to Robert's generated reference when available. ExDoc is anticipated but
unconfirmed. The generated reference's eventual host/path is a low-impact
choice and is not a prerequisite for IA work. Avoid a confusing public
`/doc` versus `/docs` distinction.

**Proposed:** Learn, Docs, Community, Blog, About, and a prominent Download
action; distinct Books and Videos catalogues; a contextual Downloads gateway;
release records; a maintained Start Here guide; consolidation of Use into Docs;
and the Draft 0.1 home-page arrangement. The flat navigation alternative remains
available. Inclusion in this roadmap does not approve these choices.

**Still to establish:** supported LFE/OTP combinations; build versus runtime
claims; historical support expectations; branch/tag and snapshot conventions;
artifact identities and production; recommended setup; exact content readiness;
design foundations; and validation criteria for the chosen experience. A
possible LFE 3.0 transition remains a question, not a promised release.

## Planning and implementation locations

| Purpose | Source of record |
| --- | --- |
| Project planning | `/Users/oubiwann/lab/lfe/lfe/.worktrees/planning/project099-site-ia/`, on branch `planning` |
| Project contract | This `project-plan.md` and sibling [ledger.md](ledger.md) |
| Website implementation | `/Users/oubiwann/lab/lfe/lfe.github.io/` |
| Language/compiler investigation or approved changes | `/Users/oubiwann/lab/lfe/lfe/`; related tooling/book repositories are named per slice |
| Inherited research/design packet | Existing `artifacts/first-steps/`, preserved in place |
| New execution evidence | The owning slice's `artifacts/` directory by default |
| Operator-requested CI planning intake | Existing project `artifacts/ci-matrix-2026-10-09.md`, exact workflow and run projections; cross-arc input, not Slice01 execution evidence |

This intentionally reuses the planning home already established in the language
repository, even though website changes belong in a separate repository. Do not
create a competing website planning tree. The existing three-digit project id
`project099-site-ia` is retained. Local conventions are recorded in [AGENTS.md](AGENTS.md).

The intake snapshot is planning commit
`09bae67ee3ffb5b2365e3dd985b3eb16595a15f0`, language source
`dae7489ebf9588e5f746a34cd4f8ff83d1469eef`, and website source
`036e231b09c48966da26d2227bccb4fa3b295df9`. These identify inspected states,
not deployed website provenance or accepted implementations. The website has
pre-existing uncommitted Makefile/build-tool changes and an untracked
`CC-verification-brief-otp-tutorials.md`; language source has an untracked
`rebar.lock`. Recheck and preserve unrelated work at each implementation intake.

The website uses `src/` content, Liquid layouts/includes, shared YAML data,
Tailwind CSS, and the Rust `lfesite` build wrapper. `_cobalt.yml` selects `src/`
and output `site/`; the installed Cobalt reports 0.20.4. The current wrapper
performs data prerendering, blog resolution, Sass, Tailwind, Cobalt, sitemap,
and Pagefind stages. `src/docs.html` currently redirects to Use. These are
local reconnaissance findings, not a build or live-site verification.

## Definition of done and working boundaries

The current definition of done is satisfied when the adopted arcs compose into
the following outcomes, with project-level evidence in [ledger.md](ledger.md):

1. Technical selection claims have explicit identities, policy, and evidence:
   LFE source, artifact, build environment, runtime, tooling, and documentation
   scopes remain distinguishable. Unknown combinations are presented honestly.
2. Maintainers have reviewed the information architecture and content authority
   model; the adopted routes, navigation, and home-page emphasis are implemented.
3. A reader can reach a validated first success, suitable reference, and useful
   script/library/application/release guidance through the promoted entrances.
4. Release acquisition connects identity, changes, available artifacts or methods,
   installation guidance, and applicable documentation. Books, videos, community,
   blog, and About preserve their useful content and provenance.
5. Design foundations support the chosen experience, including responsive
   presentation, keyboard use, readable code, and accessibility. The existing
   design-system work has been assessed before a replacement is selected.
6. The agreed legacy URL inventory has explicit, verified dispositions that
   preserve topic meaning. Search and outward documentation handoffs describe
   their actual coverage and version scope.
7. Integrated journeys, repository/build checks, publication checks, and ongoing
   ownership are evidenced; findings and deferrals have dispositions; required
   independent review and Duncan's acceptance are recorded.

Publication is part of the proposed project outcome, subject to a later approved
candidate and explicit publication authorization. Acceptance of this draft is
not authorization to merge, push, deploy, issue releases, edit tickets, or send
messages to Robert or anyone else.

The initial roadmap does not commit us to a compiler rewrite, an LFE 3.0 release,
universal historical support, every conceivable binary/container combination,
completion of all four books, a new documentation generator, or a site-generator
migration. Investigations may justify any of these as an explicit expansion.
The initial content estate is the families identified in the saved LFE audit;
Arc01 establishes the exact inventory and coverage denominator.

Robert's generated-reference publication can occur later. The current project
can deliver a functioning curated gateway using verified existing references
and record the later integration with an owner and concrete re-entry condition.
Its publication location must not become a gate for unrelated work.

## Arc roadmap

The twelve-arc roadmap is approved. **Arc01 is active; Arcs02–12 are planned;
none is closed.** Arc01's contract is [arc-plan.md](arc01-evidence-baseline/arc-plan.md).
This supersedes v1.0's proposed/not-opened state. Inputs name
deliverables and decisions, not an assumption that an entire preceding arc
must close before any investigation can begin. Detailed dependencies will be
made explicit in each arc plan. Preparatory research can proceed alongside
other tracks; consequential choices wait for their named inputs.

| Arc / eventual directory | Coherent capability | Inputs and important gates |
| --- | --- | --- |
| Arc01 — `arc01-evidence-baseline` | An explicit inventory and evidence baseline for the project's technical and content estate | Preserved handoff/audits; exact coverage and source-state records |
| Arc02 — `arc02-compatibility-policy` | Evidence-backed LFE/OTP compatibility and runtime policy | Arc01 technical inventory; Robert/Duncan decisions before support promises |
| Arc03 — `arc03-release-distribution` | Preserved, identifiable releases and a reproducible distribution model | Arc01 provenance; Arc02 semantics before final mappings and artifact claims |
| Arc04 — `arc04-ia-content-architecture` | Reviewed page roles, navigation, content authority, and migration intent | Saved Draft 0.1 and Arc01 findings; technical values supplied by Arcs02–03 as available |
| Arc05 — `arc05-design-foundations` | A coherent visual and interaction system for the adopted IA | Existing design work and Arc01 baseline; Arc04 reader tasks and page roles |
| Arc06 — `arc06-experiments-decisions` | Recorded evidence for contested navigation, layout, and selection decisions | Candidate designs from Arcs03–05; trials occur before affected implementation choices |
| Arc07 — `arc07-site-foundations` | Maintainable Cobalt templates, shared facts, routes, and components | Approved Arc04 architecture, relevant Arc05 foundations and Arc06 outcomes |
| Arc08 — `arc08-learning-docs` | Dependable learning, curated documentation, and practical project routes | Arcs04/07; Arc02 validated environment for runnable guidance |
| Arc09 — `arc09-downloads-releases` | Release selection that carries the reader through acquisition and reference | Arcs02/03 authoritative facts, Arc04 structure, Arc07 foundations |
| Arc10 — `arc10-resources-community` | Useful catalogues, participation routes, publishing discovery, and an integrated Home/About | Arcs04/07; adopted resource reviews and the destinations from Arcs08/09 |
| Arc11 — `arc11-legacy-search` | Topic-preserving old entrances and honest search across the agreed estate | Arc01 URL/anchor inventory, Arc04 dispositions, implemented destinations from Arcs08–10 |
| Arc12 — `arc12-acceptance-rollout` | An accepted, published, maintainable result demonstrated as a whole | Integrated delivery of adopted Arcs01–11; independent acceptance and publication gates |

### Arc01 — Evidence baseline

Extend the existing investigations into a concrete content/URL/anchor inventory,
repository and ownership map, and outstanding-finding register. Confirm
heading-only chapters against source and rendered content; distinguish actual
404s from extraction/access failures. Record promoted routes, legacy families,
release metadata, documentation generations, containers, channels, and known
search/accessibility gaps with dates and confidence. Obtain the actual reported
runtime failure and environment before drawing conclusions about its cause.

**Exit evidence:** reviewed inventory and coverage denominators, pinned local
source states, dated external observations where needed, and every unanswered
item assigned to an arc or explicit follow-up. The inherited audits are inputs,
not retrospective proof that this arc is already complete.

### Arc02 — Compatibility policy

Separate source compatibility, compilation environment, emitted code/runtime
compatibility, tool/plugin dependencies, and security/maintenance expectations.
Investigate the reported failure and test relevant existing code against
`develop-compiler` without assuming that branch is the answer. Establish the
supported/tested/unknown distinction, minimum/maximum/explicit-set semantics,
older-version expectations, and the implications of a possible LFE 3.0 change.
Identify any necessary runtime protection or diagnostics for a separate,
explicitly designed implementation slice.

**Exit evidence:** a maintainer-reviewed policy and reproducible matrix for the
agreed combinations, preserving failed, blocked, unrun, and unsupported cases;
a concrete disposition of the reported issue; and precise meanings for any
machine-readable compatibility fields. Site labels cannot substitute for policy.

### Arc03 — Release preservation and distribution

Inventory source tags, branches, historical snapshots, release records, and
actually available acquisition methods. Decide how static historical branches
or snapshots, release mappings, names, precompiled artifacts, packaging, and
containers preserve identity and convey the Arc02 policy. Design the smallest
maintainable release metadata and publication/validation process that supports
the agreed distribution choices. Implement the approved engineering changes
and evidence capture, with exact repository ownership named per slice.

**Exit evidence:** preserved history and traceable mappings, approved naming and
artifact semantics, reproducible production/verification of the adopted artifacts,
and authoritative data consumed by the Downloads experience. An offered source
archive is not evidence of a corresponding binary or tested runtime.

**Added 9 October 2026 at Duncan's request:** preserve the current OTP22–29 CI
matrix before narrowing HEAD coverage. OTP22–26, removed because the recent
inline-docs changes fail to compile, form the first/highest-priority historical
artifact batch: **all five OTP majors on both Linux and macOS**. Select pinned
compatible LFE snapshots and exact build/runtime/platform identities; build,
test and upload the ten targets to the agreed downloads storage. All are P0
with no prescribed order inside the batch. Do not convert removal from HEAD CI
into abandonment of historical combinations. The complete preserved workflow,
run evidence and pending source/storage choices are in the
[CI preservation record](artifacts/ci-matrix-2026-10-09.md). This supplements the
original general preservation/distribution scope; it does not replace it.

### Arc04 — IA and content architecture

Refine Draft 0.1 with Duncan and Robert: page roles; Learn/Docs distinction;
compact versus flat navigation; Home emphasis; Books/Videos discoverability;
Downloads and release relationships; and source authority. Resolve content
consolidation, rewriting, retirement, and applicability at the discovered content
family level. Define maintained homes for recurring facts and the editorial
meaning of resource status. Retain the curated `/docs` decision and make the
generated-reference handoff independent of its future publication URL.

**Exit evidence:** reviewed architecture/design revision, recorded choices and
rationale, content-disposition map, ownership model, and explicit outstanding
technical values. IA structure can be accepted while compatibility values are
still being established elsewhere.

### Arc05 — Design foundations

Assess Duncan's existing design-system work once its actual files are identified.
Explore the intended Material/Carbon/Polaris synthesis, perception, colour,
typography, semantic units, spatial composition, code presentation, imagery,
responsive behaviour, and accessibility. Establish design principles, tokens,
and reusable interaction/component patterns appropriate to the chosen site.
Preserve the alien/Lisp/retro-computing character with clear task labels.

**Exit evidence:** an assessed existing baseline, approved foundations and
representative page/component demonstrations, and explicit design acceptance
criteria. This is a parallel work track; a revised or new system remains a
choice to review rather than an automatic replacement.

### Arc06 — Experiments and decision validation

Use bounded prototypes and representative reader tasks to resolve disputed
navigation, homepage, layout, download labels, and release-selection mechanisms.
Record the question, compared options, controlled context, observations,
limitations, and resulting decision. Evaluate whether a simple release list
suffices before selecting a relationship explorer. Keep experimental results
distinct from adopted policy and successful site acceptance.

**Exit evidence:** decision-linked trial records and dispositions for the agreed
uncertainties, including inconclusive results. Compatibility/compiler trials
belong to Arc02 and artifact trials to Arc03; this arc links their outcomes
where they inform the user experience. It is not a reason to postpone all
validation until the website is finished.

### Arc07 — Cobalt site foundations

Translate the approved architecture and design into the existing site stack:
layouts/includes, navigation/footer, route conventions, shared metadata,
Markdown-to-HTML data handling, and reusable CSS/components. Reconcile the
existing `docs.html` redirect with the curated gateway's adopted route and
keep source/output responsibilities explicit. Extend the existing build and
validation tools only where the adopted contract requires it.

**Exit evidence:** rendered representative pages and checks showing that shared
facts and components behave consistently through the actual `lfesite` pipeline.
Include permalink, escaping, active-navigation, draft, and generated-output
checks appropriate to the touched surface. Publishing a skeleton full of empty
task destinations does not deliver this capability.

### Arc08 — Learning, Docs, and practical guides

Deliver the maintained first-success route, reader-background pathways, and
curated `/docs` sections. Verify and connect script, library, OTP application,
and OTP release instructions; reconcile setup guidance and expected outputs.
Link language, tooling, interoperability, deployment, and upstream OTP reference
with appropriate context. Add Robert's generated reference when ready; until
then, provide useful verified existing reference. Make book/chapter readiness
visible at the point of selection and correct the promoted routes that fail.

**Exit evidence:** the stated environment produces the promised first result
and each promoted practical task result; reference links identify their actual
scope; reader paths reach substantive help. Source/rendered chapter inspection
and executable checks must both be recorded where relevant.

### Arc09 — Downloads and release journeys

Deliver the adopted acquisition gateway, identifiable release records, available
installation choices, and development/historical context using Arcs02–03 facts.
Distinguish latest published release from maintainer-recommended setup. Carry
the chosen release into installation and applicable reference, without implying
that a moving documentation collection exactly matches an older artifact.

**Exit evidence:** representative current, older, and development journeys retain
identity through selection and acquisition; each listed artifact/method actually
exists; compatibility and documentation claims match their authorities. The
presentation does not create promises absent from the policy or evidence.

**Added 9 October 2026:** the first historical binary-download batch is the
Arc03 OTP22–26 Linux/macOS P0 queue. Present actual verified artifacts with their
release/source identity, OTP/platform/architecture context and checksums;
reconcile every requested target against offered downloads. The Downloads gateway
and the physical upload directory/storage are distinct; Arc03 selects storage
and produces artifacts, Arc09 supplies the reader journey. See the
[CI preservation record](artifacts/ci-matrix-2026-10-09.md).

### Arc10 — Resources, community, and publishing

Enrich the book catalogue and deliver the adopted video catalogue using verified
identity, attribution, audience/topic, completion, applicability, source, and
feedback information. Separate help, contribution, projects/libraries, and
channels with reviewed purpose/status. Preserve blog dates, article routes,
archive/category/tag/author access, and useful version-context work. Review
taxonomy drift. Integrate Home's examples, build actions, resource promotions,
and recent writing; retain About's history and clarify present maintenance.

**Exit evidence:** repeated resource promotions agree, catalogue selections
deliver the item described, help/contribution routes are intelligible, and
Home/About connect the implemented site. A listed channel's presence does not
prove activity or response time. Full book rewrites need separately agreed scope.

### Arc11 — Legacy preservation and search

Execute the reviewed old-path/anchor dispositions, including Use, legacy
Current/Dev docs, broken project reference routes, superseded book entrances,
and old blog-domain routes. Distinguish true successors, valuable archives, and
empty placeholders. Verify redirects/relocation pages and fragments through the
actual serving arrangements. Establish search coverage, indexing exclusions,
and clear handoffs to externally hosted books/reference; check relevant retrieval.

**Exit evidence:** the agreed inventory has a checked disposition for every
entry, representative old links preserve their topic, and search claims match
indexed coverage. Preserve historical observations while adding fresh evidence;
a blanket redirect to Home does not discharge a topic-specific migration row.

### Arc12 — Integrated acceptance, rollout, and upkeep

Demonstrate the whole experience across first use, project creation, reference,
release selection, books/videos, participation, and historical arrivals. Run
repository-native checks, inspect rendered output, and evaluate mobile/desktop,
keyboard, accessibility, navigation, search, and external handoffs. Reconcile
the candidate with deployment configuration, prepare publication/rollback
instructions, and publish only after the required authorization. Verify the
published candidate and establish release/resource review and maintenance owners.

**Exit evidence:** a project-level acceptance demonstration, attributed independent
review and Duncan's verdict, publication verification, and a maintained outcome
with all findings dispositioned. The LFE IA ticket remains open until Duncan
accepts the audit and blended direction and authorizes its final disposition;
local planning or a passed build does not close the ticket.

## Coverage and sequencing

| Original work group | Owning arcs / treatment |
| --- | --- |
| 1. Evidence and baseline assessments | Arc01; technical evidence in Arcs02–03; final comparison to delivery in Arc12 |
| 2. Compatibility and version policy | Arc02; consumers in Arcs03, 08, and 09 |
| 3. Release engineering and distribution | Arc03 engineering; Arc09 reader experience |
| 4. Information architecture and content | Arc04 decisions; Arcs07–11 implementation; Arc12 integrated acceptance |
| 5. Design foundations and visual experience | Arc05 foundations; Arc06 trials; Arc07 implementation and Arc12 acceptance |
| 6. Experiments and validation | Arc02 compiler/compatibility trials; Arc03 artifact trials; Arc06 experience trials; per-slice checks and Arc12 composition |
| 7. Scope, decisions, and collaboration | Project governance throughout; explicit maintainer decisions in Arcs02–05 and final acceptance in Arc12 |

The current focus is Arc01's local inventory correction, with
[Slice02](arc01-evidence-baseline/slice02-local-inventory/slice-plan.md)
iteration03 ready for CC after iteration02 review. Slice01 is independently accepted; was: Slice02 initial
inventory ready for CC, and earlier baseline intake ready for CC. Arc04's Draft 0.1 review remains a parallel possibility, not an
opened arc. Arcs02–03 can investigate technical questions while IA is
refined. Arc05 is a parallel design track. Arc06 trials precede the specific
choices they inform; accepted outcomes gate those parts of Arcs07–11 rather
than creating a circular requirement that all design be final before trials.

Implementation can be sliced into useful, reviewable increments as inputs become
ready. Arc12 verifies composition of the adopted delivery; it does not replace
the earlier checks. There is no schedule estimate or priority ranking in v1.0.

## Workflow, authority, and review

Use the framework's default **Two-Contributor Workflow (CDC + CC)** for subsequent
execution. Duncan is the Operator/co-planner; Sofie in this desktop chat is CDC
for this draft and planning continuity. The implementation contributor context
and independent project-close reviewer are not yet assigned. Robert is a
maintainer whose technical and documentation decisions are required where named;
this plan does not assign him tasks or enable CRC.

**Expedited Mode is selected**, effective for this project from Duncan's explicit
request on 8 October 2026. This replaces the initial unselected mode. CC commits
its named changes before review; CDC commits planning/review changes; independently
accepted slices close promptly and the next planned slice opens immediately.
After final-slice acceptance, formally close the arc and open the next arc/first
slice when its roadmap inputs and gates permit. Provide new CC paths as plain
project-relative copy/paste text. Keep explicit path allowlists in commit instructions.

Expedited Mode means no shortcuts, no skipped validation, no weaker evidence or
review, no inferred source scope and no reduction or other change in scope, no
timeline interpretation, and no override of Operator approval gates. Automatic
advancement stops at unresolved decisions or required independent/Operator gates;
it does not authorize publication, external mutations, or role changes.

Duncan's approval and opening request authorize the current planning work in the
established project home. This replaces v1.0's pending roadmap-review gate.
Each arc is detailed when active or next; each slice receives its complete open
set before assignment. Arc01 is active, Slice01 is closed and Slice02 is opened; later child directories
and close records are created only as their work becomes timely. The filenames are `arc-plan.md` and
sibling `ledger.md`; then `slice-plan.md`, `ledger.md`, and `cc-prompt.md`.
Preserve issued prompts; corrections get `cc-prompt-iterationNN.md`.

CC results remain proposed-done until CDC independently checks the actual
artifacts and evidence. Slice acceptance is recorded in `cdc-verification.md`.
Arc closure requires a composition check by a fresh independent context or
the Operator; project closure requires independent review and Duncan's
acceptance. If the reviewer implements a repair, a different verifier checks it.
No self-check is labelled independent reproduction.

Load relevant domain guidance as work becomes concrete: Cobalt for the site;
Rust/Tailwind/JavaScript or Erlang/LFE/tooling guidance for affected code;
scientific-methods for controlled trials; and testing/work-verification for
their gates. Before issuing implementation prompts, perform the framework's
source reconnaissance and prompt-author readiness work. Skill reading alone
does not supply an implementation design.

## Autonomous Mode preflight — 19 October 2026

Duncan explicitly requested: “go ahead and reload it” (collaboration-framework),
then “try out the new Autonomous Mode, using GPT 6 Luna High for CC and
GPT 6.1 Sol High for CDC work”. This authorizes the current approved Arc01,
including its existing iterations, local commits, independent review, closure
and CDC retrospectives. Stop at Arc01's endpoint; do not open Arc02. This
supersedes manual relay/no automatic dispatch in older workflow text for this
scope. Existing Expedited Mode and all Operator gates remain binding.

| Role / actual context | Explicit requested and tool-selected settings | Authority |
| --- | --- | --- |
| CDC: `/root/autonomous_cdc`, Sofie | OpenAI Codex collaboration tools; `gpt-6.1-sol`, reasoning `high` | Arc01 planning, coordination and independent CC review; assigned and acknowledged 2026-10-19 |
| CC: `/root/autonomous_cdc/cc_slice02`, separate contributor context | OpenAI Codex collaboration tools; `gpt-6-luna`, reasoning `high` | Execute the preserved current prompt, self-check and return proposed-done evidence |
| Root `/root` | Existing chat, transport/Operator liaison only | No substantive CDC authorship or planning mutations |

These are explicit role selections, not inherited defaults. No fallback is
authorized; unreported backend identity remains unknown. Default staffing is
one CC implementer and this independent CDC. A fresh reviewer is needed at
arc composition because CDC assembles that close; record that bounded
separation-of-authorship assignment when timely. `cdc-verification.md` remains
the slice convention, with actual author/context and candidate attribution.
No contributor independently accepts its own substantive repairs.

Planning home is `/Users/oubiwann/lab/lfe/lfe/.worktrees/planning`, branch
`planning`, clean at intake `21c02683b27878263b4da6b6cda3e38d0f3ccb61`.
CDC is sole mutation/index owner until a recorded serialized CC handoff; root
is read-only. Arc01 is evidence-only: this non-default planning branch is the
only mutation home. Source repos remain read-only. Any later authorized source
implementation first needs its own dedicated non-default branch, preserving
existing checkouts. No push, merge, publication, ticket, install, external
message, source repair, build/generation or compatibility trial is authorized.
Automatic CI effects must be inspected before any separately authorized
external action; none is requested here.

Current prompt remains `arc01-evidence-baseline/slice02-local-inventory/cc-prompt-iteration05.md`,
created/issued at `21c0268`, SHA-256
`187b4b9a194ac89fb01e3f8dc6fa49d991b18fd9711c8ba6a701a4f4b839bbe9`.
Its creation identity is distinct from the forthcoming execution-intake commit.
Preserve every issued prompt and all original criteria. Slice01 is historically
accepted at `9111268`; Slice02 S2-01–06 are accepted, S2-07/08 open for R16.
Current independent evidence is the Iteration04 section of the existing
[CDC review](arc01-evidence-baseline/slice02-local-inventory/cdc-verification.md).
The fifth refinement is the final existing pass; failure needs an Operator
decision, not a sixth prompt, new reviewer reset or weakened criterion.

Fresh read-only replay on 19 October found zero differences in eight repository
root/HEAD/branch/status projections, eight ordered populations, all 2,839 file
hashes/sizes and eight input hashes, before these mode edits. The eight exact
states and dirty/untracked paths remain in the frozen `inventory.json`; website
is main `036e231b`, LFE clean develop `0aa820e`, and all six related repo HEADs
and branches match. Historical acceptance remains scoped to its checked state.
All 2,839 file witnesses (including Project AGENTS) and eight input pins
remain unchanged. The new mode is recorded only in governing plans; Project
AGENTS' default two-contributor convention remains compatible. CDC owns these
prior plan edits; do not count them as CC changes in the opening-to-return
allowlist. CC compares the four allowed paths from its actual execution-intake
commit through return.

Framework authority is installed `collaboration-framework/SKILL.md` v3.0.0
under `/Users/oubiwann/Library/Application Support/ChatGPT-Profiles/personal/codex-home/skills/`,
with its bundled `knowledge/` guides. CDC loaded the full entrypoint (recovering
truncated output), posture guides, role/workflow/Autonomous contract, PM README
and applicable planning/closing guides, prompt-authoring, agent-coordination,
work-verification and stewardship. The four legacy `~/.agents/skills/` routes
in iteration05 resolve to installed standalone guides and are byte-identical
to their current bundled equivalents (prompt-authoring, row closure, independent
verification, validation gates). They remain valid required material; no prompt
bytes or reading obligations are changed. Receipts establish delivered text and
identity, not understanding; independent review checks actual evidence.

Slices03/04 remain planned, not commissioned. Their general roadmap scopes do
not supply the reloaded PM03 investigation agreement's concrete budgets. CDC
will present finite usefulness/applicability/method/limit proposals through root;
dependent investigation waits for Duncan's decision. Routine checks necessary
to finish the already-agreed R16 remain authorized. Runtime reproduction needs
the actual failure/environment packet; missing evidence cannot be fabricated.
Slice05 and arc composition preserve their existing dependencies and gates.

## Expansion, findings, and plan maintenance

Discussion, investigation, or a child close can introduce new work. Record its
origin, purpose, owner, dependencies, acceptance implications, and decision;
then update this roadmap and project ledger together. Append stable arc IDs
instead of renumbering historical work. A new arc need not wait for project
completion merely because it was absent from v1.0.

Distinguish additions from replacements in the body; record superseded choices
with their prior meaning. Add a dated Version History entry naming the child
or Operator discussion that surfaced the change and why; bump the version.
Expansion is normal. Approval of an evolving roadmap does not silently change
an already issued slice contract or lower its evidence requirements.

Every slice close bubbles findings up to its arc; every arc close checks
composition and bubbles findings up to this project. Compare scope promised
with scope delivered. Deferrals carry an owner, rationale, destination, and
re-entry condition. Failed composition creates remediation work or an explicit
accepted amendment, never retrospective criterion softening.

## Current state

The roadmap is approved and Arc01 is active. Slice01's initial delivery
`ddeecb31abb454d5619f74a178c3d0121024a35e` required R1/R2 corrections.
CDC independently accepted the lossless correction at 9d3a7e1 + 9111268 on
9 October 2026; all eight Slice01 rows are done. Was: four accepted/four open
with iteration01 pending. Initial prompts, failed review and historical
snapshots remain preserved. Its bubble-up requires no structural arc change.
Slice02's initial inventory at b43acb7 + d629b7a required R1–R6. Iteration01
at 30b58c3 + 1b9ab34 restores the contract, provenance indexes, rule witnesses
and all 966 SUMMARY labels/joins. CDC freshly reproduces 109 structural checks
and accepts S2-01. Iteration02 at 89f1ebb3 + cc401dd + ecc4993 repairs scoped reference/rich
extraction and supplies expanded consumer/family evidence. CDC freshly compares
827 ordinary Markdown nodes, 426 HTML attributes, 38 rich Markdown/39 rich HTML
nodes and all 97 reference joins; these bounded projections pass. Its structural
replay passes 109 of 114 checks, with two wrong legacy qualifiers and four stale
derived values producing five failed predicates. That review is historical.
Iteration03 at fb83798b + 6a43ab2 passes all 114
structural checks and 6,844 locator checks, with 61 independently joined rich
input edges and corrected repository/branch witnesses. CDC retains S2-01 and
accepts S2-03's source metadata/declaration scope and S2-05's declared related
entrance scope. Iteration04 at e9dc003 + 2112713 now passes 114/114 structural predicates,
6,940 source locators and 133 focused author/preservation predicates. CDC
accepts S2-02/04/06 and retains S2-01/03/05: six rows done, two open. R16 narrows
the remaining work to the intake receipt's wrong CDC-review source revision
and incomplete Workflow/Current state parent-plan extents, plus their current
return reconciliation. Was: five rows open for R14 author fields/roles and
R15 intake/proof receipts; those substantive source repairs now pass.
No source drift was found; emitted/live behavior remains unclaimed. See the
[CDC verification](arc01-evidence-baseline/slice02-local-inventory/cdc-verification.md).
No capability, sequencing or acceptance amendment is indicated. Slice03 remains
behind its existing acceptance gate. Current CC path under Expedited Mode:
`arc01-evidence-baseline/slice02-local-inventory/cc-prompt-iteration05.md`.
Was: iterations04/03/02/01 and initial `cc-prompt.md`; all are preserved.
This is the fifth and last refinement within the existing fix-loop budget;
non-convergence requires an Operator decision, not an automatic sixth prompt.
Arc composition and project outcome remain unaccepted. All 23 project ledger
rows remain open. See Slice01 CDC verification for exact retained/fresh evidence
and qualified concurrent LFE checkout drift.

Duncan's 9 October request adds the OTP22–26 Linux/macOS highest-priority
historical-download batch to Arcs03/09 and preserves the entire original CI
matrix. A scoped OTP27–29 local CI proposal is prepared against develop; its
local commit is `0aa820e` on `cicd/otp27-plus`; remote execution/green status
and artifact production remain unverified.
Was: the CI request was independent of pending Slice01 correction. That correction
is now accepted; remote CI and historical artifact delivery are still unverified.

The prior first-review questions are resolved for the initial roadmap by Duncan's
approval. Navigation/design choices remain open, and expansion remains expected.

## Version History

| Version | Date | Origin | Change and rationale | Approval state |
| --- | --- | --- | --- | --- |
| 1.0 | 2026-10-08 | Duncan's request in the local desktop chat; inherited first-steps packet | Initial twelve-arc roadmap covering all seven work groups; reuse the established planning home; add project composition ledger; make expansion an expected, tracked part of planning | Draft awaiting Duncan's review |
| 1.1 | 2026-10-08 | Duncan's explicit approval, Arc01/Slice01 opening request, and Expedited Mode selection in this chat | Record roadmap approval; open Arc01 and its first bounded evidence slice; enable expedited commit/close/advance behaviour with unchanged review and approval gates | Roadmap approved; opening authorized; no delivery acceptance implied |
| 1.2 | 2026-10-09 | Arc01 Slice01 initial delivery and CDC review | Record independently reproduced intake and partial row acceptance; issue metadata/reference correction iteration01, retain Slice02 acceptance gate and all original roadmap scope | Routine review/status update under approved Expedited Mode; no design or acceptance amendment |
| 1.3 | 2026-10-09 | Duncan's CI/docs-regression and historical-download request; Arcs03/09 | Preserve exact pre-change OTP22–29 CI; add OTP22–26 Linux/macOS P0 historical-artifact/download coverage and two checkable project rows; retain broader release scope and pending Slice01 review | Operator-directed preservation and priority addition; no support-policy or artifact-delivery acceptance implied |
| 1.4 | 2026-10-09 | Arc01 Slice01 iteration01 CDC acceptance | Record eight accepted child rows and preserved failed history; open planned Slice02 local inventory after source reconnaissance under Expedited Mode; no roadmap, scope or acceptance amendment | Routine acceptance/status advancement under existing Operator authorization |
| 1.5 | 2026-10-09 | Arc01 Slice02 initial delivery / CDC review | Record valid source-population evidence and unresolved R1–R6; issue correction iteration01 inside the existing slice, preserve initial proposals and hold Slice03; no structural or acceptance amendment | Routine review/status update under approved Expedited Mode; all 23 project rows remain open |
| 1.6 | 2026-10-09 | Arc01 Slice02 iteration01 / CDC review | Accept S2-01, retain repaired subclaims; issue iteration02 for R7–R10 semantic repairs with seven child rows open and Slice03 gated; no scope, sequence or criterion amendment | Routine review/status maintenance under approved Expedited Mode; all 23 project rows remain open |
| 1.7 | 2026-10-09 | Arc01 Slice02 iteration02 / CDC review | Retain S2-01 and repaired extraction/consumer/family subclaims; issue bounded iteration03 for R11–R13 coverage, qualification, roles/branches and honest proof; seven child rows open and Slice03 gated; no scope, sequence or criterion amendment | Routine review/status maintenance under approved Expedited Mode; all 23 project rows remain open |
| 1.8 | 2026-10-09 | Arc01 Slice02 iteration03 / CDC review | Accept S2-03/S2-05 and retain S2-01; issue bounded iteration04 for R14 author aggregate and R15 intake/proof receipts; five child rows open and Slice03 gated; unchanged scope, sequence and criteria | Routine review/status maintenance under approved Expedited Mode; all 23 project rows remain open |
| 1.9 | 2026-10-09 | Arc01 Slice02 iteration04 / CDC review | Accept S2-02/04/06 and retain S2-01/03/05; issue receipt-only iteration05 for R16 with two rows open and Slice03 gated; unchanged scope, sequence and criteria | Routine review/status maintenance under approved Expedited Mode; all 23 project rows remain open |

| 1.10 | 2026-10-19 | Duncan’s explicit framework reload and Autonomous Mode request | Record Arc01 endpoint, explicit OpenAI CC Luna High / CDC Sol High selections, actual CDC context, serialized ownership, preserved fifth prompt and source state, unchanged file fingerprints and remaining investigation/acceptance gates | Mode authorized; no capability or acceptance amendment |
