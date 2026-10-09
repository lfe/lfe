# Slice02 — CDC verification

## Initial-pass review — 9 October 2026

**Reviewer:** CDC, Sofie in the desktop planning context, independent of CC.
**Workflow:** Two-Contributor, Expedited Mode.
**Assignment:** `arc01-evidence-baseline/slice02-local-inventory/cc-prompt.md`;
opening `b5a7c50b0cb761e8e857cab6731cde103874b081`, SHA-256
`1ee213ebfed473a51572264f1445d188db84b1e8f070bb0ebc9288ad64ebdd49`.
**Candidate:** `b43acb739220ae151be752281a5d6d2cbf1ac79a`, followed by
closing receipt `d629b7a56cd3470cacdcaf1ac09efe314fdd7e07`.
**Reviewed state:** clean planning worktree at d629b7a; all eight source root
projections and all recorded working fingerprints freshly match the candidate.
**Verdict:** correction required; all eight rows remain open. The proposed
S2-03/S2-04 deferrals are not accepted. Slice03 is not opened.

### Independently reproduced evidence

CDC parsed all three complete JSON objects, inspected the seven-file delivery
and receipt diffs, original ledger contract, execution/summary/closing records,
and source witnesses. Read-only standard-library Git/hash/set/locator queries
in `/tmp/project099-cdc-slice02-review.py` produced **98 checks: 90 passing,
8 failing**. The temporary script/results are incidental review aids; this
record identifies the methods, populations, results and exceptions durably.
These checks establish their named properties, not overall semantic acceptance.

- Replayed the recorded root/HEAD/branch/status commands for all eight repositories.
  No drift: website main at 036e231b remains dirty with unrelated tool/Makefile
  changes and its untracked brief; LFE develop at 0aa820e is clean. No restoration,
  build, network check, source edit or generation was performed.
- Replayed all eight `git ls-files -z` populations and exact ordered membership:
  website 385; manual 741; tutorial 49; casting-spels 85; SICP 129; LFE doc 53;
  rebar3_lfe docs 32; legacy docs 1345. Website partition queries reproduced
  124 posts, 27 templates, 12 data, 35 pages and 187 other paths, disjoint and
  exhaustive. Omitted-path, duplicate-path and duplicate-declaration-ID controls
  were rejected by the same predicates that accept the complete sets.
- Rehashed **all 2,839 keyed file records**, checked byte sizes, Git modes and
  HEAD blob hashes using per-repository `git cat-file --batch`; reproduced each
  `matches HEAD` / `working bytes differ from HEAD` qualification.
- Rechecked all eight input pins (five inherited inputs and three accepted
  Slice01 artifacts), preserved prompt opening bytes/digest, shared intake time
  and snapshot joins. Both copied 104-row `baseline_source_catalogue` values
  equal accepted Slice01 completely. All 29 assessment and 16 question IDs
  remain present. The separate `source_ids` field is wrong, as R5 explains.
- Replayed `cobalt debug files --color never` in the website root: exit 0,
  empty stderr, 346 distinct members. Adding `src/` to its relative output
  reproduced the tracked set minus the exact 39 data/template exclusions;
  no scanner-only member. This does not establish output routes. Other CC CLI
  receipts remain attested; unsupported collection queries were not retried.
- Unique IDs pass for content units, routes, anchors, fragments, occurrences,
  destination groups and residuals. All 1,423 occurrences partition exactly
  into the 801 literal-destination groups. All 135 fragment occurrence joins
  resolve with matching raw targets. These do not prove complete extraction.
- Recursive inspection checked **4,233 source locators** against current
  qualified bytes/HEAD/line ranges. Six dirty-Rust locators have null working
  hashes. One absolute byte span crosses a line outside its recorded extent.
  Four coverage tallies, dangling residual references and the source-ID index
  account for the other six failed predicates; see R3/R5/R6.
- Delivery is exactly the seven authorized paths; the receipt commit changes
  only closing-report.md. JSON parsing, final newlines, trailing whitespace
  and `git diff --check b5a7c50 d629b7a` pass.

Supplementary independent source comparisons used
`/tmp/project099-cdc-slice02-semantic.py`, Mike Farah `yq -o=json . -`, a separate
line-oriented SUMMARY projection and Python HTMLParser:

- All **159 units** match the complete page/post path set. All 150 present
  frontmatters match source text with the stored convention of omitting the
  terminal newline; the other nine are absent. Entire parsed values and every
  recorded field's presence/value match. This is metadata proof, not a complete
  body, route-rule or resource audit.
- All **966 SUMMARY links** match source destination, line and indentation:
  673/45/61/187. **757 labels differ**: manual 536, tutorial 28, casting-spels
  49, SICP 144. Lost spaces are exemplified below. This projection does not
  independently accept every local-target status or full chapter content.
- All **22 website explicit-name anchor records** map to `meta@name`, rather
  than named anchors. Legacy indexes similarly mix metadata names with genuine
  `a@name`; correcting the classification must retain those genuine declarations.

Preliminary reviewer attempts are retained here: an input-path basis error
stopped the first checker before results; it was corrected. An overly literal
HEAD-qualification predicate expected different wording and falsely failed;
the actual byte comparison was correct. A too-strict span-end predicate flagged
255 heading records whose extents legitimately include a trailing newline;
containment against the recorded line envelope leaves the one real multiline
attribute defect. The supplementary raw-frontmatter comparison initially
included the terminal newline excluded by the stored convention; adjusted
source comparison passes all 159. These were reviewer-checker limitations,
not additional CC defects. The earlier 94-check run is superseded by the
98-check run above, which adds catalogue equality, scanner and ID controls.

### R1 — Restore the issued acceptance contract

CC replaced the original eight-column ledger with five columns, shortened
criteria and removed `Verify`, `Significance`, `Origin` and `Notes`. The original
contract is accessible at `b5a7c50:project099-site-ia/arc01-evidence-baseline/
slice02-local-inventory/ledger.md`. Restore its IDs, exact criteria, verifier
text, significance and origin. Preserve CC's initial proposals as historical
evidence and append correction evidence. All statuses remain open. CDC's added
review note does not approve the replacement table or amend acceptance.

### R2 — Finish source extraction and classify declarations correctly

Reference-definition associations, rich YAML spans/mirror relationships,
JSON/feed link fields, and code/comment/example accounting are existing Slice02
scope. Whole construct classes cannot be moved downstream merely because the
first parser did not handle them. Exact, inspected candidates may legitimately
remain unknown; expansion/rendered behavior is separate from recording source.

Concrete witnesses: `src/_data/home/books.yml` lines 8–15 has nested
`[![sicp][img-src]][img-link]`, two definitions, and the mirrored cover HTML.
Uses need pointers to definitions and individual YAML field/index/source spans.
`src/blog/feed.liquid` has **zero occurrence records**, despite version,
home_page_url, feed_url, item id and url strings/expressions. In
`src/blog/rss.liquid`, only atom:link@href is recorded; channel/item link and
permalink GUID text are omitted. Read complete enclosing Liquid loops and
record unexpanded expressions without pretending the template is valid JSON.

The 22 `meta@name` records are metadata, not explicit fragment declarations.
Use tag/attribute semantics for name-based anchors, retaining legacy `a@name`,
explicit IDs (including dynamic tokens) and source-authored heading attributes.
Inspect every residual and account for parsed/excluded/unknown candidates with
exact locations/context; parser success and zero unmatched AST nodes do not
establish lexical population completeness. Fresh semantic checks must cover the
full declared population, not only these examples.

### R3 — Repair actual rule witnesses and locators

`route-rule-cobalt-default` cites a fabricated `posts.permalink` value ending
`/{{slug}}/`. Actual `_cobalt.yml` lines 4–6 declares
`posts.default.permalink` as
`/blog/{{categories}}/{{year}}/{{month}}/{{day}}/{{hour}}{{minute}}-{{slug}}`.
Preserve that exact key/value and explicit-post override applicability; static
HTML, feeds and pagination must not all inherit a generic default-rule claim.
Existing atomic pagination records are useful; attach their actual applicable
metadata and rule references instead of relying on generic umbrella wording.

Null working hashes occur in mechanism sources for util.rs, prerender.rs,
blog_resolve.rs and build.rs (two records), plus the blog promotion consumer.
The correctly qualified file hashes already exist in inventory.files. Backfill
from matching current captured bytes; inspect actual symbol ranges. Blog's
consumer range `1–1` cannot witness its source-to-generated-data operation.

`anchor-4b8b1c1f7244c4db` in `src/_includes/home/masthead.liquid` records lines
16–16, but its byte span `[745,787]` crosses lines 16–17 and its `id` is on 17.
Correct the attribute span, enclosing raw context and line envelope together.
Apply the same locator/raw-token test to every changed and newly added record.

### R4 — Trace real data fields and consumers

The six aggregate consumer entries are not complete, validated promotion joins.
Books/videos/callouts name nonexistent YAML fields `books`/`videos`/`callouts`
where the actual sequence is `items`. The site.yml entry fabricates both `nav`
and `site.data.site.nav`; real download fields feed nav.liquid lines 62–71.
`editorial slots` is a description, not blog.yml's actual field path. Blanket
`template conditions/data-driven` wording does not identify the source branch.

Enumerate all shared promotional fields and directly consuming data accesses,
including features/masthead/callouts and generated-blog mechanisms. Resolve
loop aliases and indices to real fields; retain include/layout chains and exact
conditions. A dynamic target can stay unexpanded while its source ownership is
fully traced. The four Build It seeds and correct existing literal groups remain
valuable; do not erase duplicates or mirrored source occurrences to simplify joins.

### R5 — Correct related labels, inherited indexes and family meanings

The manual SUMMARY line 1 is `[LFE MACHINE MANUAL](README.md)`, while its stored
label is `LFEMACHINEMANUAL`; all four books have this problem. Preserve source
label text and render AST Space/SoftBreak tokens meaningfully, with raw label
kept separately if markup requires interpretation. Reconcile all 966 labels,
targets, hierarchy and local joins, plus eight legacy index declarations.

The full 104-row inherited catalogues are preserved, but `source_ids` lists 45
assessment/question IDs rather than the source catalogue's L01–L43/S01–S61.
Correct every copied index consistently without modifying inherited raw cells,
locators, findings or accepted artifacts.

Eleven matching family IDs do not mean eleven correct coverage rows. The saved
LFE report §3.3 defines Main gateway as Home/Learn/Use/Community/About and
Technical publishing as Blog/browsing indexes/posts. Current rows redefine
both as docs entrances. `src/_data/home-books.yml` does not exist; the real
path is `src/_data/home/books.yml`. Papers are linked from Learn. Restore all
eleven names/boundaries from that table, and attach actual current source
references, coverage limits, owners and next checks. Keep the curated `/docs`
decision as an additional observation, not a substituted inherited family.

### R6 — Reconcile current evidence and honest handoffs

Both inventory/route coverage copies say 1,295 occurrences and 601 residuals;
current arrays have 1,423 and 543. The lexical gap still cites 100 nonexistent
`resid-*` IDs rather than current `residual-*` records. The scanner gap remains
open despite the later exact comparison. Recompute all derived tallies and
references from authoritative records and distinguish resolved parser work from
real remaining unknowns; no count alone establishes complete extraction.

Execution initially says eight inherited assessments, corrected to 29 later.
Retain initial attempts as historical and make current receipts/readback agree
with actual commands/results and complete conditional context coverage.
The close proposes routing local consumer work to Slice04 and chapter checks
to Slice05. Existing owners are Slice02 for source extraction/consumer tracing,
Slice03 for endpoint/source/rendered readiness, Slice04 for technical/runtime
intake, Slice05 for baseline reconciliation. Correct the current bubble-up and
family/gap handoffs while preserving the initial proposed routing historically.

### Complete row walk

| ID | CDC status | Independent evidence / remaining work |
| --- | --- | --- |
| S2-01 | open | Complete root/population/file/pin checks pass; R1 contract and R5 incorrect provenance index need correction. Valid population evidence may be retained. |
| S2-02 | open | Source mechanisms and scanner reviewed; R3 false default/null locators and R4 consumer semantics prevent acceptance. |
| S2-03 | open | All 159 metadata records pass; R2 construct/anchor classification, R3 rule/locator and R6 coverage work remains. Proposed deferral not accepted. |
| S2-04 | open | Existing literal groups/fragment joins pass; R2 complete extraction and R4 real consumer joins remain. Proposed deferral not accepted. |
| S2-05 | open | All related filename sets and 966 TOC target/line/indent projections pass; R5 label repair, local joins and related semantic declaration checks remain. |
| S2-06 | open | R5 family meanings and R6 resolvable gap/owner/re-entry handoffs do not yet satisfy the inherited boundaries. |
| S2-07 | open | Intake receipts inspected at attested strength; readback permits moving required extraction downstream. R1/R2/R4/R6 require actual correction intake, commands and coverage proof. |
| S2-08 | open | Exact scoped delivery, pins, source and prompt preservation pass; R1 contract preservation and R6 current row walk/bubble-up remain. |

### Bubble-up, retention and next assignment

The inventory supplies a useful source census and metadata baseline but has not
delivered the complete local declaration/promotion capability. No capability,
criterion, sequencing or structural arc amendment is indicated: these are
repairs to the existing contract. Update status/current assignment at all three
scales; retain the Slice03 dependency. No source/code/CI changes are authorized.

What worked: independent whole-set and whole-value comparisons isolate valid
evidence from semantic defects. The copied catalogue itself prevented loss even
though its derived index is wrong. Keeping tracked/scanner populations separate
avoids false route claims. What must improve: AST success, plausible aggregate
witnesses and family-ID counts need source-semantic comparisons.

For iteration review, the reproduced root/population/fingerprint/pin,
159-metadata, TOC target/line/indent and literal-group/fragment-join subclaims
may be retained only where complete value comparisons and unchanged relevant
inputs establish continued applicability. This is subclaim retention, not any
row acceptance. Every affected/new extraction, label, rule, consumer, locator,
coverage/reference, contract and handoff requires fresh independent verification;
source preservation and scoped delivery receive fresh return checks.

**Next owner:** CC executes
`arc01-evidence-baseline/slice02-local-inventory/cc-prompt-iteration01.md`,
relayed by Duncan, then returns to CDC. Initial prompts, candidate and failed
review remain preserved; all eight statuses stay open until that review.

### CDC handoff validation

The planning delta is exactly eight paths: project plan/ledger, arc plan/ledger,
slice plan/ledger, this review and the new iteration prompt. Project/arc criteria,
verifiers, significance and origin compare equal to d629b7a; versions and current
assignments agree. All six CC artifact/report files are byte-identical to the
candidate; the issued initial prompt still equals b5a7c50. All 23 project rows
are open, A1-01 alone is done among nine arc rows, and all eight slice rows are
open. Rendered local Markdown links, UTF-8/newlines/trailing whitespace and
`git diff --check` pass, including both new documents. A preliminary link regex
treated the backticked source example `[LFE MACHINE MANUAL](README.md)` as an
active local link; excluding inline/fenced code resolved that checker false
positive. Two draft patch-context attempts made no changes before the correct
bounded edit applied. These are handoff-authoring attempts, not CC defects.
Final pre-commit source/pin replay again returns 98 checks, 90 passing and the
same eight recorded candidate failures; no new source drift. No artifact repair
was implemented by CDC. Exact-path commit and clean-status verification finish
this handoff; CC must still execute iteration01 and return its evidence.

## Iteration01 review — 9 October 2026

**Reviewer:** CDC, Sofie, separate from CC; Two-Contributor, Expedited Mode.
**Assignment:** `cc-prompt-iteration01.md`, opening
`fba770614bdd6216e243ed8119a64f41c2678d75`, SHA-256
`0fd63608b73e2e7b9a014a0dda371a06384856fdd5ed8cf5dae0235d1ad4a6d7`.
**Candidate:** evidence `30b58c3bddbe20d0ab8233e4fcc012b87ffe2d7d`, receipt
`1b9ab342fff9a0adde53c54ceeda03e118f1ac4d`. Clean planning worktree at receipt.
**Current verdict:** S2-01 independently accepted; S2-02–08 remain open.
Another correction is required within Slice02; Slice03 stays planned.
This section supersedes the initial review's current status, preserving it as history.

### Fresh proof and retained subclaims

CDC inspected the complete JSON projections, changed source claims, seven-path
evidence delta and one-report receipt, original contract and current return.
Read-only Git/hash/set/locator comparisons in the incidental
`/tmp/project099-cdc-slice02-iteration01-structural.py` produced **109/109
passing checks**, including **4,495 locator identity/hash/range/envelope checks**.
Those properties are structural proof, not semantic acceptance.

- All eight live root/HEAD/branch/status projections, exact ordered populations,
  all 2,839 file hashes/modes/HEAD comparisons, website partitions and eight
  input pins match. No source drift or source mutation was found.
- All seven immutable inventory values (`assignment`, `captured_at`, `planning`,
  `inputs`, `repositories`, `populations`, `files`) equal d629b7a completely.
  All 159 content units equal their predecessors except the authorized derived
  `default_witnesses` and added `route_rule_applicability`. Raw/parsed metadata,
  fields, IDs and source identities are preserved. Both issued prompts equal
  their opening objects. The 104-ID provenance indexes and complete inherited
  catalogues now agree, retaining 29 assessment and 16 question identities.
- The eight-column ledger's original Criterion/Verify/Significance/Origin cells
  are restored. Historical proposals remain labelled. R1 is resolved.
- The actual default key/value and qualified dirty-Rust witnesses are repaired;
  metadata-name retirement is recorded. R3's reported defects are resolved.
  CC correctly challenged my masthead example: `[745,787]` was class text,
  not the ID. Fresh bytes support the corrected `[984,1026]`, line 18. My old
  example must not be imposed on the correction.
- Destination groups partition all 1,439 current records into 809 groups;
  all 135 fragment joins resolve structurally. These counts are not accepted
  as complete or semantically correct because of the defects below.
- Separate Pandoc GFM AST and literal line projections reproduce **966/966**
  SUMMARY display labels/targets, raw labels, source lines, indentation,
  hierarchy and local tracked-source status: 673/45/61/187. This resolves the
  SUMMARY-label part of R5; it does not accept all entrance/family joins.

The semantic comparisons used the incidental
`/tmp/project099-cdc-slice02-iteration01-semantics.py`: all 12 shared-data files
were independently decoded with yq, their 61 Markdown scalars separately parsed
with Pandoc GFM, and source ownership/targets compared with the register.
Full enclosing books/masthead/features/post/blog-authors template contexts and
prerender/blog resolution functions were inspected for the claims below.
All eleven family reference arrays were queried against their inherited meanings.

Reviewer attempts/limits: an initial whole-content-unit equality predicate
wrongly rejected authorized derived rule changes; excluding only the two named
derived keys yields the 109/109 result above. The first scalar comparison used
bracket paths against dot-number paths and falsely marked all 38 Markdown nodes
unmatched; canonicalizing just array-index notation yields the 19 real mismatches
below. Temporary source lookups using `plan.md`, a planning-root `tools/` search,
and `tools/site/` were corrected to canonical plan names and website
`tools/lfesite/`; these were reviewer lookup failures, not missing sources.
Pandoc is an independent source oracle here, not a Cobalt/pulldown rendering claim.

### R7 — Scope reference definitions and finish rich-field extraction

Eight reference uses exist, but **six resolve to the wrong book**. All four
`items[n].cover_md` scalars reuse `img-src`/`img-link`; all uses currently point
to SICP's definitions on lines 55–56. For example, `occ-fa824c3b370e2fe8`
at books.yml line 12 should target `https://cnbbooks.github.io/lfe-manual`,
with its definition at line 10, rather than SICP. The nested image must use
line 9's manual cover. Tutorial lines 25–27 and casting-spels lines 40–42
have the same error. All eight definition keys say `items[0]`,
misidentifying the six definitions owned by items 1–3. Namespace references by
repository, file, exact scalar/index and normalized reference label.

Of **38 independently parsed Link/Image nodes in 61 rich Markdown fields**,
19 have no matching same-field/correct-target occurrence: these six wrong
reference targets plus **13 omitted inline nodes**. Ten are author
`links[n].label_md_inline`; the others are callouts `items[0].prefix_md_inline`
(`/images/discord-logo.png`), features `items[0].icon_md_inline`
(`/images/lisplogo-alien-tech-grey.png`) and `items[2].title_md_inline`
(`https://www.youtube.com/watch?v=rRbY3TMUcgQ`). Their HTML siblings do not
stand in for their Markdown source occurrences. Relative omissions also show
why the absolute-URL scan's zero-unclassified result is insufficient.

Mirror `mirror-2b18e5eb74670456` claims author Markdown bytes `[197,323]`,
lines 7–8, including the next HTML sibling; its Markdown occurrence list is
empty. Verify scalar boundaries and raw tokens as well as line containment.
Repair all applicable fields/definitions/mirror joins, not only these examples.

### R8 — Resolve exact consumer ownership and real conditions

The consumer joins still use field-name similarity instead of alias scope.
Books `site.data.home.books.title` and `book.title` both list root `title`
and item `items[*].title`; root and item `link_url` similarly collide.
Build It `build.title` also incorrectly includes root `title_html`.
Only a scope-resolved access can establish a direct consumer edge.

- Excerpts consumer `consumer-6563d80aab573532` omits `items[*].code_html`
  and `items[*].desc_html`, interpolated at masthead lines 35/39, including
  the first excerpt's `/images/repl.png` resource. Its two loops and
  `forloop.index` branches are substantive context.
- Masthead `logo_image` and features `link_url`/`link_text` are presented as
  consumed by their includes, which never access them. Retain such fields as
  having no observed direct consumer; separate prerender inputs from direct
  interpolations. Do not infer a consumer from a similarly named field.
- Authors' aggregate `field_path` includes nonexistent `[author-id].url`;
  their five secondary website witnesses carry the **planning** HEAD b5a7c50.
  They lack proper qualified access locators. Record post's map-key selection,
  assignment to `author`, github_user/elsif-avatar branches, links-size check
  and `link.label_html` loop, plus each actual consuming layout.
- Blog has two identical aggregate source-consumer claims
  (`consumer-603472390ecb9b5f`, `consumer-621b5cb7daec4aab`). Normalize genuinely
  duplicate claims with an explicit mapping while retaining distinct edges.
  `blog_resolve::run` lines 107–135 writes `authors_sorted` and default covers
  into `blog_resolved`; blog-authors.liquid line 24 uses `authors_sorted`.
  This generated edge is absent. Covers also have direct `site.data.blog`
  consumers; record both actual paths rather than replacing one with the other.
- Nav is included by twelve layouts, not just home. `current.lfe` at post
  line 80 is conditional on `page.data.written_for` (lines 75–83). Blanket
  “context retained in locators” prose is not a recorded branch or chain.

Trace every declared promotion field/access through its exact root, loop/index
alias or generated transformation, role, conditional extent and callers.
Field existence alone cannot prove a consumer association.

### R9 — Join eleven families by evidence meaning

All eleven inherited names/boundaries are restored, but their reference lists
remain heuristic. Historical papers has **zero current references** while
Learn contains three local PDF links at lines 120–122 and Moonual/Chineual at
154; it instead attaches all 61 Erlang source IDs without membership reasons.
Main gateway omits its five actual page keys. Community omits community.md.
Book catalogue has no book entrance joins despite four README/config/SUMMARY
sets; Earlier documentation generations has no legacy-index entrance joins.

Audit all eleven arrays, not just their counts or resolvability. Cite a concrete
membership reason for each inherited/current association, distinguish direct
family coverage from contextual cross-links, include real source populations
and avoid attaching every source that happens to match a broad word. Existing
raw inherited catalogues remain correct and must stay unchanged.

Entrance label interpretation also remains inconsistent outside SUMMARY:
manual README lines 15/17 `[Writing Phases]` is stored `WritingPhases` despite
claiming a complete Markdown projection. Apply the corrected inline-text
semantics across all declared entrance projections, retaining raw tokens.
This is entrance-source repair, not a full book-body audit.

### R10 — Classify evidence roles and reconcile the actual proof

Four non-link feed fields (`title` twice, `date_published`, `summary`) are
classified as navigation and included in destinations. The JSON Feed schema
version URI also needs its technical-identifier role distinguished from reader
navigation. Keep metadata evidence in an appropriate non-resource projection;
derive groups/counts only from actual source resources. Retain exact expressions.

Reconcile all coverage/gaps, mirrors, references, consumer/family counts and
current summary/close/readback after repair. The 951 absolute-URL candidates
and 543 template tokens prove their named scan/classification properties only;
they cannot establish relative/scalar/consumer completeness. Keep scope and
location in candidate accounting so an HTML sibling cannot cover a Markdown use.

CC's reused 98-check harness still tested the **initial** b5a7c50→d629b7a
delivery scope/receipt. CDC's fresh replay updates those predicates to the actual
fba7706→1b9ab34 correction/receipt. Correct any current claim that this old
predicate established iteration01's scoped committed return. Record actual
commands, results, semantic denominators and triggered context read extents;
retain CC's 98-check run as its attested attempt, not as overall acceptance.

### Current complete row walk and next action

| ID | CDC status | Evidence and remaining obligation |
| --- | --- | --- |
| S2-01 | done | Fresh eight-root/population/fingerprint/pin replay, exact immutable values and repaired 104-ID provenance index; restored original contract. Independent reproduced acceptance. |
| S2-02 | open | Actual default/mechanism repairs retained; R8 exact consumer/generated ownership and branches remain. |
| S2-03 | open | 159 metadata values and repaired declaration witnesses retained; R7/R10 complete source-construct/fragment coverage must be reconciled before whole-row acceptance. |
| S2-04 | open | Structural literal grouping/fragment joins pass; R7 missing/wrong rich sources, R8 consumers and R10 roles prevent acceptance. |
| S2-05 | open | Complete related filename sets and all 966 SUMMARY comparisons pass; R9 related entrance labels/joins remain. |
| S2-06 | open | Literal eleven-family boundaries pass; R9 membership and R10 real gap/check handoffs remain. |
| S2-07 | open | Correction receipts reviewed at attested strength; source omissions contradict completeness/readback; R7–R10 fresh actual proof and bounded context receipts remain. |
| S2-08 | open | Seven-path evidence/one-report receipt and source/input/prompt preservation reproduced; R10 current all-row return and calibrated bubble-up remain. |

What worked: the original contract, census, metadata, rule witnesses and SUMMARY
labels have been recovered without source drift. What must improve: same-file
membership, field existence, locator range validity and green counts must not
substitute for scalar scope, alias resolution or semantic family membership.
No capability/sequence/criterion amendment is needed. This is the second
corrective refinement within the existing slice; no source-extraction deferral
is accepted. Plans/ledgers record the new status; project composition remains
open, with A1-01 alone done and all 23 project rows open.

**Next owner:** CC implements [cc-prompt-iteration02.md](cc-prompt-iteration02.md),
relayed by Duncan, then returns fresh evidence to CDC. S2-01 stays accepted only
while its unchanged inputs/subclaims continue to apply; proposed completion of
the other seven rows remains open pending independent review. No later slice
is opened and no artifact correction was implemented by CDC.

### Iteration02 handoff validation

The eight-path planning delta contains only project/arc/slice plans and ledgers,
this appended review and the new iteration02 prompt. All original criteria,
verifiers, significance and origin are equal to 1b9ab34; project/slice Notes are
unchanged, and the arc child-status note is updated explicitly. All six CC
artifact/report files and both issued prompts are byte-identical to 1b9ab34.
The historical review is retained as an exact prefix. Versions/current assignment
agree: project v1.6, arc v1.4, slice v1.2; 23 project rows open, A1-01 alone done,
S2-01 done and seven child rows open.

The incidental `/tmp/project099-cdc-slice02-iteration02-handoff.py` passes
exact tracked-plus-untracked allowlist, status/criteria/preservation, rendered
local Markdown links, UTF-8/newlines/whitespace, manifest source-path existence,
eight fresh live root projections/ordered populations, all 2,839 working hashes
and all eight input working/pinned-object hashes. A draft handoff predicate
incorrectly demanded the dynamic arc status note stay identical; it was bounded
to invariant contract cells and project/slice Notes. A missing planning-root
lookup in that helper was corrected before the passing source replay. An
unverified illustrative definition ID was removed from the unissued prompt;
the construction now requires exact lookup from the real scalar-scope record.
These were author/checker draft corrections, not additional CC defects.
No source/CC artifact repairs or scope changes were made by CDC. Final
`git diff --check`, exact-path commit and committed-tree/status checks finish
this handoff; the new prompt must still be implemented by CC.


## Iteration02 review — 9 October 2026

**Reviewer:** CDC (Sofie), independent of CC evidence production; Two-Contributor,
Expedited Mode. Candidate: evidence `89f1ebb3bfcb5f12cf693da0102068c168afed9d`,
receipt `cc401ddded3a320c8750325aa6cfad30ea0b84e4`, receipt correction
`ecc4993685d607c5638f6a1df451e7065a67dcc3`. Earlier partial evidence/receipts
2ac918f/e1d64e6/4b94230/856dc6c remain historical. Assignment:
[cc-prompt-iteration02.md](cc-prompt-iteration02.md), opening
`295057845901fdc718a9e8c1d3d0de3055fe39c7`, prompt SHA-256
`8fd29354f68e53b92b6b2562949bdc99552bd8be6ba8193fbd5762e4a382480b`.
Planning is clean at ecc4993 before this review; opening-to-return changes are
exactly five authorized CC paths, with the final receipt changing only the
closing report. This is a partial acceptance review, not slice closure.

### Fresh checks and retained evidence

CDC independently re-queries all eight root/HEAD/branch/status projections,
ordered tracked populations, all 2,839 working hashes/sizes/Git modes/recorded
HEAD qualifications, eight input working/pinned-object hashes, immutable
assignment/capture/planning/input/snapshot/provenance values and original ledger
criteria. All pass without source drift. Website remains main at
036e231b09c48966da26d2227bccb4fa3b295df9, with its unrelated fourteen modified
Makefile/tool files and one untracked brief untouched. LFE remains clean develop
at 0aa820e93bb592a821fe0a50b1db7e8926e56b30. S2-01's independent Iteration01
acceptance therefore remains applicable; its criterion and identity inputs are
unchanged and no new finding undermines the filename/provenance claim.

The incidental structural replay reports **114 checks: 109 pass, 5 fail**.
Of **6,522 locator objects**, two fail repository-HEAD qualification; their raw
bytes and spans pass. There are zero span/line-containment failures. Four other
failed predicates are the copied resource/group tallies described in R11.
This locator predicate covers identity/range validity, not whether a span proves
its semantic claim: the five author `consumer_sources.head` values and two
misnamed branch witnesses below require separate source review.

Independent whole projections establish these bounded properties:

| Population / method | Result and limits |
| --- | --- |
| Four complete SUMMARY Markdown projections | All 966 display/raw labels, targets, lines, indentation and local-status joins match; chapter bodies remain outside scope |
| All 61 rich Markdown scalars in twelve data files, Pandoc GFM Link/Image projection | All 38 nodes match same-field source records; eight book reference uses now bind to their own scalar definitions; thirteen earlier inline omissions repaired |
| All recorded reference uses, source-scope/raw-use/target/pointer joins | 97/97 pass; manual-use/SICP-definition negative control rejected; 84 definition records remain separately represented |
| 147 website Markdown/wiki files, CommonMark extension AST Link/Image projection | 827 nodes match 827 records by file/target multiset; no missing/extra nodes or parser failures; this comparison alone is not a position or renderer-equivalence proof |
| HTML href/src attributes in those Markdown raw nodes and forty native HTML/Liquid/XML files | 426 attributes match 426 ordinary HTML records; zero missing/extra nodes; other attribute/script constructs are not covered by this predicate |
| Twelve YAML trees, literal URI/relative-path/hash-fragment field projection | 34 fields match 34 yaml-field records |
| All rich HTML scalars, parsed href/src attributes | 39 attributes match 39 same-field records, including the explicit unpaired REPL HTML image |
| JSON Feed roles | Five metadata/technical fields retained separately; no title/date/summary entry remains a navigation occurrence |
| Twelve consumer records versus all actual rich input paths | 61 inputs exist, but 26 lack a matching prerender-input role path; see R12 |

The eleven family walks and 1,872 membership-evidence records are present;
structural IDs/locators and retained filename/TOC evidence remain useful. Those
counts alone are not semantic acceptance of every membership reason. Actual
source context supports the repaired root-versus-item access distinctions,
excerpts HTML accesses, author lookup/link guards, generated authors/default
cover edges and direct-versus-contextual family treatment. Preserve these valid
subclaims; do not repeat their entire investigation merely because R11/R12
share a file with them.

Reproduction mechanics are incidental `/tmp/project099-cdc-slice02-iteration02-`
helpers `structural.py`, `semantics.py`, `constructs.py`, and `joins.py`, each
executed with `python3`, recording JSON results there. They independently query
Git/source bytes, use installed `yq -o=json` for complete YAML projections,
Pandoc JSON ASTs and standard-library HTML parsing. They are not product
artifacts or a new maintained validation requirement. Durable methods,
denominators, results and failures are recorded here so review does not depend
on the continued existence of /tmp files.

Failed/draft checker attempts are retained as methodological limits: an initial
whole-Markdown GFM projection yielded twelve synthetic nested autolinks absent
from the source construct census; source-appropriate CommonMark projection
resolved that difference. Early helper assumptions that all reference uses were
YAML, dot-index spelling was already bracketed, hash-only fields were excluded,
or Unicode-normalized IDs equalled raw labels were corrected before the
reported joins. These were checker limitations, not additional CC defects.
No build, source test, rendering, generation, network or publication was run.

### R11 — Reconcile derived coverage and repository qualification

Both inventory.json and route-anchor-register.json remain byte-identical to
2950578 and contain `coverage.resource_occurrences = 1439` and
`coverage.destination_groups = 809`. Resource-register.json now has 1,447
occurrences and 804 groups. These four derived values are affected by the
correction and cannot be retained under an immutable-input rule. Update them
from authoritative arrays and reconcile all current copies, preserving actual
assignment/capture/snapshot metadata and source identities.

In `related_entrances[10]` (legacy_docs README.md), `links[10]` and `links[11]`
are the current/dev autolinks at lines 36/40, spans [1111,1138]/[1207,1230].
Their captured_head incorrectly uses the manual HEAD eba76e31367ee8d0eaa3217acce0d6bfc5378d17.
Their owning repository's actual HEAD is
`f2348ab2c17c8e655e9bbabf151e6a6907966387`; README working SHA-256
`9b41d10d09aa925385251d4054339ec5586040e47fa5b62d0cb80c558da3b5f8`
and raw targets/spans are correct. Resolve identity from the source's own
`legacy_docs:README.md` file key, not a previously visited book root.

### R12 — Finish real prerender roles, aliases and branch evidence

A whole rich-input-to-role-path comparison finds **26 missing roles**:
Build It root `title_md_inline` (one), excerpts desc_md/code_md (fifteen), and
authors `links[*].label_md_inline` (ten). Existing 61 rich-field mirrors already
identify the scalar/sibling pairs; retain them and connect the actual
prerender::process_value/process_mapping source transformation to the consumer.
The current excerpts/authors prerender_input_fields arrays are empty, while
Build It omits its root title. Code scalars are prerender inputs even when they
contain no resource link. This finding concerns consumer roles, not twenty-six
missing navigation occurrences or twenty-six extra mirrors.

For consumer-6563d80aab573532, the second masthead loop at lines 30–44 consumes
excerpt.code_html/desc_html at 35/39. Enumerate eight descriptions and seven
code_md fields; items[0].code_html is an unpaired direct HTML field, not a
fabricated code_md input. For consumer-629617d9d8878455, resolve ten actual
map-key/link-index inputs to label_html, then post/blog-author's author selection,
links.size guards, link loops and label_html accesses. Keep YAML field paths
separate from Liquid `author_entry[0]`, `author_entry[1]`, `author` and `link`
aliases. The current field_paths contains alias tokens instead of YAML paths.
Its source_locator still lists nonexistent `[author-id].url`; remove/retire
that invalid current assertion. Exact alias edges, not suffix matches, govern.

Five author `consumer_sources` entries still carry planning HEAD
b5a7c50b0cb761e8e857cab6731cde103874b081. Qualify all five layouts with website
HEAD/hashes and actual existing source witnesses; preserve the already-correct
`exact_author_layout_witnesses` rather than duplicating incompatible identities.
Current partial notes also need explicit historical qualification or correction.

Both `detail_layout_branches.name_fallback` witnesses point only to author.name
in an image alt attribute: post line 51 and blog-author line 30. They do not
witness fallback branches. Post's actual if-author.name/else-page.data.author
branch is line 59. Blog-author has no such name-presence fallback: line 34 uses
name for initials in the avatar else branch and line 37 interpolates the heading
unconditionally. Record those different facts with actual enclosing branches.
Do not invent a branch to fill an expected slot or demand emitted behavior.

### R13 — Calibrate current coverage, intake and scoped return

Current gaps.observation still says all shared-data consumers are accounted
and only output/runtime/chapter unknowns remain. R12 disproves that source
completeness assertion. Reconcile coverage/gaps/summary/readback and the eight-row
return after repair, keeping source obligations here and real downstream checks
with their established owners. The 59 located access witnesses, lexical URL/
residual scans and the positive construct projections above each prove only
specified properties; do not turn any one into universal construct coverage.

CC's final execution record explicitly says the eight roots and 2,839 identities
were inherited, with no fresh population replay during the correction. That is
an honest unrun statement, but iteration02 required fresh intake/return replay.
CDC executed fresh checks independently; CC may cite those at their original
attribution, not relabel them as its own run. Iteration03 must record actual
fresh source/pin checks, full required read extents/data-query results and
source-cited readback, plus exact current correction range/receipt. Prior
attempts remain historical. No additional reporting layer or test product is
required.

### Complete row walk, retention and bubble-up

| ID | CDC status | Current evidence and remaining condition |
| --- | --- | --- |
| S2-01 | done | Retain Iteration01 reproduced acceptance; fresh eight-root/population/all-file/pin/provenance replay passes; original criterion and inputs unchanged |
| S2-02 | open | Valid default/wrapper/generated/direct access subclaims retained; R12 exact roles, source identities and conditional witnesses remain |
| S2-03 | open | 159 metadata records retained; fresh ordinary Markdown/HTML projections pass; R11 copied register coverage and R13 whole declared construct/limit reconciliation remain |
| S2-04 | open | Scoped references and rich link/HTML projections repaired; R11 totals and R12 prerender/consumer semantics remain |
| S2-05 | open | Related populations and all 966 SUMMARY comparisons pass; R11 two legacy entrance qualifiers prevent whole-row acceptance |
| S2-06 | open | Eleven family walks retained as evidence; R11 cross-register reconciliation and R13 source-gap/owner accounting remain |
| S2-07 | open | Reads/attempts remain attested; fresh CDC checks distinguish verified bounds; R13 actual CC intake/return checks, extents and calibrated proof remain |
| S2-08 | open | Five-path scoped return, receipt, historical inputs/prompts and source preservation pass; R13 current eight-row return, coverage and honest Arc01 bubble-up remain |

**Verdict:** retain S2-01 done and all valid repaired subclaims; S2-02–08 remain
open. No source-extraction deferral is adopted. No scope, capability, sequence
or criterion amendment is needed. Arc01 remains active with A1-01 alone done;
all 23 project rows remain open. Slice03 remains gated. What worked: scalar-local
bindings and independently matching ordinary/rich link projections. What must
improve: carry corrections through every copied value and distinguish actual
field/alias/branch meaning from locator validity.

This is the third corrective refinement, within the existing fix-loop budget.
**Next owner:** CC executes [cc-prompt-iteration03.md](cc-prompt-iteration03.md),
relayed by Duncan, then returns to CDC. CDC has changed only plans, ledgers,
review and assignment; no CC artifact repairs have been implemented by CDC.

### Iteration03 handoff validation

Independent `/tmp/project099-cdc-slice02-iteration03-handoff.py`, executed with
`python3`, passes the exact eight-path tracked-plus-untracked CDC allowlist;
unchanged Criterion/Verify/Significance/Origin and project/slice Notes; all 23
project rows open, A1-01 alone done, S2-01 done and seven child rows open.
All five CC artifacts, closing report and three issued prompts are byte-equal
to ecc4993; the prior CDC review is preserved as an exact prefix. Project v1.7,
arc v1.5 and slice v1.3 name the new iteration03 assignment consistently.
Rendered local Markdown links, UTF-8/newlines/trailing whitespace, eight fresh
root projections/ordered populations, all 2,839 working hashes and eight
working/pinned input hashes pass. `git diff --check` passes. Draft author fixes
remove an inadvertent blank line splitting the assignment-history table and
correct the prerender read boundary to include render_markdown's signature;
these are corrected before issue, not CC defects. Commit only these eight paths
and compare the actual committed tree with working bytes before handoff.
The new prompt still requires CC implementation and independent CDC return.

## Iteration03 independent CDC review — 2026-10-09

**Reviewer:** Sofie, CDC, independent of CC's evidence production.
**Candidate:** fb83798b2b20e6ac5561d4035efd2d01743c9129 plus closing-report-only
receipt 6a43ab22816e8502fec5ec9744f51b5aaf3f4a9d; assignment opening
fa86203e7985dddeb15c1250c515ec514200dad0. The complete opening-to-return range
contains exactly the seven authorized paths. Receipt changes closing-report.md
alone; all seven working deliverables equal their 6a43ab2 committed bytes and
planning was clean before CDC edits. Issued prompt03 is unchanged.

**Verdict:** retain S2-01; independently accept **S2-03 and S2-05** for the
source scopes specified in their original criteria. Five rows remain open for
R14 author aggregate fields/roles and R15 precise current intake/run receipts.
No source-extraction deferral, emitted/live acceptance or criterion amendment
is adopted. CC's successful work remains attested until the particular claim
is independently reproduced below; acceptance does not transfer to composition.

### Fresh proof and retained evidence

`python3 /tmp/project099-cdc-slice02-iteration03-structural.py` passes **114/114**
core predicates, with **6,844 locator objects** and zero hash/span/line failures.
A separate issued-prompt03 byte comparison also passes. Methods: fresh
`git rev-parse --show-toplevel`, `git rev-parse HEAD`, `git branch --show-current`
and `git status --porcelain=v1 -uall` per source root; stored NUL-separated Git
filename queries versus exact ordered populations; union/partition/uniqueness;
working SHA-256/size/decoded-line count versus all 2,839 file records, Git modes
and HEAD blobs; all eight input working hashes versus their pinned objects.
All eight roots and 2,819 population entries match; 2,835 HEAD-equal files and
four already-qualified dirty Rust files retain their distinctions. Website
385 paths partition 124/27/12/35/187. Protected original capture/assignment/
metadata and inherited 29/16/104 raw provenance match predecessor/original pins.
LFE is develop 0aa820e; website main 036e231; legacy master f2348ab. No source
checkout changed or drifted during review; unrelated dirty/untracked work is
preserved. Earlier source snapshots remain historical, not freshly recaptured.

Array-derived counts are now **1,447 occurrences / 804 destination groups** in
all current register copies. Exact opening-to-candidate equality retains the
occurrence/group/reference/mirror/family/feed/residual/build-it/exclusion/
unpaired arrays and earlier scoped extraction proof. Inventory and route JSON
change only authorized derived coverage. Source/pin equality is checked before
retaining earlier independent comparisons; no prior CC run is renamed CDC proof.
Earlier checks of 827 ordinary Markdown nodes/426 HTML attributes, 38 rich
Markdown nodes/39 rich HTML attributes, 84 scoped definitions/97 use joins,
951 lexical URL candidates/56 exclusions, 543 residual tokens, 966 SUMMARY
projections and eleven family membership walks are **retained**, not claimed
rerun wholesale. Current locators/inputs and relevant changed meanings are fresh.
The historic 346-token scanner output used by the core predicate is retained
with unchanged inputs; this review is not a fresh scanner CLI execution.

`python3 /tmp/project099-cdc-slice02-iteration03-semantic.py` freshly decodes
all twelve YAML trees using `yq -o=json . -`, recursively enumerates _md/
_md_inline paths and compares the whole actual rich-input set with the current
edge graph. **61 inputs / 61 unique edges**, no missing/extra/error edges:
authors 10, books 8, Build It 5, callouts 4, excerpts 15, features 12,
masthead 1, videos 6. All exact sibling/mirror/occurrence joins and transform/
consumer tokens resolve against source bytes/hashes. The ten protected arrays
named above are value-equal to fa86203. An initial helper expected a bare
function name where the valid locator token includes `fn`; its 122 false
mismatches were checker construction errors, fixed by the source-compatible
predicate. No CC edge repair is warranted by those false mismatches.

Complete authors YAML and post/blog-author/archive/category/tag layouts are
inspected. All five secondary author qualifications now match website HEAD,
working hashes and actual loop witnesses. The real post name-presence fallback
is **line 58**; CDC's issued prompt03 and previous review said 59 in error.
CC correctly used 58. Those historical documents stay unchanged and this note
supersedes that line claim. Blog-author's avatar-else initials at 34 and
unconditional heading at 37 are distinct; no name-presence fallback is invented.
The same branch predicate rejects predecessor image-alt witnesses at post 51
and blog-author 30. Both legacy README current/dev references now qualify
against their actual legacy root/HEAD, with unchanged valid hashes/spans/targets.

`python3 /tmp/project099-cdc-slice02-iteration03-anchors.py` freshly queries the
147 tracked Markdown/wiki source files and 40 native HTML/Liquid/XML files.
Frontmatter is removed; Pandoc GFM Header declarations and HTMLParser id /
`<a name>` declarations form an independent whole-set census. **255 headings and
17 explicit IDs** match the current 272 declaration records, no missing/extra.
Twenty-two initial text mismatches came from comparing raw Markdown record text
with rendered AST display text (links/code/emphasis); parsing each record text
under the same GFM Header projection resolves them, without changing CC records.
An initial lookup of kind `markdown` instead of actual `heading-declaration`
was also a checker failure. Source hashes/locators and whole record membership
are independently checked, so normalization is not a substitute for coverage.

All **135 nonempty-fragment references** match literal source resource joins.
Four additional literal `#` targets are empty-fragment YAML stubs, preserved in
resource occurrences; they are not four missing named-anchor references. All
159 metadata/source-route records remain equal to previously independently
checked source values; drafts (two), 124 explicit post permalinks, 14 qualified
route/pagination mechanisms and raw-HTML relocation evidence retain their
observed source boundaries. Implicit heading IDs and emitted/deployed behavior
remain unknown. These qualifications satisfy S2-03's **source** criterion; they
do not create an unbounded generated-output obligation in this slice.

Related Git populations, exact locator ownership and 27 named entrance records
are freshly checked. All 966 SUMMARY label/target/hierarchy/local-status records
and source hashes equal the previously independently compared full projections;
two changed legacy qualifiers now pass. README/config/SUMMARY, language/rebar
and eight legacy entrance coverage remains explicit. This satisfies S2-05's
filename/entrance criterion. Chapter meaning/readiness, generated/deployed
provenance and live unavailable-target resolution remain unclaimed with owners.
No build, source test, render, wrapper execution, network or publication occurred.

### R14 — Author aggregate field/role census remains incomplete

`consumer-629617d9d8878455.field_paths` now contains actual paths, but its
membership derives ordinary fields only from authors with rich links. It omits
all four actual anurag-mendhekar name/github_user/avatar/bio fields and all ten
real indexed links[n].label_html siblings. The independent leaf projection is
34 directly accessed scalar/HTML paths; fourteen are absent. Anurag's null
GitHub/avatar values and empty links list are present data, not absent author
fields. The guards/loops also access six actual links containers.

`iteration02_field_roles.direct_access_fields` still lists six Liquid tokens:
author_entry[0], author_entry[1].name/github_user/avatar/bio and
`author_entry[1].links[*].label_html`. None is an actual YAML path. Current alias
resolution already correctly explains these tokens; preserve it and distinguish
it from one current real-field role projection. The valid ten author rich-input
edges do not discharge the aggregate's ordinary/direct field accounting.

The bounded recipe walks all six source map entries independently of mirror
membership, enumerates real scalar/collection/Markdown/HTML paths (currently
50), and joins 40 direct versus ten prerender-input paths to existing qualified
access witnesses. Selected map keys remain separate from leaf paths. Counts
are consequences of the actual walk, not a quota; no empty-author edge, URL or
code_md should be fabricated. Correct the consequential current aggregate/
coverage/gap/prose only. All 61 valid edges/mirrors remain unchanged. Negative
controls must reject the predecessor aggregate omissions/alias path tokens and
real missing/duplicated fields using the same source-path membership predicate.

### R15 — Exact required intake and validation receipts

Iteration03 execution says its required-full prompt receipt is lines **1–239**;
the actual issued prompt has **266** lines. That recorded extent does not
establish required-full intake. CDC does not infer which text was actually
unread, nor use a receipt to claim attention/comprehension. Preserve the old
receipt historically; load required contexts now and record current exact
extents/recovery, without retrospectively inventing a full earlier read.

Other intake phrases (“cited guidance”, “five author/home templates”) are too
ambiguous to reconstruct actual path/section boundaries. Current source/pin
and edge validations describe positive/failed results but some custom queries
lack a replayable command or exact mechanic/exit. A concise coverage table and
actual executable query/denominator/result records in the existing execution
artifact suffice; no separate report layer, maintained tool or transcript is
required. Keep earlier checker failures and unrun work. Retained independent
proof requires attribution plus current input/claim equality, not a fabricated
CC rerun. Reconcile the current eight-row return after R14; downstream limits
remain bounded source/output/runtime/chapter obligations, not universal claims.

### Complete row walk, retention and bubble-up

| ID | CDC status | Evidence / remaining condition |
| --- | --- | --- |
| S2-01 | done | Prior acceptance retained; fresh eight-root/population/2,839-file/pin/provenance replay passes |
| S2-02 | open | Default/wrapper/source mechanism and all 61 edge/qualified branch subclaims pass; R14 aggregate real fields/aliases remain |
| S2-03 | done | Source metadata/declarations/route mechanisms, whole 255 heading/17 explicit-ID/135 fragment population and bounded parser/output limits independently satisfy unchanged source criterion |
| S2-04 | open | Reconciled 1,447/804 arrays, scoped extraction and 61 rich-input edges pass; R14 aggregate direct-field roles remain |
| S2-05 | done | Fresh related populations/qualified entrance locators, retained independent full 966 SUMMARY/27 entrance comparisons satisfy unchanged declared source scope |
| S2-06 | open | Eleven family walks/IDs/owners retained by equality; R14 consequential source-gap reconciliation and R15 current calibrated receipt/return remain |
| S2-07 | open | Structural/locator/semantic proofs reproduced; R15 current complete required intake and precise actual validation receipts remain |
| S2-08 | open | Seven-path range/receipt/committed bytes/protected inputs/prompts/source preservation pass; R14/R15 current honest whole-row/artifact return remains |

Three child rows are done, five open. Slice02/Arc01 stay open, Slice03 gated;
A1-01 alone is done and all 23 project rows remain open. No scope/capability/
sequence/criterion amendment or automatic dispatch is needed. What worked:
whole rich-input joins and actual enclosing branches resolve the previous
failures. Remaining work is narrowly aggregate source-path accounting and
precise current receipts; successful extraction is retained without ceremony.

**Next owner:** CC implements [cc-prompt-iteration04.md](cc-prompt-iteration04.md),
relayed by Duncan, then returns for independent CDC review. This is the fourth
corrective refinement within the existing budget. CDC changes only plans,
ledgers, review and assignment; no CC artifact repair is implemented by CDC.

### Iteration04 handoff validation

`python3 /tmp/project099-cdc-slice02-iteration04-handoff.py` passes the exact
eight-path tracked-plus-untracked CDC allowlist; unchanged criteria/Verify/
Significance/Origin and project/slice Notes; all 23 project rows open, A1-01
alone done and S2-01/03/05 done with five child rows open. All five CC artifacts,
closing report and four issued prompts are byte-equal to 6a43ab2; the previous
CDC review is an exact retained prefix. Project v1.8, arc v1.6 and slice v1.4
consistently name iteration04. Rendered local links, newline/whitespace, eight
fresh root/ordered population projections, all 2,839 working hashes and eight
working/pinned input hashes pass. `git diff --check` passes. Draft review
catches and fixes a missing closing backtick in the new assignment-history row;
that author error is fixed before issue, not attributed to CC. New prompt
SHA-256 is d3a90431375f8dbf219e03d24da33b0c0e3a85812a7055944b636656fadbd35e.
Commit these eight CDC paths only and compare the committed tree with actual
working bytes before handoff. This is an implementation assignment requiring
CC's correction and a subsequent independent CDC return.
