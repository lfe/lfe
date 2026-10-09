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
