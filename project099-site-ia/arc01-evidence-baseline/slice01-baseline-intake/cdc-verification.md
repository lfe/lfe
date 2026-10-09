# Arc01 Slice01 — CDC verification

**Current verdict (9 October 2026): accepted and closed, all eight rows done.**
Iteration01 review below supersedes the initial correction-required verdict;
the failed initial review remains historical evidence.

## Initial-pass review — 9 October 2026

**Reviewer:** CDC, Sofie in the desktop planning chat, independent of CC's
evidence production. Two-Contributor Workflow; Expedited Mode remains enabled.
**Candidate:** `ddeecb31abb454d5619f74a178c3d0121024a35e`.
**Assignment reviewed:** `arc01-evidence-baseline/slice01-baseline-intake/cc-prompt.md`.
**Opening:** `e8f4c394df1c5a902086fcb45f3249a52bc6c453`;
prompt SHA-256 `ac2b9a6c59bc318167e39103ec1e1ff1190efef9fd056e63973c4ba15890aeee`.
**Verdict:** correction required; slice remains open. Four rows are accepted,
four remain open. No Slice02 assignment is issued before this gate passes.

### Independent checks and limits

CDC loaded the issued contract, current plans/ledger, all four artifacts and
closing report. Required witnesses were inspected: README, Cobalt config,
download data, docs redirect, Build It data, Pagefind config, the wrapper's
`run` body and the dated release-history document. CDC compared the reports'
assessment/question/source populations against pinned objects, with manual
inspection of qualifications, assessment kinds/owners, content-family coverage,
the four URL seeds, heading-only uncertainty, and the missing failure packet.

The following checks were run independently, using Python standard-library JSON,
SHA-256 and subprocess Git queries. They establish the listed local properties;
they do not verify any external claim or that CC understood every required read.

- Five packet files: working bytes equal `git show 09bae67:project099-site-ia/<path>`,
  prompt hashes and baseline bytes/line counts. All five pass.
- Eight roots: reran `git -C <root> rev-parse --show-toplevel`, `rev-parse HEAD`,
  `symbolic-ref --short -q HEAD`, and `status --porcelain=v1 -uall`.
  Exact root/HEAD/branch/status projections match all eight snapshots.
  Planning's current HEAD is the delivery commit; its initial HEAD is checked
  against the delivery parent, rather than expecting historical HEAD to be current.
- Website: fresh `git ls-files -z -- src`, sorted and compared by exact membership
  and uniqueness; priority partitions recomputed from the prompt's rules.
  385 paths; posts/templates/data/pages/other = 124/27/12/35/187, exact match.
- Eight witnesses: recomputed full-file SHA-256 and compared bytes against each
  recorded HEAD blob. Seven match HEAD; only `build.rs` differs, as recorded.
- Pinned report rows: compared exact namespaced ID sets, literal raw cells/text,
  original IDs/numbers, dates, evidence states and atomic line locators.
  All 29 assessments, 16 questions and 104 source IDs pass. Compound destination
  cells remain intact. Owners/next checks are proposed routing, not policy.
- The same exact-ID predicate (length equals unique length and set equals expected)
  rejects omitted and duplicated IDs for each report. These controls are in memory.
- JSON parses; all local Markdown links inspected resolve; newline/trailing-space
  checks pass. Opening ledger and closing row walk both contain S1-01 through S1-08
  exactly once. `git diff ddeecb31^ ddeecb31 --check` exits 0.
- `git diff-tree --no-commit-id --name-only -r ddeecb31` matches the exact six-path
  allowlist. The initial issued prompt equals its opening blob. No parent contract,
  historical input or source repair occurs in that diff; source snapshots still match.

An ad hoc reviewer script in `/tmp/project099-cdc-slice01.py` was run with
`python3 /tmp/project099-cdc-slice01.py`. Two preliminary attempts stopped at
question-section lookup (`StopIteration`): the first assumed an Unresolved heading,
the second assumed Questions for both reports. No files were changed. Corrected
lookup uses LFE section 11 and Erlang section 13; source IDs use L/S namespaces.
The final run reports **64 checks, 62 passes, 2 failures**: the metadata shape and
assignment-path failures detailed below. The temporary script is an incidental
review aid, not a deliverable or maintained validation interface.

CC's reading extents/readback remain attested. Their stated boundaries and file
extents are consistent with the inspected inputs, but the readback missed the
binding metadata shape. Builds, HTTP checks, rendering, containers, compatibility
and source tests remain intentionally unrun and outside this assignment.

### Findings and required repairs

**R1 — binding metadata representation differs (S1-02, S1-07).** The initial
prompt's Evidence representation and construction section requires top-level
`assignment` (project-relative string), `captured_at`, and `planning` in
`baseline.json`. Instead it contains `assignment.prompt_path`,
`assignment.captured_at`, and `assignment.planning`. Both JSON files include
`project099-site-ia/` in the assignment value. Preserve the initial capture time,
planning snapshot and all evidence arrays; move the metadata to the required
locations and remove that directory prefix from the original assignment identity.
This repairs the issued contract; it does not introduce a new schema or recapture.

**R2 — evidence references misidentify existing data (S1-04, S1-08).** The ledger's
S1-04 evidence points to `findings.json` for source-register entries, which actually
live at `baseline.json#/source_catalogue`. S1-08 points to nonexistent
`artifacts/closing-report.md`; the report is in the slice root. The summary's opening
and inherited-population discussion also attribute source-register entries/provenance
collectively to `findings.json`. Correct these current references precisely. Retain
earlier review/attempt evidence as explicitly historical, rather than presenting a
failed pointer as current. The initial closing report's proposed S1-04 pointer also
needs a dated correction in its new iteration section.

### Row walk

| ID | CDC status | Independent evidence / remaining work |
| --- | --- | --- |
| S1-01 | done | Five working input hashes, pinned bytes and size/line counts reproduced; source packet preserved. |
| S1-02 | open | Nine-root planning/source identities checked; eight source snapshots match. R1 metadata layout must satisfy the issued representation. |
| S1-03 | done | Exact 385-path query and five unique/disjoint/exhaustive partitions reproduced. |
| S1-04 | open | All 104 source IDs/raw cells/locators reproduced in baseline.json; R2 current evidence pointer needs correction. |
| S1-05 | done | All 29 assessment and 16 question IDs/text/locators reproduced; dated states, kinds, limits and actionable routing inspected. |
| S1-06 | done | Eleven families/seven groups, local witnesses and scope distinctions inspected against originals; R2 summary pointer edits require affected-link recheck. |
| S1-07 | open | Intake receipt/readback inspected; checks reproduced, but R1 representation was omitted from CC's contract application. Record actual correction commands/checks and retain failed attempts. |
| S1-08 | open | Actual delivery is six authorized paths; source/prompt preservation and full row walk reproduced. R2 report pointer and iteration return record must be corrected. |

### Bubble-up and evidence retention

The initial delivery supplies the intended provenance/population content but has
two representation/reference defects. No architecture, scope, sequencing or ledger
criterion amendment is needed. Arc01 needs a status/current-assignment update only;
Slice02's existing dependency on accepted Slice01 remains. Runtime inputs remain
Slice04's gap; URL and chapter checks remain Slice03's work.

For correction review, S1-01/S1-03/S1-05 and the substantive S1-06 observations may
retain this reproduced evidence only if relevant inputs/data and witness states
are unchanged. Compare the corrective delta; rerun metadata shape/identity and
affected references/links, snapshot-preservation checks, and scoped-delivery review.
S1-06's corrected summary references receive a fresh check even if its content
evidence is retained. No earlier doer attestation becomes independent by retention.

**Next owner:** CC executes
`arc01-evidence-baseline/slice01-baseline-intake/cc-prompt-iteration01.md`, relayed
by Duncan. CDC reviews the resulting commit before closing and advancing.

## Iteration01 review — 9 October 2026

**Reviewer:** CDC, Sofie in this desktop planning context, independent of CC.
**Assignment:** `arc01-evidence-baseline/slice01-baseline-intake/cc-prompt-iteration01.md`;
opening `7f2b47f24873211c3eca66702b5a1e81ccec4897`, SHA-256
`f3160c0c43533cf8fd93686cc53665cfb00e7ce0d94a275780734be4cfebc7b2`.
**Candidate:** `9d3a7e12a9df75ceae6e7f2294c38ae12238795a`, followed by
`9111268babf8f96d98cb8b32aa3a40c9aa209b67` (prompt digest correction).
Reviewed from planning HEAD `924c8b0da132c2ae8287a51ffa5425d8816c9125`;
all six candidate slice files are byte-identical there. The later project-level
CI intake is a separate authorized change, outside this CC assignment.
**Verdict:** R1 and R2 resolved. All eight rows accepted; Slice01 closed.

### Fresh independent checks

CDC inspected the six-file delta `git diff 7f2b47f 9111268`, both entire parsed
JSON objects, current references, execution record and dated closing section.
Read-only Python standard-library JSON/SHA-256/subprocess checks reproduced:

- Exact baseline top-level shape and original project-relative assignment string;
  original capture time and planning snapshot are unchanged. Both real predecessor
  objects fail the same respective metadata/path predicates that the correction passes.
- Complete equality of all five non-metadata baseline values against ddeecb31;
  the entire findings object equals its predecessor with only the assignment value
  replaced. This covers ordering and all 29/16/104/385 records, beyond count checks.
- All five packet files still equal their recorded hashes, sizes/line counts and
  pinned Git objects. Eight source-witness working hashes and HEAD qualifications
  match. The website's fresh 385-path query equals the recorded population.
- Seven source root/HEAD/branch/status projections still match exactly. LFE has
  concurrent Operator CI work, qualified below; the historical snapshot is preserved.
- Both issued prompts match their opening blobs. The actual iteration digest agrees
  in execution and close records, including the 9111268 correction. CC preserved
  reviewer and parent records. The correction delta is exactly the six allowed paths.
- Source-register pointers resolve to `baseline.json#/source_catalogue`, findings
  to `findings.json`, and the current report to slice-root `closing-report.md`.
  The initial close is labelled historical. Summary and ledger pointers agree.
  Local Markdown links, final newlines, trailing whitespace and scoped diff check pass.
- The iteration closing row walk accounts for S1-01 through S1-08 exactly once;
  inventory contains four supporting artifacts and two first-class changed documents.
  Intake/readback distinguishes the initial snapshot from the correction assignment,
  and retains preliminary failures and deliberately unrun checks.

The inline reviewer checker first reported 78 checks / 76 passes / 2 failures:
one live LFE mismatch and one overly literal `slice-root` phrase predicate.
Direct report inspection and a corrected predicate verified the actual wording
(`closing-report.md` at the slice root). Final disposition is 77 passing checks
and one explicitly qualified concurrent-state difference. The preliminary checker
failure is retained here; it was not a new delivery defect. Temporary results at
`/tmp/project099-cdc-iteration01-results.json` are incidental review aids.

### Concurrent state and retained evidence

The LFE checkout now has HEAD `0aa820e93bb592a821fe0a50b1db7e8926e56b30`,
branch `develop`, clean status. The baseline recorded master at dae7489e and an
untracked `rebar.lock`. `git show --stat` / `diff-tree` show the CI commit's sole
changed path is `.github/workflows/cicd.yml`, on parent
`387d0aa1092bdda0ca17481869e833b24f11971a`. This agrees with Duncan's separate
CI update and is recorded as concurrent drift, not attributed to CC or relabelled
as the original snapshot. Future language observations must use the new state.
The Slice01 contract explicitly permits qualified concurrent drift; it does not
require restoring a historical checkout. No compiler/CI behavior is accepted here.

The initial-pass reproduced evidence for S1-01/S1-03/S1-05 and the substantive
S1-06 remains applicable: criteria and relevant pinned report/input/payload values
are unchanged; website population and witnesses freshly match. The LFE branch
change cannot affect those packet/website claims. This retains the original
review's revision/date/method; it does not claim fresh extraction or promote CC
attestation. S1-06's affected references were freshly checked. Source-state and
correction claims for the other four rows receive the fresh checks above.

### Current row walk

| ID | CDC status | Basis |
| --- | --- | --- |
| S1-01 | done | Retained initial independent packet evidence; five hashes/objects freshly match. |
| S1-02 | done | Correct metadata/snapshot identity reproduced; live roots checked and concurrent LFE drift qualified. |
| S1-03 | done | Retained exact partition proof; full value equality and fresh 385-path membership reproduced. |
| S1-04 | done | Retained 104 literal cells/locators; complete catalogue equality and corrected pointer reproduced. |
| S1-05 | done | Retained 29/16 semantic/provenance review; complete findings equality reproduced. |
| S1-06 | done | Retained content-family/source-boundary evidence; affected summary references freshly verified. |
| S1-07 | done | Correction readback/attempts inspected; actual metadata/control/preservation checks reproduced. Reading receipts remain attestation. |
| S1-08 | done | Exact six-path delta, preserved prompts/parent records, current digest/pointers, eight-row walk and artifact inventory reproduced. |

### Bubble-up to Arc01 and What Worked

The delivered provenance, source map, filename denominator, inherited registers
and qualified handoff meet Arc01's assigned Slice01 capability. Scope-as-specified
and scope-as-delivered match; no silent drop or deferred criterion remains.
Live URLs, source/rendered chapter checks and the missing runtime packet were
never Slice01 acceptance work; their existing owners remain Slices03/04.
No capability, scope, sequence or acceptance amendment is indicated. Update
Arc01's status/current assignment and child-close row, then open its planned
Slice02. Arc01 and project composition remain open.

Whole-value comparisons protected the evidence while making the metadata repair
falsifiable. Preserving historical attempts and qualifying live drift permits
reuse of valid independent evidence without confusing old snapshots with current
checkouts. This CDC record is the acceptance authority; CC's historical artifacts
retain their proposed-done wording.
