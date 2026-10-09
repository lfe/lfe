# Arc01 Slice01 — CDC verification

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
