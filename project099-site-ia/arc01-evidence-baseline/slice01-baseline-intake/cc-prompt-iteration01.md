# CC assignment — Arc01 Slice01 correction, iteration01

## Identity, authority and required outcome

Execute this evidence correction and return to CDC (Sofie in the desktop chat)
through Duncan. Project-relative assignment:
`arc01-evidence-baseline/slice01-baseline-intake/cc-prompt-iteration01.md`.
Predecessor: `arc01-evidence-baseline/slice01-baseline-intake/cc-prompt.md`.
Candidate being corrected: `ddeecb31abb454d5619f74a178c3d0121024a35e`.
Review: [cdc-verification.md](cdc-verification.md), initial-pass review, R1/R2.

This is a pass inside Slice01, not Slice02. Preserve the initial issued prompt,
inherited packet, accepted content evidence and all source repositories.
Expedited Mode authorizes a scoped commit before independent acceptance; it
does not authorize network checks, builds, installs, source repairs, publication,
messages/tickets, policy decisions or parent-contract edits.

Let P be `/Users/oubiwann/lab/lfe/lfe/.worktrees/planning/project099-site-ia`;
D is P's `arc01-evidence-baseline/slice01-baseline-intake` directory.
Use the existing four artifact files and two CC close/evidence documents only.
S1-01, S1-03, S1-05 and S1-06 are CDC-accepted at the reviewed revision; retain
their `done` status and reproduced evidence. S1-02, S1-04, S1-07 and S1-08
remain open until CDC checks this correction. Do not repair reviewer records.

## Required reading and queries before edits

| Exact item | Extent and state | Purpose |
| --- | --- | --- |
| This prompt; D/slice-plan.md, ledger.md, cdc-verification.md; P/AGENTS.md | Required-full, current committed opening of iteration01 | Current assignment, R1/R2, accepted evidence and write boundary |
| D/cc-prompt.md | Required-section: Assignment and authority; Binding scope and preservation; Evidence representation and construction; Applied guidance and checks; Write allowlist, commit and return; opening e8f4c39 | Original representation and unchanged evidence/preservation rules; initial required-full investigation reads are not repeated for this bounded metadata repair |
| P/project-plan.md | Required-section: Workflow, authority, and review; Current state | Authority and pending acceptance |
| P/arc01-evidence-baseline/arc-plan.md | Required-section: Capability and authority; Scope and boundaries; Slice breakdown; Workflow and current assignment | Arc boundary and Slice02 gate |
| D/artifacts/baseline-summary.md, execution.md; D/closing-report.md | Required-full at ddeecb31, compare current state | Correct pointers; preserve initial attempts and append correction evidence |
| D/artifacts/baseline.json and findings.json | Required-data: full parsed JSON, all keys/types and complete before/after equality projections specified below, compare ddeecb31 objects | Shape repair with lossless payload preservation |
| /Users/oubiwann/.agents/skills/work-verification/guides/05-independent-verification.md | Required-section: Separation Rule; Assigned Reviewer Slice Verification; Evidence During Correction Review | Preserve CDC evidence without claiming fresh independent review |
| /Users/oubiwann/.agents/skills/project-management/guides/README.md | Required-section: Notes for Codex; Expedited Mode | Workflow and exact-path commit |
| /Users/oubiwann/.agents/skills/testing/guides/03-validation-gates.md | Required-section: Gate Selection; Proportionate Proof; Gate Evidence | Bounded real proof and retained failed/unrun attempts |
| Five original inputs and eight source roots / eight witnesses named in initial prompt | Required-data: hashes and Git projections; no new content investigation | Fresh preservation/drift check against original evidence |

Record complete text extents, data-query coverage, commands/exits and a source-cited
readback in a new iteration01 section of `artifacts/execution.md`. Recover truncated
required reads. Explain the distinction between the original snapshot assignment
in JSON and this active correction assignment in the execution/closing records.
Identify this prompt's opening commit/digest using its exact Git history path.
Once intake is coherent, execute the repair; a readback alone is not completion.

## Binding repair and construction

1. **R1:** In `baseline.json`, replace the nested assignment object with the
   original project-relative string
   `arc01-evidence-baseline/slice01-baseline-intake/cc-prompt.md`.
   Move its original `captured_at` and `planning` values intact to top-level keys.
   The resulting top-level key set is exactly `assignment`, `captured_at`,
   `planning`, `inputs`, `repositories`, `tracked_src`, `source_witnesses`,
   `source_catalogue`. Preserve every other parsed value, including array ordering.
   In `findings.json`, change only the `assignment` string to that same original
   project-relative path. This is a lossless representation correction: do not
   replace the original capture time/HEAD/digests with iteration intake state or
   relabel the initial snapshot as captured under iteration01. This iteration's
   real intake/assignment belongs in the appended execution and closing sections.
2. **R2:** Correct current references: source-register objects belong to
   `artifacts/baseline.json` at `source_catalogue`, not findings.json;
   the canonical report is `closing-report.md` in D, not `artifacts/closing-report.md`.
   Update ledger S1-04/S1-08 evidence and the summary's opening/inherited handoff
   references. Separate assessment/question provenance in findings.json from source
   provenance in baseline.json. Retain the initial closing report as a dated initial
   attempt and append an iteration01 correction/row walk with correct pointers.
   An explicitly historical description of a failed pointer may remain labelled so.
3. Keep ledger criteria/IDs and CDC-accepted evidence unchanged. Add iteration01
   doer evidence to the four open rows at `attested` strength; leave them open.
   S1-06's summary content remains accepted, but its new references must be checked.
   Append to execution.md the actual repair/validation commands and results,
   failures, before/after payload comparisons and preservation/drift results.
   Do not claim the reviewer's earlier checks were newly run by you.
4. Append a dated iteration01 section to closing-report.md: active assignment,
   predecessor/delivery identity, actual changed-path inventory, all eight row
   dispositions (four accepted at prior review / four newly proposed complete),
   R1/R2 resolution, validation and limits, What Worked and Bubble-up to Arc01.
   Keep the initial failed candidate and its historical proposed-done row walk.
   Return the resulting commit after creation; no self-referential commit rewrite.

## Checks and oracles

Use ad hoc standard-library JSON comparisons, hashes, Git queries and local
reference inspection; no new helper/schema/validator deliverable is requested.
Retrieve predecessor JSON with `git show ddeecb31:<worktree-relative-path>` and
compare against corrected working JSON. Select these checks, not a site build:

- Parse both JSON files. Assert the exact baseline top-level key set, string
  assignment equal to the original project-relative path, top-level timestamp
  equal to predecessor `assignment.captured_at`, and top-level planning exactly
  equal to predecessor `assignment.planning`. Assert complete equality for all
  five other baseline payload values. Findings must equal its predecessor after
  replacing only `assignment`. Totals remain 29/16/104/385. Compare entire values,
  not counts alone; no inherited data may be rewritten or silently recaptured.
- Run that same shape/path predicate on both predecessor and repaired objects:
  predecessor must fail, repaired must pass. The real rejected predecessor is
  the negative control; no extra mutation suite is needed.
- Resolve every current evidence reference touched. Confirm the actual 43/61
  source arrays in baseline.json, assessment/question arrays in findings.json,
  and D/closing-report.md. Check local Markdown links in changed documents.
  Historical failed pointers must be labelled historical, not active evidence.
- Recompute five input hashes against the initial prompt and pinned packet.
  Compare eight source HEAD/branch/status projections and witness hashes to the
  original baseline. Record concurrent drift separately; do not change sources
  or overwrite original snapshots to conceal it. Material uncertainty returns to CDC.
- `git diff --check`, trailing-whitespace/newline checks, exact changed-path
  allowlist, and preservation of both issued prompts and all CDC/parent documents.
  Record actual commands and results, including exceptions and unrun work.

CDC will inspect the delta and decide whether prior reproduced claims remain
applicable. R1/R2 and affected references/links require fresh reviewer checks.
Retention does not waive preservation, current scoped-diff review or acceptance.

## Exact write/commit allowlist and return

Only these six paths relative to `/Users/oubiwann/lab/lfe/lfe/.worktrees/planning`
may be changed; files unchanged by the correction need no artificial rewrite:

```text
project099-site-ia/arc01-evidence-baseline/slice01-baseline-intake/artifacts/baseline.json
project099-site-ia/arc01-evidence-baseline/slice01-baseline-intake/artifacts/findings.json
project099-site-ia/arc01-evidence-baseline/slice01-baseline-intake/artifacts/baseline-summary.md
project099-site-ia/arc01-evidence-baseline/slice01-baseline-intake/artifacts/execution.md
project099-site-ia/arc01-evidence-baseline/slice01-baseline-intake/ledger.md
project099-site-ia/arc01-evidence-baseline/slice01-baseline-intake/closing-report.md
```

Stage explicit literal changed paths only; `git commit --only` with the same
explicit paths. No directory-wide add, push or source commit. Suggested subject:
`Correct Project099 Slice01 metadata and evidence references`.
If current JSON differs materially from ddeecb31, the source packet changes,
accepted payload needs rewriting, or required inputs fail, stop affected edits
and report exact conflicting state. No acceptance/schema amendment is delegated.

Return the exact iteration path and commit, changed files, R1/R2 outcomes,
row dispositions and validation/retention limits to CDC via Duncan. Slice01 and
Arc01 remain open pending review; no Slice02 work is authorized by this prompt.
