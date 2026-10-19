# Project 099 — composition ledger

**Contract:** [project-plan.md](project-plan.md), v1.22, 19 October 2026; delivery criteria unchanged.
Original roadmap approved 8 October; Duncan added CI preservation and historical
artifact priority on 9 October. The original 21 rows are retained and two rows
are added; all 23 delivery rows remain open. Roadmap approval is not completion.

Evidence levels are `asserted`, `attested`, `reproduced`, and `reconciled`.
Doer evidence is `attested` and proposed-done. A `done` row requires at least
`reproduced` evidence from an independent check at this scale. Child close
pointers alone do not establish project composition. Final statuses are
`done`, `deferred`, or `no-op`; a deferral needs reason, owner, destination,
and re-entry condition, and a no-op needs a checkable rationale.

## Child arc closure

These rows verify the adopted arcs' closure packets and their bubble-ups.
Arc01 is opened with a plan and ledger. Later arc directories will be created
as detailed planning becomes timely; no close packet exists yet.

| ID | Criterion | Verify | Significance | Origin | Status | Evidence | Notes |
| --- | --- | --- | --- | --- | --- | --- | --- |
| P-01 | Arc01's evidence baseline is closed against its adopted contract. | Inspect `arc01-evidence-baseline/` plan, ledger, closing report and attributed composition gate; reconcile inventory coverage and bubble-up. | correctness-grade | Roadmap Arc01 | open | Slices01–04 independently closed; Slice04 [CDC review](arc01-evidence-baseline/slice04-technical-intake/cdc-verification.md) accepts8/8 with retrospective complete; finite Slice05 open set issued. Arc composition not yet performed [reproduced child-status inspection only]. Historical six-row Slice02/iteration05 evidence remains in its preserved review. | P-01 remains open until accepted five-child close and independent Arc01 composition/bubble-up; no child status alone closes it. |
| P-02 | Arc02's compatibility policy is closed against its adopted contract. | Inspect `arc02-compatibility-policy/` close packet, maintainer policy decisions, matrix evidence, failure disposition and bubble-up. | serious | Roadmap Arc02 | open | | Preserve failed, blocked, unrun and unknown cases. |
| P-03 | Arc03's release/distribution capability is closed against its adopted contract. | Inspect `arc03-release-distribution/` close packet, preserved release mappings, artifact verification and bubble-up. | serious | Roadmap Arc03 | open | | Evaluate available artifacts against approved claims. |
| P-04 | Arc04's IA/content architecture is closed against its adopted contract. | Inspect `arc04-ia-content-architecture/` close packet and reviewed design, ownership/disposition maps and decisions. | correctness-grade | Roadmap Arc04 | open | | `/docs` remains the curated gateway. |
| P-05 | Arc05's design foundations are closed against their adopted contract. | Inspect `arc05-design-foundations/` close packet, existing-design assessment, demonstrations and adopted criteria. | correctness-grade | Roadmap Arc05 | open | | Replacement of existing design is a decision, not an assumption. |
| P-06 | Arc06's decision experiments are closed against their adopted contract. | Inspect `arc06-experiments-decisions/` close packet; match each agreed question to trial evidence, limits and decision/disposition. | correctness-grade | Roadmap Arc06 | open | | Inconclusive results remain visible. |
| P-07 | Arc07's site foundations are closed against their adopted contract. | Inspect `arc07-site-foundations/` close packet, source/build states, rendered demonstrations and bubble-up. | correctness-grade | Roadmap Arc07 | open | | Build success alone does not prove reader task completion. |
| P-08 | Arc08's learning/Docs routes are closed against their adopted contract. | Inspect `arc08-learning-docs/` close packet and runnable-guide/reference evidence for every promoted task. | correctness-grade | Roadmap Arc08 | open | | Future generated-reference publication is handled by an explicit readiness disposition. |
| P-09 | Arc09's download/release journeys are closed against their adopted contract. | Inspect `arc09-downloads-releases/` close packet; check authority and acquisition evidence for offered choices. | serious | Roadmap Arc09 | open | | Support, artifact and documentation scopes stay distinct. |
| P-10 | Arc10's resource/community/publishing capability is closed against its adopted contract. | Inspect `arc10-resources-community/` close packet, catalogue/resource identity checks and integrated Home/About evidence. | correctness-grade | Roadmap Arc10 | open | | Listing a channel does not establish activity. |
| P-11 | Arc11's legacy/search capability is closed against its adopted contract. | Inspect `arc11-legacy-search/` close packet, exact inventory dispositions, serving/anchor checks and search coverage. | correctness-grade | Roadmap Arc11 | open | | Retain the coverage denominator and unsuccessful checks. |
| P-12 | Arc12's acceptance/rollout capability is closed against its adopted contract. | Inspect `arc12-acceptance-rollout/` close packet, actual candidate checks, publication authorization/verification and upkeep handoff. | serious | Roadmap Arc12 | open | | Publication requires later authorization. |

## Project composition and final gates

These checks demonstrate that the accepted arcs form the intended experience.
At closure, record exact source/output/plan states, verifier identity, command
results or witnessed task steps, coverage, and limitations. Independent review
must inspect the actual candidate, not just the assembler's summary.

| ID | Criterion | Verify | Significance | Origin | Status | Evidence | Notes |
| --- | --- | --- | --- | --- | --- | --- | --- |
| P-13 | Technical facts and public selection claims agree across the integrated candidate. | Compare offered releases/artifacts, recommended setup and reference links with the adopted policy/mappings; acquire and verify representative environments at project scale. | serious | DoD 1, 4 | open | | Include current, older and development contexts agreed in the coverage plan. |
| P-14 | The integrated Learn/Docs/build routes deliver their promised tasks. | Independently traverse Home to first success, language/OTP reference, and script/library/application/release guides; run the stated steps and compare expected results. | correctness-grade | DoD 2, 3 | open | | Check substantive help as well as destination response. |
| P-15 | Integrated discovery presents consistent, identifiable resources and participation routes. | Traverse Home/Learn/Docs to Books/Videos and Community/Blog/About; compare repeated promotions with actual items, status, provenance and adopted architecture. | correctness-grade | DoD 2, 4 | open | | Dates, readiness, applicability and maintenance are separate facts. |
| P-16 | The integrated visual/interaction experience meets the adopted criteria. | Witness representative task runs on the agreed desktop/mobile, keyboard and accessibility surfaces; compare rendered candidate with Arc05 criteria and record defects/results. | correctness-grade | DoD 5 | open | | Exact devices/tasks/criteria are set before their execution slices. |
| P-17 | Historical arrival and search preserve meaning and expose actual coverage. | Run the agreed old-path/fragment inventory checks against the serving candidate; demonstrate representative search tasks and external handoffs; reconcile inventory totals. | correctness-grade | DoD 6 | open | | No universal preservation claim from a sampled route. |
| P-18 | The accepted candidate is reproducibly built, authorized for publication, and verified after publication. | Re-run the adopted repository-native gates and actual `lfesite` build; inspect output/CI/deployment configuration, authorization and published candidate; check rollback instructions. | serious | DoD 7 | open | | Exact required commands/results are specified in execution contracts; no implementation checks run by this draft. |
| P-19 | Maintenance responsibilities and any later integrations have a concrete home. | Inspect the upkeep handoff with release/resource review owners and triggers; check generated-reference follow-up and ticket disposition decisions with Duncan. | correctness-grade | DoD 7 | open | | Ticket edits and messages require explicit authorization. |
| P-20 | Every adopted arc and surfaced finding is accounted for without a silent drop. | Walk the current roadmap against P-01–P-12, child bubble-ups, Version History and every project row; verify each deferral/no-op rationale and assigned re-entry route. | serious | Expansion and closure discipline | open | | Add rows when the roadmap expands; amendments preserve the prior contract. |
| P-21 | Independent project review and Duncan's acceptance address the same candidate. | Inspect project `closing-report.md` for attributed independent DoD/composition review, exact candidate state, unresolved-findings disposition and explicit Operator verdict. | serious | Workflow and final acceptance | open | | Self-checks and closed children do not substitute for this gate. |
| P-22 | The original CI matrix is preserved and every removed OTP22–26 combination has a dispositioned P0 Linux/macOS artifact target. | Compare preserved workflow/hash/run records with the pinned original; join all five removed OTP majors across both platforms to Arc03's exact source/runtime/platform recipes, build/install/test evidence, checksums and verified upload locations; independently check artifacts or explicitly approved missing-target dispositions. | serious | Duncan's 2026-10-09 request; Arc03 | open | [CI intake](artifacts/ci-matrix-2026-10-09.md): exact matrix/run preservation and ten-target queue [attested planning input]. | All five majors equally P0; no support promise, upload or binary availability claimed by intake. |
| P-23 | Downloads exposes the verified highest-priority historical artifacts with their correct identities and usable acquisition guidance. | Reconcile Arc09's offered downloads against the ten-target Arc03 P0 queue; acquire offered artifacts, verify checksum/platform/source/OTP applicability and installation/reference journey; retain explicit missing-target decisions. | serious | Duncan's 2026-10-09 request; Arc09 | open | | A source ZIP or landing page alone does not satisfy a binary target. |

## Revision history

| Version | Date | Change | Approval state |
| --- | --- | --- | --- |
| 1.0 | 2026-10-08 | Initial 12 child-closure rows and 9 project composition/governance rows, paired with project-plan v1.0 | Draft awaiting review; 21 open rows |
| 1.1 | 2026-10-08 | Record roadmap approval and Arc01 opening against project-plan v1.1; criteria unchanged | Roadmap approved; 21 delivery rows remain open |
| 1.2 | 2026-10-09 | Pair with project-plan v1.3; retain P-01–P-21 unchanged, add P-22/P-23 for exact matrix retention and OTP22–26 Linux/macOS P0 artifact/acquisition coverage | Operator-requested addition; 23 rows open, artifact execution/acceptance pending |
| 1.3 | 2026-10-09 | Pair with project-plan v1.5; record Slice02 correction status in P-01 without changing any criterion | Routine review/status maintenance; all 23 rows open |
| 1.4 | 2026-10-09 | Pair with project-plan v1.6; record Slice02 partial acceptance/iteration02 without changing any criterion | Routine review/status maintenance; all 23 rows open |
| 1.5 | 2026-10-09 | Pair with project-plan v1.7; record Slice02 iteration02 review/bounded iteration03; unchanged criteria | Routine review/status maintenance; all 23 rows open |
| 1.6 | 2026-10-09 | Pair with project-plan v1.8; record Slice02 iteration03 partial acceptance/iteration04; unchanged criteria | Routine review/status maintenance; all 23 rows open |
| 1.7 | 2026-10-09 | Pair with project-plan v1.9; record Slice02 six-row acceptance and receipt-only iteration05; unchanged criteria | Routine review/status maintenance; all 23 rows open |

| 1.8 | 2026-10-19 | Pair status with project-plan1.19: preserve all23 criteria/open statuses, supersede historical P-01 child-status summary with actual Slices01–04 state | Routine status maintenance; Arc01 composition still open |
