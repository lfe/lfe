# Arc01 Slice01 — acceptance ledger

**Contract:** [slice-plan.md](slice-plan.md) and [cc-prompt.md](cc-prompt.md),
initial pass, 8 October 2026. All eight rows remain open at issuance.
CC appends attested evidence/proposed disposition and leaves self-attested rows
open; CDC sets `done` only after independent reproduction. Any deferred/no-op
row requires an explicit reason, owner/next gate and re-entry condition or rationale.

| ID | Criterion | Verify | Significance | Origin | Status | Evidence | Notes |
| --- | --- | --- | --- | --- | --- | --- | --- |
| S1-01 | All five inherited inputs have exact pinned identities and remain byte-for-byte unchanged. | Independently recompute SHA-256 before/after; compare prompt expectations, baseline records and planning commit objects. | serious | Preservation and provenance | open | | Identity is not a reading receipt. |
| S1-02 | Planning and all eight named source roots have an honest current-state record. | Re-run root/HEAD/branch/status queries or inspect retained access failures; compare baseline/execution records and drift notes. | correctness-grade | Local source map | open | | Missing related roots may be recorded; primary roots are prerequisites. |
| S1-03 | The website tracked `src/` filename population is captured and partitioned exactly once per path. | Re-query `git ls-files -z -- src`; compare exact sets and recorded partitions; check uniqueness/disjointness and union. | correctness-grade | Declared population | open | | No route/deployment/content-correctness claim from filenames. |
| S1-04 | All 43 LFE and 61 Erlang source-register IDs preserve their literal labels/destinations and resolve to input locations. | Compare exact source-row ID sets, raw cells and line locators against the pinned reports, including compound destination cells. | correctness-grade | Source registers | open | | 104 source IDs do not mean 104 unique URLs or live checks. |
| S1-05 | All 15 LFE + 14 Erlang assessment rows and 8 + 8 unresolved questions are carried without weakening or false resolution. | Compare exact namespaced ID sets, original cells/text, dates and locators; inspect concrete owner/next-check/limits fields for every record. | serious | Inherited registers | open | | Positive findings stay positive; questions remain unresolved. |
| S1-06 | The handoff accounts for the saved content families and seven groups while keeping local observations separate from inherited/external evidence. | Inspect summary against LFE §3.3, classification and witness reads; trace four Build It targets, `/docs` boundary, chapter uncertainty and runtime-input gap. | correctness-grade | Coverage and authority | open | | No support policy or inferred published commit. |
| S1-07 | Required text/query intake, contract readback and validation attempts are recorded honestly. | Check execution record against manifest and actual tool outputs/ranges; JSON/local-link checks plus semantic witness review; retain failures/unrun checks. | correctness-grade | Prompt intake and proof | open | | Reading/query receipts remain attestation. |
| S1-08 | Only authorized outputs changed and the return packet preserves a scoped proposed-done handoff. | Inspect actual diff, source before/after status/digests, explicit-path commit and closing-report row walk/artifact inventory/bubble-up. | serious | Scope and Expedited Mode | open | | No parent-plan/prompt edits or independent-verification file by CC. |
