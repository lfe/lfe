# Arc01 Slice01 baseline summary

**Captured:** 2026-10-09 UTC (see the exact timestamp in [baseline.json](baseline.json))
**Assignment:** `arc01-evidence-baseline/slice01-baseline-intake/cc-prompt.md`
**Status:** CC proposed-done evidence; independent CDC acceptance is pending.

## What this baseline records

The [baseline](baseline.json) identifies the opened prompt, five preserved inputs, planning state, eight source repository snapshots, the website's complete tracked `src/` filename population, and eight bounded source witnesses. [findings.json](findings.json) carries each inherited assessment, unresolved question, and source-register entry with its original report locator, raw text/cells, and a proposed arc owner/next check. The source packet remains byte-identical to its pinned `09bae67ee3ffb5b2365e3dd985b3eb16595a15f0` objects.

At intake, planning was on `planning` at `e8f4c394df1c5a902086fcb45f3249a52bc6c453`. That commit opened this assignment; the issued prompt's SHA-256 is recorded in `baseline.json`. Repository HEADs and complete porcelain status projections are in that file. In particular, the website was at local `main` HEAD `036e231b09c48966da26d2227bccb4fa3b295df9` with modified build-tool files and an untracked verification brief; LFE was at `master` HEAD `dae7489ebf9588e5f746a34cd4f8ff83d1469eef` with untracked `rebar.lock`. These are local checkout identities, not deployed-site provenance.

The website's `git ls-files -z -- src` population contains 385 unique tracked paths: 124 posts, 27 templates, 12 data files, 35 pages, and 187 other files. The partition is disjoint and exhaustive. Ignored/untracked files and generated output are outside this denominator. A filename does not prove that Cobalt emits a route or that a URL is live.

## Local witnesses and limits

The current website source selects `src/` as Cobalt's source and `./site` as destination in [`_cobalt.yml`](../../../../../../../lfe.github.io/_cobalt.yml) (website-relative evidence is fingerprinted in `baseline.json`). [`src/_data/site.yml`](../../../../../../../lfe.github.io/src/_data/site.yml) labels 2.2.0 current and 2.1.5 prior, with `develop` as prerelease. Those are source labels, not a support statement. [`src/docs.html`](../../../../../../../lfe.github.io/src/docs.html) redirects to `/use`; the established project decision remains that `/docs` is the hand-curated gateway and generated reference is a later link when available. ExDoc is anticipated, not confirmed.

[`src/_data/home/buildit.yml`](../../../../../../../lfe.github.io/src/_data/home/buildit.yml) provides four practical URL seeds:

- Scripts: `https://lfe.io/reference/lfe-rebar3/current/command-ref/projects/main.html`
- Stand-alone Libraries: `https://lfe.io/reference/lfe-rebar3/current/command-ref/projects/lib.html`
- OTP Applications: `https://lfe.io/reference/lfe-rebar3/current/command-ref/projects/app.html`
- OTP Releases: `https://lfe.io/reference/lfe-rebar3/current/command-ref/projects/release.html`

The four URLs match inherited report references, but this local source read does not reproduce the historical HTTP 404 results. Similarly, the inherited heading-only manual results remain qualified observations: this slice did not inspect or render the book chapters, so source/rendered readiness remains for Arc01 Slice03. The report's §2.3 retrieval limits remain in force.

[`pagefind.yml`](../../../../../../../lfe.github.io/pagefind.yml) scopes the search index to `site` and `blog/**/*.html`, excluding navigation, header, footer, and blog pagination. This is configuration evidence, not a search coverage or quality result. The current `lfesite` [`build.rs` run function](../../../../../../../lfe.github.io/tools/lfesite/src/cmd/build.rs) sequences prerender, blog resolution, Sass, Tailwind, Cobalt, sitemap generation, and Pagefind. Its working bytes differ from website HEAD; the function describes a local wrapper and does not establish publication or deployment. The complete file fingerprint and exact read extent are in `baseline.json`.

## Inherited population and handoff

The saved reports yielded 15 LFE and 14 Erlang assessment rows, 8 unresolved questions in each report, and 43 LFE plus 61 Erlang source IDs. The question records remain `unresolved-in-report`; the assessment and source records remain dated inherited evidence. Source IDs can contain compound destinations and do not mean one unique URL or a completed live check. No external claim was refreshed here.

The eleven LFE content families in §3.3 of the [LFE investigation](../../../artifacts/first-steps/lfe-io-ia-investigation.md) are: main gateway; book catalogue; technical publishing; book chapters; language reference; release registry; tooling documentation; earlier documentation generations; legacy reference targets; community resources; and historical technical papers. The seven work groups in the [classification](../../../artifacts/first-steps/lfe-work-classification.md) are evidence/baseline assessments; compatibility/version policy; release engineering/distribution; information architecture/content; design foundations/visual experience; experiments/validation; and scope/decisions/collaboration. These lists preserve their original scope; they are not a route census or implementation order.

The [handoff](../../../artifacts/first-steps/HANDOFF.md) records an established, curated `/docs` gateway. Navigation, content, release, design, compatibility, and support recommendations in the [Draft 0.1 proposal](../../../artifacts/first-steps/lfe-ia-content-proposal.md) remain proposals except for that explicit `/docs` decision. The dated release-history document is a local witness with claims through June 2026, including inferred compatibility and recommendations. It does not identify the deployed site snapshot or establish current support policy; no recommendation from it is selected here.

The inherited packet has no technical-failure stacktrace/environment packet. Arc01 Slice04 must request or locate those inputs; no failure was reproduced and no cause is inferred. Concrete per-record routing and all remaining provenance are in [findings.json](findings.json). Reading/query extents, the contract readback, validation attempts, and limits are in [execution.md](execution.md). The assignment and row-by-row proposed disposition are in the [closing report](../closing-report.md).
