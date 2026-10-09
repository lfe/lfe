# lfe.io information architecture investigation

**Date:** 8 October 2026  
**Status:** Evidence and assessment for the LFE IA investigation; proposals remain open for discussion.  
**Companion:** [erlang.org IA investigation](erlang-org-ia-investigation.md)

## 1. Main finding

lfe.io has a compact entrance layer and a much larger information estate behind it. Its strongest qualities are an immediately recognizable Lisp/BEAM identity, practical demonstrations, and generous access to community knowledge. Its greatest weakness is that the links between those layers do not consistently preserve currency, completion status, or release identity.

The most consequential findings are concrete:

- All four home-page “Build It” calls to action lead to old reference URLs that returned HTTP 404 during inspection. [L01, L25–L28]
- The global download menu identifies 2.2.0 as current; the official release registry identifies v2.2.2 as latest. [L02, L09–L10]
- The promoted setup route combines a 2.2.0 dependency pin with prerequisites that describe OTP 19–23 as modern. This is a mixed-age route, not an established support matrix. [L17–L18]
- An older documentation surface still labels itself Current/Dev and directs many different tasks to the same `XXX` placeholder. One such destination was confirmed to return 404. [L22–L24]
- Two important manual destinations returned chapter headings without substantive instructions. Their completeness needs source/rendered verification before the site treats them as adequate task destinations. [L19–L20]

**Assessment:** The next IA discussion should focus on making the existing knowledge dependable at the point where a reader chooses it. Adding more top-level pages alone would leave the significant problems intact.

The Erlang investigation supplies useful patterns for release identity and documentation relationships. LFE already supplies strengths that should survive any blend: its personality, worked examples, books, talks, and relatively small navigation. A final blended architecture is deliberately not selected here.

## 2. Scope and evidence boundaries

### 2.1 What was inspected

This investigation examined the public home, Learn, Use, Community, About, Books, and Blog surfaces; the blog's four browsing indexes; official language and tooling repositories; all four promoted books' entrances; representative setup, version, interop, and deployment chapters; and the older `docs.lfe.io` Current and Dev surfaces. It followed the four prominent build links individually and compared acquisition claims with the official release registry.

The scope includes externally hosted content that lfe.io presents as part of the reader's journey. A book on another domain is still relevant when Learn or Use sends a user there to complete a task.

### 2.2 Evidence classes

| Class | Meaning in this report |
|---|---|
| Observed | Text, a link destination, a directory count, or a response directly retrieved |
| Assessed | A reasoned implication of those observations; user difficulty has not been measured |
| Proposed | A candidate improvement or decision input, not an approved design |
| Unresolved | Evidence insufficient to establish behaviour, completeness, or present policy |

Task journeys below are link traces, not usability tests. No installation, compiler, container, or LFE/OTP compatibility test was run. No changes were made to the website, repositories, or tickets.

### 2.3 Retrieval limitations

Public retrieval can return cached representations. Several pages were reported crawled today, while some repository and book representations were older. The observed menu mismatch was checked against multiple main-site representations, including search results reported crawled today. Even so, this report is an evidence record, not a pinned deployment snapshot.

Extracted pages sometimes omitted the global header and interactive chrome. Their omission is not evidence that navigation or search is absent. Fragment destinations and scrolling behaviour were not verified. Search relevance, keyboard interaction, responsive layout, contrast, and assistive technology behaviour remain untested.

A reported HTTP 404 is distinguished from restricted retrieval, an inaccessible URL, or an empty extraction. Failed access to GitHub ZIP downloads, Docker tags, the sitemap, and some source files is **not** counted as a broken site link. Heading-only manual results are flagged for further verification rather than declared definitively empty.

This is a structural inventory with representative destination checking, not a complete URL census, an audit of every blog post or chapter, or an accessibility certification.

## 3. Information estate and navigation

### 3.1 Global entrances

The retrieved header has five section links and a Download menu. Home is available through the site identity. The menu has three acquisition choices: current 2.2.0, previous 2.1.5, and development HEAD. Those targets are GitHub ZIP archives; the development target names `develop`. [L02, L09]

| Entrance | Destination | Principal role | IA assessment |
|---|---|---|---|
| Home | `/` | Explain and demonstrate LFE | Strong identity and discovery; also carries many resource promotions |
| Learn | `/learn/` | Introduce language concepts and learning paths | Useful newcomer orientation; handoffs need consistent currency |
| Use | `/use/` | Route working developers to tooling and reference | Functional documentation gateway despite its broad label |
| Community | `/community/` | Connect people, channels, and projects | Useful reach; classifications mix resource types and tasks |
| About | `/about/` | Language background, history, creator | Gives provenance and continuity |
| Blog | `/blog/` | Technical archive and current writing | Substantial content family with its own browsing hierarchy |
| Download | External GitHub archives | Acquire selected source | Selection bypasses a release-information landing page |
| Books | `/books/` | Book catalogue and contribution routes | Exists outside the five main section links |

Evidence: L01–L07, L09. Table roles and assessments are analytical classifications.

The word **Use** combines reference, setup, interoperability, and deployment. That is economical, but a reader arriving specifically for “documentation” must infer which section covers that need. This is a candidate label/wayfinding question, not evidence that every reader struggles with it.

### 3.2 Footer and local navigation

The footer supplies a second navigation layer: Learn, Use, Community, and About, each with four subject links. Several Use links resolve to the same Use page in the retrieved representations. This can be entirely appropriate for section anchors; fragment and scroll behaviour were not established. [L01–L06]

The blog adds Archive, Categories, Tags, and Authors. Books has its own catalogue. The result is a small global hierarchy with richer local structures, a sound pattern to preserve if the labels and destinations remain dependable. [L06–L08]

### 3.3 Page and content families

| Family | Location | Observed structure | Boundary |
|---|---|---|---|
| Main gateway | `/`, `/learn/`, `/use/`, `/community/`, `/about/` | Five subject/task surfaces around Home | All inspected |
| Book catalogue | `/books/` | Four promoted books, plus three superseded standalone books | Catalogue and published entrances inspected |
| Technical publishing | `/blog/` and dated post URLs | Featured/recent selections, four browsing indexes, articles | Representative posts, not full corpus review |
| Book chapters | `cnbbooks.github.io` | Separate manuals/tutorials with their own navigation | Representative chapters inspected |
| Language reference | `github.com/lfe/lfe/.../doc/src/` | Guide and module/man-page sources | Guide destination inspected; other links identified |
| Release registry | `github.com/lfe/lfe/releases` | Tagged release records and changes | Latest and registry inspected |
| Tooling documentation | `github.com/lfe/rebar3` | README, quick start, commands, contribution material | README inspected |
| Earlier documentation generations | `docs.lfe.io/current/`, `/dev/` | Distinct layouts and legacy version navigation | Both entrances and representative link inspected |
| Legacy reference targets | `/reference/lfe-rebar3/current/...` | Old project-type documentation routes | Four home-page targets returned 404 |
| Community resources | Chat services, video channels, GitHub organizations, Docker Hub | Outward links rather than centrally hosted content | Main directory inspected; service activity unmeasured |
| Historical technical papers | Links from Learn | Background on Erlang and Lisp | Identified, not individually content-audited |

Evidence: L01–L07, L11–L24, L25–L29, L32–L34.

The blog archive lists year counts of 8, 2, 9, 84, and 21 for 2026, 2019, 2016, 2015, and 2014 respectively: **124 listed entries** when added. That is an index-derived figure, not a verified count of unique published URLs. The migration announcement discusses 116 posts at migration time; these are different dated observations, not automatically conflicting counts. [L08, L12]

**Assessment:** “Small site” describes the gateway better than the corpus. The work is manageable, but any future migration scope needs to include the books, archive, and legacy entry points.

## 4. What is already working

### 4.1 LFE has a clear identity

The home page demonstrates language features and promotes practical project forms, books, and talks. Its alien/Lisp Machine imagery and humour create a distinctive register. [L01] The important IA advantage is that readers can encounter actual language shape early, instead of having to decode a purely abstract pitch.

The design question is how to maintain that character while making the next action unmistakable. Playful language can coexist with precise installation and support information; changing the visual personality is not a prerequisite for improving the structure.

### 4.2 Learning and working have separate entrances

Learn introduces a progression; Use organizes practical destinations. [L02–L03] That distinction is worth preserving even if later discussion changes the labels. The pages can serve different intentions without duplicating a complete reference corpus inside the marketing site.

### 4.3 The book catalogue exposes provenance

Each book has a published destination, source repository, and feedback route. Three older standalone books are explicitly identified as superseded by the manual. [L06] This is a useful lifecycle pattern already present in LFE, rather than something that must be imported from Erlang.

The opportunity is to make that status travel with each resource wherever it is promoted.

### 4.4 The blog has substantial recent groundwork

The June 2026 migration account describes a unified home for the blog, retained old URLs, Pagefind search, and version-context banners whose historical metadata work remains unfinished. [L12] A sampled old post URL returns a relocation page linking to its new destination. This verifies a preserved route, not the announcement's universal redirect claim or HTTP 301 mechanism. [L30]

The dated archive and article metadata help distinguish publication time. Dates alone do not establish present technical applicability, but they give the proposed version-context work a useful foundation.

### 4.5 Existing maintenance structure offers a starting point

The site repository describes separate page content, homepage resource data, shared download configuration, and templates. [L14] That separation suggests an economical place to centralize repeated facts. It does not prove that current metadata is generated from release records or that every resource promotion shares one record.

## 5. Acquisition, releases, and version context

### 5.1 The download label is stale

The site's current/previous menu values are 2.2.0 and 2.1.5. GitHub's registry includes v2.2.1 and v2.2.2 and marks v2.2.2 latest. [L09–L10] The stale “current” label is the confirmed problem; the site might intentionally preserve 2.1.5 as an older branch example, so its “previous” label needs definition rather than an assumed support interpretation.

The v2.2.2 record explicitly discusses OTP 28/29 compatibility and a fix for an OTP 28+ command-line evaluation crash. [L10] That makes release choice materially relevant to users following present installation guidance. It does not establish that all older combinations fail or that the reported project-specific runtime issue has this cause.

### 5.2 An archive is not a release explanation

Selecting Download transfers the reader directly to source archives. The menu does not itself provide release notes, an installation path, verified OTP combinations, support status, or matching documentation. These may exist elsewhere; they are not brought together at this selection point. [L09–L10]

**Assessment:** A download can be technically obtainable while the decision to choose it remains under-explained. Erlang's release-record pattern is particularly useful here: a durable release identity can connect several acquisition mechanisms without making the homepage carry all their detail.

### 5.3 The setup path mixes eras

| Surface | Observed version context | What the observation establishes |
|---|---|---|
| Global download menu | Current 2.2.0; previous 2.1.5 | Site selection metadata is behind the official latest release |
| Official release registry | Latest v2.2.2 | An identifiable newer release record exists |
| Manual Development Setup | LFE 2.2.0 and plugin 0.5.5 pins | This specific recipe selects those versions |
| Manual Prerequisites | Erlang 19–23 described as modern at writing time | Prerequisite guidance retains an older temporal frame |
| Manual setup transcript | OTP 23 and LFE 1.3-dev | Sample output comes from a different environment than the recipe |
| Tutorial installation | `master` stable, `develop` unstable | Installation uses branch identity rather than an exact tagged release |
| SICP Versions page | Current content targets LFE 2.0-dev | This book exposes version intent, but its temporal explanation has aged |
| Tooling README | Reports testing on OTP 25–29 | Repository documentation provides a different, explicitly scoped test claim |

Evidence: L09–L10, L17–L18, L21, L29, L31.

These are distinct claims: recipe pins, historical examples, branch policy, and test coverage cannot be combined into one support promise. The plugin's reported test range also does not establish every LFE/compiler/runtime combination.

**Assessment:** Newcomers should not have to reconcile these sources before they can decide what to install. A candidate improvement is one maintained setup recommendation with explicit prerequisites, a verification step, and links to exact release information. The underlying technical promises still require maintainer validation.

### 5.4 Containers need their own acquisition evidence

The site offers an unqualified image command; the 2019 Docker tutorial documents older LFE/OTP images. [L02–L03, L32] The current tag listing could not be inspected, and no image was pulled. Consequently this report makes no claim about what that command installs today.

The useful IA requirement is independent of that uncertainty: a recommended container should disclose the selected environment or explain how to verify it. A container's moving default tag is a different identity from a source release.

## 6. Documentation, books, and historical seams

### 6.1 Authority has several overlapping homes

Readers encounter a machine manual, a tutorial, language/man-page source in GitHub, Erlang reference, tooling documentation, and legacy Current/Dev sites. [L03, L06, L15–L24, L29] Multiple homes are reasonable because they serve different kinds of information. The weakness is that there is no consistently carried explanation of which source owns which kind of answer and which release it describes.

| Question | Natural authority to consider | What needs to accompany the handoff |
|---|---|---|
| What syntax or library operation does LFE support? | Language reference tied to a source release | Version and scope |
| How do I learn the language? | Tutorial or maintained learning path | Audience, prerequisites, completion state |
| How do I create/build a project? | Tooling documentation | Plugin/rebar3/LFE environment |
| What does an OTP API do? | Erlang reference | Applicable OTP release |
| How does LFE use an OTP concept? | LFE-specific examples or manual chapters | Working example and tested environment |
| Is an old article still applicable? | Article plus review/version metadata | Historical status and a present alternative |

This is a proposed division of responsibility, not a decision to merge repositories or rewrite the books.

### 6.2 Promoted destinations need completion checks

Use's Releases and Calling Erlang links returned chapter headings without substantive body text. Creating LFE Projects also returned a heading-only representation. Other chapters, including setup and the REPL overview, returned substantial text through the same retrieval mechanism. [L17, L19–L20, L33–L34]

**Observed:** The retrieved destinations did not supply the promised practical explanation.  
**Unresolved:** A rendered/source check is still needed to exclude an extraction omission and establish exact completion status.  
**Assessment:** Until verified, these are weak endpoints for prominent task links. A book-wide “in progress” note does not tell the reader which individual route is ready.

There is also a narrow label/destination mismatch: Learn labels its Part II link as Data Types, while the retrieved destination title is Code as Data. Its text covers data structures, so this is an orientation inconsistency rather than evidence of an unrelated destination. [L02, L35]

### 6.3 The old documentation estate remains reachable

`docs.lfe.io/current/` has an older resource-directory structure with GitBook and man-page links. Its “new docs” link reaches `/dev/`. That page offers legacy version choices and sends many different listed tasks to `/dev/XXX/`; a representative click returned 404. [L22–L24]

This surface should be treated as a distinct legacy generation in future planning. It was reached from the older repository and historical documentation references, not established as a prominent current global-nav destination.

**Assessment:** “Current” and “Dev” are especially strong words in a historic URL. Leaving them without temporal context allows an obsolete experiment to appear authoritative to search arrivals and older inbound links.

The appropriate future treatment could be archival context, selective redirects, or replacement. This investigation does not select among them. A single redirect of every legacy route to Home would preserve neither task meaning nor historical specificity.

### 6.4 Book lifecycle information is uneven

| Book | Published destination | Observed status/context |
|---|---|---|
| LFE Machine Manual | https://cnbbooks.github.io/lfe-manual/ | Promoted as a comprehensive work in progress; sampled chapters have varying readiness |
| LFE Tutorial | https://cnbbooks.github.io/lfe-tutorial/ | Structured language tutorial; installation chapter uses stable/development branches |
| Casting SPELs in LFE | https://cnbbooks.github.io/lfe-casting-spels/ | Project-based learning through a text adventure; entrance inspected, full technical currency not assessed |
| SICP, LFE edition | https://cnbbooks.github.io/lfe-sicp/ | Homepage describes partial completion; Versions page names LFE 2.0-dev |

Evidence: L01, L16–L21, L31, L40–L42. These are publication destinations, not four centrally hosted lfe.io books.

The homepage acknowledges unfinished book material; the catalogue chiefly provides provenance links, with explicit supersession for old standalone books. [L01, L06] The SICP Versions page still frames its current content around 2.0-dev and a future 2.0 release. [L21]

**Assessment:** Publication, completeness, and technical applicability are separate states. A published work may remain unfinished; an old tutorial may still teach a valid concept while requiring revised installation instructions. Resource metadata should distinguish those dimensions rather than assigning one broad “old” label.

### 6.5 Blog taxonomy is rich but accumulated

The indexes provide chronological, categorical, topical, and author-based browsing. The tag index also contains related labels such as docs/documentation, language/languages, release/releases, and howto/howtos. [L07–L08, L11, L36–L37]

**Assessment:** The archive's breadth is valuable, while vocabulary drift can split related material across entrances. Normalizing discovery labels could improve retrieval without changing historical prose or breaking old tag URLs. Search effectiveness and the effect of these overlaps remain unmeasured.

## 7. User journeys

| Task | Route traced | Available help | Friction or boundary | Evidence |
|---|---|---|---|---|
| Understand why LFE exists | Home → Learn/About | Language identity, background, examples | Assessments of pitch clarity need actual reader testing | L01–L02, L05 |
| Try LFE locally | Learn → manual setup → prerequisites | Explicit recipe and REPL steps | Mixed version eras and missing single current recommendation | L02, L17–L18 |
| Obtain a chosen language release | Download → GitHub ZIP | Three acquisition choices | Stale current label; notes/compatibility not joined at selection | L09–L10 |
| Create a script | Home → Scripts call to action | Concrete project intention | Destination returned 404 | L25 |
| Create a library | Home → Libraries call to action | Concrete project intention | Destination returned 404 | L26 |
| Create an OTP application | Home → Applications call to action | Concrete project intention | Destination returned 404 | L27 |
| Build an OTP release | Home → Releases call to action; Use → manual Releases | Two available entrances | First returned 404; second supplied no instructions in extraction | L19, L28 |
| Look up LFE language behaviour | Use → GitHub language guide | Reference maintained with language source | Branch-relative source is not automatically installed-version reference | L15 |
| Look up an OTP operation | Use → Erlang module reference | Direct access to upstream reference | Sampled old route redirects to moving current documentation | L38 |
| Learn through a book | Home/Books → published book | Catalogue, source, feedback | Completion/version context varies by entrance | L06, L16–L21 |
| Find an old technical article | Blog → archive/category/tag/author | Multiple browsing paths; dated articles | Currency must be judged separately; search untested | L07–L08, L11–L12 |
| Ask for help | Home Discord or Community → channel | Multiple discussion routes | No measured evidence of channel activity or response time | L01, L04 |
| Find ecosystem code | Community → GitHub organizations | Broad discoverability | Organization names require reader knowledge of project purpose/status | L04 |
| Arrive through an old blog link | Old domain → relocation page → current article | Sampled path retains topic identity | Universal coverage and HTTP redirect status unverified | L30 |

These traces identify interruption points, not measured failure rates. In particular, replacing a dead link with a responding page is insufficient if that page does not help the reader perform the task.

## 8. Assessment register

Priority describes likely user consequence, not an implementation schedule. Confidence distinguishes directly observed facts from inferred effects.

| ID | Finding | Consequence assessed | Evidence/confidence | Significance |
|---|---|---|---|---|
| F01 | Compact navigation and distinct learning/working entrances | Useful structure to retain | High for observed hierarchy | Strength |
| F02 | Four prominent build destinations return 404 | Interrupts advertised routes into practical work | High; each followed separately | High |
| F03 | Menu current version trails latest release | Can direct acquisition to an older source release without explanation | High; multiple site representations plus official registry | High |
| F04 | Setup, prerequisites, and transcripts carry different version eras | Readers must reconcile suitability themselves | High for content; usability effect inferred | High |
| F05 | Download selection bypasses release explanation | Version choice lacks joined prerequisites and documentation context | High for link model | High |
| F06 | Important manual destinations yield heading-only results | Promoted task assistance may be incomplete | Medium; source/rendered verification outstanding | High if confirmed |
| F07 | Legacy Dev tasks share a placeholder that returns 404 | Old search/inbound routes can reach unfinished navigation | High for sampled target and repeated link identity | High |
| F08 | Legacy Current/Dev labels lack clear temporal framing | Historical surface can be mistaken for active guidance | High for labels; confusion inferred | High |
| F09 | Book source and feedback links are explicit | Useful provenance and participation model | High | Strength |
| F10 | Completion/version information varies between resource entrances | Readers cannot reliably choose applicable material | High for sampled differences | Medium–high |
| F11 | Blog migration has preservation and version-context groundwork | Existing infrastructure can support lifecycle improvements | High for announcement; mechanism/coverage untested | Strength/opportunity |
| F12 | Related blog tags divide vocabulary | Topic discovery can fragment | High for labels; search impact unmeasured | Medium |
| F13 | Community taxonomy mixes chat, publishing, video, code, and historical organizations | Getting help and choosing maintained libraries need reader interpretation | High for directory; activity unknown | Medium |
| F14 | Upstream reference links can target moving current OTP docs | Installed environment can lose version context | High for sampled redirect | Medium–high |
| F15 | Shared site configuration exists | Repeated current-release facts have a plausible maintenance home | High for repository description; synchronization unproven | Opportunity |

## 9. Comparison with erlang.org

The Erlang column summarizes the separately sourced companion investigation, especially its navigation, release, documentation, resource, and assessment sections. This is a comparison of observed patterns, not a fresh exhaustive Erlang audit.

| Dimension | erlang.org finding | lfe.io finding | Input to later blending discussion |
|---|---|---|---|
| Global structure | Explicit Download and Documentation entrances | Learn/Use split; Download archives; Books outside main section list | Choose labels around reader tasks while retaining a compact gateway |
| Release identity | Major overviews and exact patch records | GitHub records exist; site menu points directly to archives | Link source identity, changes, acquisition, and applicable docs |
| Documentation versions | Major collections and current aliases; exact artifact identity differs | Branch source, moving OTP docs, books, and legacy Current/Dev | Make each documentation surface's identity and scope explicit |
| Learning formats | Books and videos distributed across directories | Books and talks are highly visible | Retain deliberate learning-format entrances |
| Historical material | Archives and older docs coexist; sampled redirects preserve routes | Legacy docs, dead reference paths, preserved blog route | Inventory and preserve meaningful old destinations |
| Content reliability | Confirmed stale FAQ and mismatched book metadata | Stale acquisition label, mixed-era setup, broken build routes | Validate the promise and applicability of a link as well as its existence |
| Personality | Broad language/platform gateway | Distinctive Lisp/BEAM identity and examples | Borrow structural precision without flattening LFE's voice |
| Maintenance | Generated release data and declared provenance | Shared config/resource files and recent publishing work | Reuse existing structure; decide authoritative facts and consumers |
| Scale | Large release/documentation estate | Smaller gateway, substantial linked corpus | Use only structures justified by LFE's actual tasks |

**Assessment:** The strongest transferable Erlang pattern is the relationship among release, artifact, documentation, and support information. LFE's own learning and publishing strengths offer a better starting point for its books, talks, and identity. The blend should be evaluated against actual journeys rather than visual resemblance.

## 10. Candidate discussion agenda

The following are proposals for review, not assigned work or approved architecture.

| Topic | Evidence driving it | Concrete decision still needed |
|---|---|---|
| Practical entry points | Four dead build links | Which verified instructions should each project-type entrance reach? |
| Recommended environment | Stale menu and mixed setup guidance | What LFE/OTP/tooling combination is recommended and tested? |
| Release information | Existing GitHub records, sparse site selection context | What release facts belong on lfe.io, and which are linked upstream? |
| Documentation authority | Several overlapping homes | Which surface owns reference, learning, tooling, and historical material? |
| Resource status | Incomplete/aged book guidance | What per-resource and per-chapter status needs to appear before selection? |
| Legacy handling | Old docs and reference URLs | Which routes need contextual archives, replacements, or targeted redirects? |
| Navigation labels | Use carries the documentation role | Can likely users reliably find setup and reference with the current labels? |
| Blog discovery | Accumulated tags and categories | Which discovery vocabulary should be normalized while preserving old URLs? |
| Community directory | Mixed resource grouping | How should readers distinguish help, participation, and maintained code? |
| Shared metadata | Existing configuration and homepage data | Which repeated facts can have one maintained source? |

A small release/resource model could carry: stable identity, audience/task, owner/source, release applicability, tested environment, completion state, review date, publication URL, and replacement/archive relationship. These fields are a candidate checklist; the project should keep only those it can maintain reliably.

A proposed evaluation sequence is to repair and verify interrupted routes, establish current guidance and authority, then test alternate navigation/selection designs. That sequence is advisory and does not pre-empt the later decision with Robert.

## 11. What remains open

1. Actual supported and tested LFE source/build/runtime/OTP combinations, including the reported runtime failure.
2. Exact deployed source commit and an exhaustive URL/redirect inventory.
3. Current container contents, tags, and platform behaviour.
4. Rendered/source confirmation of heading-only manual chapters and the complete chapter readiness inventory.
5. Search coverage across the main site, blog, external books, and reference sources; current retrieval effectiveness.
6. Header/footer anchor behaviour, mobile navigation, keyboard interaction, and accessibility.
7. Activity and recommended status of community channels and ecosystem projects.
8. The final shared direction for navigation, documentation versions, release selection, design, and preservation.

The two investigations now provide a comparative basis for that last discussion. They do not resolve the underlying compatibility policy or authorize implementation.

## 12. Source register

Inspected 8 October 2026. Retrieval qualifications are in §2.3. Repeated appearances of a URL are one underlying source. URLs are evidence destinations; moving aliases may change later. A 404 entry records a direct tool-reported HTTP result, not an independently captured server log.

| ID | Source | URL |
|---|---|---|
| L01 | Home, resource promotions, build links, footer | https://lfe.io/ |
| L02 | Learn; header and current setup promotion | https://lfe.io/learn/ |
| L03 | Use; reference/tooling/deployment gateway | https://lfe.io/use/ |
| L04 | Community directory | https://lfe.io/community/ |
| L05 | About, history, creator | https://lfe.io/about/ |
| L06 | Book catalogue and superseded standalone books | https://lfe.io/books/ |
| L07 | Blog landing | https://lfe.io/blog/ |
| L08 | Blog archive and year counts | https://lfe.io/blog/archive/ |
| L09 | Download menu targets, identified by following header links | https://github.com/lfe/lfe/archive/2.2.0.zip ; https://github.com/lfe/lfe/archive/2.1.5.zip ; https://github.com/lfe/lfe/archive/refs/heads/develop.zip |
| L10 | Official release registry and latest record | https://github.com/lfe/lfe/releases ; https://github.com/lfe/lfe/releases/tag/v2.2.2 |
| L11 | Blog category index | https://lfe.io/blog/categories/ |
| L12 | Blog migration announcement; stated plans and limitations | https://lfe.io/blog/tutorials/2026/06/03/0230-welcome-to-the-new-lfe-blog/ |
| L13 | April site tooling announcement | https://lfe.io/blog/tutorials/2026/04/20/0005-lfeio-50-rust-tooling-all-the-way-down/ |
| L14 | Official website repository README and content structure | https://github.com/lfe/lfe.github.io |
| L15 | Linked language guide; develop branch | https://github.com/lfe/lfe/blob/develop/doc/src/lfe_guide.7.md |
| L16 | Manual entrance and table of contents | https://cnbbooks.github.io/lfe-manual/ ; https://cnbbooks.github.io/lfe-manual/toc.html |
| L17 | Manual Development Setup | https://cnbbooks.github.io/lfe-manual/part1/intro/setup.html |
| L18 | Manual Prerequisites | https://cnbbooks.github.io/lfe-manual/part1/intro/prereq.html |
| L19 | Manual Releases; heading-only retrieval | https://cnbbooks.github.io/lfe-manual/part5/rels/ |
| L20 | Manual Calling Erlang; heading-only retrieval | https://cnbbooks.github.io/lfe-manual/part8/erl-int/call-erl.html |
| L21 | SICP Versions | https://cnbbooks.github.io/lfe-sicp/versions.html |
| L22 | Legacy documentation Current | https://docs.lfe.io/current/ |
| L23 | Legacy documentation Dev | https://docs.lfe.io/dev/ |
| L24 | Legacy Dev placeholder; HTTP 404 | https://docs.lfe.io/dev/XXX/ |
| L25 | Home Scripts target; HTTP 404 | https://lfe.io/reference/lfe-rebar3/current/command-ref/projects/main.html |
| L26 | Home Libraries target; HTTP 404 | https://lfe.io/reference/lfe-rebar3/current/command-ref/projects/lib.html |
| L27 | Home Applications target; HTTP 404 | https://lfe.io/reference/lfe-rebar3/current/command-ref/projects/app.html |
| L28 | Home Releases target; HTTP 404 | https://lfe.io/reference/lfe-rebar3/current/command-ref/projects/release.html |
| L29 | Tooling repository README; old organization URL redirects here | https://github.com/lfe/rebar3 |
| L30 | Sampled old blog route and relocation link | https://blog.lfe.io/tutorials/2019/05/13/1549-running-lfe-in-docker-updated/ |
| L31 | Tutorial installation and stable/development branch advice | https://cnbbooks.github.io/lfe-tutorial/introduction/install.html |
| L32 | Historical Docker article | https://lfe.io/blog/tutorials/2019/05/13/1549-running-lfe-in-docker-updated/ |
| L33 | Manual REPL overview | https://cnbbooks.github.io/lfe-manual/part1/repl/ |
| L34 | Manual Creating LFE Projects; heading-only retrieval | https://cnbbooks.github.io/lfe-manual/part3/projects/ |
| L35 | Manual Part II destination title and subject | https://cnbbooks.github.io/lfe-manual/part2/ |
| L36 | Blog tag index | https://lfe.io/blog/tags/ |
| L37 | Blog author index | https://lfe.io/blog/authors/ |
| L38 | Sampled upstream OTP link and destination | https://erlang.org/doc/man/erlang.html → https://www.erlang.org/doc/apps/erts/erlang.html |
| L39 | Legacy documentation source repository and old entrance links | https://github.com/lfe/docs |
| L40 | Tutorial published entrance | https://cnbbooks.github.io/lfe-tutorial/ |
| L41 | Casting SPELs published entrance | https://cnbbooks.github.io/lfe-casting-spels/ |
| L42 | SICP published entrance | https://cnbbooks.github.io/lfe-sicp/ |
| L43 | Manual Part V overview | https://cnbbooks.github.io/lfe-manual/part5/ |

**Companion evidence:** `erlang-org-ia-investigation.md`, inspected through its saved report for this comparison. Its source register remains the authority for the Erlang observations summarized in §9.
