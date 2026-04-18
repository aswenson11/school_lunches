# Andrew's Preferences – School Lunches Research Project

## About This Project
Research on Food Service Management Company (FSMC) procurement in NJ (and MI) school districts.
Goal: collect historical RFP documents, bid proposals, and scoring sheets to study FSMC market structure and competition.

## Research Question
The immediate hook is **NJ's ordinal cost-scoring rule** and how it distorts both bidder behavior and districts' choice of scoring weights. This sits inside a broader project on FSMC market competition and its downstream effects on cost and student health.

**The ordinal-scoring mechanic.** A district's FSMC RFP scores each bidder on two things: (1) category weights (e.g., x% on cost, 1−x% on quality), and (2) a within-category rule that converts raw inputs into points. NJ fixes the within-cost rule as purely **ordinal**: the lowest-cost bidder gets 5 points, 2nd gets 4, 3rd gets 3, etc., regardless of the dollar gap. Michigan uses a **cardinal** formula: points = `(1 − (bid − lowest)/lowest) × max_points`, so the scoring adapts to whatever cost dispersion shows up.

**Why the ordinal rule is distortionary.**
- It violates IIA in the cost dimension: adding a 3rd bidder can flip the winner between two existing bidders even though their cost/quality attributes haven't changed.
- Two auctions with competitors at ($100, q=4) vs. ($100,000, q=5) and ($100, q=4) vs. ($100.01, q=5) get scored identically — the district has no way to express that it would prefer the first bidder in scenario 1 and the second in scenario 2.
- It forces districts to **guess ex ante at cost dispersion** when choosing weights: if they expect costs to cluster, low cost-weight is safer (don't reward bidders 1¢ apart with a full-point gap); if they expect wide dispersion, high cost-weight is safer (don't barely-penalize an extreme high-cost bidder). A cardinal rule folds that adaptation into the scoring itself; ordinal makes it a one-shot bet.
- Possible implication: the rule mostly adds variance rather than reflecting district strategy. Firms may also exploit this by letting randomness in quality scoring do the sorting rather than competing on price.

**Broader project aims.**
- **Aim 1 — Descriptive**: correlation of FSMC market HHI / bidder count with per-meal prices and child obesity (EHR-linked Medicaid), moderated by the district's cost-vs-quality weight.
- **Aim 2 — Quasi-experimental DiD**: compare districts whose auctions saw bidder-count shifts against propensity-matched controls (self-managed districts as a clean control group); staggered treatment timing from contracts' statutory max length (1-year initial + up to 4 renewals, so districts are only "treated" at renewal).
- **Aim 3 — IV**: shocks from adjacent institutional food-service markets (sports franchise arrivals/departures, nearby college openings/closings, warehouse openings) as instruments for bidder entry. This aim's first stage is feasible before EHR data arrives, so it's a useful early deliverable.

**Control/comparison states.** Michigan is the main cardinal-scoring comparison. NY, IL, OH, PA, AZ are other outsourcing-heavy states; IL is especially interesting because it only introduced scoring auctions in SY 2022–23 (pre/post variation). Policy shocks to watch: 2014 and 2018 USDA nutrition-standard changes.

**Data strategy.** Doc collection via OPRA/FOIA is the backbone. Plan is to extract structured fields (auction year, bidders, bids, scores, formula weights, district geo) using multiple LLMs (Claude, GPT, Gemini) in parallel and reconcile discrepancies by hand.

**Reference doc.** Andrew's full brainstorm lives at `/Users/andrewswenson/Documents/Research/School Lunch Docs Brainstorm.pdf` (modeling ideas, policy variation, lit review, to-do list, and meeting notes with Natalia / Adrienne).

## Simulation (Code/ folder)

BNE scoring-auction simulator comparing NJ ordinal, MI cardinal, and a reference-price cardinal rule. Two firms, private costs c_i ~ Uniform[cL_i, cH_i], known quality q_i, one district, one auction. Files: `Code/simulation_functions.R` (BNE solver + welfare integration) and `Code/app.R` (Shiny UI with live bid-function tab, button-triggered welfare-curve tab, summary table).

### Scoring rules in the model
- **Ordinal (NJ):** `cs_i = S` if cost rank 1, `S − 1` if rank 2.
- **Cardinal (MI):** `cs_i = max(0, (1 − (b_i − b_min)/b_min) · S)`. Anchored at the realized minimum bid.
- **Cardinal ref-price:** `cs_i = max(0, (1 − (b_i − p_ref)/p_ref) · S)`. Anchored at a district-announced reference price. Slider defaults to midpoint of the two priors' midpoints and auto-syncs when cost ranges change.

### Key theoretical takeaways from simulation work
- **Optimal w ≠ λ under any rule.** Even under cardinal, w\* ≠ λ in general: (1) the MI cost score is normalized by b_min (endogenous), so the scoring rule is nonlinear in bids; (2) w also disciplines markups, not just selection — higher w intensifies price competition, so rent-extraction pushes optimal w above what selection alone would dictate. In the symmetric-firms limit, w\* → 1 regardless of λ (selection is trivial, maximal price competition is optimal).
- **Shut-out = degenerate equilibrium.** When a firm's win probability is zero at every bid (ordinal A1=A2=TRUE "firm1_monopoly" regime, or cardinal with a dominating quality gap), the loser is indifferent across all b ≥ c. The simulator defaults to bid = cost (appears as a flat 45° line in the plot), but any bid above cost is equally a best response. This is economically meaningful output, not a bug: it's telling the user that firm isn't a real competitor under those parameters.
- **Ref-price cardinal should dominate MI cardinal** in welfare because the score is linear in the bid and exogenous to competitors' bids, giving the district a second instrument (p_ref alongside w) that can tune the rule to match linear utility. Clean argument for the paper: NJ could jump past MI's cardinal and capture even more welfare with a ref-price rule.
- **Quality is deterministic and common knowledge** in the current model. Empirical evidence from the presentation deck ("quality score dispersion — identical firms score differently across panels") suggests a real-world stochastic component we should eventually model as a second source of uncertainty; should amplify ordinal's distortion further.
- **Boundary tie-break:** A1/A2 regime checks in `solve_bne` use `>=` (not `>`) to match the `ts1 >= ts2` tie rule in `auction_outcome` — prevents exact-threshold misclassification into a monopoly regime.
- **Bids above cost is correct behavior** (not a bug). Profit = bid − cost in a procurement auction. Equilibrium bid functions β\*(c) lie above the 45° line because firms shade bids above private cost to earn positive expected profit.

### Known issues / TODOs
1. **Ref-price cap logic is dumb — fix later.** Current `cs = max(0, (1 − (b − p_ref)/p_ref) · S)` only floors at 0; cost scores exceed S whenever b < p_ref, which creates an artificial unbounded incentive to bid very low. Need to revisit: probably want to anchor the S-cap at the min of realized bids (hybrid with MI's b_min anchoring) so p_ref sets the scoring slope but the lowest-bid firm always scores exactly S. Need to think through whether that breaks the "exogenous scoring" property we liked about this rule, or whether a simple `min(S, ...)` clip at the top is enough.
2. Extend to private or noisy quality (two-dim private-type BNE, or common q_i plus idiosyncratic panel noise ε_i scored at the auction).
3. Extend to N > 2 firms.
4. Reserve price currently set heuristically to 2·max(cH1, cH2); may want to expose it as a district-chosen slider.

## File Organization
- `NJ Files/` — State-level NJ documents (forms, contract templates, statewide lists)
- `NJ Files/OPRA Responses/[District Name]/` — Downloaded response files from each district, one subfolder per district
- `Michigan Files/` — Michigan equivalent documents
- `Doc Org/Doc Intake.xlsx` — Master tracking spreadsheet (two tabs: Files Index + OPRA Tracker NJ)

## Doc Intake.xlsx Structure
- **Files Index tab**: One row per document file. Columns: File Path, Link, Form/Title, File Type, Description, Source, Review Status, Notes, State, AI Summary
- **OPRA Tracker NJ tab**: One row per NJ district with active FSMC contract. Columns include OPRAMachine Link, OPRA Status, Date Sent, Date Received, etc.
- When adding new files: always add to Files Index. If the district is in OPRA Tracker NJ, also update that row's OPRA Status and notes.
- AI Summary column should be concise (1-2 sentences): state the state + district, what the document is, and any key gaps or flags.
- File Path column uses blue text (hardcoded inputs). Local files use full Mac paths. Online-only files use the direct URL.

## OPRA Request Details
- Sent via OPRAMachine Pro (batch requests)
- The request asks for: (1) all historical RFPs, (2) all bids/proposals from each RFP, (3) scoring sheets and evaluation forms, (4) executed contracts + amendments
- 7 business day response window under NJ OPRA law
- When evaluating responses, flag: "most recent year only", "bid docs missing", "scoring sheets missing", "denied – overly broad", "N/A – no FSMC", "N/A – SFA-to-SFA"

## Downloading Files from OPRAMachine
- Use fetch() + blob URL trick in Chrome to download PDFs (bypasses Chrome's PDF viewer):
  ```js
  async function dlFile(url, filename) {
    const resp = await fetch(url, {credentials: 'include'});
    const blob = await resp.blob();
    const blobUrl = URL.createObjectURL(blob);
    const a = document.createElement('a'); a.href = blobUrl; a.download = filename;
    document.body.appendChild(a); a.click(); document.body.removeChild(a);
  }
  ```
- Chrome may block multiple automatic downloads — user must click "Allow" in the notification bar at the bottom of Chrome window
- DOCX/XLSX files download fine via JS `a.click()`; PDFs need the blob URL method above
- After downloading, move files from ~/Downloads to correct district subfolder under NJ Files/OPRA Responses/

## Response Evaluation Flags (use in OPRA Tracker and Notes)
- **Good response**: All 4 items provided for multiple historical years
- **Partial – [Year] Only**: Only provided most recent procurement cycle
- **Denied – Overly Broad**: District refused, claimed request scope too broad; follow up with specific year range
- **Pushback – Needs Scope**: District asked for time period clarification; respond with "please provide records going back 7 years (since [year])"
- **N/A – No FSMC**: District self-manages food service, no FSMC records exist
- **N/A – SFA-to-SFA**: District uses another school district as food service provider, no competitive bid records
- **N/A – No Lunch Program**: Small district with no school lunch program
- **Acknowledged – Pending**: District confirmed receipt and gave a response date; follow up after that date

## Follow-up Language

### For "Overly Broad" denials (Plumsted, Freehold):
> Dear [Custodian], thank you for your response. To narrow the scope of my request, I am now requesting the same records (RFPs, bids, scoring sheets, and executed contracts) for FSMC procurements conducted in the last seven years (2019–2026). Please let me know if you need any further clarification.

### For acknowledgments awaiting documents (Trenton, past due date):
> Dear [Custodian], I am following up on my OPRA request dated April 13, 2026. I received your acknowledgment indicating a response by [date]. Could you please provide an update on the status and expected delivery of the records?

### For partial responses (Berkeley, Ocean Gate — most recent only):
> Dear [Custodian], thank you for the documents provided. My request asked for records going back as far as available. Could you please also provide the same records (RFPs, bids, scoring sheets, contracts) for any prior FSMC procurement cycles the district conducted before [year]?

## Formatting Preferences
- Keep AI Summaries brief (1-2 sentences max)
- Don't over-format responses with excessive bullet points — prefer prose
- When updating the Excel, preserve the existing formatting and color scheme exactly
- Don't ask unnecessary clarifying questions — use context from this file and the folder structure to infer intent
- State abbreviations: NJ = New Jersey, MI = Michigan

## Food Data Note
- Andrew is NOT currently requesting food/nutritional/menu data — focus only on FSMC procurement docs
- Food data would require a separate OPRA request and should be scoped carefully

## Key Contacts Encountered
- Kevin O'Shea: responded for both Ocean Gate and Central Regional School Districts
- Tyler Verga (CPA, BA/Board Secretary): Berkeley Township and Seaside Heights School Districts
- Meghan Lee: Long Beach Island Consolidated School District
- Jessica Penkola (Confidential Secretary): Trenton Board of Education
- Robert DeVita: Freehold Township Board of Education
- Laurie Considine: Bay Head School District
- Sean Gately (Business Administrator): Plumsted Township Board of Education
