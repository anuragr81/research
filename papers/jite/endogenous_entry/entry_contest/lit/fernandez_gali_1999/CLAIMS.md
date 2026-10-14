# Claims about Fernández and Galí (NBER WP 5930, 1997; RES 1999)

**Source.** Raquel Fernández and Jordi Galí, "To Each According To...? Markets, Tournaments,
and the Matching Problem With Borrowing Constraints", NBER Working Paper 5930, February 1997
(46 pages, page images only), read in full [F, WP version]. Published in *Review of Economic
Studies* 66(4): 799-824 (1999), which is not used. Cited in `refs.bib` as `FernandezGali1999`.
Pages are PDF pages; the printed page is the PDF page minus one.

The text was made by OCR (`ocr_fernandez_gali.py`, RapidOCR on pages rendered at 300 dpi) and
is pinned by sha256. Every row not marked P is checked verbatim, on the page named, against
that text, ignoring case, punctuation and spaces. Every such quotation was also read on the
page. Rows marked P were read from the page image, which `verify_fernandez_gali.py` pins by
sha256. The results we use are proved in `lean/mathlib/FernandezGali.lean` (see `LEAN`).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| FG-1 | 2 | "We compare the performance of markets and tournaments as allocative mechanisms in an economy with borrowing constraints." | The question |
| FG-2 | 3 | "In this paper we investigate the properties of one such alternative mechanism" | A tournament as an allocation mechanism |
| FG-3 | 6 | "The economy consists of a continuum of agents each of whom is characterized by an endowment of" | A continuum of agents |
| FG-4 | 6 | "we assume that these agents are distributed uniformly on the unit square" | Ability and wealth uniform and independent; no spread is studied |
| FG-5 | 7 | "The utility function is assumed to be increasing in consumption." | Utility of consumption |
| FG-6 | 7 | "The latter equals income X, plus initial wealth, minus any expenditures realized to obtain this allocation, plus a possible lump-sum transfer." | Consumption is linear in wealth and spending (`kappa_linear`) |
| FG-7 | 4 | "A tournament, in contrast to prices, allocates individuals according to rank order in some contest." | Rank-order allocation |
| FG-8 | 11 | "A tournament allocates each individual to the school whose quality rank equals the rank of her performance in the contest." | The tournament rule |
| FG-9 | 4 | "We assume that these expenditures are non-productive" | The signal expenditure is wasted |
| FG-10 | 12 | "expenditures are necessary in order to emit a positive signal" | `V(a, 0) = 0`: who spends nothing has signal zero |
| FG-11 | 12 | "The last assumption guarantees that agents with zero wealth are allocated to the lowest quality school" | Who cannot spend gets the lowest school |
| FG-12 | 12 | "Furthermore, we assume eva ≤ 0 (the marginal cost of signaling is nonincreasing" | The cost condition of `tournament_sorting` (the OCR reads e_va as "eva") |
| FG-13 | 13 | "perfect capital markets ensure that with tournaments agents' initial wealth distribution has no impact on the resulting equilibrium allocation" | Without a constraint, wealth does not matter |
| FG-14 | 15 | "the price of a given quality school in the market equilibrium is never below the expenditure needed by an agent with ability a to obtain the same assignment under tournaments" | Eq. (3.17) (`price_ge_expenditure`) |
| FG-15 | 20 | "in the presence of credit constraints the market equilibrium allocation is inefficient" | Eq. (4.5) (`clearing_lt`) |
| FG-16 | 27 | "with borrowing constraints the allocation achieved by tournaments always delivers higher aggregate output than does the market system" | Theorem 4.18 |
| FG-17 | 28 | "schools are more diverse in terms of wealth levels under tournaments than under markets" | Lemma 4.15's reading |
| FG-18 | 29 | "In a tournament, on the other hand, identical expenditures by non-identical agents do not lead to identical outcomes." | Why ability relaxes the constraint; quoted in row L30 at its printed page, 28 |
| FG-19 | 29 | "The preceding lemma states that by increasing the sensitivity of the signaling technology to variations in ability (relative to variations in expenditures)" | Lemma 4.19's reading |
| FG-20 | 30 | "We have not attempted to find the optimal mechanism in this environment." | The rule is not chosen by a designer |
| FG-21 | 33 | "rules out s < s' since the latter would make the expression inconsistent with the assumption Xas > 0" | Lemma 4.3(i) (`market_sorting`) |
| FG-P1 | 29 | "Lemma 4.19. lim θ→+∞ ∫₀¹ e^θ(a) da = 0" | The family `V^θ` with `V^θ_a/V^θ_e = θ π(a, e)`, `π = V_a/V_e`, and the limit, read from the page image (the OCR loses the formulas) |

## Readings recorded

| ID | Point | Our reading |
|---|---|---|
| FG-D1 | FG-4, FG-6, FG-13 | Wealth works through a borrowing constraint on spending, with linear utility. Without the constraint, wealth has no effect. In our model wealth works through the utility cost of a fixed fee, `κ(w) = u(w) − u(w − c)`, which is constant in wealth under linear utility (`kappa_linear`) and falls with wealth under concave utility (`kappa_log_not_const`). |
| FG-D2 | FG-3, FG-8, FG-10 | The spending is continuous and everyone spends. An agent who spends nothing emits signal zero and is placed in the lowest school (FG-10, FG-11), so ability alone earns nothing in the contest. In ours the choice is binary, and those who do not pay still compete on the base component. |
| FG-D3 | FG-4 | The wealth distribution is fixed (uniform, independent of ability). No spread of wealth is signed, there is no count of entrants, and no share of positions held by those who spend. |
| FG-D4 | FG-P1, FG-19 | Raising `θ`, the weight of ability relative to spending in the signal, sends aggregate spending to zero in the limit. This is a limit, not a sign at every `θ`, for a continuous expenditure and a continuum. It points the same way as M28 (entry does not rise as the rule weighs the base component more). |
| FG-D5 | FG-20 | The mechanism is compared, not designed: the paper does not choose `θ` or the mechanism, as our model does not choose `μ`. |
| FG-D6 | p.3, FG-2 | The OCR of p.3 lost one line ("efficiently is replaced by an af-" before "fordability requirement"), read on the page. No quotation uses it. |
