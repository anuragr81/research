# Entities and the relationships to be understood, 23 October 2026

A planning note, not part of the paper. It follows the ledger spec of Paper A
(`papers/subjective_racism/paper_a/notes/entities.md` and `ledger_diagram.md`)
and lists every entity that the theory paper's sources name or need, and every
relationship between entities that a row would have to settle. The sources are
the parent `MANUSCRIPT.tex` (the plain-English summary, rows M1 to M16, the
Terminology and Primitives appendices), the Entities and Actions tables of the
parent `MEASUREMENT_MAP.tex` (N1 to N7, A1 to A11, the empirical ledger of 19
October 2026), and the records under the parent `lit/`. Quotation IDs are rows
of those records.

E numbers are entities and U numbers relationships still to be understood.
Each relationship carries one of Paper A's three status words. **Settled**
means a model row proves it. **Stated** means the model takes it as a rule or
an input and no row derives it. **Open** means no row, no assumption and no
object of the measurement map carries it.

Nothing here is decided. Section 3 proposes the reduction under the author's
rule, and section 4 lists what the author decides.

## 1. Entities

Sixteen items. One is a player (E1) and one an object (E2). The shareholders
and the creditors face no choice in the model (E3, E4), and three items are
cases of them (E5, E6, E16). Two are rules (E7, E12), two are enforcers that
the theory model does not have as agents (E8, E9), two are instruments (E10,
E11), one is a construct (E13), one an action (E14) and one a value of the
state (E15).

| ID | Entity | Kind | Choice faced | Where it stands in the theory model | Parent ledger | In the literature |
|---|---|---|---|---|---|---|
| E1 | The bank | Player | Its risky exposure, the dividend barrier $y^*$, the recapitalisation trigger $x_L$ and the injection target $y_{\text{post}}$ | The thresholds are settled in their comparative statics (M4 to M7, M10, M16). The exposure is not derived. M2, M3 and M15 hold for exposure at or under the cap, and that the optimum binds the cap is assumed (K1's scope note) | N1 | The bank whose size is risk-weighted assets (PK-Q4). The kinked cap is the channel through which regulation acts on its payout and recapitalisation (BCVW-Q7) |
| E2 | Equity | Object | None. Dividends draw it down and issues add to it | Equity over liabilities is $x-1$. M8 maps it to the capital buffer $z$ | N2, drawn as a box | Shareholders' equity, the numerator of the solvency ratio (BCVW-Q9) |
| E3 | The shareholders | Counterparty | None in the model. They receive dividends and supply any issue at the fixed and proportional issuance costs | The objective is their value net of the shortfall penalty, and the bank chooses for them. The supply of capital at $K$ and $\kappa$ is Stated (Primitives, "The fixed issuance cost") | N3, investors | In Peura and Keppo the owners choose the issue (PK-Q9) |
| E4 | The creditors, the holders of the liabilities | State | None. The liabilities follow their own shock, with volatility $\sigma_L$, correlation $c$ with the risky return, and drift $r_L$ | Inputs of M1 to M3, M9 and M15. One block of liabilities | N4 and N5, split by deposit insurance. The model does not split them (N4, G9) | Outflows as a fixed fraction of liabilities (BCVW-Q10) |
| E5 | The insured creditors | A case of E4 | None | Not separated in the theory model | N4 | |
| E6 | The uninsured creditors | A case of E4 | Lend and withdraw, pricing default risk | Not separated in the theory model. In the parent ledger they enforce the liquidity branch for banks outside the liquidity rule (N5) | N5 | Creditors price default risk (ledger R14, BDF-Q22) |
| E7 | The regulator, as setter of required capital $q$ and the regulatory cap $u(x)$ | Rule | None. $q$ and $a_1,a_2,a_3$ are inputs | Primitives, "The regulatory cap". M2, M3, M15 | Not a node. A rule enters through the agent who enforces it (author, 19 October 2026) | The cap of Bayraktar et al., eq. (4) (BCVW-Q1, BCVW-Q8) |
| E8 | The supervisor | Enforcer | In the world, assigns the capital category, restricts and closes | Not in the theory model as an agent. The cap's solvency branch and the requirement are its rules (N6). The parent reads the shortfall penalty as standing in for its restrictions (ledger R7) | N6 | A costly regulatory intervention is what the buffer guards against (MW-Q2). The corrective action at the minimum is liquidation in Peura and Keppo (PK-Q6) |
| E9 | The insurer | Enforcer | In the world, prices insurance on capital | Not in the theory model (N7) | N7 | |
| E10 | The risky asset | Instrument | None. Drift $\mu_s$, volatility $\sigma$ | Inputs of M1 to M3, M15 | Not a node. Its record is risk-weighted assets on the bank (N1) | |
| E11 | The riskless asset | Instrument | None. Rate $r$ | Input of M9, through the drift $r-r_L$ at the distress boundary | Not a node | Counted fully as high-quality liquid assets (BCVW-Q11) |
| E12 | Required capital $Q$ | Rule, an attribute of E1 | None | The denominator of the capital buffer (M8). Grows with equity under balanced growth (M12) | On N1, through the requirement | |
| E13 | The shortfall penalty, with reference $R$ and asymmetry parameter $\lambda_S$ | Construct | None | In the objective. Vanishes at $\lambda_S=1$, concave running payoff for $\lambda_S\ge1$ (M11). Moves the thresholds (M4 to M6) | Not a node. Annotated on the supervisor's restriction (R7), the insurer's price (R8) and the uninsured creditors' pricing (R9), each copied below the reference only | A graduated penalty below the requirement (PK-Q18) |
| E14 | The issue, the recapitalisation | Action | The bank chooses when and how much | M5, M7, M10, M14, M16 | A2 and A3 | Lumpy issues from a fixed cost (BCW-Q3, MW-Q3) |
| E15 | The distress boundary $x=1$ | A state value | None | M8 sends it to minus infinity in $z$. M9 gives the drift along the cap there. Whether it is reached (H2b in the parent `TODO.md`) is not a row | Closure by the supervisor at 2% tangible equity (A10, R10) | Repelling when $r>r_L$ (BCVW-Q3) |
| E16 | The parent holding company | A case of E3 | Injects capital | Not separated from outside buyers (ledger R3) | A3 on N3 | |

## 2. Relationships to be understood

| ID | Entities | What has to be understood | Status | Rows and sources |
|---|---|---|---|---|
| U1 | E1 pays E3, drawing on E2 | Where dividends are paid, and how the barrier moves with the asymmetry parameter and the fixed issuance cost | Settled in part. M4 gives the comparative statics. That the optimal policy is a barrier rests on the verification hypothesis, which Lean does not carry (parent Appendix B) | M4, PK-Q7, BCW-Q1 |
| U2 | E3 recapitalises E2 at E1's choice | Where an issue starts and where it lands, at what cost | Settled in part. M5, M6, M7, M10, M14, M16 settle the trigger, the target and their statics. The supply of capital at $K$ and $\kappa$ is Stated | M5 to M7, M10, M14, M16, PK-Q9, BCW-Q7 |
| U3 | E1 takes exposure to E10 under E7 | How much risky exposure the bank takes | Stated. The rows bound every compliant exposure (M15) and give the limits when the cap binds (M2, M3). That the optimum binds the cap is assumed, and supported only by the numerical solve (K1) | M2, M3, M15, BCVW-Q7 |
| U4 | E4 moves E1 | How the liability shock enters buffer variance | Stated as an input. Its effect is settled. $\sigma_L$ bounds buffer variance (M15) and enters both limits (M3) | M1 to M3, M15 |
| U5 | E7 on E1, enforced by E8 or E6 | Who enforces each branch of the cap. The solvency branch and the requirement by the supervisor, the liquidity branch by the supervisor only for the largest banks and otherwise by the uninsured creditors | Open in the theory model. The parent map says it (N5, N6), and no row uses it | N5, N6 |
| U6 | E13, on behalf of E8, E9 or E6 | Whose cost the shortfall penalty is, and so what $\lambda_S$ stands for | Open. The model charges the penalty in the objective. The parent's reading puts it on the supervisor's restrictions (R7) and, below the reference only, on the insurer's and creditors' pricing (R8, R9). No row derives it | M11, ledger R7 to R9, PK-Q18 |
| U7 | E1 at E15, and E8 | What happens at zero equity | Settled in part. M9 gives the drift along the cap. Reachability is not a row (H2b). No agent closes the bank in the model, while the supervisor closes at 2% tangible equity (R10) | M8, M9, BCVW-Q2, BCVW-Q3, PK-Q6 |
| U8 | E2 and E12 | How equity and the requirement grow together, and what the persistence of the buffer then is | Settled under the maintained model (M12). The persistence coefficients are maintained, not derived (Primitives) | M12, M13 |
| U9 | E1 and E3 | Whether the bank and its shareholders are one decision maker | Stated. The bank maximises the shareholders' value net of the penalty, so the two are one in the objective | PK-Q9 |

## 3. Proposed reduction under the author's rule

Superseded, 23 October 2026, by `notes/ledger.md`, which follows the
bundle's agreed ledger (`00_reader/ledger_model.py`): equity is an object
node and the creditors are two entities. The row numbers below are the
parent manuscript's.

The author's rule of 17 October 2026 (Paper A, `notes/ledger_diagram.md`,
section 1): an entity is something that acts or is acted on and is not a case
of another entity. Roles, attributes and actions are not entities. The parent
ledger adds the decision of 19 October 2026: a rule is not a node, and enters
through the agent who enforces it.

Applied to the sixteen items, four entities remain.

| Entity | Attributes | Roles |
|---|---|---|
| Bank (E1) | the state $x$, equity $x-1$ (E2), required capital $q$ (E12), risky exposure $u$ and the two assets (E10, E11) | none |
| Shareholders (E3) | the cost of an issue, $K$ and $\kappa$ | the parent holding company (E16), outside buyers |
| Creditors (E4) | the liability shock $\sigma_L$, $c$, $r_L$ | insured (E5), uninsured (E6) |
| Supervisor (E8) | enforces the requirement and the cap (E7) | none |

What leaves the list, and where it goes.

| Item | What it is under the rule |
|---|---|
| Equity (E2) | An attribute of the bank, which dividends and issues move |
| Insured and uninsured creditors (E5, E6) | Roles of the creditors, since no theory row gives them different actions |
| Regulator (E7) | Its rules, entering through the supervisor |
| Insurer (E9) | Outside the theory model, since it neither acts nor is acted on there |
| Risky and riskless asset (E10, E11) | Attributes of the bank's balance sheet, with their returns as inputs |
| Required capital (E12) | An attribute of the bank, set by the supervisor's rule |
| Shortfall penalty (E13) | A construct of the objective, annotated on the action it stands for once U6 is decided |
| Issue (E14) | An action, from the shareholders to the bank |
| Distress boundary (E15) | A value of the bank's state |
| Parent holding company (E16) | A role of the shareholders |

## 4. Decisions for the author

1. Equity. An attribute of the bank, as the rule says, or a node as in the
   parent ledger (N2), where it is what dividends and issues act on.
   Recommended: an attribute, so that dividends and issues are actions between
   the bank and the shareholders.
2. Creditors. One entity with insured and uninsured as roles, or two as in the
   parent ledger (N4, N5). Recommended: one, since the theory model has one
   block of liabilities. The split returns if a row gives the uninsured
   creditors the liquidity branch (U5).
3. Insurer. Out of the theory ledger, as recommended, or in with open actions
   only. If out, Appendix D says it lies outside the model.
4. Supervisor. In the theory ledger as the enforcer of the cap and the
   requirement (recommended, following the decision of 19 October 2026), or
   out, with the cap as a bare constraint on the bank.
5. Bank and shareholders. Two entities (recommended), with dividends and
   issues as actions between them, or one decision maker as U9 has it in the
   objective.
6. The shortfall penalty (U6). Which action it stands for decides what
   $\lambda_S$ means, and so what K2's identification identifies. This is the
   question the paper's answer most depends on.
