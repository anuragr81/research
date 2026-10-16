# Corneo and Jeanne (1994), notes

## What was verified

- Nineteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the text layer (LC-2). CJ-Q1, CJ-Q4 and CJ-Q8
  carry elisions where the text layer drops an accent or a ligature (CJ-D3).
- The model is implemented in Lean in the paper's notation (LC-4). The
  coverage table in `CLAIMS.md` lists every numbered result with its status.
- Our reading is consistent with the discussion paper on these points, with
  the reading CJ-D2 of the proof of Proposition 1.

## What could not be verified

- The JPubE (1997) article is not read (CJ-D1). Rows that cite the 1997 article
  for anything in this record carry an unverified mark until it is read.
- Figures 1 and 2 are blank in the copy read (CJ-D6).

## The bearing on Paper A1

Judged against the corrected account of A1 (16 October 2026).

### The puzzle

Not addressed. The paper has no contact, no friendship and no groups other
than buyers and non-buyers.

### Budget compatibility

The paper gives a budget mechanism by which a priced, shared consumption
gathers people by income. A consumer buys when the signaling value exceeds
the utility cost of the price (`eq4_buy`), and with concave `u` that cost falls
in income, so the buyers are the richest `n` (`buyers_upper`, CJ-Q4, CJ-Q5).
When the budget binds, the threshold is the price itself (eq. 6, `eq17`). The
sorting comes from cost and concavity, not from a taste for similar others,
and it reverses under convex `u` (`control_convex_buyers`). This is close to
A1's budget compatibility, that a shared activity needs budgets that can
afford it. The paper says nothing about whom the buyers then meet, so it
gives the sorting into an activity and not the homophily of a relationship.

### Avoidance that depends on the observer's class

Every spectator reads the signal the same way (CJ-Q3), so nothing depends on
the observer's rank, and nobody chooses contact. The consumer's motives are
about being pooled. Under conformism the motive is the fear of being
identified with the poor (CJ-Q8); under snobbism it is the hope of being
identified with the rich (CJ-Q19). Which motive holds depends on the shape
of the rank utility (`gamma_one_concave`, `gamma_one_convex`), and the link
is not general (`control_fn8_conformist_not_concave`). Neither A1 direction is
supported or contradicted.

### The penalty vanishing when class is shown

Not in the paper. Income is never shown. The nearest statement is that under
passive conjectures the good carries no meaning, its signaling value is nil,
and nobody buys (`prop3_passive`, `prop4_passive`).

### The shadow market

The paper has the price-as-information half of the shadow market. The price
signals the quality of the consumer, not of the product (CJ-Q14). The authors
place their analysis next to Becker's restaurant pricing (CJ-Q15). Clubs
charge above the market price to make contributors visible (CJ-Q10,
CJ-Q11). A cheap good has no signaling value under snobbism (CJ-Q16,
`prop2`), so a seller with market power can create a status signal by holding
the price up (CJ-Q17). In the labour-market reading, a signal of income stands
in for an unobserved productive ability (CJ-Q18). The paper has no law, no
group and no banned criterion, so it says nothing on discrimination moving to
channels the law does not reach.

### Premises of A1 the source does not share

- Status is a rank utility of inferred rank, not a payoff from engaging with
  someone.
- Observers are homogeneous (CJ-Q3).
- There is one indivisible good and no visible group marker. The only groups
  are those the purchase creates.
- The equilibrium values each group at its own mean rank utility (`eq8`), the
  same consistency condition A1 imposes on group standing, but for a chosen
  signal rather than a group.

## Controls

- LC-2. A fabricated quotation, "Thus the message contained in conspicuous
  consumption is interpreted differently by rich and poor spectators.", the
  opposite of CJ-Q3, is not found. A true quotation is not found on a
  neighbouring page.
- Lean. `control_conformist_price_rises` shows the falling price of
  Propositions 1 and 2 needs snobbism, `control_rising_rank_utility` that a
  positive signaling value needs a rank utility that does not rise in income,
  `control_fn8_conformist_not_concave` that conformism and concavity part
  outside `γ = 1`, and `control_convex_buyers` and `control_prop3_concavity`
  that the upper set of buyers and Proposition 3 need concave `u`.
