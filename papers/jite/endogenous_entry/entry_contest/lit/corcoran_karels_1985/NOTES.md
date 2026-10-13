# Notes — Corcoran and Karels (1985), pass 1 (read in full from page images)

Run `python3 lit/corcoran_karels_1985/verify_corcoran_karels.py` from the
repository root. The OCR text it checks against is made by
`ocr_corcoran_karels.py` and pinned by sha256.

## Verdict for C5, C6 and C13

This was the last paper the novelty claims waited on. None of the three is
overturned.

- **C5** (the sign rule of C1 is new): settled.
  - The players are identical, and there is no wealth and no spread of entry
    costs.
  - The comparative statics are in the exponent r, the minimum bet and the
    commitment assumed (hit and run or hardball, Cournot-Nash or collusion).
- **C6** (count invariance with heterogeneous entry costs): settled.
  - The players are identical, which adds the paper to the list.
  - Under hit-and-run entry several counts are stable equilibria (CK-21,
    CK-24). The multiplicity comes from incumbents choosing among pre-emptive
    bets, not from entrants who differ.
  - With a fixed fee (the minimum bet) and identical players the count is
    ⌊X/Z⌋ (`minbet_dissipation`).
- **C13** (the share of the prize taken by those who enter): settled. Those who
  do not bet cannot win, so all of the prize goes to entrants and the share is
  not defined there.

## The closest point to our model

The minimum-bet case is a fixed entry fee (CK-13). With every entrant at the
minimum, entry continues while nZ ≤ X, which is M10's bound (m+1)κ ≤ V binding.
It binds because those who do not pay win nothing. In ours outsiders compete on
the base score, so the gain from paying can fall short of V/(m+1).

## Findings from the formalisation

1. Note 8 obtains the lowest pre-emptive bet from the entrant's first-order
   condition and zero profit. The formalisation shows the entrant's best reply
   then earns at most zero, not merely a stationary zero, by weighted AM-GM
   (`key_amgm`, `ent_le`, `ent_zero`).
2. The hardball count has a closed form the paper does not state: m ≤
   r^{r/(r−1)}/(r − 1) (`range_nonempty_iff`). It reproduces every count of
   Table 6, and it refutes "for r-values larger than 4, only one player can be
   sustained" at r = 4.2 (`two_sustained_r42`).
3. The one-player minimum X/2 at r = 2 is a global minimum over r > 1
   (`blow_one_ge_half`).
4. Splitting bets does not "always pay" for r ≤ 1. It fails at r = 1, by the
   paper's own Table 1 (CK-D2).
5. Misprints:
   - **Table cells:** three in Table 1, one in Table 2, one in Table 3 and one
     in Table 4.
   - **Text and notes:** two aggregates in the text (CK-D3, CK-D4), n = 2.6 for
     8/3 (CK-D1), and note 9's "equation (30)" (CK-D6).
