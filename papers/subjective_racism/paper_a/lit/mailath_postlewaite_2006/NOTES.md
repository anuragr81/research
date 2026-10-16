# Mailath and Postlewaite (2006), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite in the text layer of
  WP 06-003, by exact normalised containment (LC-2).
- The Section 3 results Paper A1 would contrast with are Lean theorems in the
  paper's notation (LC-4). These are the assortative values, Proposition 1,
  eq. (3.4), Proposition 2, eq. (3.8), the appendix values and Proposition 3.
- Our reading is consistent with the source on these points.

## What could not be verified

- Propositions 4, 6, 7 and 8 and Examples 1 and 2 rest on limit, continuity
  and fixed-point arguments, and are read, not formalised.
- The printed difference and threshold (5.2) of the rationed case are not
  consistent with the paper's own value equations (MP-D4). Our corrected
  reading is a Lean theorem. The published version is not read, so whether it
  corrects the slip is unknown.
- The published article in the International Economic Review is not read
  (MP-D1).

## The bearing on Paper A1

Nothing in the paper contradicts the A1 model. The paper is a model of a
different world. In it, income is seen at matching (MP-Q6, MP-Q7), and the
authors say there is "no asymmetric information, hence no role for
signalling" (MP-Q14). A visible attribute, with lighter skin among the
authors' own examples (MP-Q1), is valued in matching because children who
inherit it match better (MP-Q9), not because it tells a partner anything
about income. That is the rival channel to A1's. In A1 a marker matters
because class is unseen and the marker carries the group's class
distribution. In Mailath and Postlewaite a marker matters although income is
seen, because an equilibrium matching prices it.

1. **Group avoidance from class homophily under inference.** Not in the
   paper. Nothing is inferred (MP-Q14). Preferences over a partner's income
   are vertical, every agent prefers a richer partner (MP-Q5), so there is no
   squared gap between one's own class and a partner's. Which agents trade
   income for the attribute does depend on the chooser's own characteristic.
   Only a rich agent without the attribute (`Hn`) accepts a poorer partner who
   has it (`Ly`), MP-Q9 and `prop2`. That dependence runs through having
   income to trade, not through distance in class. The paper's variance is the
   variance of consumption across society, which the mixed matching lowers
   (MP-Q4, MP-Q11). It is not a penalty on a group's within spread in partner
   choice, and nothing in the paper resembles the `σ_g²` term.
2. **Budget compatibility as the micro-foundation of class homophily.** Not in
   the paper. Assortative matching on income arises from vertical preference
   under joint consumption and stability (MP-Q5, `prop1`), the standard
   alternative that A1's budget-compatibility account has to be set against.
   The paper is a citable source for that alternative in a matching model with
   a heritable marker.
3. **Venue exclusion and price screening.** Not in the paper. Section 6 lets
   parents buy the attribute for their children at a cost, which is
   acquisition of the attribute and not a price that screens on an unseen
   type. In the white-shoe example young lawyers self-select into
   apprenticeship by their own second-period output at a common wage, with no
   observer inferring anything.
4. **The class-channel test.** The paper is the clearest statement of the
   world in which A1's test gives a different answer. In Mailath and
   Postlewaite the premium on a visible attribute exists with income shown,
   because the attribute is valued only through matching (MP-Q2, MP-Q3). A1's
   test, that the group penalty in contact vanishes when class is shown,
   separates A1's channel from this one. A penalty that survives the display
   of class is what the social-asset channel predicts, and A1 should cite the
   paper as the alternative its test rules in or out.

The equilibrium consistency in the paper is of a different kind from A1's.
The distribution of attributes in each period is induced by the previous
period's matching (p. 7), and in Proposition 8 the share holding the
attribute must be consistent with parents' purchases at a fixed point (pp.
34–36). Neither is a condition that an imputed group mean equal the actual
mean of the group's visible members, and the paper has a law of motion across
generations, which A1 does not.

What A1 can cite. MP-Q1 and MP-Q2 for the idea that a visible attribute,
skin colour among the authors' examples, can be valued in matching for
reasons other than what it reveals. MP-Q14 for the authors' own separation of
that channel from the signalling channel of Fang (2001), MP-Q13. MP-Q5 for
vertical preference over partners' income under joint consumption.

## Controls

- LC-2. A fabricated quotation, "agents cannot observe a partner’s income and
  infer it from the attribute", the opposite of MP-Q7 and MP-Q14, is found on
  no page. A true quotation is not found on a neighbouring page.
- Lean. `control_no_altruism` shows that Proposition 2 needs care for
  descendants (`β > 0`), `control_risk_neutral` that (3.8) and Proposition 3
  need concavity (`u > 1/2`), and `control_productive_blocks` that the
  stability of assortative matching at `k = 0` fails for some productive
  attribute.
- LC-4 build root. The module is not yet in `lakefile.toml`, so the build-root
  check fails until the coordinator adds it, and the suite's axiom audit,
  which imports the module, fails with it. Every other check passes. On 16
  October 2026 the 17 cited theorems were audited by appending
  `#print axioms` to a copy of the file. Each uses only `propext`,
  `Classical.choice` and `Quot.sound`.

## Page images read

PDF pp. 7, 9, 11, 19 and 20 of WP 06-003 and PDF p. 20 of WP 04-025 were
rendered at 110 dpi with `pdftoppm` from the pinned PDFs. They are
reproducible from them and not pinned separately.
