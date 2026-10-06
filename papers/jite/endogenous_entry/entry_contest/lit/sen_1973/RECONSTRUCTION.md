# Sen (1973), reconstruction

Amartya Sen, "Behaviour and the Concept of Preference", *Economica*, New Series,
40(159), August 1973, pp. 241-259. Read in full on 6 Oct 2026 from the JSTOR
copy in Drive folder `1r7J3xJ2rotpmlRaBANCNhviy2d8a1bx_`, file
`sen73-behaviour and the concept of preference.pdf`. The text is an inaugural
lecture, so its formal content is short and its argument is mostly in prose.

## Objects, in our notation

- A choice correspondence `C`, giving for each menu `S` the elements chosen
  from `S`.
- Revealed strict preference of `x` over `y` in menu `S`, meaning `x` is chosen
  from `S`, `y` is in `S`, and `y` is not chosen. Sen's phrase is "choosing x
  and rejecting y" (p.245).
- The Weak Axiom in the form Sen uses in §III. If `x` is chosen and `y` rejected
  in one menu, then `y` is not chosen from any menu that contains `x`.
- A two-player game with sentences as negative payoffs (p.249).

## Derivation chain

1. §III (pp.244-247). If the domain of `C` contains all pairs and triples, the
   Weak Axiom forces transitivity of revealed strict preference. The argument
   is a case analysis on the choice from the triple `{x, y, z}`. A cyclic
   pattern of pairwise choices therefore violates the Weak Axiom, whether or
   not the triple is observed.
2. §III (p.246). In demand theory the domain is budget sets only, so the
   triple is never a menu, and a consumer can satisfy the Weak Axiom on every
   observable choice while holding an intransitive preference.
3. §V (pp.249-252). In the prisoners' dilemma confessing is strictly dominant,
   mutual non-confession is better for each than mutual confession, and a
   prisoner who follows a code of non-confession chooses an action that is
   worse for him against either action of the other. His choice therefore
   reveals no preference of his own. Maximising the other's welfare makes
   non-confession dominant and leaves each better off in his own terms.
4. §§VI-X (pp.253-259). Preference can be defined as the binary
   representation of choice, or kept in line with welfare, but not in general
   both at once.

## What could not be reconstructed formally

- Step 2 uses budget sets with divisible goods. The Lean control uses a
  pairs-only domain instead, which shares the feature that matters (the triple
  is absent from the domain) but is not Sen's construction.
- Step 4 and the remarks on stated preferences (pp.257-258) are conceptual
  claims about interpretation. They are verified by quotation, not by Lean.
