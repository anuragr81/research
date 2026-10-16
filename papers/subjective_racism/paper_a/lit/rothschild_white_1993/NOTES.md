# Rothschild and White (1993), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite in the text layer of
  the NBER chapter, by exact normalised containment (LC-2).
- Eq. (1) and its implementation of any allocation, the ranking lemma for
  anonymous prices, the efficiency of grouping similar abilities under a
  variance penalty, and the variances of the diversity example are Lean
  theorems in the chapter's notation (LC-4). The chapter is verbal apart from
  eq. (1), so these are formalisations of our reading of its structure, and
  the special cases are stated in the Lean file. Our reading is consistent with
  the source on these points.

## What could not be verified

- The 1995 *Journal of Political Economy* article, which is the formal
  treatment of customers as inputs, is not read (RW-D1). Nothing about its
  model is attributed here.
- The claims the chapter cites rather than derives, the Spence-type segregating
  price (RW-Q8) and possible non-existence (RW-Q9).

## The bearing on Paper A1

Nothing in the chapter contradicts the A1 model. Its two examples pull in
opposite directions, a variance penalty in section 1.3.2 and a gain from mixing
in section 1.3.3, and A1 is closer to the first.

The candidate contributions of A1, in turn.

1. **Group avoidance from class homophily under inference.** The chapter has a
   penalty on a pool's variance in matching, the nearest found so far. It
   supposes that the benefit a college confers rises with its students' mean
   ability and falls with their variance, so that similar students should be
   grouped (RW-Q7, `sorted_pairing_efficient`). It differs from A1's σ² term in
   three ways. It is supposed, not derived. It is the realised dispersion of
   abilities within a pool, not the uncertainty an observer has about one
   partner after reading a marker. And it is the same for every student,
   since every student gains from a higher mean, so the ranking of pools is
   vertical and does not depend on the student's own ability. Our reading,
   inferred and not checked here, is that averaging A1's cost `κ(c_i − c_j)²` over a
   venue's members gives `κ((c̄ − c_i)² + σ²)` by the same identity as A1's
   expectation, so A1's utility yields the chapter's variance penalty plus a
   gap term that depends on the client's own class. A1 can cite the chapter for
   the variance penalty as a supposition in the matching of students to
   colleges and claim the observer-dependent gap and the derivation as its own.
2. **Budget compatibility as the source of class homophily.** Not in the
   chapter. Wealth appears only as a limit on a price-based allocation, through
   imperfect capital markets (RW-Q14).
3. **Venue exclusion and pricing on client composition.** The premise is here.
   Clients enter as inputs to the venue's output (RW-Q1, RW-Q2), and the venue
   prices and admits on client composition. It underprices to draw a large
   pool and then selects by admission and by scholarship (RW-Q4). A denial of
   admission is an infinite price (RW-Q5, `pA_finite_iff`). Prices may
   condition on observable characteristics (RW-Q10, RW-Q11), and clients pay
   for their peers through tuition (RW-Q13). What is not here is any inference. The college
   observes the characteristics it prices on, and no client infers anything
   about another from a group marker. So the chapter confirms the verdict of
   the motivation scan. Contribution 3 is new only as exclusion built from
   clients' inference from a group marker, and A1 should cite this chapter
   with Pesendorfer (1995) for the customers-as-inputs premise.
4. **A test of the class channel.** Not in the chapter.

A caution A1 should take from the chapter. With preferences over classmates,
stable allocations may not exist (RW-Q12, RW-Q15), and competition among
colleges for a desired mix has the structure of Rothschild and Stiglitz, where
the obvious equilibria may not exist (RW-Q9). A1's equilibrium with venue
choice has clients who care about the composition of a venue, so A1 needs an
existence argument or a restriction that delivers one.

Feldstein's comment (RW-D3, PDF p. 30) attributes parents' willingness to pay
private tuition to "the reputation and presumed exclusiveness" of private
colleges. That is Feldstein's remark, not the authors', and it is not relied
on.

## Controls

- LC-2. A fabricated quotation, "Allocations differ in efficiency even when
  there is no synergy.", the opposite of RW-Q3, is not found. A true quotation
  is not found on a neighbouring page.
- Lean. `control_no_synergy` shows that without a variance penalty all
  allocations tie. `control_perfect_correlation` shows that the gain from
  mixing needs inputs that are not perfectly correlated.
  `control_personalised_separates` shows that personalised prices do what
  anonymous ones cannot.
