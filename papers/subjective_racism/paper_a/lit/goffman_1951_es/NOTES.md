# Goffman (1951), Spanish translation, notes

## What was verified

- Fifteen quotations, in the translation's Spanish, are found on the PDF
  pages they cite in the text layer (LC-2).
- The cached PDF and text match their pinned sha256 (LC-3).
- Seven Lean theorems, three of them controls, formalise our reading of the
  essay's structure (LC-4). The essay states no model, and the formalisation
  is of our reading of its argument, not of anything the essay states
  formally.
- Our reading is consistent with the translation on these points. Nothing is
  claimed about the English original, which is not read (GF-D1).

## What could not be verified

- The English wording of every passage. The English renderings in `CLAIMS.md`
  are ours, from the Spanish. A1 must not quote them as Goffman's words. To
  quote Goffman in English, the original must be read.

## The bearing on Paper A1

Two points of the essay sit against A1 as described, and they come first.
- **An inherited marker is a caste symbol.** Goffman places inherited
  characteristics used as status symbols in a caste society, not a class
  society (GF-Q12). A1 reads class from a visible group marker, which may be
  inherited. In Goffman's terms A1's marker is not a class symbol but
  evidence about class through the group, closer to his *prueba* (GF-Q3) at
  the level of a group than to a symbol. A1 should say which it means and
  should not cite Goffman for the reading of class from an inherited marker.
- **Fraud in a weakened sense.** For Goffman class position is a
  configuration of sources judged within a margin of doubt, so a class claim
  within that margin cannot be shown false (GF-Q5). A1 gives each person a
  true class `c`, so passing has a definite meaning. The difference is in the
  premises, and A1 should state it.

Contribution by contribution.
1. **Group avoidance derived from class homophily.** Not in the essay. Symbols
   maintain solidarity within categories and hostility between them (GF-Q2),
   but this is asserted as a function of symbols and not derived from a
   preference. Nothing depends on the observer's own class, and there is no
   penalty on a group's spread. The essay does give a rival reason for
   contact, association with high-status persons as a status symbol (GF-Q7),
   which is a vertical motive, the kind the first-wave verdicts found in
   every economic source.
2. **Budget compatibility.** Not in the essay. The intrinsic restriction
   (GF-Q6) is a budget constraint on displaying a symbol, not on a joint
   activity between two people.
3. **Venue exclusion and price screening.** Price as certification of wealth
   is in the essay in words, as the intrinsic restriction (GF-Q6,
   `intrinsic_is_prueba`), decades before Pesendorfer (1995). Venue attendance
   as a class symbol is in the essay too, the theatre against the cinema
   (GF-Q8), protected by the limited size of the audience rather than by
   price or by clients' inference. Venue exclusion built up from clients'
   inferences from a group marker is not in the essay.
4. **A test of the class channel.** Not in the essay.

What A1 can cite, as the translation. The premise that class is read from
signs and the reading sets treatment (GF-Q1). Misrepresentation as always
possible because the sign is distinct from the class (GF-Q4), with every
restriction evaded somewhere (GF-Q11). Speech, intonation and dialect as class
symbols protected by socialisation, with imitation detectable (GF-Q9,
GF-Q10), which the Sutton Trust testimonials match (ST-Q5). A verbal precursor
of A1's consistency condition, a symbol whose value falls when lower-standing
people take it up (GF-Q15, `association_lowers_value`), and of passing
lowering the standing imputed to a sign, which A1 already credits to Kim and
Loury (2012) in formal terms.

## Controls

- LC-2. A fabricated quotation, "su asociación con estos símbolos aumenta el
  valor de estas señales ante los ojos de los demás", the opposite of GF-Q15,
  is not found. A true quotation is not found on a neighbouring page.
- Lean. `control_counterfeit` shows that without the intrinsic restriction a
  priced symbol certifies nothing. `control_association_from_above` shows
  that the fall in value needs lower-standing newcomers.
  `control_alternative_restrictions` shows that combining restrictions
  protects a symbol only if a pretender must evade all of them.
