# Goffman (1951), Spanish translation, notes

## What was verified

- Twenty-five quotations, in the translation's Spanish, are found on the PDF
  pages they cite in the text layer (LC-2).
- The cached PDF and text match their pinned sha256 (LC-3).
- Twenty-six Lean theorems, nine of them controls, formalise our reading of
  every structural claim in the essay that has a structure to formalise
  (LC-4). The essay states no model. The formalisation is of our reading of
  its argument, not of anything the essay states formally.
- On 16 October 2026 the second-pass file was compiled to a scratch `.olean`.
  The suite's own audit read the project's earlier build and audited 7 of 26
  names. The audit was then run by hand, `#print axioms` on all 26 cited names
  against the scratch `.olean`, and every one depends on no axiom beyond
  `propext`, `Classical.choice` and `Quot.sound`.
- Our reading is consistent with the translation on these points. Nothing is
  claimed about the English original, which is not read (GF-D1).

## What could not be verified

- The English wording of every passage. The English renderings in `CLAIMS.md`
  are ours, from the Spanish. A1 must not quote them as Goffman's words. To
  quote Goffman in English, the original must be read.

## The bearing on Paper A1

Nothing in the essay contradicts the corrected account outright. Three of its
premises differ from A1's and are listed last. The essay bears most on the
shadow market.

**The puzzle.** The essay does not discuss mixed settings or friendship
across groups. Two of its claims bear on the puzzle, and both are inferences
of ours. Ethnic and racial loyalty restrains people from taking up another
class's symbols even when they can afford them (GF-Q19, `moral_deters`), which
keeps class symbols tied to groups. When lower-standing people take up a
symbol its value falls (GF-Q15, `association_lowers_value`), and its
originators move to symbols "not yet contaminated" (GF-Q24). In a mixed
setting more symbols are shared across groups, so on Goffman's account their
holders have more reason to seek cues that separate them. Neither claim is
about friending.

**Budget compatibility.** The essay ties a class's symbols to outlays its
position must sustain. Intrinsic symbols of wealth imply that the owner has
at least their price (GF-Q6, `intrinsic_is_prueba`), and a declining class is
left with "símbolos y expectativas que su posición económica y política ya no
puede mantener" (GF-Q20). That supports the premise that a class's way of life
is a pattern of spending that a smaller budget cannot sustain. The essay's own
origin of distance across class is different. A class's style is "poco
indicados psicológicamente" for those raised in another class (GF-Q25), and
symbols keep categories apart (GF-Q2). That is a difference of socialisation
and style, not of budgets. The essay also gives a vertical motive, association
with high-status persons as a status symbol (GF-Q7), whose scarcity comes from
the limited number of intimates (`association_capacity`).

**Avoidance that depends on the observer's class.**
- Direction (i), avoidance both ways. Symbols maintain "hostilidad entre
  categorías diferentes" (GF-Q2), stated symmetrically, with no claim that one
  side avoids more. The essay supports direction (i) in words and gives no
  structure for it.
- Direction (ii), a low-class member of the high group preferring outsiders.
  The essay's nearest case runs the other way. Ethnic and racial loyalty keeps
  people with the means from taking up another class's symbols (GF-Q19), a
  restraint that the opinion of their own group reinforces
  (`scruple_reinforces_sanction`). Nothing in the essay supports direction (ii).
- The reading itself depends on the observer's class. A class keeps exclusive
  the situations in which the meaning of an act is taught (GF-Q22), so one
  class may read as a symbol what another does not. In A1 the posterior from a
  marker is common to all observers and only its use depends on the observer's
  class. Goffman makes the reading depend on the observer.

**The penalty vanishing when class is shown.** The essay's nearest statement
is that those who give public proof of their society's favoured values, among
them education, ability and talent, "no se les puede negar abiertamente el
status" (GF-Q18). The conditions are two. The proof must be public, and the
statement binds only open treatment. Covert treatment is unconstrained
(`open_treatment_equal`, `control_covert_treatment`). The essay also denies
that class can be shown exactly, since it is a configuration of sources
judged within a margin of doubt (GF-Q5, `opposite_rankings`). A dominant
profile is the exception (`control_dominance_agrees`). For A1 this limits the
property. Showing class removes the group penalty only where class can be
shown and only in the treatment the showing can bind. That is the
statistical-discrimination property for the open channel. The covert channel
is where Goffman locates the benefits of class.

**The shadow market.** The essay bears on it more than any source read so
far.
- The law's reach. Occupational symbols are protected by legal penalties and
  corroborating records (GF-Q23). Class symbols are not, and misrepresenting
  class is "un atrevimiento, no un crimen" (GF-Q16). The boundary of the law
  in the essay is the boundary between the predecessor's contracting channel
  and contact channel.
- What the shadow market trades. The benefits of class are preferential
  treatment in jobs, services and economic exchanges "que no está abierta u
  oficialmente aprobado" (GF-Q17). Public proof binds open treatment only
  (GF-Q18, `control_covert_treatment`).
- Venues and price. Theatre attendance is a class symbol because its audience
  is limited, and cinema attendance is not (GF-Q8, `association_capacity`). A
  priced symbol certifies wealth (GF-Q6, `intrinsic_is_prueba`). These are
  the predecessor's atmosphere and price as class signals, stated in 1951.
- Intermediaries. Curators build and service the "maquinaria de status"
  (GF-Q21), recruited from lower classes and expert in higher symbols. Their
  presence breaks a symbol's reliability (`curator_breaks_prueba`). The
  predecessor's platform, which produces atmosphere, is such an intermediary.
- Information about meaning has value. Knowledge of which acts signal class
  is kept exclusive (GF-Q22), which is information about where to go and whom
  to meet acquiring value.
- Reliability and its erosion. A cue is reliable only while restrictions keep
  lower-standing people from displaying it (`scarcity_prueba`,
  `refinement_upper_set`, `socialisation_deters`). Every restriction is
  evaded somewhere (GF-Q11, `evasion_always_possible`), and a cue loses value
  as it spreads (GF-Q15, `association_lowers_value`). In a static equilibrium
  the value of a cue is the mean standing of its users, which is A1's
  consistency condition applied to shadow cues.
- What the essay does not say. Nothing on legal strictness raising the value
  of shadow information, on the weight clients put on social quality, or on a
  group's standing. The predecessor's comparative statics have no support or
  contradiction here.

**Premises of A1 the essay does not share.**
- An inherited group marker is a caste symbol, not a class symbol (GF-Q12). A1
  reads class from such a marker. In Goffman's terms A1's marker is evidence
  about class through the group, a *prueba* at the level of the group (GF-Q3),
  not a class symbol. A1 should say which it means.
- Class is a configuration judged within a margin of doubt (GF-Q5,
  `opposite_rankings`), not a scalar `c` with a true value, so fraud holds only
  in a weakened sense.
- The meaning of a sign depends on the observer's class (GF-Q22). A1 has one
  posterior per marker.
- Symbols change over time, conferring position under mobility (GF-Q14) and
  circulating (GF-Q24). A1 is static.

## Controls

- LC-2. A fabricated quotation, "su asociación con estos símbolos aumenta el
  valor de estas señales ante los ojos de los demás", the opposite of GF-Q15,
  is not found. A true quotation is not found on a neighbouring page.
- Lean. Each main result has a control that drops one condition.
  `control_counterfeit` (no intrinsic restriction),
  `control_association_from_above` (newcomers not below the mean),
  `control_alternative_restrictions` (restrictions as alternatives),
  `control_no_moral_restriction` (no scruple or sanction), `control_not_top`
  (scarce units not held by the top), `control_undetectable` (no detection),
  `control_cost_not_falling` (cost of time not falling in standing),
  `control_dominance_agrees` (no conflict between sources) and
  `control_covert_treatment` (treatment not restricted to public proof).
