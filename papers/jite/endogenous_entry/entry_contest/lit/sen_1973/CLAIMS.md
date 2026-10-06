# Sen (1973), claims our documents make or rely on

Cited in `MEASUREMENT_MAP.tex` (framing paragraph, the `V` row, the `ΣU_i`
row). Not cited in `PROOFS.tex` or `LITERATURE.tex`, and not in `refs.bib`.
Read in full [F].

| ID | Claim | Locator | Verbatim source | Verified by |
|---|---|---|---|---|
| SEN-1 | The observer infers preference from choice, reversing the chooser's own direction | p.241 | "choices are observed first and preferences are then presumed from these observations" | quotation |
| SEN-2 | With pairs and triples in the domain, the Weak Axiom forces transitivity | p.245 | "The Weak Axiom not only guarantees two-term consistency, it also prevents the violation of transitivity." | Lean `weak_axiom_forces_transitive_pair` |
| SEN-3 | A cycle of strict pairwise choices violates the Weak Axiom | pp.246-247 | "If a consumer has chosen x rejecting y in one case, chosen y rejecting z in another, and chosen z rejecting x in a third case, then he has not only violated transitivity, he must violate the Weak Axiom of Revealed Preference as well." | Lean `cyclic_choice_violates_weak_axiom` |
| SEN-3c | Restricting the domain lets an intransitive pattern satisfy the Weak Axiom | p.246 | "the man can get away satisfying the Weak Axiom over all the cases in which his behaviour can be observed in the market and nevertheless harbour an intransitive preference relation" | Lean controls `cycle_on_pairs_satisfies_weak_axiom`, `cycle_on_pairs_is_cyclic` (pairs-only analogue) |
| SEN-4 | Prisoners' dilemma payoffs; confessing is strictly dominant; mutual non-confession is better for each | p.249 | matrix "Confess -10,-10 0,-20 Not Confess -20, 0 -2, -2"; "Each prisoner sees that it is definitely in his interest to confess no matter what the other does." | Lean `confess_strictly_dominant`, `mutual_nonconfession_better_for_each`; SymPy SEN-4 |
| SEN-5 | A prisoner who does not confess has not revealed a preference of his own | p.251 | "The prisoner does not prefer to go to prison for twenty years rather than for ten; nor does he prefer a sentence of two years to being free. His choice has not revealed his preference in the manner postulated." | Lean `nonconfession_reveals_no_preferred_outcome`; SymPy SEN-5 |
| SEN-6 | Maximising the other's welfare makes non-confession dominant and leaves each better off in own terms | p.252 | "if both prisoners try to maximize the welfare of the other, neither will confess in the case outlined since non-confession will be a superior strategy no matter what is assumed about the other person's action" | Lean `other_regarding_makes_nonconfession_dominant`, `as_if_play_better_in_own_terms`; SymPy SEN-6 |
| SEN-7 | The dilemma depends only on the ordering of the penalties | p.252 | "even with asymmetrical prison sentences as long as the orderings of the penalties are the same we can get exactly the same dilemma" | Lean `dilemma_from_orderings` (general), `dilemma_survives_asymmetric_sentences` (instance), control `control_dilemma_needs_reward_above_punishment` |
| SEN-8 | Preference can be defined as the binary representation of choice | p.253 | "the underlying relation in terms of which individual choices can be explained"; "preference will simply be the binary representation of individual choice" | quotation |
| SEN-9 | An as-if preference that represents choice is not thereby welfare | p.254 | "a numerical representation of the as if preference cannot be interpreted as individual welfare" | quotation |
| SEN-10 | Information on preference need not come from choices alone, and questionnaires have known limits | pp.257-258 | "information need not be restricted to distant observations of choices made"; "well-known limitations of the questionnaire method" | quotation |
| SEN-11 | Correspondence with choice and correspondence with welfare cannot in general both be kept | p.259 | "it is not in general possible to guarantee both simultaneously" | quotation |
