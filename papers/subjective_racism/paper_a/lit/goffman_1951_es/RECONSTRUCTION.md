# Goffman (1951), Spanish translation, reconstruction

Erving Goffman, "Símbolos de status de clase", Spanish translation by
Servando Ortoll of "Symbols of Class Status", *British Journal of Sociology*
2(4), 1951, pp. 294–304. Universidad de Sonora, published with the written
permission of Blackwell Publishing Ltd. 16 pages.

## Source read

- The translation as a 16-page PDF, downloaded from
  `https://sociologia.unison.mx/docs/publicaciones/cuadernodetrabajo/4simbolosdeestatusdeclase.pdf`
  (PDF metadata, author "Servando Ortoll, PhD", created 28 February 2008).
  Cached at `~/.cache/paper_a/sources/goffman_1951_es.pdf`, sha256
  `eee9d3ac…8dbc`, with its text layer at
  `~/.cache/paper_a/sources/goffman_1951_es.txt`, sha256 `ebe1c1dc…3ffa`.
- Read in full [F] on 16 October 2026, every page from the text layer, with
  page images of PDF pp. 6 and 13 rendered at 80 dpi to check the numbering
  of the lists (GF-D3). PDF page equals printed page.
- **The English original is not read** (GF-D1). Every claim in this record is
  a claim about the translation. English renderings are ours.

## The essay, from its own primitives

**Section I** (PDF pp. 1–5). A status is a set of rights and duties. Since
rights and duties are poorly suited to communication, specialised means of
displaying position arise, the *símbolos de status*, signs that select the
status to be imputed to a person and how others treat him (GF-Q1). Status
symbols divide the social world into categories, with solidarity within and
hostility between them (GF-Q2). They designate position, not performance in
it, and differ from symbols of esteem. Any trait of conduct is a sign of
position. A sign used regularly to place a person is a status symbol, and a
sign that gives reliable evidence of position is a *prueba de status*. The
essay is about the gap between the two (GF-Q3). A symbol has categorical
significance (it identifies status) and expressive significance (it expresses
a style of life). Because a symbol is distinct from what it signifies, it can
always be used "fraudulently" (GF-Q4), so its continued use requires
mechanisms that restrict misrepresentation. Occupational symbols are tied to
their referent by legal penalties and corroborating information. Class is a
configuration of sources, judged within a margin of doubt, with no legal
penalty for misrepresenting it. A person who claims a class that a
well-informed majority would not grant commits an impertinence, not a crime
(PDF p. 5). So class symbols channel others' opinion more than they represent
or misrepresent position (GF-Q5), and fraud has a weakened sense.

**Section II** (PDF pp. 6–12). Six restrictions on misuse.
1. *Moral*. Internalised scruple, backed by the opinion of the person's own
   group and of the class whose symbols he would use.
2. *Intrinsic*. A symbol that consumes what it symbolises. Wealth is shown by
   spending it, so the owner of an expensive symbol has at least its market
   price (GF-Q6). The high price of scarce objects is explained by the social
   benefit of displaying them.
3. *Natural*. Natural scarcity, from rare materials, historical closure,
   production that takes a large share of available means, an exclusive
   connection to production (the artist, the family name), the limited number
   of intimate associates, so that association with high-status persons is a
   symbol (GF-Q7), and the limited audience of a live performance, so that the
   theatre is a symbol and the cinema is not (GF-Q8).
4. *Socialisation*. Social style, made of etiquette, dress, gesture,
   intonation, dialect and vocabulary (GF-Q9). It is learned as a whole and
   cannot be broken into parts small enough to be taught. A habitual act can
   be told from a conscious imitation (GF-Q10). A style expresses a class's
   way of life and is psychologically ill suited to those raised in another.
   Classes keep exclusive the situations in which the meaning of an act is
   taught.
5. *Refinement*. Expertise that requires long unpaid attention, discipline and
   perseverance, among them foreign languages and composure.
6. *Organic*. Physical traits that record long-run diet, work and environment,
   such as the hands and height of men. Inherited traits used as status
   symbols belong to a caste society, not a class society (GF-Q12).

**Section III** (PDF pp. 12–16). Classes select as symbols those parts of
their conduct that are expressive and restricted. No restriction withstands
every contingency, and every one is routinely evaded (GF-Q11), for example by
the British public schools, which remake the middle class in the image of the
aristocracy. Stable classes therefore rely on symbols resting on several
restrictions at once, so that one restriction checks another's failure
(GF-Q13). Three problem areas follow.
1. *Class movement*. Mobility makes symbols confer the position they indicate
   (GF-Q14). This slows the rise of the new rich and the fall of the old. A
   declining class comes to rely on symbols that need no current outlay, and
   its association with them lowers their value in others' eyes (GF-Q15). A
   rising class acquires costly symbols before those of refinement and
   socialisation.
2. *Curators*. Servants, decorators, models and others are recruited from
   lower classes and become expert in handling symbols of a higher position,
   an institutional source of misrepresentation.
3. *Circulation*. Systematic evasion makes symbols circulate down and up. The
   originators must keep moving to something not yet contaminated, and the
   adopting class uses a symbol that signifies status but expresses little of
   its own life.

## Derivation chain

The essay derives nothing formally. The argument runs as follows. A symbol is
distinct from its referent, so misrepresentation is possible (GF-Q4). Use of
a symbol therefore persists only with restrictions on misrepresentation
(PDF pp. 3–4). Every restriction is evaded somewhere (GF-Q11), so stable
classes combine restrictions (GF-Q13), and evasion drives circulation, with
symbols losing value as lower-standing users take them up (GF-Q15).

## The structure we attribute, and the Lean

`lean/mathlib/Goffman1951es.lean` formalises our reading of every structural
claim in the essay that has a structure to formalise. The essay states these
claims in words only, and every formal version is ours.

- The *prueba* as a symbol all of whose displayers have at least a given
  standing (`IsPrueba`), with fraud as a displayer below it
  (`not_prueba_iff`, `curator_breaks_prueba`).
- Five of the six restrictions. Moral, scruple and the sanction of opinion
  against the benefit (`moral_deters`, `scruple_reinforces_sanction`).
  Intrinsic, a priced symbol as a *prueba* of its price (`intrinsic_is_prueba`,
  `prueba_mean_ge`). Natural, scarce units held by the top
  (`scarcity_prueba`) and the capacity bound on intimate association and on a
  live audience (`association_capacity`). Socialisation, imitation detected
  with a probability (`socialisation_deters`). Refinement, a cost of time
  falling in standing (`refinement_upper_set`).
- The price of scarce objects as experiential plus display value
  (`price_from_display`).
- Restrictions in combination (`conjunctive_restrictions`,
  `one_checks_failure`, `evasion_always_possible`).
- The value of a symbol as the mean standing of its displayers, which falls
  when lower-standing users take it up (`association_lowers_value`), the
  mechanism of decline, of the new rich and of circulation.
- The margin of doubt as disagreement over the weights of the sources of class
  (`opposite_rankings`).
- Public proof binding open treatment and not covert treatment
  (`open_treatment_equal`, `control_covert_treatment`).

## What could not be reconstructed

The coverage table in `CLAIMS.md` lists every claim. Those not formalised,
and the special cases, are these.

- Anything about the English wording. The translation's "fraudulenta",
  "prueba de status" and "distorsión" are taken as the translator's renderings
  of terms not seen here.
- Claims with no stated structure: solidarity within and hostility between
  categories, categorical and expressive significance, partial symbolism, and
  the expressive loss of an adopting class.
- The organic restriction, a claim about biological growth, and the caste
  footnote, a classification.
- Class movement, a claim about change over time. A1 is static, and the essay
  gives no structure for the lag.
- The originators' move to an uncontaminated symbol under circulation. The
  fall in value is formalised, the move is not.
- The special cases are two sources of class with linear weights, additive
  scruple and sanction, detection as a probability, a linear cost of time, two
  independent restrictions, and a two-group mean.
- Any quantity. The essay has no data. Section III closes by calling for
  empirical studies of the social careers of particular symbols.
