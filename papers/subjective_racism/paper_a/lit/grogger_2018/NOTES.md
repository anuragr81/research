# Grogger (2018), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite in the text layer,
  by exact normalised containment (LC-2).
- The definition of mainstream speech with its doubling rule, the occupational
  sorting model of eqs. (1) and (2) with footnote 19, and the arithmetic of the
  coefficients the text reports are Lean theorems in the paper's notation
  (LC-4). Our reading is consistent with the source on these points.

## What could not be verified

- The regression estimates. The audio-linked NLSY97 data are restricted, so the
  numbers are taken as printed. Where the text and Table 4 disagree, the table
  is read (GRG-D2).
- Whether the premium is customers' and coworkers' taste or a social skill. The
  paper does not separate them (GRG-Q10, GRG-Q14).
- The cited Grogger (2011) article. Nothing in this record supports a claim
  about it beyond what its author says of it here (GRG-D1).

## The bearing on Paper A1

Nothing in the source contradicts the A1 model. One premise differs. Within
race, speech correlates with family income, schooling and AFQT (GRG-Q6, Table
2), and listeners of both races rate AAVE speakers lower in socioeconomic
status (GRG-Q5, [C]). An observer who hears a person learns about that
person's class beyond the group, which A1's group sufficiency rules out. A1
should say that its marker is the group as seen, and that a heard dialect is a
second, individual cue that the model leaves out.

Is speech read as race, as class or as skill. The measure is listeners' reports
of race, so speech is read as race by construction (GRG-Q3), and the penalty
falls on whites heard as black too (GRG-Q7). Speech is also read as class by
listeners (GRG-Q5) and correlates with it (GRG-Q6). The author does not frame
the penalty as an inference about class. He argues against speech as general
skill, because the premium appears only in interaction-intensive work
(GRG-Q11), and he leaves taste and social skill open (GRG-Q10).

Does the penalty survive controls for skill. Yes. With AFQT, family income,
family structure, maternal education, school type and state-by-urban effects,
the black mainstream premium is 0.119 (Table 4, column 2), and personality,
skin colour, industry by occupation and criminal records leave it between
0.104 and 0.124 (GRG-Q9). Black men heard as mainstream earn about what
comparably skilled whites earn (GRG-Q7). For Southern whites the premium does
not survive location controls.

Speech as a marker one can change. The source treats it as nearly fixed after
puberty (GRG-Q4). Few black men move toward the mainstream between 2006 and
2011 (GRG-Q12, `table7`), sorting is the same at the first and the last job
(Table 8), and a code-shifting measure earns no significant premium (Table 4,
column 6). Speech is a weak channel for passing in adulthood. The source's
reading of Moving to Opportunity is that children who moved young used less
AAVE, [C] for those studies.

The candidate contributions of A1 are taken in turn.

1. **Group avoidance from class homophily under inference.** Not in the
   source. Listeners of both races rank AAVE below the standard in the same
   direction (GRG-Q5), a ranking that does not depend on the listener's own
   group. The wage results do not change when listener characteristics are
   purged from the speech measure (PDF pp. 15 to 16). Nothing depends on the
   observer's own class, and no group's spread enters.
2. **Budget compatibility as the source of class homophily.** Not in the
   source.
3. **Venue exclusion from client inference, and price screening.** Not in the
   form A1 has it. The nearest material is the consumer taste for mainstream
   speech (GRG-Q2) and the concentration of the premium in work with customers
   and coworkers (GRG-Q1, Table 6). That is an employer pricing clients'
   preference over a worker's marker. It is a taste, not an inference about
   class, and there is no venue or price that screens clients.
4. **A test of the class channel.** Not in the source, and the source runs the
   opposite manipulation. A1's test shows class and keeps the marker, and
   predicts that the group penalty in contact vanishes. Grogger holds skill
   proxies fixed and varies the race heard, and finds the penalty attached to
   the race heard (GRG-Q7), concentrated in contact-intensive work (GRG-Q1).
   His evidence that the penalty survives AFQT and family background (GRG-Q9)
   is evidence against the penalty being only a proxy for those measures of
   class and skill. It is a wage result, not a contact decision, and the
   author cannot say whether the penalty is taste or productivity (GRG-Q10).

What A1 can cite the source for. Speech as a heard marker of race that also
correlates with class within race (GRG-Q3, GRG-Q6), the survival of the
speech penalty under skill controls (GRG-Q9), its location in interaction-
intensive work (GRG-Q1), and speech as a marker that adults rarely change
(GRG-Q4, GRG-Q12). If A1 cites Grogger (2011), the citation must be checked
against that article, which this record does not cover.

## Controls

- LC-2. A fabricated quotation that says the opposite of GRG-Q9, "the
  speech-related wage gap among blacks is fully explained by AFQT scores and
  family background", is not found on any page. A true quotation is not found
  on a neighbouring page.
- Lean. `control_doubling_needed` shows that the doubling rule is needed for
  speakers heard on one prompt. `control_no_sorting` shows that sorting needs
  `λ₁ > λ₀`. `control_fn19` shows that footnote 19 needs `α₁ < α₀`.
