# Mill and Stein (2012), notes

## What was verified

- Twenty quotations are found on the PDF pages they cite in the text layer,
  by exact normalised containment (LC-2).
- Eqs. (4.1) to (5.4), the within-family estimator for two brothers, the
  inflation by colour distance of PDF p. 29, the Roy argument, and every number
  in the text that follows from printed numbers are Lean theorems in the
  paper's notation (LC-4). The coverage table in `CLAIMS.md` lists each result.
  Our reading is consistent with the source on these points, except where a
  reading is recorded (MLS-D2 to MLS-D4, MLS-D12, MLS-D13).
- The Roy argument is formalised in our reading, with log income linear in one
  earning potential.

## What could not be verified

- The regression estimates. The microdata are not available here, so the
  numbers are taken as printed.
- The linking procedure, whose appendix is absent from this version (MLS-D7).
- Thirteen readings are recorded as MLS-D1 to MLS-D13. Footnote 5's 11.4%
  (MLS-Q14) and footnote 4's match rates (MLS-Q15) are quoted only to record
  that they disagree with the tables.

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1. The account
below follows the six headings of the second reading.

### The puzzle

The source does not study contact across class. It records that contact
across the race line was refused by the high group in public places
(MLS-Q20), and that within the black group there were clubs that admitted
only the light-skinned (MLS-Q5). Passing is defined as a change of social ties
with appearance fixed (MLS-Q1). None of this bears on why friending across
class should be lower in diverse settings, and the source leaves that open.

### Budget compatibility

The source gives segregation and the refusal of whites as the origin of
separate contact (MLS-Q20), not budgets. Its one economic reason concerning
contact is in Myrdal's teacher, who valued "the economic stability he gained
from the relative scarcity of skilled African-American workers" (MLS-Q8). It
says nothing about affordability or shared consumption as a source of
homophily, and so neither supports nor contradicts budget compatibility.

### Avoidance that depends on the observer's class

Direction (i), avoidance both ways. The source has avoidance from the high
group, collective and partly legal (MLS-Q20), and it does not report avoidance
from the low group. It does not vary either by the observer's class.

Direction (ii), a low-class member of the high group preferring outsiders. Not
in the source.

The mirror of (ii), a high-class member of the low group preferring the high
group's pool. Passing is this choice, and the source finds it selected on
schooling and family (MLS-Q13). Its mechanism is a higher return to skill among
Whites (MLS-Q10, MLS-Q12), which our reading formalises as `roy_variance` and
`passers_upper`, not class homophily. The source also limits the mirror.
Myrdal's teacher could pass and chose not to, citing his status in the black
community and "his richer social life among the higher ranks of
African-Americans" (MLS-Q8). Our reading is that in his own community his class
was known, so his contacts there were with partners of known class, while
outside it he would have been imputed the White mean. The mirror then holds
only where class is not shown within one's own group. The teacher is reported
second hand, [C] for Myrdal.

### The penalty vanishing when class is shown

The source has a statement of this shape for colour within the black group.
The population earnings premium for lighter skin "renders it not statistically
different from zero" once schooling is controlled (MLS-Q16), and between
brothers the colour gaps in literacy, schooling and earnings are small (Tables
4 to 6). The conditions are a researcher's control for schooling or a family
fixed effect (`within_family`, `within_estimator`), not an observer who is
shown class, and the authors read the result as family background, not as
inference. For the race line itself the premium survives schooling in the
population (MLS-Q2) and is 14% to 21% between brothers (MLS-Q3), but between
brothers with the same schooling it is near zero (MLS-D5). The source does not
link either result to statistical discrimination.

### The shadow market

The formal rule ran on ancestry, so appearance decided only where ancestry was
unknown (MLS-Q18). White markets refused to hire or trade with African-Americans,
and professional passing "opened many doors" (MLS-Q17). Passing could be
partial, venue by venue, on a bus or at a theatre (MLS-Q19). In the predecessor's
terms, discrimination ran through markets and venues on the marker, and passing
was a way around it by changing the marker that a venue or employer read. Venues
also screened inside the group. The blue-vein societies admitted on skin colour
(MLS-Q5), and in Bond's account their pool emptied as their members passed
(MLS-Q4, [C] for Bond). The source has no anti-discrimination law, so it says
nothing on discrimination moving when the law tightens.

### Premises of A1 the source does not share

- The finer recorded marker carries status within the group. Mulatto
  households own their homes more often (0.380 against 0.244) and have literate
  heads more often (0.664 against 0.562) (Table 2), and the authors suggest
  that enumerators coded children of higher-status households as Mulatto
  (MLS-Q6). A1's group sufficiency rules this out.
- The penalty on the race line is formal and on ancestry (MLS-Q18), not an
  inference about class.
- What A1 can take from the source. Positive selection into passing on
  schooling and family (MLS-Q13), the size of the return to passing (MLS-Q2,
  MLS-Q3), the within-group colour gap as a family-background gap (MLS-Q16),
  and the blue-vein societies as a venue that screened on the marker (MLS-Q5).

## Controls

- LC-2. A fabricated quotation that says the opposite of MLS-Q13, "we see that
  passers were negatively selected on education", is not found on any page. A
  true quotation is not found on a neighbouring page.
- Lean. `control_family_effect` and `control_within_orthogonality` show what
  the sibling design needs. `control_selection_biases` shows that selection
  biases the measured return. `control_equal_slopes` and `control_roy_sign`
  show what the Roy selection needs. `control_no_inflation` shows the
  colour-distance correction. `control_log_not_percent` shows that log points
  are not percentages.
