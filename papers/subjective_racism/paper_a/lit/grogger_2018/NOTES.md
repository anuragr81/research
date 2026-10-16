# Grogger (2018), notes

## What was verified

- Sixteen quotations are found on the PDF pages they cite in the text layer,
  by exact normalised containment (LC-2).
- The definition of mainstream speech with its doubling rule, the occupational
  sorting model of eqs. (1) and (2), footnote 19 without disturbances,
  footnote 20 in a three-type example, and every number in the text that
  follows from printed numbers are Lean theorems in the paper's notation
  (LC-4). The coverage table in `CLAIMS.md` lists each result. Our reading is
  consistent with the source on these points, except where a reading is
  recorded (GRG-D2, GRG-D3, GRG-D13, GRG-D14).

## What could not be verified

- The regression estimates and test statistics. The audio-linked NLSY97 data
  are restricted, so the numbers are taken as printed. Where the text and
  Table 4 disagree, the table is read (GRG-D2).
- Whether the premium is customers' and coworkers' taste or a social skill. The
  paper does not separate them (GRG-Q10, GRG-Q14).
- The cited Grogger (2011) article. Nothing in this record supports a claim
  about it beyond what its author says of it here (GRG-D1).

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1. The account
below follows the six headings of the second reading.

### The puzzle

The source does not study friendship or contact choice. It bears on the puzzle
through the origin of the marker. Native accent is acquired from linguistic
peers in childhood (GRG-Q4), so the composition of a child's peers fixes a
marker the adult carries, and adults rarely change it (GRG-Q12, `table7`).
Whether a heard marker of this kind makes contact across class lower in
diverse settings is not addressed and is left open.

### Budget compatibility

The source's origin of sorting is productivity in a Roy model (eqs. 1 and 2,
`sorting`), not homophily, and it says nothing about cost, affordability or
shared consumption. It neither supports nor contradicts budget compatibility.

### Avoidance that depends on the observer's class

Direction (i), avoidance both ways. The source measures listeners' reports and
wages, not avoidance. Black and white listeners rate AAVE speakers lower in the
same direction (GRG-Q5, [C]). That ordering is common to observers of both
races, which limits any reading of the speech penalty as avoidance by each
group of the other.

Direction (ii), a low-class member of the high group preferring outsiders. Not
in the source. Whites heard as black earn 16.2% less (GRG-Q7), 21 workers, but
this is a penalty on carrying the marker, not a preference, and their class is
not reported.

The mirror of (ii). Black men heard as mainstream sort into
interaction-intensive work (GRG-Q1). The marker is set in childhood and rarely
changed (GRG-Q4, GRG-Q12), so this is sorting on a marker one has, not passing
by choice.

### The penalty vanishing when class is shown

The source's nearest statement is that black men heard as mainstream earn 1.6%
less than comparably skilled whites, with a standard error of 5.1% (GRG-Q7,
`table3_col5`). The racial gap vanishes when the marker is not heard, given
controls for schooling, experience, region, urban residence and marriage. That
is the marker removed, not class shown to an observer. The penalty on
distinctive speech itself survives controls for AFQT, family income, parental
education and location (GRG-Q9), so in the source showing a measure of skill or
background to the researcher does not remove it. The author does not link the
result to statistical discrimination. He offers customers' taste (GRG-Q2,
GRG-Q14) and social skill (GRG-Q10).

### The shadow market

The source has two cases of screening on a marker correlated with race where
race itself is not the stated criterion. Landlords show apartments more often
to renters who sound white on the phone (GRG-Q16, [C] for the audit studies),
where the caller's race is not seen and the voice stands in for it. Employers
pay for mainstream speech in work with customers and coworkers (GRG-Q1,
GRG-Q2), which is a rule on a variable that is neutral in form and correlated
with race. The premium appears only where interaction is most intense (Table
6, PDF p. 29). Our reading is that this matches the predecessor's item that
shadow information gains value with the weight clients put on social quality.
The source does not discuss law, so it says nothing on discrimination moving
when the law tightens.

### Premises of A1 the source does not share

- Within race, speech correlates with family income, schooling and AFQT
  (GRG-Q6, `table2_extremes`), and listeners read it as status (GRG-Q5). A
  heard dialect is an individual cue to class, which A1's group sufficiency
  leaves out.
- The penalty may be productivity rather than inference, since social skill is
  one of the two explanations (GRG-Q10).
- What A1 can take from the source. Speech as a heard marker of race that also
  carries class (GRG-Q3, GRG-Q6), the survival of its penalty under skill
  controls (GRG-Q9), its location in contact-intensive work (GRG-Q1), screening
  on voice in housing (GRG-Q16), and speech as a marker that adults rarely
  change (GRG-Q4, GRG-Q12). A citation of Grogger (2011) must be checked against
  that article.

## Controls

- LC-2. A fabricated quotation that says the opposite of GRG-Q9, "the
  speech-related wage gap among blacks is fully explained by AFQT scores and
  family background", is not found on any page. A true quotation is not found
  on a neighbouring page.
- Lean. `control_doubling_needed` shows that the doubling rule is needed for
  speakers heard on one prompt. `control_no_sorting` shows that sorting needs
  `λ₁ > λ₀`. `control_fn19` shows that footnote 19 needs `α₁ < α₀`.
  `control_fn20_no_selection` shows that the bias of footnote 20 comes from
  selection.
