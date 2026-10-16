# McPherson, Smith-Lovin and Cook (2001), reconstruction

Miller McPherson, Lynn Smith-Lovin and James M. Cook, "Birds of a Feather:
Homophily in Social Networks", *Annual Review of Sociology* 27 (2001),
pp. 415–444.

## Source read

- The published article, 30 PDF pages, journal pp. 415 to 444, with the
  Annual Reviews download stamp of 14 February 2005 in the margin. Cached at
  `~/.cache/paper_a/sources/mcpherson_2001.pdf` (278,637 bytes), sha256
  `22f8c389…87ad`, with the text layer at
  `~/.cache/paper_a/sources/mcpherson_2001.txt`, sha256 `bd681033…0b10`.
  The text has 31 form-feed segments. Segment `n − 1` is PDF page `n`, and
  PDF page `n` is journal page `n + 414`. Segment 30 is empty.
- Every page, 415 to 444, was read from the text layer on 16 October 2026, so
  the evidence tag is [F]. Pages 439 to 444 are the reference list. The
  extraction is clean and no page needed an image. Pages are cited as journal
  pages.
- The download stamp "by Columbia University on 02/14/05. For personal use
  only." is extracted into the middle of each page's text, and words broken by
  a hyphen at a line end are not rejoined by the suite (MSC-D1, MSC-D2).

## What the source is

A review. It states no model and proves nothing. It organises a literature by
the type of tie, the dimension of similarity and the source of homophily, and
it reports other authors' findings. Every empirical number in it is another
study's, so a claim taken from it about that study is tagged [C] for the
study and [F] only for what the review says.

## The structure, from its own terms

1. **Homophily** (p. 416). "A contact between similar people occurs at a higher
   rate than among dissimilar people" (MSC-Q1).
2. **Two measures** (p. 419). Some studies count in-category and out-category
   ties, which group sizes drive. Others measure homophily as a deviation from
   random assortment given the opportunity pool. The review calls the first
   **baseline homophily**, created by "the demography of the potential tie
   pool", and the second **inbreeding homophily**, measured "over and above the
   opportunity set" (MSC-Q3).
3. **What inbreeding includes** (p. 419, n. 5). Inbreeding covers homophily
   induced by structures below the population level, by other dimensions
   correlated with the focal one (Blau's consolidation) and by personal
   preference. So it "does not in any sense indicate choice or agency purified
   of structural factors" (MSC-Q5).
4. **Status and value homophily** (p. 419). After Lazarsfeld and Merton (1954),
   status homophily is similarity in ascribed or acquired status, including
   race, sex, age, religion, education and occupation, and value homophily is
   similarity in values, attitudes and beliefs (MSC-Q4). The review treats
   value homophily as often "derivative of social positions themselves".
5. **The ordering of dimensions** (p. 415). Race and ethnicity create the
   strongest divides, followed by age, religion, education, occupation and
   gender "in roughly that order" (MSC-Q2).
6. **Race and ethnicity** (pp. 420–422). Race homophily combines the baseline
   from group sizes with "the differences in racial/ethnic groups' positions on
   other dimensions (e.g., education, occupation, income, religion) and the
   personal prejudices that often result from the latter" (MSC-Q6). Blau's
   size-based prediction fails for Black and Anglo intermarriage because areas
   with more African Americans have larger class differences between the two,
   and once that consolidation is controlled, population shares predict ties
   again (MSC-Q7). Race also shows the highest inbreeding homophily of any
   dimension (MSC-Q8). Marsden (1988) finds no social distance effect for race
   once baseline and inbreeding are accounted for, and the review reads the key
   distinction as "same-different" (MSC-Q9).
7. **Education, occupation and class** (pp. 426–427). Every educational group
   shows inbreeding and a social distance effect, with ties less likely "as
   their difference from others' achievement increases" (MSC-Q10). The review
   reports that the cultural rather than the economic aspects of educational
   and occupational homophily "drive the structure" (MSC-Q11).
8. **Values** (pp. 428–429). Political homophily may be "due to actual
   political similarity or similarity on other social characteristics that are
   correlated with political beliefs" (MSC-Q15).
9. **Sources** (pp. 429–435). Geography, family, organisational foci,
   isomorphic positions and cognitive processes. Foci produce ties about twice
   as homogeneous as chance (MSC-Q14). Under cognitive processes, the review
   writes that if demographic similarity indicates shared knowledge, people
   would associate with similar others for ease of communication and shared
   tastes (MSC-Q12).
10. **Dissolution** (pp. 435–437). Dissimilar ties dissolve faster, and a
    member near the edge of a group's niche, unlike the other members, is more
    likely to leave (MSC-Q13).
11. **Future research** (pp. 437–438). Multiplexity, dynamic data, and the
    co-evolution of foci and networks.

## What is formalised, and from what

`lean/mathlib/McPhersonEtAl2001.lean` formalises our reading of every structural
claim the review makes. None is a result the review proves. The table "Coverage
of the source's results" in `CLAIMS.md` lists each claim with its Lean name.

- Homophily, the ordering of dimensions, baseline and inbreeding (MSC-Q1 to
  MSC-Q3, `race_strongest`, `baseline_minority`, `inbreeding_antitone_size`).
- Consolidation and foci, in any number of classes or foci
  (`consolidation_inbreeding`, `consolidation_pos`, `foci_inbreeding_nonneg`,
  with controls), which also covers value homophily derived from status and
  tracking in schools (MSC-Q5, MSC-Q7, MSC-Q14, MSC-Q19).
- A graded distance effect against same-different (MSC-Q9, MSC-Q10, MSC-Q18,
  `distance_effect`, `control_same_different_flat`).
- Multiplex ties, selective dissolution, the edge of a niche and co-membership
  counts (`multiplex_cumulates`, `dissolution_raises_homophily`,
  `niche_decomposition`, `comembership_ties`, `comembership_per_member`).

## What could not be reconstructed

- No result of the review can be derived from it, because it states none. Its
  numbers are other studies' estimates and are read as reported. Of them, only
  Marsden's benchmark follows arithmetically from printed numbers
  (`marsden_random_benchmark`).
- The claims that selection outweighs influence and that structure outweighs
  choice are empirical comparisons with no stated structure, and are not
  formalised.
- Four formalisations are special cases chosen by us. The distance effect uses
  a quadratic form, the multiplex claim a multiplicative odds model, the niche
  claim mean squared distance with no exit rule, and the size claim a fixed
  in-group share.
- Marsden (1988), Blau et al. (1982), Laumann (1973), Kalmijn (1998),
  Hallinan and Smith (1985) and the other studies the review cites are not
  read. Anything attributed to them through this record is [C].
- The review does not define inbreeding homophily by a formula. The excess
  `H − p` is our reading of "over and above the opportunity set".
