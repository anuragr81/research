# Claims about Laurison and Friedman (2016)

**Source.** Accepted author version (LSE Research Online), read in full [F];
pages are the accepted version's. Cited in `MANUSCRIPT.tex` (row L17) and in
`refs.bib` as `LaurisonFriedman2016`. The published version is not checked,
and the manuscript row says so. Every claim is a claim about what the paper
reports, verified by quotation; the arithmetic that links the printed
numbers is checked in `LaurisonFriedman2016.lean` (core Lean, `decide`), and
the two places where the text and the tables disagree are recorded as
controls.

## Quotations

| ID | Page | Quotation | What we rely on | Verified by |
|---|---|---|---|---|
| LF-1 | 1 | "even when those who are not from professional or managerial backgrounds are successful in entering high-status occupations, they earn sixteen percent less, on average, than those from privileged backgrounds" | Their main finding is a pay gap after entry, which the model does not contain | quotation |
| LF-2 | 1 | "beyond entry, the mobile often face an earnings “class ceiling” within high-status occupations" | The ceiling is beyond entry; the model stops at entry | quotation |
| LF-3 | 6 | "it conflates occupational access with class position, and inadvertently suggests that all individuals enter occupations on an equal footing" | Their critique of access-only mobility research applies to the model, which is an access model | quotation |
| LF-4 | 11 | "those from NS-SEC 1 backgrounds are nearly twice as common in NS-SEC 1 as in the general population (26.6% vs 14.1%)" | Closure of NS-SEC 1 as a whole | quotation; Lean `nearly_twice` |
| LF-5 | 11 | "greater economic capital does not necessarily map onto greater social closure in a UK context" | Closure does not follow pay across occupations; in the model a larger prize alone raises the paying class's share, so the cross-occupation pattern needs the rule weight μ, not V | quotation |
| LF-6 | 13 | "the higher professions remain significantly more elitist in terms of restricting access for those from working class backgrounds" | Access is more closed in the professions than in management, which earns more (p. 11) | quotation |
| LF-7 | 12 | "the traditional—or “gentlemanly” (Miles and Savage 2012)—professions of law, medicine, finance, life science, academia and science contain a particularly high concentration of those from NS-SEC 1 backgrounds" | Which occupations are closed in access | quotation |
| LF-8 | 12 | "less than 7% of doctors, veterinarians, dentists or physical scientists, for example, are from routine or semi-routine working class or no-earner family origins" | Closure at the top of the professions | quotation; Lean `under_seven_percent` against Table 2 |
| LF-9 | 14 | "who as a group earn an average of only 83% as much as intergenerationally stable, which translates into £141 less per week, or an annual difference of about £7350" | The size of the raw pay gap | quotation; Lean `eighty_three_percent`, `weekly_gap`, `annual_gap` |
| LF-10 | 16 | "differences between the stable and the upwardly mobile account for 46% of the class pay gap but 54% remains unexplained" | The decomposition | quotation; Lean `explained_share` against Table 4 |
| LF-11 | 17 | "“talent” is routinely evaluated by large graduate employers according to attributes rooted in middle-class socialisation" | Their reading of the recruiters' rule is a rule that weights a class-rooted component, which is μ < 1 in the model's terms | quotation |
| LF-12 | 17 | "the inter-generationally stable are more than 1.5 times as likely to work in London as the long-range upwardly mobile (27% vs 16%, see Table A3)" | A sorting channel after entry | quotation; Lean `london_ratio` against Table A3 (26.7 and 16.0) |
| LF-13 | 20 | "science, academia and work on built environment all have pay gaps estimated to be close to zero in both models" | Where the pay gap is absent | quotation |
| LF-14 | 21 | "In finance, for example, the upwardly mobile have average predicted earnings less than 75% of the intergenerationally stable." | Where the pay gap is largest | quotation |
| LF-15 | 22 | "a good portion of the gap is accounted for by what could be termed sorting mechanisms" | Their own word for the explained part | quotation |

## Discrepancies recorded (controls in the Lean file)

| ID | Page | Text | Table | Lean |
|---|---|---|---|---|
| LF-D1 | 12 | "53% of doctors16 are the children of higher managers and professionals" | Table 2 gives 52.3% for medical practitioners; Table A7 gives 52.6% | `control_doctors_rounding` (52.3 rounds to 52, 52.6 to 53) |
| LF-D2 | 17 | "only 27% of people from working-class origins are in 500+ person firms, as compared with 37% of people from NS-SEC 1 origins" | Table A3 gives 31.1% (long-range), 33.0% (mid-range) and 39.8% (stable) | `control_firm_size_text_ne_table` |

## What is not claimed

- Nothing about causes. The authors say they "cannot conduct a proper causal
  analysis" (p. 16).
- Nothing about μ by occupation. The paper measures no rule weight; the
  mapping of "traditional" professions to a heavier weight on the bought
  component is our reading of LF-11, not theirs.
- Nothing about the within-occupation pay gap as a model object. The model
  has one prize and no earnings after entry.
