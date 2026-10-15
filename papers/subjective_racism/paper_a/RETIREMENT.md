# Retirement of the predecessor manuscript (pass 6, 15 October 2026)

`papers/subjective_racism/theory_and_decisions/manuscript.tex` was removed
after this record was written. Its last version is in git at commit
`91a74cb1`
(`git show 91a74cb1:papers/subjective_racism/theory_and_decisions/manuscript.tex`).
Line numbers below refer to that version. `checks/verify_retirement.py`
(suite 5) reads it from git, takes an inventory of its environments, sections,
figures, Lean names and numbers, and fails if any item is neither present in
the documents that replace it nor listed here with a reason. Since 15 October
2026 it also checks every prose quotation of the predecessor in
`MANUSCRIPT.tex` and `MEASUREMENT_MAP.tex` against the retired text (R8). Its
first run caught two of our own misquotations, a lower-cased "Under base
parameters" and a misdescription of which forms of group sufficiency the
predecessor joins with "equivalently", both since corrected.

The documents that replace it are `MANUSCRIPT.tex` (model rows M1 to M17,
assumptions A1 to A13, refuted conjecture R1, Appendices A to G), `MEASUREMENT_MAP.tex`, `TODO.md`
and `lean/mathlib/README.md`.

Not retired, and left where they are. `theory_and_decisions/welfare_analysis_addendum.tex`
is a standalone reference that says it is not part of the submission, and
its claims are not rows (`TODO.md`, Q12). `theory_and_decisions/figures/`
keeps the scripts that drew the two figures, which M17 reads its parameters
from. `main/manuscript.tex` is an older copy of the same paper, moved there
by commit `b3857490`, and is untouched. `jeffrey_vs_bayes_micromacro/` is
Paper B.

## Environments

| Line | Environment | Goes to |
|---|---|---|
| 335 | assumption `ass:timescale`, timescale separation | A7, and Appendix D, "The law of motion". The composed map takes the discrimination rate of a generation from that generation's standing, which is what the assumption buys |
| 435 | assumption `ass:sufficiency`, group sufficiency | A4, and M1, which proves the likelihood and posterior forms equivalent under full support, and Appendix D |
| 494 | example `ex:jeffrey-bayes`, the stranger and the accent | M17 (`stranger_example`), the mixture of M2 and M3. The Jeffrey reading is dropped (D4, Appendix D) |
| 535 | example `ex:misaddressed`, the misaddressed parcel | Not carried. Its Bayesian agent revises the within-group association from an anomalous second signal, which group sufficiency excludes (M1). Two readings of a marker are outside the rows (Appendix D, "The updating rule") |
| 675 | assumption `ass:preferences`, risk-neutral payoff in squared class distance | Not a row. It feeds the engagement decision, which lies upstream of `F` (Appendix D, "The law of motion"; `TODO.md` Q12) |
| 683 | assumption `ass:beliefs`, Beta–de Finetti within-group beliefs | `MEASUREMENT_MAP.tex`, rows Within-group dispersion and Mobility. It enters the rows only through `gamma_tilde = gamma/(1 + nu)` |
| 798 | assumption `ass:exclusion-cost`, the cost of exclusion | A5 and A6, M4 to M12 through the composed map, and the Terminology row Cost of exclusion |
| 848 | assumption `ass:Freg`, regularity of `F` | A9, M12, which uses continuity and `F(1) = F'(1) = 0` only, and M4 |
| 891 | definition `def:closure`, closure equilibrium | M5, the fixed points of the composed map, and the Terminology rows Closure trap and Discrimination rate |
| 903 | proposition `prop:class-closure`, class closure | M5 and M6 for the canonical `F`, M12 for a general `F` |
| 951 | proposition `prop:coincidence`, the structural trap | M1 to M3, and Appendix D, "The updating rule". Its across-generations half is moot under D1 and D4 (`TODO.md` Q6) |
| 1000 | remark `rem:graded`, graded discrimination | Not carried. It is a statement about the engagement decision, which is not a row (`TODO.md` Q12) |
| 1014 | remark `rem:salience`, the marker salience effect | Not carried as stated, for the same reason. Its dynamic consequence, that salience raises the mobility threshold, is M10 |
| 1049 | proposition `prop:bifurcation`, the mobility threshold | M9, with the threshold in closed form, positive only when `b > 4 mu`, and the slope 1 at the merged root |
| 1085 | corollary `cor:salience-threshold` | M10, which restricts it to `s_g delta >= mu` and shows it fails below |
| 1104 | definition, outsider group | Terminology row Outsider group, M11 |
| 1119 | proposition `prop:enemy`, the outsider trap | M11, with the bounds of M6 added. The independence of the two groups is structural (Appendix D, "Two groups") |
| 1561 | proposition `prop:passing`, adverse selection through passing | M14, for a finite population |
| 1668 | proposition `prop:shadow`, shadow market intensity | Not a row. `S*` and `S_rand` are undefined (Appendix D, "The platform channel") |
| 1709 | proposition `prop:exclusion`, platform exclusion | M15 (i) and (ii) |
| 1740 | proposition `prop:tokenism`, partial inclusion | Refuted as stated, Appendix F, R1 |
| 1778 | proposition `prop:spillover`, host destabilisation | M15 (iii) and (iv) |
| 1856 | proposition `prop:loury-nesting`, the intensity threshold | M13. Its `rho* ~ 0.31` is not carried (Appendix D, "The predecessor's numbers") |
| 1884 | remark `rem:platform-bayes` | Not carried. It reads `rho` as rigidity of updating, which M13 shows is a scale on the exclusion pressure. What survives is that below the threshold there is no tipping point, so `eta_crit` has nothing to cross (M9, M15) |
| 1906 | remark `rem:resolution`, individual resolution | Not carried. The signal precision `tau` enters no row (`TODO.md` Q12) |
| 1946 | corollary `cor:Tstar`, critical slowing | M16, as a lower bound with exponent `-1/2`. The constant `C = pi / sqrt(|ab|)` is not stated |

## Sections

| Section | Goes to |
|---|---|
| Introduction | The research question (D2) and the headlines K1 to K6 |
| Related Literature | Section 3, empty until papers are read, and the pending conclusions C12 to C16 |
| Model | Section 2, rows M1 to M17 |
| Primitives | Appendix D and `MEASUREMENT_MAP.tex`, Inputs |
| Social engagement and Jeffrey Conditioning | M1 to M3, and Appendix D, "The updating rule" |
| Social Engagement Decision | Not a row. Upstream of `F` (`TODO.md` Q12), and named in `MEASUREMENT_MAP.tex` as the engagement threshold |
| Intergenerational dynamics | M4, and Appendix D, "The law of motion" and "The group prior across generations" |
| Closure Equilibrium | M5, M6 and M12 |
| The updating rule and the structural trap | M1 to M3, and Appendix D, "The updating rule" |
| Comparative statics and the mobility threshold | M9, M10 and M11 |
| Role of Institutions: the platform channel | M15, R1, and Appendix D, "The platform channel" |
| Discussion | C1 to C11 where a row carries it, C13 to C15 where it waits on a paper. Its claim that forced integration leaves the trap to regenerate for later cohorts is not carried, since in M6 a standing pushed above the tipping point converges to integration and no row has cohorts |
| Conclusion | Section 4, Conclusions |
| Proofs | Appendix A |
| Adverse Selection through Passing | M14 |
| The Platform Channel | M15 and R1 |
| The Discrimination-Intensity Parameter | M13 |
| Intergenerational Persistence (\texorpdfstring{$T^*$}{T*}) | M16 |
| Beta--De~Finetti Structure of Within-Group Heterogeneity | `MEASUREMENT_MAP.tex`, Within-group dispersion |
| Statements and Declarations | Not carried. Journal front matter, to be written at submission |

## Figures

| Figure | Goes to |
|---|---|
| figures/fig_composed_map | M17. Parameters `mu = 0.05`, `delta = 0.5`, `s_g = 1`, start `0.55`, and `gamma_tilde = 0.05` and `0.30` in the two panels, read from `figures/generate_figures.py` |
| figures/fig_bifurcation_delta | M10 (`bStar_at_figure`), `delta* ~ 0.29` confirmed |

The loop figure `fig:loop` is TikZ inside the file. Its content, the
re-learned prior, is Appendix D, "The group prior across generations" (D1).

## Numbers not carried

Numbers the checker finds in the replacing documents are not listed.

| Number | Where | Reason |
|---|---|---|
| 0.25 | line 393 | TikZ label position in `fig:loop` |
| 2.6,0 | line 392 | TikZ coordinate in `fig:loop` |
| 62 | line 366 | TikZ text width, `62mm`, in `fig:loop` |
| 16 | line 367 | TikZ fill shade, `black!16`, in `fig:loop` |
| 2.2 | lines 370, 371 | TikZ arrow length in `fig:loop` |
| 0.7 | lines 499 to 511 | The host standing in the stranger example. M17 carries it as `7/10` |
| 0.31 | line 1880 | `rho*` under unstated base parameters. Appendix D, "The predecessor's numbers", gives `0.58 < rho* < 0.60` at Figure 1's panel (a) instead |
| 65 | line 1967 | Generations to escape after raising mobility by `0.01`, with no parameters given. Appendix D, "The predecessor's numbers" |
