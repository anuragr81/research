# Literature formalizations

One subdirectory per paper cited in `PAPER_B_MANUSCRIPT.tex`, its notes or its
change plan. Each subdirectory holds `sympy/` and, where the paper makes a
formal claim, a `lean/` symlink into `../lean/Literature/`, with scripts that
formalize and check that paper's own mathematical claims -- not Paper B's. A
`README.md` in each records: full citation, which claims were formalized, the
result, how it bears on Paper B, and (for records added 2026-09-29) the
citation-audit findings with proposed corrected wording.

This directory is a research aid (independent verification of the literature
this paper positions itself against), not manuscript content. The audit that
drove the 2026-09-29 records is `../notes/citation_audit.md`.

    python3 run_all.py                 # every literature script
    cd ../lean && lake build           # every Lean file, incl. Literature/*
    cd ../lean && lake env lean check_axioms.lean

Caveat: the sympy-only records (first seven rows) print their results but do
not assert, so they exit 0 whatever they print; `field1978`'s script has a
known bug (audit item L1). Adding Lean to them is audit item R2.

| Paper | Formalization | Status |
|---|---|---|
| [field1978](field1978/README.md) | sympy only | done |
| [diaconis_zabell1982](diaconis_zabell1982/README.md) | sympy only | done |
| [wagner2002](wagner2002/README.md) | sympy only | done |
| [wagner2003](wagner2003/README.md) | sympy only | done |
| [pettigrew_weisberg2025](pettigrew_weisberg2025/README.md) | sympy only | done |
| [hawthorne2004](hawthorne2004/README.md) | Lean + sympy (asserting) | done 2026-09-30 |
| [garber1980](garber1980/README.md) | sympy only | done |
| [augenblick_rabin2021](augenblick_rabin2021/README.md) | Lean + sympy | done (dropped from write-ups) |
| [shmaya_yariv2016](shmaya_yariv2016/README.md) | Lean + sympy | done (dropped from write-ups) |
| [weisberg2009](weisberg2009/README.md) | Lean + sympy | done 2026-09-29 |
| [doring1999](doring1999/README.md) | Lean + sympy | done 2026-09-29 |
| [domotor1980](domotor1980/README.md) | Lean + sympy | done 2026-09-29 |
| [goodmittal1987](goodmittal1987/README.md) | Lean + sympy | done 2026-09-29 |
| [fgt1984](fgt1984/README.md) | Lean + sympy | done 2026-09-29 |
| [hogarth_einhorn1992](hogarth_einhorn1992/README.md) | Lean + sympy | done 2026-09-29 |
| [asch1946](asch1946/README.md) | Lean (Tables 7/8 as data) + sympy | done 2026-09-30 |
| [cripps2021](cripps2021/README.md) | Lean + sympy | done 2026-09-29 |
| [dietrich2021](dietrich2021/README.md) | Lean + sympy | done 2026-09-29 |
| [bhw1992](bhw1992/README.md) | Lean + sympy | done 2026-09-29 |
| [banerjee1992](banerjee1992/README.md) | Lean + sympy | done 2026-09-29 |
| [phelps1972](phelps1972/README.md) | Lean + sympy | done 2026-09-29 |
| [arrow1973](arrow1973/README.md) | Lean + sympy | done 2026-09-29 |
| [coate_loury1993](coate_loury1993/README.md) | Lean + sympy | done 2026-09-29 |
| [bohren_imas_rosenberg2019](bohren_imas_rosenberg2019/README.md) | Lean + sympy | done 2026-09-29 |
| [heckman1998](heckman1998/README.md) | Lean + sympy | done 2026-09-29 |
| [bcgs2016](bcgs2016/README.md) | Lean + sympy | done 2026-09-29 |
| [jeffrey1983](jeffrey1983/README.md) | Lean (shared Jeffrey.lean) | done 2026-09-30 |
| [jeffrey2004](jeffrey2004/README.md) | Lean (shared Jeffrey.lean); Drive copy is the 2002 draft | done 2026-09-30 |
| [benjamin2019](benjamin2019/README.md) | Lean + sympy | done 2026-09-30 |
| [zhao_osherson2010](zhao_osherson2010/README.md) | Lean + sympy | done 2026-09-30 |
| [zhao2012](zhao2012/README.md) | Lean + sympy | done 2026-09-30 |
