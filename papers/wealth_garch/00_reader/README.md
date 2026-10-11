# Reader layer

One file, `ledger_empirical.py`. The prose documents that used to sit here
were retired on 11 Oct 2026; `RETIREMENT.md` says where each went.

## ledger_empirical.py

EMPIRICAL_v2's claims, each recording what it asserts, over which
population, and on what support. `verify()` checks that every model row a
claim cites exists in `MANUSCRIPT.tex`, that every cited artefact exists,
that a claim asserted over a country population has panel support, that a
specification-only claim reports no computed result, and that the null
figures quoted in the document match `03_empirical/results/lambda_V_curve.csv`.

The scope guard is the substantive one: it stops the headline result being
restated over a population the panel does not cover.

## Running it

    python3 00_reader/ledger_empirical.py

`04_reproduce/run_all.sh` runs it.

## What a green verify() does and does not mean

It means the recorded relations are mutually consistent: cited rows and
artefacts exist, quoted figures match the data files, and no claim is stated
over a population its support does not reach. It does not mean the
mathematics is right, which is what the Lean layer is for, and it does not
mean the encoding faithfully represents the document. A mis-encoding can
pass. Where the ledger and the document disagree, find out which is wrong
before changing either.
