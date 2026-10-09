# Reconstruction of Morgan, Tumlinson and Vardy, "The Limits of Meritocracy"

**Source read.** John Morgan, Justin Tumlinson and Felix Várdy, "The Limits of
Meritocracy", IMF Working Paper WP/18/231, November 2018, 87 pages, supplied
by the author on 9 Oct 2026 as `wp18231.pdf` on Drive. This is the working
paper; the published version is *Journal of Economic Theory* 201 (2022),
105414, which is not checked, so proposition numbers may differ there. The
text was extracted with `pdftotext` and cached at
`~/.cache/entry_contest/morgan_tumlinson_vardy_2018.txt`; the extraction drops
Greek letters and most mathematical symbols, so quotations used here are
prose only. Read: the introduction, literature review, Section III (baseline
model, equilibrium, limits of meritocracy) and Section IV (heterogeneous
contestants) through Theorem 2. Not read: Section V (finite-player contests),
the conclusion, and the appendices with the proofs.

**Why this paper.** Ryvkin and Drugov (2020, footnote 23, p.1607) cite its
working paper for "aggregate effort can be nonmonotone in noise intensity due
to players dropping out". It is the nearest candidate precedent for M28,
which signs how the count of entrants moves with the weight on the base
score.

## The model

1. **Agents.** A unit mass of homogeneous, risk-neutral agents (Section III;
   heterogeneous abilities in Section IV). Each chooses output at a convex
   cost, or drops out (output zero, log-output minus infinity, payoff zero).
2. **Performance and meritocracy.** Measured performance is output times
   i.i.d. noise; in logs, log-output plus noise. Noise has a scale parameter
   `σ`, and precision `1/σ` is "meritocracy". Perfect meritocracy is
   `σ → 0`. The noise density is strictly log-concave.
3. **Prizes.** A mass `q` of the best measured performers win a prize; with a
   continuum, this is a deterministic standard to beat.

## Results used

1. **Proposition 1 (p.13-14).** For large `σ` everyone participates. For small
   `σ` agents mix between producing and dropping out, and participation rises
   with `σ`: "the intensity of competition is high, and contestants compete
   away all rents" (p.14).
2. **Theorem 1 (p.15).** Output is maximised at the critical `σ` where
   dropping out begins; perfect meritocracy reduces output and is Pareto
   inefficient when marginal costs strictly increase.
3. **Proposition 4 (p.22).** With heterogeneous abilities, participation is
   single-crossing in ability: types above a threshold participate, below it
   they drop out, and the threshold falls as `σ` rises (so participation
   rises with noise).
4. **Literature (p.8).** They report that Fu, Jiao and Lu (2015) find a
   contest organiser may prefer a noisier winner selection to encourage
   entry.

## What could not be reconstructed

Section V, the proofs, and the published version. The direction of
participation in noise is taken from the statements of Proposition 1 and
Proposition 4 and their surrounding text, whose symbols the extraction
drops; the prose on pp.14-15 and p.4 states the same direction in words.
