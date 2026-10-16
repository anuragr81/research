# Charles, Hurst and Roussanov (2007), reconstruction

Kerwin Kofi Charles, Erik Hurst and Nikolai Roussanov, "Conspicuous
Consumption and Race", NBER Working Paper 13392, September 2007. Published as
*Quarterly Journal of Economics* 124(2), 2009, pp. 425–467, which is not read.

## Source read

- The NBER working paper, 65 PDF pages, with the title page, abstract, text pp. 1–36
  (PDF 3–38), references (PDF 39–41), Data Appendix (PDF 42–43), Theory
  Appendix (PDF 44–45), Tables 1–10 and A1–A5 (PDF 46–61), Figures 1 and 2a–2f
  (PDF 62–65).
- Cached at `~/.cache/paper_a/sources/charles_hurst_roussanov_2009.pdf`,
  sha256 `f386f956629c4d2379e00c4c8644992976106b2cfa39c6b2d6cf8fd3c6239e62`,
  and its text layer at `~/.cache/paper_a/sources/charles_hurst_roussanov_2009.txt`,
  sha256 `21630c72500a7805f65a3028cdcc91d7edf56fa29ff949c456b1308bd28444f1`.
  The file name says 2009, but the title page is the September 2007 working
  paper (CH-D1).
- Every page was read from the text layer on 16 October 2026 [F]. PDF pages
  22, 23, 44, 45 and 62 to 65 were also rendered with `pdftoppm` and read as
  images, because the text layer garbles eqs. (3), (4) and the Theory
  Appendix and drops the figures' regression lines (Figures 2b and 2e).

## The model, from its own primitives

1. **Group and preferences** (p. 21). A social group `G` has incomes `y^i`
   with mean `μ_G`. Utility is `u(y^i − c) + α u(c^i) + s`, with `c` visible
   consumption and `y − c` everything not visible (food, health, saving).
2. **Status** (p. 22). Outsiders know the person's visible consumption and
   "what is known about the income of the group", in the simplest case `μ_G`
   (CH-Q2). In equilibrium their inference is correct (CH-Q3), and status is
   `s^i = s(c^i, μ_G)` with `s_c > 0`. The sign of `s_μ` is left open, with
   `s_μ < 0` for status as income relative to the group, and `s_μ > 0` in
   Ireland (1994), where a richer group confers status (CH-Q5).
3. **First order condition** (p. 22, eq. 3). `s_c(c*, μ_G) = u'(y − c*) − α u'(c*)`.
4. **Comparative statics** (p. 23, eq. 4). Differentiating (3) in `μ_G` gives
   `s_cμ = −c*_μ [u''(y − c*) + α u''(c*) + s_cc]`. The bracket is negative by
   the second order condition, so `c*_μ` has the sign of `s_cμ`.
5. **Simplest status** (pp. 23–24). With status as income relative to the
   group mean (CH-Q6), `s_cμ < 0`, and visible consumption falls in the group
   mean at fixed own income.
6. **Variance** (p. 22, footnote 24). Status may depend on the variance of the
   group's income, which "will matter in a pure signaling model" but is hard
   to separate from the mean (CH-Q4). The model does not carry it.
7. **Theory Appendix** (PDF 44–45). Utility `log(y − c) + α log c + s`, status
   `s = g(c)/μ_G` (CH-Q13), first order condition
   `α/c − 1/(y − c) + g'(c)/μ_G = 0`, and in equilibrium `g(c) = y`, so
   `g'(c) = μ_G (1/(g(c) − c) − α/c)`. With `c = x y` the appendix writes
   `w'(x) = μ_G (1/(1 − x) − α/x)` and integrates to
   `w(x) = −μ_G ln(1 − x) − μ_G α ln x + K`, with the initial condition that
   the poorest do not signal, `x_min = α/(1 + α)`. The share `x` rises in income
   and falls in `μ_G` at fixed minimum income (CH-Q14).
8. **Race** (p. 25). Race and state define the reference group (CH-Q7). Race
   is not a signal of income in the model. The motivation notes that race is
   strongly correlated with economic status (CH-Q1).

## The empirical results, as reported

- Blacks and Hispanics spend about 30 percent more on visible goods than
  Whites at the same total expenditure (Table 2, row 7, 0.32 and 0.31).
- Within each race, visible spending falls in the mean income of the
  own-race, same-state group (Tables 6 and 7). Other groups' mean income has
  no negative effect (Table 6 column 3, Table 7 column 5).
- The dispersion regressor is negative for Whites, −0.74 (0.29), positive and
  not significant for Blacks and Hispanics, 0.42 (0.23), and 0.23 (0.14) in the
  pooled specification of Table 8 (CH-Q8, CH-Q9, CH-D3).
- Controlling for the own-group mean income removes the racial gap, from 0.32
  to −0.09 for Blacks (Table 8, CH-Q10, CH-Q11).
- The share of visible spending accounts for part of the racial gaps in
  education and health spending (Table 9) and in wealth (Table 10).

## Derivation chain of what Paper A uses

- Status `g(c)/μ_G` gives a marginal status return `g'(c)/μ_G` whose
  derivative in `μ_G` is `−g'(c)/μ_G²` (`s_c_mu`, `s_c_mu_neg`).
- Eq. (4) gives `c*_μ < 0 ↔ s_cμ < 0` under the second order condition
  (`eq4_sign`). Reversing the sign of `s_cμ` or of the bracket reverses the
  conclusion (`control_status_gain_rising`, `control_soc_needed`).
- The no-status share is `α/(1 + α)` (`no_status_share`). For the appendix's
  stated solution, the share falls in `μ_G` (`F_strictMonoOn`,
  `share_decreasing_in_mu`).
- The appendix's change of variables omits a factor (`chain_rule_gap`, CH-D2).

## What could not be reconstructed

- Whether the corrected ODE in the share preserves the sign of the appendix's
  conclusion is not checked. A comparison argument suggests it does, since a
  larger `μ_G` steepens `g` at every point, but this is our inference and is
  not formalised.
- The general model leaves `s(c, μ_G)` unspecified, so nothing beyond the sign
  argument of eq. (4) is formalised for it.
- The regressions are not reproduced. Only the printed regression lines of
  the figures were read, from the images.
