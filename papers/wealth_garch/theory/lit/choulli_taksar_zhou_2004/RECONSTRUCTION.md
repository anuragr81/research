# Reconstruction: Choulli, Taksar and Zhou (2004)

## Source read

- Tahir Choulli, Michael Taksar and Xun Yu Zhou, "Interplay between dividend
  rate and business constraints for a financial corporation", *The Annals of
  Applied Probability* 14(4):1810-1837, 2004, DOI 10.1214/105051604000000909.
  Read in the arXiv reprint math/0503541v1, 29 pages, in full.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file `math0503541.pdf`), uploaded
  2026-10-30. Cached at
  `~/.cache/wealth_garch/choulli_taksar_zhou_math0503541.pdf`, sha256
  `d5c8ecb5ac8238245de811cb8fb2bf4acdeee7dbc59cd7780066a417d2ec4cbf`.
  Printed page = PDF page.

## Primitives

- Reserve $dR=(a\mu-\delta-c)\,dt+a\sigma\,dW$, exposure $\alpha\le a\le\beta$
  constant bounds, dividend rate $0\le c\le M$ (2.1). Bankruptcy at $R=0$.
- Objective $E\int_0^\tau e^{-\gamma t}c_t\,dt$ (2.3). No issuance, no
  penalty.

## Derivation chain

1. HJB (2.7). Verification for concave $C^2$ solutions with finite $V'(0)$
   (Theorem 2.2).
2. Above the dividend threshold $x_1$, (3.3). The feedback
   $a(x)=-\mu V'/(\sigma^2V'')$ (3.5) satisfies the ODE (3.13) where it is
   interior, which forces it to be the constant $\tilde c$ (3.15) there.
   Proposition 3.3: once $a\ge\alpha$ (or $\ge\beta$) at $x_1$ it stays so.
3. Theorem 3.4: three closed forms above $x_1$ by the position of $\tilde c$
   against $\alpha,\beta$. The middle case has exponent
   $-\mu/(\sigma^2\tilde c)$ (`rtm_at_ctilde`).
4. Below $x_1$, cases by $2\delta/\mu$ against $\alpha$ and $\beta$ and by
   $M$ against $M_\alpha,M_\beta,M_0$ (Sections 3.1 to 3.3).
5. Theorem 4.1 and Tables 1 and 2: the policy. The exposure is $\alpha$ on
   $[0,x_\alpha]$, increases on $[x_\alpha,x_\beta]$ and is $\beta$ above
   $x_\beta$, with either level possibly $0$ or $\infty$.

## Formalised

- The constants of the middle case and the condition of the upper case
  (`ChoulliTaksarZhou.lean`). The case analysis and verification are not.
