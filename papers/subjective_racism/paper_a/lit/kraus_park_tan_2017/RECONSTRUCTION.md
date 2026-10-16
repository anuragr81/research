# Kraus, Park and Tan (2017), reconstruction

Michael W. Kraus, Jun Won Park and Jacinth J. X. Tan, "Signs of Social Class:
The Experience of Economic Inequality in Everyday Life", *Perspectives on
Psychological Science* 12, 2017, pp. 422–435 (volume and pages as given in
reference 10 of Kraus et al. 2019).

## Source read

- The accepted manuscript, 45 PDF pages, marked "accepted for publication at
  Perspectives on Psychological Science" and "may not reflect the version
  that is eventually published" (p. 1). Cached at
  `~/.cache/paper_a/sources/kraus_park_tan_2017.pdf`, sha256
  `49c866cc…b853`, with its text layer `kraus_park_tan_2017.txt`, sha256
  `4568bdd4…2148`. The published version is not read (KPT-D1).
- Every page read on 16 October 2026 from the text layer. Pages 43 and 44
  (Figure 1 and Figure 2) were also read as images at 110 dpi, because the
  text layer has the axis labels of Figure 2 and not its bars. Page 45 is
  blank. Evidence tag [F]. Pages are PDF pages, which equal the manuscript's
  own page numbers.

## The argument, from its own primitives

A theoretical review with new data. Social class is "one's position in the
economic hierarchy in society that arises from a combination of annual
income, educational attainment, and occupation prestige" (p. 4). Class
signals are "behaviors that provide information about a person's income,
educational attainment, or occupation status" (p. 3). Three predictions
(Figure 1, p. 43).

1. **Signals are frequent, rapid and accurate** (pp. 5–13).
   - Two prior studies by the first author. Observers rate class on a
     10-rung ladder after 60 s of a get-acquainted interaction, r(98) = .28,
     or after 20 Facebook photographs, r(111) = .27, both robust to the
     target's race and gender (KPT-Q2).
   - A re-analysis by quartiles and halves of the sample. An estimate is
     correct when the observer's quartile (half) matches the target's,
     against chance of 25% (50%) (KPT-Q3). Figure 2 (p. 44, read from the
     image, values approximate) shows about 35% (behaviour), 34.5% (Facebook),
     36.5% (speech) and 35.5% (all) for quartiles, and about 60%, 60%, 62%
     and 61% for halves.
   - A new study. 213 IDEA speakers who reported education and occupation,
     seven isolated words, 568 Qualtrics observers. Perceived class
     correlates with the speaker composite, r(211) = .22, and β = .14 with
     speaker race, gender, pitch and zip-code characteristics as controls
     (KPT-Q4). The association holds in three of four Census regions
     (KPT-Q5). Quartile and half accuracy beat chance (KPT-Q6).
2. **Signals augment group boundaries** (pp. 13–20), by sorting
   ("similarity enhances liking", KPT-Q7), stereotyping and class conflict.
   The evidence for sorting is cited work, including Bergeron and Zanna
   (1973), in which the lower-class writer was an indigenous Peruvian
   (KPT-Q9), and Côté et al., in which affiliation follows the participant's
   own class (KPT-Q8).
3. **Signals perpetuate inequality** (pp. 20–24), through dehumanisation,
   strategic sharing (Nishi et al. 2015, KPT-Q11) and justifying ideologies.

## Derivation chain of what Paper A1 would use

- The chance benchmark. With categories that are sample quantiles, each
  holds the share `1/n`, and every stimulus-blind guess is correct with
  probability `1/n` whatever its own distribution (`blindHit_uniform`,
  `chance_quartile`, `chance_half`). With unequal categories, such as the
  four population classes the manuscript cites from Hout with most people in
  the middle two, a fixed guess beats `1/4` (`control_nonuniform_categories`).
  The quantile construction of p. 8 is what makes 25% and 50% the right
  benchmarks.
- The sorting argument, in our formal reading. An observer of class `cᵢ`
  affiliates when `(ĉ − cᵢ)² ≤ Δ`. An accurate signal blocks affiliation
  across a class distance above `Δ` (`accurate_signal_blocks`). A signal that
  reads everyone alike admits a partner an accurate signal would block
  (`invisible_signal_admits`). Utility `−(ĉ − cᵢ)²` makes the ranking of
  partners depend on the observer's own class (`own_class_ranking`), the
  pattern cited from Côté et al. A vertical utility does not
  (`control_vertical_common_ranking`). The manuscript states the argument in
  words only. The squared-distance form is ours and is A1's own utility.

## What could not be reconstructed

- The bars of Figure 2 are read from the image, not from numbers in the text.
  The text gives the χ² statistics only.
- The cited studies (Côté et al., Bergeron and Zanna, Nishi et al., Rivera)
  are known only through this manuscript's description [C].
- The supplementary materials the manuscript points to for the seven-word
  study are not cached.
