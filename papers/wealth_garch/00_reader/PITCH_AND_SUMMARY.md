## ELEVATOR PITCH

Empirical tests of reference-dependent preferences in banking have a
calibration problem: regulation manufactures the very signature the tests
look for. A bank facing a state-dependent exposure cap shows asymmetric
volatility across its reference point even with no asymmetric preference
at all, because the cap tightens exposure exactly as the capital buffer
erodes. Testing asymmetry against a symmetric null therefore cannot
distinguish preference from regulation. We solve the bank's control
problem -- classical control of risk exposure under the cap, singular
control of dividends, impulse control of recapitalisation -- and compute
what the null actually is: a risk-neutral regulated bank already produces
a variance-asymmetry ratio of roughly 1.3 to 1.5. Only asymmetry in
excess of that is evidence about preferences.

The data then deliver a puzzle with the opposite sign. Applied to the
IMF's cross-country capital-ratio panel, the estimator returns a median
of 1.04 -- below not just the loss-aversion prediction but the entire
range a risk-neutral regulated bank can produce. Where structural
decompositions of the equity-volatility leverage effect found the
mechanical component to be nearly nothing, so that preference-based
explanations survived, here the mechanical component is the whole
admissible range and the data fall short of even that. Something
suppresses deficit-state volatility below what the constraint alone
should generate.

Two findings travel beyond this application. The direction in which
measured asymmetry responds to the underlying preference parameter
depends on the regime-classification convention -- it falls under a
moving threshold and rises under a fixed one, a sign reversal we believe
has not previously been documented -- so asymmetry estimates classified
against different references are not comparable across studies. And a reference
constructed one-sidedly from a series' own history cannot register the
below-reference regime in a trending series at all: in our panel this
silently removes two thirds of countries, disproportionately the deepest
banking systems, selecting the sample on the dynamics of the very
variable under study. The theory layer is machine-verified throughout.


## SHORT SUMMARY

A firm chooses risk exposure, dividends and equity issuance to manage a
capital buffer measured against a required rate of return, penalised for
shortfall at a rate set by an asymmetry parameter. Three controls of
different types are coupled -- classical under a state-dependent
regulatory cap, singular at the payout boundary, impulse at the
recapitalisation trigger with fixed and proportional costs -- and the
problem is characterised by a quasi-variational inequality. Two features
make the formulation workable where S-shaped specifications are not. At
unit asymmetry the penalty vanishes identically, so the problem nests a
risk-neutral benchmark exactly rather than in a limit. And because the
penalty is kinked-linear, the negative of the running penalty remains
concave: S-shaped utility is convex below the reference and destroys
concavity globally through the preference itself; here what
non-concavity arises is localised and traceable to a fixed issuance
cost. The optimal policy is a band, and both of its boundaries move
outward as asymmetry rises -- a more asymmetric institution retains a
larger buffer before paying out and recapitalises from a higher level of
capital.

The empirical core of the paper is a corrected null. A discrete-time
counterpart yields a closed-form fourth-power relation between a
reduced-form asymmetry parameter and the ratio of conditional variances
either side of the reference, and a theta-free variance-ratio estimator
of that parameter whose finite-sample recovery is verified by
simulation. The saturation-limit value of the parameter is fixed by cap
geometry alone and carries no information about preferences, so it
cannot serve as a benchmark; the usable null is obtained by applying the
estimator to paths simulated from the solved model. A risk-neutral bank
under the cap already produces roughly 1.31 to 1.48 depending on the
classification convention, and no admissible preference parameter in the
solved range produces a value below 1.23. Because the cap tightens as
capital falls, unequal volatility across the reference is what
regulation looks like, not what loss aversion looks like; the two are
separated only by magnitude, and the model supplies the dividing line.
Structural decompositions of the equity-volatility leverage effect posed
the analogous question for balance-sheet leverage and found the
mechanical component explains almost none of the observed asymmetry;
posed here for the regulatory cap in the capital ratio's own dynamics,
the mechanical component is instead the entire admissible range.

Against that null the data land on the wrong side. On the IMF
cross-country capital-ratio panel the estimator returns a median of
1.04, below every value the model can produce under any convention
examined. The reading this supports is deliberately narrow. The
classification requires each country to spend time on both sides of a
reference built from its own history, and a ratio that trends upward
never registers a shortfall quarter, so two thirds of the panel drops
out -- disproportionately advanced economies with the deepest banking
systems -- leaving a mean-reverting subsample selected on the dynamics
of the variable being measured. Within that subsample the dispersion is
wide: roughly a third of countries sit at or above the lowest model
value, and a third fall below one, showing less volatility in deficit
than in surplus. The finding is therefore a statement about the
mean-reverting subsample, not banking systems in general: there, the
median shows less variance asymmetry than a risk-neutral regulated bank
should exhibit, and something -- risk-weight optimisation, forbearance,
smoothing, or measurement -- suppresses deficit-state volatility below
its mechanical floor. An alternative reference construction that
retains the trending countries is specified in advance, with its costs
and a fixed interpretation rule stated before any result is computed,
as a robustness analysis still to be run.

Two methodological findings apply beyond banking. First, the direction
in which measured variance asymmetry responds to the structural
preference parameter is convention-dependent: it decreases under
moving-threshold classifications, because the threshold itself shifts
with the parameter, and increases under a fixed threshold. To our
knowledge this sign reversal has not been documented, and it means
asymmetry estimates produced under different classification conventions
are not comparable across studies -- a live concern, since fitted
targets, realised means, and fixed regulatory constants all circulate
as references in adjacent literatures. Second, a reference constructed
one-sidedly from a series' own history is structurally blind to the
below-reference regime of a trending series, so applying it to a panel
selects the surviving cross-section on the measured variable's own
dynamics; stated as a general caution, we believe this composed point
is new, and the panel here -- where post-crisis capital ratios trend
upward almost everywhere -- is its demonstration.

The variance ratio supports a test rather than an estimate: theta and
the asymmetry parameter are not jointly identified from routine-sized
excursions, so the estimator is built to need no theta at all, and the
payout and issuance thresholds, monotone in the structural parameter,
remain the candidate route to identification. The theoretical layer is
verified end to end -- proof-assistant and exact symbolic checks, one
canonical verifier per claim -- and every figure quoted above is
regenerated from the shipped scripts against convergence-certified
solves.
