# Lean formalisation, asymmetric capital buffer control

Lean v4.32.0-rc1 with Mathlib v4.32.0-rc1, commit pinned in
`lake-manifest.json`. Every `.lean` file here is a build root in
`lakefile.toml`, with one namespace per file.

## Build

    cd lean/mathlib
    lake exe cache get
    lake build

`checks/verify_mathlib.py` builds the project, searches every source for
`sorry`, and runs `#print axioms` on every declared theorem. `VERIFICATION.md`
records the result.

## Files

| File | Namespace | PROOFS_v2 results |
|---|---|---|
| `RateBased.lean` | `RateBased` | `prop:persistence`, `thm:lambda4` (saturation limit), `prop:sojourn` |
| `QVI_Part1.lean` | `QVI_Part1` | `prop:nesting`, `prop:concave-payoff` |
| `SmoothFit.lean` | `SmoothFit` | `prop:smf` |
| `Envelope.lean` | `Envelope` | algebraic cores of `lem:envelope`, `lem:envelopeK`, `rem:smfn` |
| `ImpulseCount.lean` | `ImpulseCount` | summability core of `lem:fin` |

Each file's header states which analytic inputs enter as hypotheses.
