import ClassPosterior

namespace Stranger

/-- The predecessor's worked example of the stranger and the accent. Group
weights `(1/2, 1/2)` mix standings `7/10` and `3/10` to `1/2`, and weights
`(1/5, 4/5)` mix them to `19/50`, the mixture of M2 and M3. -/
theorem stranger_example :
    ClassPosterior.mixture (fun g (_ : Fin 1) => (![7 / 10, 3 / 10] : Fin 2 → ℝ) g)
        ![1 / 2, 1 / 2] 0 = 1 / 2 ∧
      ClassPosterior.mixture (fun g (_ : Fin 1) => (![7 / 10, 3 / 10] : Fin 2 → ℝ) g)
        ![1 / 5, 4 / 5] 0 = 19 / 50 := by
  constructor <;> norm_num [ClassPosterior.mixture, Fin.sum_univ_two]

end Stranger
