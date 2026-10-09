namespace MorganTumlinsonVardy

inductive Dir where
  | up
  | down
  | flat
  deriving DecidableEq

structure Prediction where
  participationInAccuracy : Dir
  effortMargin : Bool

def mtv : Prediction := ⟨.down, true⟩

def entryModel : Prediction := ⟨.up, false⟩

def opposite : Dir → Dir → Bool
  | .up, .down => true
  | .down, .up => true
  | _, _ => false

theorem opposite_signs :
    opposite mtv.participationInAccuracy entryModel.participationInAccuracy = true := rfl

theorem differ_in_effort_margin : mtv.effortMargin ≠ entryModel.effortMargin := by decide

def noEffortVariant : Prediction := ⟨.up, false⟩

theorem control_same_margin_same_sign :
    opposite noEffortVariant.participationInAccuracy entryModel.participationInAccuracy = false := rfl

end MorganTumlinsonVardy
