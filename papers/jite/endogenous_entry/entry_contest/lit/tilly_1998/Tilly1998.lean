namespace Tilly1998

structure Resource where
  valuable : Bool
  renewable : Bool
  monopolizable : Bool
  supportsNetwork : Bool
  enhancedByNetwork : Bool

def conditions (r : Resource) : Bool :=
  r.valuable && r.renewable && r.monopolizable && r.supportsNetwork && r.enhancedByNetwork

structure World where
  hoards : Resource → Bool
  bounded : Bool
  sufficiency : ∀ r, conditions r = true → hoards r = true

def categorical (w : World) (r : Resource) : Bool := w.hoards r && w.bounded

theorem hoards_of_conditions (w : World) (r : Resource) (h : conditions r = true) :
    w.hoards r = true :=
  w.sufficiency r h

theorem categorical_of_bounded (w : World) (r : Resource) (h : conditions r = true)
    (hb : w.bounded = true) : categorical w r = true := by
  simp [categorical, w.sufficiency r h, hb]

def notRenewable : Resource := ⟨true, false, true, true, true⟩

def permissive : World where
  hoards := fun _ => true
  bounded := true
  sufficiency := fun _ _ => rfl

theorem control_not_necessary :
    permissive.hoards notRenewable = true ∧ conditions notRenewable = false := ⟨rfl, rfl⟩

def unbounded : World where
  hoards := fun _ => true
  bounded := false
  sufficiency := fun _ _ => rfl

theorem control_categorical_needs_bound :
    categorical unbounded notRenewable = false := rfl

end Tilly1998
