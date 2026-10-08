namespace Wegener1992

inductive Source where
  | order
  | conflict
  deriving DecidableEq

inductive Orientation where
  | normative
  | rational
  deriving DecidableEq

inductive Foundation where
  | charisma
  | achievement
  | honor
  | esteem
  deriving DecidableEq

inductive Theorist where
  | shils
  | eisenstadt
  | davisMoore
  | parsons
  | weber
  | kluth
  | homans
  | blau
  deriving DecidableEq

def foundation : Source → Orientation → Foundation
  | .order, .normative => .charisma
  | .order, .rational => .achievement
  | .conflict, .normative => .honor
  | .conflict, .rational => .esteem

def cell : Theorist → Source × Orientation
  | .shils => (.order, .normative)
  | .eisenstadt => (.order, .normative)
  | .davisMoore => (.order, .rational)
  | .parsons => (.order, .rational)
  | .weber => (.conflict, .normative)
  | .kluth => (.conflict, .normative)
  | .homans => (.conflict, .rational)
  | .blau => (.conflict, .rational)

def isClosureRow : Source → Bool
  | .order => false
  | .conflict => true

def prestigeOf (t : Theorist) : Foundation := foundation (cell t).1 (cell t).2

theorem foundation_injective (s s' : Source) (o o' : Orientation)
    (h : foundation s o = foundation s' o') : s = s' ∧ o = o' := by
  cases s <;> cases s' <;> cases o <;> cases o' <;> simp_all [foundation]

theorem weber_honor : prestigeOf .weber = .honor := rfl

theorem weber_conflict_row : isClosureRow (cell .weber).1 = true := rfl

theorem weber_with_kluth : cell .weber = cell .kluth := rfl

theorem parsons_achievement : prestigeOf .parsons = .achievement := rfl

theorem control_weber_not_hierarchy : isClosureRow (cell .weber).1 ≠ false := by decide

theorem control_weber_not_achievement : prestigeOf .weber ≠ .achievement := by decide

end Wegener1992
