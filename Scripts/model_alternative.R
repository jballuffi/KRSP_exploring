food_control <- subset(food, treatment_ind == "control")
food_supp    <- subset(food, treatment_ind == "supplemented")

mod_control <- brm(
  f_GR + f_LS + set_rescor(TRUE),
  data = food_control
)

mod_supp <- brm(
  f_GR + f_LS + set_rescor(TRUE),
  data = food_supp
)