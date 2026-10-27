
# function to make data set for bayesian predictions/figures
# makes data based on a data frame (x) and a treatment/category (t)

library(data.table)

make_dataset <- function(x, t){
  dat <- as.data.table(expand.grid(
    density_std = seq(min(x$density_std), max(x$density_std), length.out = 100), #full range of density
    treatment_ind = levels(t),  #all treatments
    cones_std = mean(x$cones_std, na.rm = TRUE),  #full range of tree cone count
    mother_age_std = mean(x$mother_age2_std, na.rm = TRUE),  #avg mother age
    mother_age2_std = mean(x$mother_age2_std, na.rm = TRUE),  #avg mother age
    bjulian_std = mean(x$bjulian_std),  #avg birth date
    mast = "no",  # predicting for a non-mast year
    sexratio = mean(x$sexratio),  #avg sex ratio
    gridyear = NA,  #Using NA because we want a population-level prediction
    mother_byear = NA  # same as grid year
  ))
  return(dat)
}
