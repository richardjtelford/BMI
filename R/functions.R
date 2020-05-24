

# Calculate BMI
BMI <- function(mass, height) { 
  mass / height ^ 2
}


#Simulate BMI from mass and height. Assumes Gaussian distribution
bmi_sim <- function(mass_mean, mass_sd, height_mean, height_sd, n, ...){
  stopifnot(height_mean < 2, height_sd < 0.5) # check height in m
  masses <- rnorm(n, mass_mean, mass_sd)
  heights <- rnorm(n, height_mean, height_sd)
  bmi <- BMI(masses, heights)
  tibble(mean  = mean(bmi), sd = sd(bmi))
}
