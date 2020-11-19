
# all drinks should be a homogeneous format
prepare_drinks <- function(drinks) {
  #assert(check_list(drinks), check_numeric(drinks), combine = "or")    # not necessary, asserted again after unlist()
  
  # unlist lists or vectors to get the same format
  drinks <- unlist(drinks)
  assert_numeric(drinks, lower = 0, any.missing = FALSE)
  
  possible_drinks <- c("hoibe", "massn", "schnaps", "wein")
  assertNames(names(drinks), subset.of = possible_drinks)
  
  # extend drinks with all possible drinks
  for (d in possible_drinks) {
    if (is.na(drinks[d])) {
      drinks[[d]] <- 0
    }
  }
  
  # combine if the same drink is mentioned multiple times
  drinks <- tapply(drinks, names(drinks), sum)
  drinks
}


check_drinking_age <- function(age, drinks) {
  if (age < 16 & any(drinks > 0)) {
    warning("Don't drink when you're younger than 16, that's illegal!")
  }
  if (age < 18 & drinks[["schnaps"]] > 0) {
    warning("Hard liquor under 18? That's illegal!")
  }
}


# calculate the alcohol intake for drinks
get_alcohol_consumed <- function(drinks) {
  drinks <- prepare_drinks(drinks = drinks)
  
  alcohol_consumed <- 0.8 * (
    drinks[["massn"]] * 1000 * 0.06 +
      drinks[["hoibe"]] * 500 * 0.06 +
      drinks[["schnaps"]] * 40 * 0.4 +
      drinks[["wein"]] * 200 * 0.11)
  alcohol_consumed
}



get_gkw_sex <- function(age, sex = c("male", "female"), height, weight) {
  sex <- tolower(sex)
  sex <- match.arg(sex)
  gkw_male <- 2.447 - 0.09516 * age + 0.1074 * height + 0.3362 * weight
  gkw_female <- 0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight
  gkw_sex <- if (sex == "male") {
    gkw_male
  } else {
    gkw_female
  }
  gkw_sex
}



get_blood_alcohol_concentration <- function(age, sex, height, weight, drinks) {
  alcohol_consumed <- get_alcohol_consumed(drinks = drinks)
  gkw_sex <- get_gkw_sex(age = age, sex = sex, height = height, weight = weight)
  blood_alcohol_concentration <- 0.8 * alcohol_consumed / (1.055 * gkw_sex)
  blood_alcohol_concentration
}



library(checkmate)
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight, drinking_time, drinks) {
  assert_numeric(age, lower = 0, upper = 120, any.missing = FALSE)
  assert_character(sex)
  assert_numeric(height, lower = 0, upper = 220, any.missing = FALSE)
  assert_numeric(weight, lower = 0, upper = 350, any.missing = FALSE)
  assert_posixct(drinking_time, any.missing = FALSE, len = 2, sorted = TRUE)
  #assert(check_list(drinks), check_numeric(drinks), combine = "or")
  #assert_numeric(drinks, lower = 0, any.missing = FALSE)
  #assertNames(names(drinks), subset.of = possible_drinks)
  drinks <- prepare_drinks(drinks = drinks)
  check_drinking_age(age = age, drinks = drinks)
  
  blood_alcohol_concentration <- get_blood_alcohol_concentration(age = age, sex = sex, height = height, weight = weight, drinks = drinks)
  concentration_final <- blood_alcohol_concentration - 
    max(0, ((as.numeric(difftime(drinking_time[2], 
                                 drinking_time[1], units = "hours")) - 1) * 0.15))
  concentration_final <- max(0, concentration_final)
  concentration_final
}


