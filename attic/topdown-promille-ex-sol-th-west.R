#' Function to check age and drinks
#' @param age age of that person
#' @param drinks list or named vector of drinks

#' @return possible warning message due to illegal activity
checking_age <- function(age, drinks) {
  drinks <- unlist(drinks)
  age_class <- ifelse("schnaps" %in% names(drinks), "adult", "minor")
  if (age_class == "adult" & age < 18) {
    warning(" Consumption of hard liquor for minors is illegal")
  }
  
  if (age_class == "minor" & age < 16) {
    warning("Consumption of alcohol for minors younger than 16 is illegal")
  }
}

#' Function that computes the mass of alcohol in grams
#' @param drinks a list or named vector containing  alcoholic beverages
#' and their count
#'
#' @return the total amount of alcohol in grams
calculate_mass <- function(drinks) {
  mass_sum <- 0
  drinks <- unlist(drinks)
  checkmate::assert_subset(names(drinks),
                           choices = c("massn", "hoibe", "wein", "schnaps"),
                           empty.ok = FALSE)
  drinks <- tapply(drinks, names(drinks), sum)
  alcohol_mass_list <- list(
    "hoibe" = 500 * 0.06 * 0.8,
    "massn" = 1000 * 0.06 * 0.8,
    "wein" = 200 * 0.11 * 0.8,
    "schnaps" = 40 * 0.4 * 0.8
  )
  
  for (i in names(drinks)) {
    checkmate::assert_numeric(as.vector(drinks[i]), lower = 0)
    mass_sum <- mass_sum + alcohol_mass_list[[i]] * as.vector(drinks[i])
  }
  
  mass_sum
}



#' Function to transform different inputs of variable sex
#' @param sex a character declaring sex of a person

#' @return value female or male for sex
getting_gender_right <- function(sex = c("male", "m", "M",
                                         "female", "f", "F")) {
  sex <- tolower(sex)
  sex <- match.arg(sex)
  switch(sex,
         male = "male",
         m = "male",
         female = "female",
         f = "female"
  )
}

#' Function that calculates total body water
#' @param age the age of a specific person
#' @param sex the gender of that person
#' @param height height in cm
#' @param weight weight in kg

#' @returntotal body water numeric
calculate_gkw <- function(age, sex = c("male", "female"), height, weight) {
  sex <- getting_gender_right(sex = sex)
  sex <- match.arg(sex)
  switch(sex,
         female = 0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight,
         male = 2.447 - 0.09516 * age + 0.1074 * height + 0.3362 * weight
  )
}


#' Function that calculates blood alcohol level
#' @param  gkw total body water
#' @param  alc_mass mass of alcohol in grams

#' @return  blood alcohol level per thousand
calculate_blood_alcohol <- function(gkw, alc_mass) {
  blood_level <- (0.8 * alc_mass) / (1.055 * gkw)
  blood_level
}

#' Function that calculates blood alcohol level decline during drinking
#' starting from second hour after start
#' @param  blood_level blood level of alcohol per thousand
#' @param  drinking_time vector of POSIXct Values.

#' @return  final level of blood alcohol at the end of drinking time
calculate_blood_alcohol_final <- function(blood_level, drinking_time) {
  drinking_time <- (as.numeric(drinking_time[[2]])
                    - as.numeric(drinking_time[[1]])) / 3600
  checkmate::assert_numeric(drinking_time, lower = 0)
  if (drinking_time >= 1) {
    blood_level <- blood_level - ((drinking_time - 1) * 0.15)
  }
  max(0, blood_level)
}


#' Function that calculates the blood level of alcohol after some time of
#'  drinking
#' @param age the age of the person
#' @param sex the gender of the person
#' @param height height in cm
#' @param weight weight in kg
#' @param drinking_time a vector of class POSIXct and length 2 with start and
#'                      end time of drinking
#' @param drinks a list or named vector with different drinks and their count

#' @return the final blood level of alcohol per thousand
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  checkmate::assert_numeric(age, lower = 0, upper = 120,
                            any.missing = FALSE)
  checkmate::assert_numeric(height, lower = 0, upper = 220,
                            any.missing = FALSE)
  checkmate::assert_numeric(weight, lower = 0, upper = 350,
                            any.missing = FALSE)
  checkmate::assert_posixct(drinking_time)
  checkmate::assert_character(sex)
  
  
  
  checking_age(age = age, drinks = drinks)
  mass <- calculate_mass(drinks = drinks)
  body_water <- calculate_gkw(
    age = age, sex = sex, height = height,
    weight = weight
  )
  blood_level <- calculate_blood_alcohol(gkw = body_water, alc_mass = mass)
  blood_level_final <- calculate_blood_alcohol_final(
    blood_level = blood_level,
    drinking_time = drinking_time
  )
  
  blood_level_final
}
