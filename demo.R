# Task and inputs
# Nadja Pfister

testfun <- function(something){
  print(something)
}

testfun("this funcion does slighly more, but still not much")

get_age <- function(my_birthday, my_units = "days"){
  today <- Sys.Date()
  difftime(today,my_birthday)
  # retrun(a)
  
}

get_age("1996-03-14")