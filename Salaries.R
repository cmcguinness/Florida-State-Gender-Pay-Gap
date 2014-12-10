# #############################################################################
# loadBaseSalaries (local)
#
# Get the raw salary data from the state of Florida's website and parse it...
# #############################################################################
loadBaseSalaries <- function() {
  if (! file.exists("Florida/dmssalaries.csv")) {
    if (! file.exists("Florida")) {
      dir.create("Florida")
    }
    print("Downloading data from state of Florida")
    download.file(url="http://dmssalaries.herokuapp.com/downloads/dmssalaries.csv", destfile="Florida/dmssalaries.csv")
  }
  salaries <<- read.csv("Florida/dmssalaries.csv")
  nSalary <- as.numeric(gsub("[ $,]","",as.character(salaries$Salary)))
  salaries$nSalary <<- nSalary
  salaries$Class.Title <<- as.character(salaries$Class.Title) # Not as a factor please...
}

source("getNamesSimple.R")
# #############################################################################
# computeGender (local)
#
# Add in a column to the salary data of the guestimated gender of each 
# employee...
# #############################################################################
computeGender <- function() {
  
  # print("Estimating genders for employees")
  getNamesSimple()
  
  genders <- sapply(salaries$First.Name, getGender)
  salaries$gender <<- genders

}

# #############################################################################
# getSalaries
#
# Get the salary data into a data frame.  Caches a copy of it in an .RData
# file to speed up re-runs
# #############################################################################
getSalaries <- function() {
  if (file.exists("salaries.RData")) {
    load("salaries.RData", .GlobalEnv)
    return(invisible("cached"))
  }
  loadBaseSalaries()
  computeGender()
  save(salaries,file="salaries.RData")
  return(invisible("generated"))
}