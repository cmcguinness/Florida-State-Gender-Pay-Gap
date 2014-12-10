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
computeGender <- function() {
  
  # print("Estimating genders for employees")
  getNamesSimple()
  
  genders <- sapply(salaries$First.Name, getGender)
  salaries$gender <<- genders

}


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