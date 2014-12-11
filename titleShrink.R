# #############################################################################
# titleShrink
#
# Find all the titles with "levels" in them or that have - GROUP appended
# to them and remove that from the title.
#
# The intent is to flatting the levels within various job types, so a
# HACKER I, HACKER II, and HACKER III - SES just get turned into HACKER
#
# This supports the logic that tries to see if there's a bias where women
# are kept at lower grades than men
# #############################################################################
titleShrink <- function (salaries) {
  salaries$Class.Title <- gsub("- ...(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub("- ....(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub("-...(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub("-....(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub(" III(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub(" II(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub(" I(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub(" IV(\\s*)$","", salaries$Class.Title)
  salaries$Class.Title <- gsub(" V(\\s*)$","", salaries$Class.Title)
  
  return(salaries)
}