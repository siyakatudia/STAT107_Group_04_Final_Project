#Load datasets to be used here:
dfa_levels <- read.csv("Data/Raw/DFA/dfa-race-levels.csv")
dfa_levels_detail <- read.csv("Data/Raw/DFA/dfa-race-levels-detail.csv")
scf_mean <- read.csv("Data/Raw/SCF/interactive_bulletin_charts_racecl4_mean.csv")
dfa_education_detail <- read.csv("Data/Raw/DFA/dfa-education-levels-detail.csv") 
scf_education_mean <- read.csv("Data/Raw/SCF/interactive_bulletin_charts_edcl_mean.csv")

#' This function will rename the column names given the dataset, index, and toRename 
#' argument. Accesses Dataset -> checks if the nameOfCol argument is valid -> renames column name to argument toRenameVec
#' This function assumes only whole integer numbers are inputted (will fix in the future for chars and doubles)
renameColumn <- function(dataset, nameOfCol, toRenameVec) {
  if (all(nameOfCol %in% names(dataset)) && (length(nameOfCol) == length(toRenameVec))) {
    names(dataset)[match(nameOfCol, names(dataset))] <- toRenameVec
    
    return(dataset)
  } else {
    stop("Input valid column name and identical length to replace.")
  }
}

# functions marked with a star * are referred to as "master sets" which we called repeatedly in each of the following functions.
# basically filtered data called to be filtered even further by row

# !! DFA PREFIXED FUNCTIONS MEANT TO BE USED BY DFA PREFIXED SETS ONLY. WILL NOT FUNCTION WITH SCF DATASETS

# Retrieves quarters from each year and combines them to a single year. To do this
# it takes the average of each numerical category and returns a dataset that has combined quarters

# Access dataset -> seeks category argument -> retrieves quarters and combines by year for that category
# (e.g. 2020:Q1, 2020:Q2, 2020:Q3, 2020:Q4 for net worth is combined into
# 2020 and net worth) -> repeat for all years -> return processed dataset

# This function will be used to further filter by race in the following functions *
dfaRetrieveQuartersPerYearAvg <- function (data, category) {
  if (category %in% names(data)) {
    yrsOnly <- substring(data$Date, 1, 4)
    
    numYrsConv <- as.numeric(yrsOnly)
    logVecMin <- (numYrsConv >= 1990)
    logVecMax <- (numYrsConv <= 2025)
    logVecRange <- (logVecMin & logVecMax)
    numYrsConv <- numYrsConv[logVecRange] 

    filtered_category <- data[[category]][logVecRange]
    filtered_race <- data$Category[logVecRange]
    
    #code starting with "numYrsConv" and above in an effort to limit the range to 1990 and 2025, as
    # 1989 and 2026 have not reported all 4 quarters.
    
    qYearsFilteredByCat <- aggregate(filtered_category, by = list(Date = numYrsConv, Category = filtered_race), FUN = mean)
    #aggregate function used to then find avg of each year followed by their category and race
    
    return (qYearsFilteredByCat)
    
  } else {
    stop("Enter valid column name argument (From your data column names)")
  }
}

dfaEduRename <- function(data, cat) { # *
  data <- dfaRetrieveQuartersPerYearAvg(data, cat)
  data <- renameColumn(data, "Category", "Education Level") 
  data <- renameColumn(data, "Date", "Year") 
  data <- renameColumn(data, "x", cat)
  data[["Education Level"]] <- factor(data[["Education Level"]], levels = c("NoHS", "HS", "SomeCollege", "College"), 
                                      labels = c("No High School Diploma", "High School Diploma", "Some College", "College Degree"))
  return(data)
  
} 


# !! SCF PREFIXED FUNCTIONS MEANT TO BE USED BY SCF PREFIXED SETS ONLY. WILL NOT FUNCTION WITH DFA DATASETS

# Will remove the "non-hispanic" suffix from SCF tables inputted. Only works with scf_mean
# Will assume column that contains race is named "Category". 
# Will plan to fix generalization before final due date
scfPrefixRemovable <- function (data) {
  data[[2]] <- gsub(", non-Hispanic", "", data[[2]]) 
  return(data)
}


# Renames Columns and row categories to fit appropriately.
# Can work with either edu sets found in the SCF Raw tables. For now,
# we only use the Edcl mean chart (Our current saved var scf_education_mean)
# This means that it is assumed that a type of edcl chart is inputted
# Will fix in the future

scfEduRename <- function (data) { # *
  data <- renameColumn(data, "Category", "Education Level")
  data <- renameColumn(data, "year", "Year")
  
  data[["Education Level"]] <- factor(data[["Education Level"]], levels = c("No high school diploma", "High school diploma", "Some college", "College degree"), 
         labels = c("No High School Diploma", "High School Diploma", "Some College", "College Degree"))
  
  return(data)
}


# Function will work similar to DFA functions, where it will retrieve a given category
# while keeping year and race in the table (simply removing every other column but year, race, and 
# given category)
scfYRCatSelection <- function (data, givenColName) { # *
  # ^^ (means scf Year-Race Category Selection)
  if (givenColName %in% names(data)) {
    data <- scfPrefixRemovable(data) # am aware that function is called with each call of this function (calls to filter again)
    raceCol <- data[[2]] #<- column location of "Category"
    yearCol <- data[[1]] # <- column location of "year"
    chosenCat <- data[[givenColName]]
    
    newData <- data.frame(Year = yearCol, Category = raceCol, Value = chosenCat) #can be renamed with renameColumn func
    return(newData)
  } else {
    stop("Enter valid column name argument (From your data column names)")
  }
}




# Functions with the prefix "retrieveRaceVs" are planned 
# to accept multiple race arguments to make comparing data much more accesible between races
# rather than calling the function 4 seperate times for each race category. For now leave as is.

# Uses function above to further filter through Net worth in terms of race, descending, using the dfa_levels
# dataset.
retrieveRaceVsNetworth <- function (race) { 
  if (race %in% dfa_levels[[2]]) { # remember to fix here by adding length check to accept multiple race args
    ntWrthTbl <- dfaRetrieveQuartersPerYearAvg(dfa_levels,"Net.worth")
    ntWrthTbl <- renameColumn(ntWrthTbl,"Date", "Year")
    

    corrRace <- (ntWrthTbl$Category == as.character(race)) # filters through the race levels to select only the input race
                                                            # and match it to the table created for the category
    raceNtWrthTbl <- ntWrthTbl[corrRace, ]
    
    raceNtWrthTbl <- raceNtWrthTbl[(order(raceNtWrthTbl$Year, decreasing = TRUE)), ] # orders year descending
    
    ntWrthTbl <- renameColumn(ntWrthTbl,"x", "Net Worth")
    ntWrthTbl <- renameColumn(ntWrthTbl, "Category", "Race")
# these were commented out by siya because there are inconsistencies in column naming    
    
    return(raceNtWrthTbl)
    
  } else {
    stop("Enter valid 'race' argument")
  }
}

# This function will retrieve a dataframe, given the race parameter, Race and Financial Assets 
# based on the dfa_levels_details dataset. This will also include being ordered by date, descending.
retrieveRaceVsFinancialAssets <- function (race) {
  if (race %in% dfa_levels[[2]]) { # remember to fix here by adding length check to accept multiple race args
    finAssetsTbl <- dfaRetrieveQuartersPerYearAvg(dfa_levels_detail,"Financial.assets")
    finAssetsTbl <- renameColumn(finAssetsTbl,"Date", "Year")
    
    corrRace <- (finAssetsTbl$Category == as.character(race))
    raceFinAssetsTbl <- finAssetsTbl[corrRace, ]
    
    raceFinAssetsTbl <- raceFinAssetsTbl[(order(raceFinAssetsTbl$Year, decreasing = TRUE)), ]
    
    finAssetsTbl <- renameColumn(finAssetsTbl,"x", "Financial Assets")
    finAssetsTbl <- renameColumn(finAssetsTbl, "Category", "Race")
    
    return(raceFinAssetsTbl)
    
  } else {
    stop("Enter valid 'race' argument")
  }
}


# This function will retrieve a dataframe, given the race parameter, Race and Debt 
# based on the scf_mean dataset. This will also include being ordered by date, descending.
retrieveRaceVsDebt <- function (race) {
  if (race %in% scfPrefixRemovable(scf_mean)[["Category"]]) {
    debtTbl <- scfYRCatSelection(scf_mean, "Debt") #similar to the DFA functions but without the extra steps
  
    
    corrRace <- (debtTbl$Category == as.character(race)) #will filters through levels and select input race and pair it 
                                                          # with year and selected category (debt in this case)
                        # ^^ also aware that this naming convention is fragile. will work on a fix later
    
    raceDebtTbl <- debtTbl[corrRace, ] 
    raceDebtTbl <- raceDebtTbl[(order(raceDebtTbl$Year, decreasing = TRUE)), ]
    
    raceDebtTbl <- renameColumn(raceDebtTbl, "Category", "Race")
    raceDebtTbl <- renameColumn(raceDebtTbl, "Value", "x")      # chnaged from debt to x 
    
    return (raceDebtTbl)
    
  } else {
    stop("Enter valid 'race' argument")
  }
                    
}

# This function will retrieve a dataframe, given the race parameter, Race and Retirement Account Worth
# based on the scf_mean dataset. This will also include being ordered by date, descending.
retrieveRaceVsRetrMntAccWorth <- function (race) {
  if (race %in% scfPrefixRemovable(scf_mean)[["Category"]]) {
    rtrmAccTbl <- scfYRCatSelection(scf_mean, "Retirement_Accounts") 
    
    corrRace <- (rtrmAccTbl$Category == as.character(race))
    
    raceRtrmAccTbl <- rtrmAccTbl[corrRace, ] 
    raceRtrmAccTbl <- raceRtrmAccTbl[(order(raceRtrmAccTbl$Year, decreasing = TRUE)), ]
    
    raceRtrmAccTbl <- renameColumn(raceRtrmAccTbl, "Category", "Race")
    raceRtrmAccTbl <- renameColumn(raceRtrmAccTbl, "Value", "x")
    
    return (raceRtrmAccTbl)
    
  } else {
    stop("Enter valid 'race' argument")
  }
  
}

# This function will retrieve a dataframe, given the race parameter, Race and Financial Assets 
# based on the dfa_levels_detail dataset. This will also include being ordered by date, descending.
retrieveRaceVsLiabilities <- function (race) {
  if (race %in% dfa_levels[[2]]) { # also remember to fix so you can enter multiple race args
    liabTbl <- dfaRetrieveQuartersPerYearAvg(dfa_levels_detail,"Liabilities")
    liabTbl <- renameColumn(liabTbl,"Date", "Year")
    
    corrRace <- (liabTbl$Category == as.character(race))
    raceLiabTbl <- liabTbl[corrRace, ]
    
    raceLiabTbl <- raceLiabTbl[(order(raceLiabTbl$Year, decreasing = TRUE)), ]
    
    raceLiabTbl <- renameColumn(raceLiabTbl, "Category", "Race")
    raceLiabTbl <- renameColumn(raceLiabTbl, "x", "Liabilities")
    
    return(raceLiabTbl)
    
  } else {
    stop("Enter valid 'race' argument")
  }
  
}




# This function is also meant to take in only data sets where there is an education level
# present. Otherwise it is not to be called for other data sets.
# !! RENAME FUNCTION scfEduRename() AND lookup MUST BE CALLED AND SAVED BEFORE RUNNING THIS FUNCTION

lookup <- data.frame(
  args = c("Net Worth", "Liabilities", "Debt", "Retirement Account Value", "Financial Assets"),
  source  = c("DFA", "DFA", "SCF", "SCF", "DFA"),
  column  = c("Net.worth", "Liabilities", "Debt", "Retirement_Accounts", "Financial.assets"))
scfEduRenamed <- scfEduRename(scf_education_mean)

retrieveEduVsOf5Args <- function (eduLvl, arg) { 
  if (!(eduLvl %in% scfEduRenamed[["Education Level"]])) { # <- Only needs to check one table since each share same labels
    stop("Input Valid of 4 Education Levels: No High School Diploma, High School Diploma, Some College, College Degree for first argument")
    
  }
  
  if (!(arg %in% lookup$args)) {
    stop("Input Valid of 5 Arguments: Net Worth, Liabilities, Debt, Retirement Account Value, or Financial Assets for second argument.")
  }
  
  lookupVar <- match(arg, lookup$args)
  sourceVar <- lookup$source[lookupVar]
  rawNameVar <- lookup$column[lookupVar]

  
  if (sourceVar == "DFA") {
    argTable <- dfaEduRename(dfa_education_detail, rawNameVar)

    corrVal <- (argTable$`Education Level` == as.character(eduLvl))
    
    finalArgTable <- argTable[corrVal, ]
    
    finalArgTable <- renameColumn(finalArgTable, rawNameVar, arg)

    
    finalArgTable <- finalArgTable[(order(finalArgTable$Year, decreasing= TRUE)), ]
    
    
    return(finalArgTable)
    
  } else if (sourceVar == "SCF") {
    argTable <- scfEduRenamed
    
    
    yearCol <- argTable[["Year"]] 
    eduCol <- argTable[["Education Level"]]
    argCol <- argTable[[rawNameVar]]
    
    corrVal <- (argTable$`Education Level` == as.character(eduLvl))
    
    
    
    finalArgTable <- data.frame(Year = yearCol, Edu = eduCol, argu = argCol)
    
    finalArgTable <- finalArgTable[corrVal, ]
    
    finalArgTable <- renameColumn(finalArgTable, "Edu", "Education Level")
    finalArgTable <- renameColumn(finalArgTable, "argu", arg)
    
    finalArgTable <- finalArgTable[(order(finalArgTable$Year, decreasing = TRUE)), ]
    
    return(finalArgTable)
    
  } else {
    stop("Ran into error")
  }

}
# possibly add droplevels() to the functiona at a later time


# This function will retrieve a dataframe that combines all 5 arguments/measurements for a racial wealth gap
# into one function. This would be useful in the case that we would only want to look at one race vs
# all 5 wealth measurement categories at once rather than calling each function 4 times, once for each
# race identified. This will also include being ordered by date, descending.
retrieveRacevsFiveArguments <- function (race) {
  #left incomplete on purpose as merging inequal rows may cause some inaccuracies with the data
  # we will think of a solution for this before the final deadline
  # if called, it will return 0
  return(0)
}






# This function will order by which category in the processed datasets, descending, For example,
# if the function calls to be ordered by net worth, the function will return a dataset
# that shall be ordered by net worth, descending. This will overwrite the default ordered by date, descending.
orderBy <- function(data, category) {
  if (category %in% names(data)) {
    newOrder <- order(data[[category]], decreasing = TRUE)
    data <- data[newOrder, ]
    return(data)
  } else {
    stop("Enter a valid category for your data to order by")
  }
}





