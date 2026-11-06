# This function has the purpose of changing the levels in scf_mean dataset, categories section, into simple
# race levels rather than compounded "non-hispanic" postfixes

renameColumn <- function (column) {
  
}

# This function will retrieve all quarters from a given year
retrieveQuartersPerYearAvg <- function ()


# This function will retrieve a dataframe, given the race parameter, Race and Total Net Worth 
# based on the dfa_levels dataset. This will also include being ordered by date, descending.
retrieveRaceVsNetworth <- function (race) { 
  
}

# This function will retrieve a dataframe, given the race parameter, Race and Financial Assets 
# based on the dfa_levels dataset. This will also include being ordered by date, descending.
retrieveRaceVsFinancialAssets <- function (race) {
  
}

# This function will retrieve a dataframe, given the race parameter, Race and Financial Assets 
# based on the dfa_levels dataset. This will also include being ordered by date, descending.
retrieveRaceVsDebt <- function (race) {
  
}

# This function will retrieve a dataframe, given the race parameter, Race and Retirement Account Worth
# based on the dfa_levels dataset. This will also include being ordered by date, descending.
retrieveRaceVsRetrMntAccWorth <- function (race) {
  
}

# This function will retrieve a dataframe, given the race parameter, Race and Financial Assets 
# based on the dfa_levels_detail dataset. This will also include being ordered by date, descending.
retrieveRaceVsLiabilities <- function (race) {
  
}


# This function will retrieve a dataframe that combines all 5 arguments/measurements for a racial wealth gap
# into one function. This would be useful in the case that we would only want to look at one race vs
# all 5 wealth measurement categories at once rather than calling each function 4 times, once for each
# race identified. This will also include being ordered by date, descending.
retrieveRacevsFiveArguments <- function (race) {
  
}


# This function will order by which category in the processed datasets, descending, For example,
# if the function calls to be ordered by net worth, the function will return a dataset
# that shall be ordered by net worth, descending. This will overwrite the ordered by date, descending.
orderBy <- function(category) {
  
}





