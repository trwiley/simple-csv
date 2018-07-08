#' @name Simple Read CSV
#'
#' @description This function is meant to create a CSV/TSV reading function that makes more sense than the default read.csv and read.csv2 functions.
#' @param separator This indicates which character is being used to demlimit a CSV. Accepted values are comma, semicolon, and tab.
#' @param decimal This indicates which character is being used to indicate decimal places in a number. Acceptable values are a comma or period/full stop.
#' @param theFile The name of the CSV or TSV.
#' @keywords tsv csv simple read
#' @export
#' @examples
#' simple_read_csv(separator = '\t', decimal = ',', theFile = 'bob.csv')

simple_read_csv <- function(separator, decimal, theFile) {

  if((separator == ',') && (decimal == '.'))
    read.csv(theFile)
  else if ((separator == ';') && (decimal == ','))
    read.csv2(theFile)
  else if ((separator == '\t') && (decimal == '.'))
    read.delim(theFile)
  else if ((separator == '\t') && (decimal == ','))
    read.delim2(theFile)
  else
    print("ERROR: check that the separator and decimal values line up.")

}

