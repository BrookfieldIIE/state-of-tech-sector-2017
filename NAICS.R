###################################
#Function to help with NAICS codes#
###################################
#Version 1.0
###################################

#Loading Required packages
require(data.table)
require(stringr)

#Loading main database of NAICS. 2012 version. Also setkey for efficient extraction
NAICS <- fread("NAICS-SCIAN-2012-Structure_B.csv")
setkey(NAICS,"Class title","Code")

##################################
#' Converting one NAICS name to code
#'
#' This function allows you to convert one NAICS class name to its lowest NAICS code
#' @param name NAICS class name. Should be string
#' @keywords NAICS
#' @export code Lowest level NAICS code corresponding to the input name
#' @examples
#' convert_one_name("Accommodation and food services")
convert_one_name <- function(name){
  return(last(NAICS[`Class title`==name,Code]))
}

##################################
#' Converting many NAICS name to code
#'
#' This function allows you to convert many NAICS class name to its lowest NAICS code
#' @param name Vector of NAICS class name. Should be vector of strings
#' @keywords NAICS
#' @export code Vector of lowest level NAICS code corresponding to the input name
#' @examples
#' convert_all_name(c("Administrative and support services","Accommodation and food services"))
convert_all_names <- function(vector_of_names){
  return(sapply(vector_of_names,convert_one_name))
}


#################################
#Convert one NACIS into a list of classes (including their parents class). If loop as shown
#is more efficient than lapply over the vector (benchmarked.)
convert_one_code <- function(code){
  require(stringr)
  length_code <- str_length(code)
  final_vector <- c()
  if(length_code >= 3){
    two_letter <- str_sub(code,1,2)
    final_vector <- c(final_vector,NAICS[Code==two_letter,`Class title`])
    if(length_code >= 4){
      three_letter <- str_sub(code,1,3)
      final_vector <- c(final_vector,NAICS[Code==three_letter,`Class title`])
      if(length_code >= 5){
        four_letter <- str_sub(code,1,4)
        final_vector <- c(final_vector,NAICS[Code==four_letter,`Class title`])
        if(length_code >= 6){
          five_letter <- str_sub(code,1,5)
          final_vector <- c(final_vector, NAICS[Code==five_letter,`Class title`])
        }
      }
    }
  }
  return(final_vector)
}

#Convert a vector of NAICS codes into a list of names
show_NAICS  <- function(vector_of_codes){
  require(stringr)
  return(lapply(vector_of_codes,convert_one_code))
}