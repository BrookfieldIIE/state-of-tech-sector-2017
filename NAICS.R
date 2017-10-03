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

##################################
#' Converting one NAICS code to name
#'
#' This function allows you to convert one NAICS code to its NAICS name, along with all the parent level names
#' @param code NAICS code. Should be string
#' @keywords NAICS
#' @export name Vector of names, starting from the highest parent level to the name that corresponds directly to the code
#' @examples
#' convert_one_code("111111")
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
##################################
#' Converting many NAICS code to name
#'
#' This function allows you to convert many NAICS code to their NAICS name, along with all the parent level names
#' @param code NAICS code. Should be string
#' @keywords NAICS
#' @export list_of_name List of vector of names; each vector starting from the highest parent level to the name that corresponds directly to the code
#' @examples
#' convert_many_code(c("111111","4132","1111","1112"))
show_NAICS  <- function(vector_of_codes){
  require(stringr)
  return(lapply(vector_of_codes,convert_one_code))
}
