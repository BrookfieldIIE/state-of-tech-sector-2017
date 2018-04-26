###################################
#Function to help with NAICS_dic codes#
###################################
#Version 1.0
###################################

#Loading Required packages
require(data.table)
require(stringr)

#Loading main database of NAICS_dic. 2012 version. Also setkey for efficient extraction
NAICS_dic <- fread("NAICS-SCIAN-2012-Structure_B.csv")
setkey(NAICS_dic,"Class title","Code")

##################################
#' Converting one NAICS_dic name to code
#'
#' This function allows you to convert one NAICS_dic class name to its lowest NAICS_dic code
#' @param name NAICS_dic class name. Should be string
#' @keywords NAICS_dic
#' @export code Lowest level NAICS_dic code corresponding to the input name
#' @examples
#' convert_one_name("Accommodation and food services")
convert_one_name <- function(name){
  return(last(NAICS_dic[`Class title`==name,Code]))
}

##################################
#' Converting many NAICS_dic name to code
#'
#' This function allows you to convert many NAICS_dic class name to its lowest NAICS_dic code
#' @param name Vector of NAICS_dic class name. Should be vector of strings
#' @keywords NAICS_dic
#' @export code Vector of lowest level NAICS_dic code corresponding to the input name
#' @examples
#' convert_all_name(c("Administrative and support services","Accommodation and food services"))
convert_all_names <- function(vector_of_names){
  return(sapply(vector_of_names,convert_one_name))
}

##################################
#' Converting one NAICS_dic code to name
#'
#' This function allows you to convert one NAICS_dic code to its NAICS_dic name, along with all the parent level names
#' @param code NAICS_dic code. Should be string
#' @keywords NAICS_dic
#' @export name Vector of names, starting from the highest parent level to the name that corresponds directly to the code
#' @examples
#' convert_one_code("111111")
convert_one_code <- function(code){
  require(stringr)
  length_code <- str_length(code)
  final_vector <- c()
  if(length_code >= 3){
    two_letter <- str_sub(code,1,2)
    final_vector <- c(final_vector,NAICS_dic[Code==two_letter,`Class title`])
    if(length_code >= 4){
      three_letter <- str_sub(code,1,3)
      final_vector <- c(final_vector,NAICS_dic[Code==three_letter,`Class title`])
      if(length_code >= 5){
        four_letter <- str_sub(code,1,4)
        final_vector <- c(final_vector,NAICS_dic[Code==four_letter,`Class title`])
        if(length_code >= 6){
          five_letter <- str_sub(code,1,5)
          final_vector <- c(final_vector, NAICS_dic[Code==five_letter,`Class title`])
        }
      }
    }
  }
  return(final_vector)
}

##################################
#' Converting many NAICS_dic code to name
#'
#' This function allows you to convert many NAICS_dic code to their NAICS_dic name, along with all the parent level names
#' @param code NAICS_dic code. Should be string
#' @keywords NAICS_dic
#' @export list_of_name List of vector of names; each vector starting from the highest parent level to the name that corresponds directly to the code
#' @examples
#' convert_many_code(c("111111","4132","1111","1112"))
show_NAICS_dic  <- function(vector_of_codes){
  require(stringr)
  return(lapply(vector_of_codes,convert_one_code))
}


