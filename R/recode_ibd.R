#' Recode IBD sharing.
#' 
#' \code{recode.ibd} recodes pairwise IBD sharing information.
#' 
#' At any marker, there are 15 possible IBD states between the four genes of two individuals. "ibdstate" represents the standard coding of the 15 states from 1111 to 1234. "alphanumeric" and "jacquard" are different ordering of "ibdstate" from 1 to 15. "jacquard.reduced" is a condensed ordering from 1 to 9 for the genotypically distinct groups of IBD states when phasing is unknown. "relatedness" refers to local relatedness coefficient taking values in (0, 0.5, 1, 2). 
#' 
#' "ibdstate", "alphanumeric" and "jacquard" are of the highest level (complete information), "jacquard.reduced" is of mid level, whereas "relatedness" is of the lowest level. Conversion cannot go from lower level to higher level. 
#' 
#' @param ibdvec numeric vector of input IBD sharing information.
#' @param from,to string, IBD sharing information options include "ibdstate", "alphanumeric", "jacquard", "jacquard.reduced" and "relatedness".
#' @return A numeric vector of recoded IBD states.
#' @export
#' @examples 
#' test.state = c(1111, 1122, 1212, 1222, 1234)
#' recode.ibd(test.state, "ibdstate", "alphanumeric")
#' recode.ibd(test.state, "ibdstate", "jacquard")
#' recode.ibd(test.state, "ibdstate", "jacquard.reduced")
#' recode.ibd(test.state, "ibdstate", "relatedness")
recode.ibd = function(ibdvec, from, to){
  ibdstate = c(1111, 1112, 1121, 1122, 1123, 1211, 1212, 1213, 1221, 1222, 1223, 1231, 1232, 1233, 1234)
  alphanumeric = c(1:15)
  jacquard = c(1, 3, 4, 2, 5, 6, 9, 11, 10, 7, 13, 12, 14, 8, 15)
  jacquard.reduced = c(1, 3, 3, 2, 4, 5, 7, 8, 7, 5, 8, 8, 8, 6, 9)
  relatedness = c(2, 1, 1, 0, 0, 1, 1, 0.5, 1, 1, 0.5, 0.5, 0.5, 0, 0)

  data = data.frame(ibdstate, alphanumeric, jacquard, jacquard.reduced, relatedness)
  
  if(from == "relatedness"){
    stop("Insufficient information to make the conversion.")
  }
  
  if(from == "jacquard.reduced" && to != "relatedness"){
    stop("Insufficient information to make the conversion.")
  }

  
  output = sapply(ibdvec, function(x) data[[to]][which(data[[from]] == x)])
  return(output)
}
