#' A What video game function
#'
#' This function allows you to ask Sangyeon what video game he plays nowadays.
#' @param ask Do you want to ask him? Defaults to TRUE.
#' @keywords practice
#' @export
#' @examples
#' wt_vid()

wt_vid <- function(ask=TRUE){
  if(ask==TRUE){
    print("I play Legend of Zelda nowadays!")
  }
  else {
    print("I work all day.")
  }
}
