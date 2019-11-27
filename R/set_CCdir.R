

#' Set CloudCompare directory
#'
#' @param CCdir - character, directory to CloudCompare.exe file
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' #' pc_files <- list.files('C://', pattern='.las$',full.names=T)
#' CC(merge_clouds(files= pc_files))
#' }
#'
#'
set_CCdir <- function(CCdir= 'C:\\Program Files\\CloudCompare\\CloudCompare.exe'){

  CC_dir <- paste('"',CCdir,'"',sep='')

  return(CC_dir)


}
