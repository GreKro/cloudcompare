
#' CC function run CloudCompare software
#' @param cc_syntax - CloudCompare Command Line Mode syntax, character, output of cloudcompare package functions
#' @return - nothing
#' @export
#'
#' @examples
#' \dontrun{
#' #' pc_files <- list.files('C://', pattern='.las$',full.names=T)
#' CC(merge_clouds(files= pc_files))
#' }
#'
#'
CC <- function(cc_syntax){


  CC_cmd <- paste(CC_dir, cc_syntax, sep=' ')

  for (f in 1:length(CC_cmd)){

    system(command= CC_cmd[f])

  }

}

