
#' Extract Conected Components
#'
#' @param file a
#' @param octree_level a
#' @param nop a
#' @param global_shift a
#' @param global_shift_type a
#' @param c_export_fmt a
#' @param c_ext a
#' @param silent a
#' @param no_timestamp a
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
extract_cc <- function(file, octree_level=8, nop=50,
                       global_shift = F, global_shift_type= 'AUTO',
                       c_export_fmt= 'LAS', c_ext= 'las', silent= T, no_timestamp= T) {


  if(class(file) != 'character' ){
    stop(paste('Wrong file is a ', class(file),', must be a character'))

  }

  if((octree_level > 21 || octree_level < 1)){
    stop(paste('Wrong octree_level, must be a integer value between 1 and 21'))

  }

  if(class(nop) != 'numeric' ){
    stop(paste('Wrong nop is a ', class(file),', must be a numeric'))

  }

  if(class(global_shift) != "logical" ){
    stop('Wrong global_shift must be a logical value - TRUE or FALSE')
  }


  if(!(global_shift_type %in% c('AUTO', 'FIRST')) && (class(global_shift_type) != 'numeric' || length(global_shift_type) != 3)){
    stop('Wrong global_shift_type must be character value : "AUTO" or "FIRST" (avaliable since CC v.2.11) or numeric vector with 3 values for dimensions x,y,z ')
  }

  if(class(no_timestamp) != "logical" ){
    stop('Wrong no_timestamp must be a logical value - TRUE or FALSE')
  }

  if(!c_export_fmt %in% c('ASC','BIN', 'PLY', 'LAS', 'E57', 'VTK', 'PCD', 'SOI', 'PN', 'PV')){
    stop(paste('Wrong c_export_fmt: ',c_export_fmt,' (',class(c_export_fmt),'), must be character value, one of these : ASC, BIN, PLY, LAS, E57, VTK, PCD, SOI, PN, PV',sep=''))

  }

  if( (!c_ext %in% c('ASC','BIN', 'PLY', 'LAS', 'LAZ', 'E57', 'VTK', 'PCD', 'SOI', 'PN', 'PV') && !c_ext %in% tolower(c('ASC','BIN', 'PLY', 'LAS','LAZ', 'E57', 'VTK', 'PCD', 'SOI', 'PN', 'PV'))) ){
    stop(paste('Wrong c_ext: ',c_ext,' (',class(c_ext),'),
               must be character value, one of these :
               ASC,BIN, PLY, LAS, LAZ, E57, VTK, PCD, SOI, PN, PV
               or
               asc, bin, ply, las, laz, ,e57, ,vtk, pcd, soi, pn, pv',sep=''))

  }




  if (silent==T){
    s1= '-SILENT'

  }else{
    s1= ''
  }


  if (no_timestamp== T) {
    s2= '-NO_TIMESTAMP'
  } else{
    s2=''
  }

  s3= paste('-C_EXPORT_FMT',c_export_fmt,'-EXT',c_ext,sep=' ')


  if (global_shift == T){

    s4= paste('-O -GLOBAL_SHIFT',global_shift_type, file,sep=' ')

  } else {

    s4= paste('-O',file,sep=' ')
  }



  s5= paste('-EXTRACT_CC',as.integer(octree_level),as.integer(nop), sep=' ')


  cc_syntax <- paste(s1,s2,s3,s4,s5,sep=' ')

  return(cc_syntax)
}
