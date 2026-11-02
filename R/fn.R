#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Filename : fn.R
# Use      : Utility functions for GPS
# Author   : Tomas Sou
# Created  : 2026-10-27
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Notes
# na
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Updates
# na
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Functions
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Set 521 view and export the variable
#'
#' @returns `base` variable as a string.
#' @export
#' @examples
#' \dontrun{
#' set_view()
#' }
set_view = function(){
  if(Sys.getenv("CLEARCASE_GPS_GMF_COMPILE") == ""){
    # In interactive non-GMF mode, use the users private view
    user = Sys.getenv("USER")
    base = paste0("/view/",user,"_view/vob")
  } else {
    cat("RUNNING UNDER GMF!!!\n")
    base = "/vob"
  }
  return(base)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Functions

# GPS commands
addsc  = '; cleartool mkelem -mkpath -ci -ptime -c "Add to S.C. keeping creation time" '
addscp  = '; echo 1 | cleartool mkelem -mkpath -ci -ptime -c "Add to S.C. keeping creation time" '
addscr = '; gps_mkelem -r * -c "Adds all files in the current folder and subfolders to source control recursively" '
cmd_qc_ready = '; echo yes | gps_promote_ready_for_qc.sh -gps_comment "Ready_for_QC" -gps_validation_level "Critical" '

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Add Monolix Project with results to source control on GPS
#'
#' @param mlx_run `<chr>` Monolix project file name.
#' @param path `<chr>` Location to the Monolix project file.
#' @returns Nothing.
#' @export
#' @examples
#' \dontrun{
#' loc = "CLOU064C1/mas/mas_2/model/pgm_001/mt92414/A02_PopPK_final/run"
#' gps_sc_mlx("r01_base.mlxtran",loc)
#' }
gps_sc_mlx = function(mlx_run,path){
  # Monolix project
  base = set_view()
  mlx_name = tools::file_path_sans_ext(mlx_run)
  mlx_path = file.path(base,path)
  mlx_tran = mlx_run
  if(!grepl(".mlxtran",mlx_run)) mlx_tran = paste0(mlx_run,".mlxtran")
  # Mlxtran and result folder
  system(paste0("cd ",mlx_path,addscp,mlx_tran,addsc,mlx_name))
  # Key result files in result folder
  system(paste0("cd ",file.path(mlx_path,mlx_name),addsc," *"))
  # Everything else
  system(paste0("cd ",file.path(mlx_path,mlx_name),addscr))
  # Charts data
  system(paste0("cd ", file.path(mlx_path,mlx_name,"ChartsData"),addscr))
  # Internals
  system(paste0("cd ",file.path(mlx_path,mlx_name),addsc,(" .Internals")))
  system(paste0("cd ", file.path(mlx_path,mlx_name,".Internals"),addscr))
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Add Simulx project with results to SC
#'
#' @param sim_run `<chr>` Simulx project file name.
#' @param path `<chr>` Location to the Simulx project file.
#' @returns Nothing.
#' @export
#' @examples
#' \dontrun{
#' loc = "CLOU064C1/mas/mas_2/model/pgm_001/mt92414/A02_PopPK_final/sim"
#' gps_sc_sim("r01_base.smlx",loc)
#' }
gps_sc_sim = function(sim_run,path){
  # Simulx project
  base = set_view()
  sim_name = tools::file_path_sans_ext(sim_run)
  sim_path = file.path(base,path)
  sim_smlx = sim_run
  if(!grepl(".smlx",sim_run)) sim_smlx = paste0(sim_run,".smlx")
  # Simulx and result folder
  system(paste0("cd ",sim_path,addscp,sim_smlx,addsc,sim_name))
  # Key result files in result folder
  system(paste0("cd ",file.path(sim_path,sim_name),addsc," *"))
  # Everything else
  system(paste0("cd ",file.path(sim_path,sim_name),addscr))
  # Internals
  system(paste0("cd ",file.path(sim_path,sim_name),addsc,(" .Internals")))
  system(paste0("cd ", file.path(sim_path,sim_name,".Internals"),addscr))
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' GPS check in/out Monolix/Simulx project
#'
#' @param mlxs `<chr>` Monolix/Simulx project file name ending with either .mlxtran or .smlx.
#' @param path `<chr>` Location to the Monolix/Simulx project file.
#' @param io `<chr>` Either "ci" or "co" for check in or out, respectively.
#' @param resonly `<lgl>` `TRUE` to check the result folder only.
#' @returns Nothing.
#' @export
#' @examples
#' \dontrun{
#' loc = "CLOU064C1/mas/mas_2/model/pgm_001/mt92414/A02_PopPK_final/run"
#' gps_cio_mlx("r01_base.mlxtran",loc,"ci")
#' }
gps_cio_mlx = function(mlxs,path,io=c("ci","co"),resonly=FALSE){
  if(!grepl(".mlxtran|.smlx",mlxs)) stop("Please indicate .mlxtran or .smlx in filename!")
  if(missing(io)) stop("Please indicate check in our out!")
  io = match.arg(io)
  if(io=="ci") cmd = "; gps_ci "
  if(io=="co") cmd = "; gps_co "
  # Monolix project
  base = set_view()
  mlx_name = tools::file_path_sans_ext(mlxs)
  mlx_path = file.path(base,path)
  mlx_fpath = file.path(mlx_path,mlxs)
  if(!file.exists(mlx_fpath)) stop(paste0("No project found: ",mlx_fpath))
  # Mlxtran/Simulx project file
  if(!resonly) system(paste0("cd ",mlx_path,cmd,mlxs))
  # Result folder
  system(paste0("cd ",mlx_path,cmd,mlx_name))
  # Key result files in result folder
  system(paste0("cd ",file.path(mlx_path,mlx_name),cmd,"-r *"))
  # Internals
  system(paste0("cd ",file.path(mlx_path,mlx_name),cmd,(" .Internals")))
  system(paste0("cd ", file.path(mlx_path,mlx_name,".Internals"),cmd,"-r *"))
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Add a file or a folder and everything inside to source control
#'
#' @param fdir `<chr>` Filename.
#' @param path `<chr>` Location to the Monolix/Simulx project file.
#' @returns Nothing.
#' @export
#' @examples
#' \dontrun{
#' loc = "CLOU064C1/mas/mas_2/model/pgm_001/mt92414/A02_PopPK_final/pgm"
#' gps_sc("test.txt",loc)
#' }
gps_sc = function(fdir,path){
  base = set_view()
  vob_path = file.path(base,path)
  if(dir.exists(file.path(vob_path,fdir))) cat("\nDirectory detected!\n")
  system(paste0("cd ",vob_path,addsc,fdir))
  if(dir.exists(file.path(vob_path,fdir))) system(paste0("cd ",file.path(vob_path,dir),addscr))
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' GPS check in/out a file or a folder and everything inside
#'
#' @param fdirs `<chr>` A vector of file and folder names.
#' @param path `<chr>` Location to the Monolix/Simulx project file.
#' @param io `<chr>` Either "ci", "co" or "unco" for check in, out or undo check out, respectively.
#' @returns Nothing.
#' @export
#' @examples
#' \dontrun{
#' loc = "CLOU064C1/mas/mas_2/model/pgm_001/mt92414/A02_PopPK_final/pgm"
#' gps_cio("test.txt",loc,"ci")
#' }
gps_cio = function(fdirs,path,io=c("ci","co","unco")){
  if(missing(io)) stop("Please indicate check in our out!")
  io = match.arg(io)
  if(io=="ci") cmd = "; gps_ci "
  if(io=="co") cmd = "; gps_co "
  if(io=="unco") cmd = "; cleartool unco -rm "
  fdirs = fdirs[fdirs!=""]
  base = set_view()
  vob_path = file.path(base,path)
  lapply(
    fdirs,
    \(x){
      if(dir.exists(file.path(vob_path,x))) cat("\nDirectory detected!\n")
      system(paste0("cd ",vob_path,cmd,x))
      if(dir.exists(file.path(vob_path,x))){
        system(paste0("cd ",file.path(vob_path,x),cmd,"-r *"))
      }
    }
  )
  invisible()
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Examine file properties with GPS describe
#'
#' @param fnames `<chr>` A vector of file names.
#' @param path `<chr>` Location to the files.
#' @returns Nothing.
#' @export
#' @examples
#' \dontrun{
#' loc = "CLOU064C1/mas/mas_2/model/pgm_001/mt92414/A02_PopPK_final/pgm"
#' gps_exam("test.txt",loc)
#' }
gps_exam = function(fnames,path){
  base = set_view()
  fnames = fnames[fnames!=""]
  vob_path = file.path(base,path)
  cmd = '; gps_describe '
  lapply(fnames, \(x) system(paste0("cd ",vob_path,paste0(cmd,x))))
  invisible()
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Promote for QC at the critical level
#'
#' @param fnames `<chr>` A vector of filenames.
#' @param path `<chr>` Location to the files.
#' @returns Nothing.
#' @export
#' @examples
#' \dontrun{
#' loc = "CLOU064C1/mas/mas_2/model/pgm_001/mt92414/A02_PopPK_final/pgm"
#' gps_qc_ready("test.txt",loc)
#' }
gps_qc_ready = function(fnames,path){
  base = set_view()
  fnames = fnames[fnames!=""]
  vob_path = file.path(base,path)
  cmd = cmd_qc_ready
  lapply(fnames, \(x) system(paste0("cd ",vob_path,paste0(cmd,x))))
  invisible()
}
