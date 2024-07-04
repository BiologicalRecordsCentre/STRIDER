#' If the spatraster slots are character filepaths then load in the rasters using terra::rast(), alternatively, if the spatraster slots are of class PackedSpatRaster then unwrap the rasters. Internal function.
#' @param sim_obj a SimulationObject
#' @noRd
read_sim_obj_rasters <- function(sim_obj){
  #load background
  if(is.character(sim_obj@background)){
    sim_obj@background <- terra::rast(sim_obj@background)
  }
  if(class(sim_obj@background)[1] == "PackedSpatRaster"){
    sim_obj@background <- terra::unwrap(sim_obj@background)
  }


  #load state env
  if(is.character(sim_obj@state_env)){
    sim_obj@state_env <- terra::rast(sim_obj@state_env)
  }
  if(class(sim_obj@state_env)[1] == "PackedSpatRaster"){
    sim_obj@state_env <- terra::unwrap(sim_obj@state_env)
  }

  #load state_target_suitability
  if(is.character(sim_obj@state_target_suitability)){
    sim_obj@state_target_suitability <- terra::rast(sim_obj@state_target_suitability)
  }
  if(class(sim_obj@state_target_suitability)[1] == "PackedSpatRaster"){
    sim_obj@state_target_suitability <- terra::unwrap(sim_obj@state_target_suitability)
  }

  #load state_target_realised
  if(is.character(sim_obj@state_target_realised)){
    sim_obj@state_target_realised <- terra::rast(sim_obj@state_target_realised)
  }
  if(class(sim_obj@state_target_realised)[1] == "PackedSpatRaster"){
    sim_obj@state_target_realised <- terra::unwrap(sim_obj@state_target_realised)
  }

  #return the object
  sim_obj
}

#' Write the raster and return the filename
#' @param x a SpatRaster
#' @param filename The file name
#' @param overwrite whether to overwrite existing file or not
#' @noRd
write_raster_return_filename <- function(x, filename,overwrite=T, ...){
  writeRaster(x,filename,overwrite, ...)
  filename
}


#' Generate a Hash for a SimulationObject
#'
#' This function generates a hash for a `SimulationObject` by extracting and hashing its components. Useful for tracking changes when using {targets} pipelines.
#'
#' @param sim_obj A `SimulationObject` for which to generate the hash.
#' @return A hash string representing the `SimulationObject`.
#' @examples
#' \dontrun{
#' background <- terra::rast(matrix(0, 500, 500))
#' sim_obj <- SimulationObject(background)
#' hash <- hash_sim_obj(sim_obj)
#' print(hash)
#' }
#' @noRd
hash_sim_obj <- function(sim_obj){
  sim_obj <- read_sim_obj_rasters(sim_obj)
  sim_obj@hash <- NULL

  #get values from rasters
  if(!is.null(sim_obj@background)){
    sim_obj@background <- terra::values(sim_obj@background)
  }
  if(!is.null(sim_obj@state_env)){
    sim_obj@state_env <- terra::values(sim_obj@state_env)
  }
  if(!is.null(sim_obj@state_target_suitability)){
    sim_obj@state_target_suitability <- terra::values(sim_obj@state_target_suitability)
  }
  if(!is.null(sim_obj@state_target_realised)){
    sim_obj@state_target_realised <- terra::values(sim_obj@state_target_realised)
  }

  digest::digest(sim_obj)
}

#' Export Simulation Report Data Frame
#'
#' This function exports the report data frame from a SimulationObject.
#'
#' @param sim_obj A SimulationObject containing the report data frame.
#'
#' @return A data frame containing the report from the SimulationObject.
#' @examples
#' \dontrun{
#' report_df <- export_df(simulation_object)
#' }
#' @export
export_df <- function(sim_obj){
  sim_obj@report
}
