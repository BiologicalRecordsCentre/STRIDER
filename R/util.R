#' If the spatraster slots are character filepaths then load in the rasters using terra::rast()
#' @param sim_obj a SimulationObject
#' @noRd
read_sim_obj_rasters <- function(sim_obj){

  #load background
  if(is.character(sim_obj@background)){
    sim_obj@background <- terra::rast(sim_obj@background)
  }

  #load state env
  if(is.character(sim_obj@state_env)){
    sim_obj@state_env <- terra::rast(sim_obj@state_env)
  }

  #load state_target_suitability
  if(is.character(sim_obj@state_target_suitability)){
    sim_obj@state_target_suitability <- terra::rast(sim_obj@state_target_suitability)
  }

  #load state_target_realised
  if(is.character(sim_obj@state_target_realised)){
    sim_obj@state_target_realised <- terra::rast(sim_obj@state_target_realised)
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
