#' Simulates a uniform state of the environment
#'
#' @param simulation_object an R object of class 'SimulationObject' containing all the necessary information for the simulation
#' @param spatraster a SpatRaster to be used as the environment
#' @return An updated simulation object with the newly added state of the environment in the correct slot
#' @examples
#' \dontrun{
#' sim_state_env_uniform(simulation_object, 0)
#' }
sim_state_env_byod <- function(simulation_object,filename = NULL, spatraster) {
  simulation_object_original <- simulation_object
  simulation_object <- read_sim_obj_rasters(simulation_object)

  background <- simulation_object@background
  #check that it is a SpatRaster
  if(class(spatraster) != "SpatRaster"){
    stop(paste0("Your provided layers are of class ",class(spatraster), "whereas SpatRaster is required"))
  }
  #check that dimensions are correct
  if(nrow(background) != nrow(spatraster)){
    stop(paste0("@background has ",nrow(background)," rows whereas your provided SpatRaster has ",nrow(spatraster)," rows. Number of rows must be equal"))
  }
  #check that dimensions are correct
  if(ncol(background) != ncol(spatraster)){
    stop(paste0("@background has ",ncol(background)," columns whereas your provided SpatRaster has ",ncol(spatraster)," columns. Number of columns must be equal"))
  }

  #save raster and return filename if filename isn't null
  if(!is.null(filename)){
    spatraster <- write_raster_return_filename(spatraster,filename)
  }

  simulation_object_original@state_env <- spatraster

  #create hash, having set the hash to NULL
  tmp <- read_sim_obj_rasters(simulation_object_original)
  tmp@hash <- NULL
  simulation_object_original@hash <- digest::digest(tmp)

  # Return the updated simulation_object
  return(simulation_object_original)
}
