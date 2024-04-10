#' The state of the environment
#'
#' @param simulation_object an R object of class 'SimulationObject' containing all the necessary information for the simulation
#' @param filename a file name and path to save the spatraster
#' @param spatraster a SpatRaster to be used as the environment
#' @param function either a function to be used, or a name of a provided function
#' @return An updated simulation object with the newly added state of the environment in the correct slot
#' @examples
#' \dontrun{
#' sim_state_env(simulation_object, fun = "uniform",value=1)
#' }
sim_state_env <- function(simulation_object, fun= NULL, filename = NULL, spatraster=NULL, ...) {

  #load in rasters but create a copy with the original version
  simulation_object_original <- simulation_object
  simulation_object <- read_sim_obj_rasters(simulation_object)

  #if a spatraster has been provided:
  if (!is.null(spatraster)){
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

  # OR if a function has been defined
  } else if (is.character(fun)){
    if(!(fun %in% c("gradient","uniform"))){
      stop("Provided function must either be 'gradient' or 'uniform'")
    }

    fun_got <- get(paste0("state_env_",fun))
    spatraster <- fun_got(simulation_object,...)

  # or finally if user has provided a function
  } else {
    spatraster <- fun(simulation_object,...)
  }

  #save raster and return filename if filename isn't null
  if(!is.null(filename)){
    spatraster <- write_raster_return_filename(spatraster,filename)
  }

  simulation_object_original@state_env <- spatraster

  #create hash, having set the hash to NULL
  simulation_object_original@hash <- hash_sim_obj(simulation_object_original)

  # Return the updated simulation_object
  return(simulation_object_original)
}
