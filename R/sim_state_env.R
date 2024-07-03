#' Define the state of the environment
#'
#' This function updates the state environment of a given simulation object by
#' either using a provided SpatRaster, a predefined function (`gradient` or `uniform`),
#' or a user-defined function. The updated simulation object with the new state environment
#' and metadata is returned.
#'
#' @param simulation_object An object representing the simulation. The object should contain
#'                          a `background` slot and a `state_env` slot.
#' @param fun A character string specifying the name of a predefined function            (`"gradient"` or `"uniform"`) or a user-defined function.
#' @param filename A character string specifying the filename to save the resultant SpatRaster. If `NULL`, the SpatRaster is not saved to a file.
#' @param spatraster A `SpatRaster` object to be used directly as the state environment. If provided, it overrides the `fun` parameter.
#' @param ... Additional arguments to be passed to the function specified in `fun`.
#'
#' @return An updated simulation object with the newly added state of the environment in the correct slot
#'
#' @details
#' - If a `spatraster` is provided, the function checks that its dimensions match
#'   those of the simulation object's background.
#' - If `fun` is provided as a character string, it must be either `"gradient"` or `"uniform"`.
#' - If `fun` is provided as a user-defined function, it will be applied to the simulation object.
#' - If `filename` is provided, the resultant SpatRaster is saved, and the filename is returned.
#'
#' @examples
#' \dontrun{
#' sim_obj <- sim_state_env(sim_obj, fun = "uniform", value = 0.5)
#' sim_obj <- sim_state_env(sim_obj, fun = "gradient", from = 0, to = 1)
#' sim_obj <- sim_state_env(sim_obj, spatraster = my_spatraster)
#' sim_obj <- sim_state_env(sim_obj, fun = my_custom_function)
#' sim_obj <- sim_state_env(sim_obj, fun = "uniform", filename = "output.tif")
#' }
#'
#' @export
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

  #set metadata
  fun_args <- as.list(match.call())
  simulation_object_original@metadata[["state_env"]] <- fun_args[3:length(fun_args)]

  # Return the updated simulation_object
  return(simulation_object_original)
}
