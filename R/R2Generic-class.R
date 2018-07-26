#' R2Generic
#' 
#' This class prepares objects of class `Collection` to be written to disk in a generic format (GeoTIFF) for 
#' the User-Defined Functions(UDFs) to run. Apart from the binary raster files, it also writes an ASCII "legend" 
#' in CSV format which acts as a look-up table containing metadata on the individual rasters. This look-up table 
#' is parsed to load the rasters into a `stars` object from the function `run_UDF()` in the package `openEO.R.UDF`.
#' 
#' @field granules Object of class Granule
#' 
#' @importFrom R6 R6Class
#' @export
R2Generic = R6Class(
  "R2Generic",
  
  # public ----
  public = list(
    # attributes ====
    scenes = NULL,
    legend_counter = NULL,
    legend = NULL,
    
    # functions ====
    initialize = function(scenes = NULL, legend_counter = NULL, legend = NULL)
    {
      self$scenes = scenes
      self$legend_counter = 0
      
      self$legend = matrix(ncol = 10, nrow = dim(scenes)[1]) # For extents, filename, timestamp and band - more could be added later by increasing the value of ncol
      colnames(self$legend) = c("xmin", "xmax", "ymin", "ymax", "filename", "time_index", "timestamp", "band_index", "band", "whether_raster")
      self$legend = as.data.frame(self$legend)
    },
    
    legend_to_disk = function(dir_name)
    {
      cat("Writing legend file to disk...\n")
      tryCatch( {
        legend_file_path = paste(dir_name, "legend.csv", sep = "/")
        if (!file.exists(legend_file_path)) {
          file.create(legend_file_path)
        }
        
        con = file(legend_file_path)
        write.csv(x = self$legend, file = con)
      }, finally = function() {
        close(con)
        cat("Done!\n")
      })
      
    },
    
    write_scenes = function(scene_table = self$scenes, dir_name = "disk")
    {
      cat("Writing scenes to disk...\n")
      if (!dir.exists(dir_name)) {
        dir.create(dir_name,recursive = TRUE)  
      }
      
      t_num = unique(scene_table$time)
      table_cols = colnames(scene_table)
      scene_table = cbind(scene_table, NA)
      colnames(scene_table) = c(table_cols, "time_index")
      
      for(i in 1:length(t_num))
      {
        print_statement = paste("\tWriting observations at t = ", i, "\n", sep = "")
        cat(print_statement)
        dir.create(paste(dir_name, "/t_", i, sep = ""))
        b_num = scene_table$band[scene_table$time == t_num[i]]
        for(j in 1:length(b_num))
        {
          s = subset(x = scene_table, subset = scene_table$band == b_num[j] & scene_table$time == t_num[i])
          file_path_rel = paste("t_", i, "/b_", j, sep = "")
          file_path_str = paste(dir_name, file_path_rel, sep = "/")
          writeRaster(s$data[[1]], filename = file_path_str, format = "GTiff", bylayer = TRUE, suffix = "numbers", overwrite = TRUE) #progress = "text",
          space_extent = c(s$data[[1]]@extent@xmin, s$data[[1]]@extent@xmax, s$data[[1]]@extent@ymin, s$data[[1]]@extent@ymax)
          
          self$legend_counter = self$legend_counter + 1
          #Assuming band is the same as band_index (Need band label in scene_table - discuss with Florian)
          #Assuming it is raster now
          self$legend[self$legend_counter, ] = c(space_extent, paste(file_path_rel, ".tif", sep = ""), i, as.character.Date(t_num[i]), j, b_num[j], 1)
        }
      }
      cat("Done!\n")
    }
    
  )
)

#' Write generics to disk
#' 
#' @description This function writes rasters in a consistent directory structure to disk in generic GeoTIFF formats. 
#' It takes as input an object of class `Collection` and the path where the files are to be written to disk. Once the files 
#' have been written to disk, it can be loaded into a `stars` object by the user after which custom functions could 
#' be applied to it.
#'
#' @param collection_obj Object of class Collection as produced in the previous step while executing the process graph before encountering the UDF
#' @param dir_name Path where the generics are to be written to disk. This could be obtained from the UDF process if it is defined by the user while registering it.
#'
#' @export
#'
write_generics = function(collection_obj, dir_name = "disk") #dir_name could be obtained if it is defined while registering the UDF
{
  scene_table = collection_obj$getData()
  R2G_obj = R2Generic$new(scenes = scene_table)
  R2G_obj$write_scenes(dir_name = dir_name)
  
  R2G_obj$legend_to_disk(dir_name)
  
}
