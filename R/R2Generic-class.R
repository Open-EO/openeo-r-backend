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
  
  public = list(
    scenes = NULL,
    legend_counter = NULL,
    legend = NULL,
    
    initialize = function(scenes = NULL, legend_counter = NULL, legend = NULL)
    {
      self$scenes = scenes
      self$legend_counter = 0
      # t_bands = max(as.numeric(lapply(X = granules, FUN = function(X){length(X$bands)})))
      # t_bands = length(unique(scenes$band))
      
      self$legend = matrix(ncol = 10, nrow = dim(scenes)[1]) # For extents, filename, timestamp and band - more could be added later by increasing the value of ncol
      colnames(self$legend) = c("xmin", "xmax", "ymin", "ymax", "filename", "time_index", "timestamp", "band_index", "band", "whether_raster")
      self$legend = as.data.frame(self$legend)
      # dir.create("disk")
    },
    
    legend_to_disk = function(dir_name)
    {
      write.csv(x = self$legend, file = paste(dir_name, "legend.csv", sep = "/"))
    },
    
    write_scenes = function(scene_table = self$scenes, dir_name = "disk")
    {
      dir.create(dir_name)
      t_num = unique(scene_table$time)
      table_cols = colnames(scene_table)
      scene_table = cbind(scene_table, NA)
      colnames(scene_table) = c(table_cols, "time_index")
      
      for(i in 1:length(t_num))
      {
        print(paste("Writing observations at t = ", i, sep = ""))
        dir.create(paste(dir_name, "/t_", i, sep = ""))
        b_num = scene_table$band[scene_table$time == t_num[i]]
        for(j in 1:length(b_num))
        {
          # s = scene_table$data[scene_table$band == b_num[j] && scene_table$time == t_num[i]]
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
      cat("Writing legend file to disk...")
      self$legend_to_disk(dir_name)
      cat("Done!")
    }
    
  )
)
