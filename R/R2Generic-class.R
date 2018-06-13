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
    granules = NULL,
    legend_counter = NULL,
    legend = NULL,
    
    initialize = function(granules = NULL, legend_counter = NULL, legend = NULL)
    {
      self$granules = granules
      self$legend_counter = 0
      t_bands = max(as.numeric(lapply(X = granules, FUN = function(X){length(X$bands)})))
      
      self$legend = matrix(ncol = 10, nrow = length(granules) * t_bands) # For extents, filename, timestamp and band - more could be added later by increasing the value of ncol
      colnames(self$legend) = c("xmin", "xmax", "ymin", "ymax", "filename", "time_index", "timestamp", "band_index", "band", "whether_raster")
      self$legend = as.data.frame(self$legend)
      # dir.create("disk")
    },
    
    legend_to_disk = function(dir_name)
    {
      write.csv(x = self$legend, file = paste(dir_name, "legend.csv", sep = "/"))
    },
    
    write_time = function(time_observation, t_num, out_path)
    {
      space_extent = c(time_observation$extent@xmin, time_observation$extent@xmax, time_observation$extent@ymin, time_observation$extent@ymax)
      space_proj4string = time_observation$srs@projargs
      time_obs = time_observation$time
      bands_num = length(time_observation$bands)
      
      dir_name = paste(out_path, "/t_", t_num, sep = "")
      dir.create(dir_name)
      dir_basename = paste("t_", t_num, sep = "") #path relative to legend file
      
      writeRaster(time_observation$data, filename = paste(dir_name, "/b", sep = ""), format = "GTiff", bylayer = TRUE, suffix = "numbers", overwrite = TRUE) #progress = "text",
      
      # The following two lines save metadata regarding the individual bands and the granules - which are not needed as such
      # lapply(time_observation$bands, FUN = self$write_bands_meta, t_num, time_obs, out_path)
      # save(space_extent, space_proj4string, time_obs, bands_num, file = paste(out_path, "/high_meta.txt", sep = ""), ascii = TRUE)
      
      legend_timeslice = matrix(ncol = 4+1+2+2+1, nrow = bands_num)
      colnames(legend_timeslice) = c("xmin", "xmax", "ymin", "ymax", "filename", "time_index", "timestamp", "band_index", "band", "whether_raster")
      legend_timeslice = as.data.frame(legend_timeslice)
      
      time_index = t_num
      timestamp = as.character.POSIXt(time_obs)
      band_names = names(time_observation$bands)
      whether_raster = 1 #This will be 0 for a feature/vector file
      
      for(j in 1:bands_num)
      {
        filename = paste(dir_basename, "/b_", j, ".tif", sep = "")
        self$legend[self$legend_counter + j,] = c(space_extent, filename, time_index, timestamp, j, band_names[j], whether_raster)
      }
      self$legend_counter = self$legend_counter + bands_num
    },
    
    write_bands_meta = function(bands, t_num, time_obs, out_path)
    {
      dir_name = paste("t", t_num, sep = "_")
      
      band_id = bands$band_id
      resolution = c(bands$res[["x"]], bands$res[["y"]])
      scale = bands$scale
      wavelength = as.numeric(bands$wavelength_nm)
      save(band_id, resolution, scale, wavelength, file = paste(out_path, "/", dir_name, "/", "meta_", band_id, ".txt", sep = ""), ascii = TRUE)
    },
    
    write_granules = function(granule_list = self$granules, dir_name = "disk")
    {
      dir.create(dir_name)
      t_num = length(granule_list)
      for(i in 1:length(granule_list))
      {
        print(paste("Writing observations at t = ", i, sep = ""))
        # cat(i)
        self$write_time(time_observation = granule_list[[i]], t_num = i, out_path = dir_name)
      }
      cat("Writing legend file to disk...")
      self$legend_to_disk(dir_name)
      cat("Done!")
    }
  )
)
