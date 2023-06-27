library(sf)
library(rnaturalearth)
library(raster)
library(ggplot2)
library(stars)




data <- st_read("D:/Rayshader R/output.gpkg")
head(data)
dim(data)



# Load necessary libraries

library(ggplot2)


# Get the world map data
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Filter for the United Kingdom
uk_map <- subset(world_map, admin == "United Kingdom")

# Plot the map using ggplot
ggplot() +
  geom_sf(data = uk_map) +
  ggtitle("Map of the United Kingdom")

# Check the CRS of both datasets
crs_data <- st_crs(data)
crs_uk_map <- st_crs(uk_map)





# Print the CRS of both datasets
cat("CRS of data:\n")
print(crs_data)
cat("CRS of UK shape:\n")
print(crs_uk_map)


data <- st_transform(data, crs_uk_map)
crs_data_after_transform <- st_crs(data)
# Check if the CRS of both datasets are the same after transformation
if (crs_data_after_transform == crs_uk_map) {
  cat("CRS transformation was successful.\n")
} else {
  cat("CRS transformation failed.\n")
}


#crs_data_after_transform <- st_crs(data)
#if (crs_data_after_transform == crs_uk_map) {
#  cat("CRS transformation was successful.\n")
#} else {
#  cat("CRS transformation failed.\n")
#} 






# Check if the CRS of both datasets are the same
if (crs_data == crs_uk_map) {
  cat("Both datasets have the same CRS.\n")
} else {
  cat("The datasets have different CRS. Transforming data to the same CRS as UK shape.\n")
  data <- st_transform(data, crs_uk_map)
}


# Perform the intersection and handle any errors
tryCatch({
  intersected_data <- st_intersection(data, uk_map)
}, error = function(e) {
  cat("Error in intersection:", e$message, "\n")
})

# Check if 'intersected_data' exists and contains data
if (exists("intersected_data") && nrow(intersected_data) > 0) {
  cat("Intersection was successful.\n")
  
  # Optionally, you can plot the intersected data using ggplot
  ggplot() +
    geom_sf(data = intersected_data) +
    ggtitle("Intersected Data")
} else {
  cat("Intersection failed or resulted in an empty dataset.\n")
}



cat("Number of features in intersected_data:", nrow(intersected_data), "\n")


cat("Column names in intersected_data:\n")
print(names(intersected_data))



cat("First few rows of intersected_data:\n")
print(head(intersected_data))






# Get the bounding box of the intersected data
bb <- st_bbox(intersected_data)

# Print the bounding box to the console
cat("Bounding Box:\n")
print(bb)


#Extract the bottom-left and bottom-right points
bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(intersected_data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(intersected_data))


# Check by plotting points
ggplot() +
  geom_sf(data = intersected_data) +
  geom_sf(data = bottom_left, color = "blue") +
  geom_sf(data = bottom_right, color = "red") +
  ggtitle("Population Density of the United Kingdom with Bounding Box Points")


width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) %>% 
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)







# handle conditions of width or height being the longer side
if (width > height) {
  w_ratio <- 1
  h_ratio <- as.numeric(height / width)
} else {
  h_ratio <- 1
  w_ratio <- as.numeric(width / height)
}

# convert to raster so we can then convert to matrix
size <- 3000

# Replace 'intersected_data' with the appropriate dataset (e.g., st_florida, intersected_data, etc.)
uk_rast <- st_rasterize(intersected_data, 
                        nx = floor(size * w_ratio),
                        ny = floor(size * h_ratio))

# Convert the raster to a matrix
mat <- matrix(uk_rast$population, 
              nrow = floor(size * h_ratio), # Note that 'h_ratio' is used for rows
              ncol = floor(size * w_ratio)) # and 'w_ratio' is used for columns



# Check if 'mat' exists
if (exists("mat")) {
  cat("'mat' exists.\n")
  
  # Check the type of 'mat'
  cat("Type of 'mat':\n")
  print(class(mat))
} else {
  cat("'mat' does not exist.\n")
}


# Check the dimensions of the matrix
cat("Dimensions of 'mat':\n")
print(dim(mat))

# Optionally, print the first few rows and columns of the matrix
cat("First few rows and columns of 'mat':\n")
print(mat[1:5, 1:5])












library(rayshader)
library(MetBrewer)
library(rgl)


# Create a color palette
c1 <- met.brewer("OKeeffe2")



# Generate a texture using the color palette
texture <- grDevices::colorRampPalette(c1, bias = 2)(256)

# Close any existing rgl windows
rgl::close3d()






mat %>% 
  height_shade(texture = texture) %>% 
  plot_3d(heightmap = mat,
          zscale = 100 / 5,
          solid = FALSE,
          shadowdepth = 0)

# Set the camera angle and zoom for the 3D plot
render_camera(theta = -20, phi = 45, zoom = .8)

# Define the output file name
outfile <- "final_plot.png"





# Start rendering time
start_time <- Sys.time()
cat("Rendering started at:", start_time, "\n")

# Create the output file if it doesn't exist
if (!file.exists(outfile)) {
  png::writePNG(matrix(1), target = outfile)
}

# Render the high-quality plot
render_highquality(
  filename = outfile,
  interactive = FALSE,
  lightdirection = 280,
  lightaltitude = c(20, 80),
  lightcolor = c(c1[2], "white"),
  lightintensity = c(600, 100),
  samples = 450,
  width = 6000,
  height = 6000
)

# End rendering time
end_time <- Sys.time()
diff <- end_time - start_time
cat("Rendering completed in:", diff, "\n")
}



















