source("../spl2018take2/All_scripts/Merging/Merging.R")

# Heatmap of toronto by population 
# Read the neighborhood shapefile data and plot
geo.data <- data.frame(agg.2016)
geo.data$Hood_ID <- str_pad(geo.data$Hood_ID, width = 3, side = 'left', pad = '0')

# the path to shape file

toronto <- readOGR(dsn = "Shapefiles/Neighbourhoods_Toronto" ,"NEIGHBORHOODS_WGS84")


# fortify and merge: muni.df is used in ggplot
toronto@data$id <- rownames(toronto@data)
toronto.geo <- fortify(toronto)
toronto.geo <- join(toronto.geo, toronto@data, by="id")
names(toronto.geo)[names(toronto.geo) == 'AREA_S_CD'] <- 'Hood_ID'

toronto.geo <- join(geo.data, toronto.geo, by = "Hood_ID") # join data from census to data from shapefile

# Define function to generate heat maps with an upper limit on scale (input dataframe and desired cluster)
# for variables with an outlier that throws off the colour scale
HeatMapLimit <- function(data, x, lower, upper) {
  # Heat map with an upper limit on the scale
  #
  # Args:
  #   data: data to be used
  #   x: variable t be used as fill
  #   lower: lower cutoff of the scale
  #   upper: upper cutoff of the scale
  #
  # Returns: A heatmap with a lower and upper limit to reduce the distortion of outliers on the scale
  plot(ggplot(data = data, aes(x = long, y = lat, group = group))  + 
    geom_polygon(aes_string(fill = x, colour = shQuote(""))) +    # draw polygons and add fill with density variable
    geom_path(color = "light grey" ) +  # draw boundaries of neighbourhoods
    coord_equal() + 
    scale_fill_gradient(low = "#7ff4f0", high = "#000c8c",  # Set colour scale
                        space = "Lab", 
                        na.value = "#000647", # any value above upper limit or below lower limit will be considered as NAs 
                        limits = c(lower, upper), # Nas are darker colour than rest, set upper and lower limits of scale
                        guide = "colourbar") + # Add colour scale on side
    scale_colour_manual(values = NA) +              
    guides(colour=guide_legend(paste(">", upper, sep = " ", collapse = NULL), override.aes = list(fill="#000647"))) + # label guide
    labs(title = x)) # Add title
 
}


# plot auto thefts with an upper limit of 100 on the scale
HeatMapLimit(toronto.geo, "auto.theft", 0, 100)

# plot average income of neighbourhoods with a minimum of 25 000 and maximum of 100 000 on the scale
HeatMapLimit(toronto.geo, "avg.income", 25000, 100000)
