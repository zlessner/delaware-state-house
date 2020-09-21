library(plotly)
library(rjson)



url <- 'https://opendata.arcgis.com/datasets/85e5da1b74c949e58bb9d64f7498b076_2.geojson'
geojson <- rjson::fromJSON(file=url)
df <- Dems
g <- list(
  fitbounds = "locations",
  visible = FALSE
)
colorscale <- data.frame(z=c(0, 0.3, 0.45, 0.499, 0.5, 0.5, 0.501, 0.55, 0.7, 1),col=c("Red", "#F70000", "#FC6767", "#FDCFCF", "purple", "purple", "#AFD2FF", "#3186F5", "#227DF5", "blue"))
fig <- plot_ly() 
fig <- fig %>% add_trace(
  type="choroplethmapbox",
  geojson=geojson,
  locations=df$district,
  text = paste(df$Candidate, gsub(" ", "", paste(df$voteBreakdown*100, "%"))),
  z=df$voteBreakdown,
  colorscale = colorscale,
  marker=list(line=list(
    width=1.5, color="grey")
  ),
  featureidkey="properties.DISTRICT",
  hoverinfo = "text"
)
fig <- fig %>% colorbar(title = "Vote Percentage", tickformat = "0%")
fig <- fig %>% layout(
  mapbox=list(
    style="carto-positron",
    zoom =7.3,
    center=list(lon=-75.5277, lat=39.1)),
  title = "2018 Delaware Congressional Election"
)
fig



# #Using GGPlot (without hover)
# library(sf)
# library(tidyverse)
# library(ggplot2)
# library(ggiraph)
# library(scales)
# 
# delaware_shp <- read_sf("https://opendata.arcgis.com/datasets/85e5da1b74c949e58bb9d64f7498b076_2.geojson")
# 
# Delaware_State_Rep
# 
# # calculate points at which to plot labels
# centroids <- delaware_shp %>% 
#   st_centroid() %>% 
#   bind_cols(as_data_frame(st_coordinates(.)))    # unpack points to lat/lon columns
# 
# #pipe
# Party_Break %>% 
#   filter(`party` == 'democrat') %>%
#   mutate(district = toupper(district)) %>% 
#   left_join(delaware_shp, ., by = c('DISTRICT' = 'district')) %>% 
#   ggplot() + 
#   labs(fill = "Vote Percentage") +
#   xlab("Latitude") +
#   ylab("Longitude") +
#   ggtitle("2018 Democratic Vote Share by District") +
#   geom_sf(aes(fill = voteBreakdown)) + 
#   geom_text_interactive(aes(X, Y, label = NAME), data = centroids, size = 2, color = 'white') +
#   scale_fill_gradientn(colours = c("Red", "#FA7D7D", "dodgerblue3", "blue"),
#                        values = scales::rescale(c(0, .45, .5, .55, 1)),
#                        na.value = "Red",
#                        labels = percent,
#                        limits = c(0, 1))
# 
