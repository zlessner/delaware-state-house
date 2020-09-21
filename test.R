# library(sf)
# library(tidyverse)
# 
# nepal_shp <- read_sf('https://raw.githubusercontent.com/mesaugat/geoJSON-Nepal/master/nepal-districts.geojson')
# nepal_data <- read_csv('https://raw.githubusercontent.com/opennepal/odp-poverty/master/Human%20Poverty%20Index%20Value%20by%20Districts%20(2011)/data.csv')
# 
# # calculate points at which to plot labels
# centroids <- nepal_shp %>% 
#   st_centroid() %>% 
#   bind_cols(as_data_frame(st_coordinates(.)))    # unpack points to lat/lon columns
# 
# nepal_data %>% 
#   filter(`Sub Group` == "HPI") %>% 
#   mutate(District = toupper(District)) %>% 
#   left_join(nepal_shp, ., by = c('DISTRICT' = 'District')) %>% 
#   ggplot() + 
#   geom_sf(aes(fill = Value)) + 
#   geom_text(aes(X, Y, label = DISTRICT), data = centroids, size = 1, color = 'white')
# 
# 
# 
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
# 
# Delaware_State_Rep %>% 
#   filter(`year` == 2018) %>%
#   mutate(district = toupper(district)) %>% 
#   left_join(delaware_shp, ., by = c('DISTRICT' = 'district')) %>% 
#   ggplot() + 
#   geom_sf(aes(fill = totalvotes)) + 
#   geom_text(aes(X, Y, label = DISTRICT), data = centroids, size = 1, color = 'white')
# 
#   
#   
#   
# 
# library(plotly)
# library(rjson)
# 
# url <- 'https://opendata.arcgis.com/datasets/85e5da1b74c949e58bb9d64f7498b076_2.geojson'
# geojson <- rjson::fromJSON(file=url)
# df <- Delaware_State_Rep
# 
# g <- list(
#   fitbounds = "locations",
#   visible = FALSE
# )
# fig <- plot_ly() 
# fig <- fig %>% add_trace(
#   type="choropleth",
#   geojson=geojson,
#   locations=df$district,
#   z=df$totalvotes,
#   colorscale="Viridis",
#   featureidkey="properties.DISTRICT"
# )
# fig <- fig %>% layout(
#   geo = g
# )
# fig <- fig %>% colorbar(title = "Total Votes")
# fig <- fig %>% layout(
#   title = "2013 Montreal Election"
# )
# fig



library(sf) ; library(tidyverse) ; library(classInt) ; library(viridis)

sf_gb <- st_read("https://opendata.arcgis.com/datasets/07194e4507ae491488471c84b23a90f2_3.geojson", quiet = TRUE)

glimpse(sf_gb)
st_geometry(sf_gb)

plot(st_geometry(sf_gb))

lookup <- read_csv("https://opendata.arcgis.com/datasets/046394602a6b415e9fe4039083ef300e_0.csv") %>%
  filter(LAD17NM %in% c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan")) %>%
  pull(WD17CD)

sf_gm <- sf_gb %>%
  filter(wd17cd %in% lookup)

plot(st_geometry(sf_gm))

sf_gm <- sf_gm %>%
  select(area_code = wd17cd, area_name = wd17nm)

df <- read_csv("/Users/zacharyl/Downloads/bulk.csv")

df_census <- df %>%
  select(area_code = `geography code`,
         n = `Qualification: No qualifications; measures: Value`,
         total = `Qualification: All usual residents aged 16 and over; measures: Value`) %>%
  mutate(percent = (n/total)*100)

sf_gm_census <- left_join(sf_gm, df_census, by = "area_code")




ggplot(sf_gm_census, aes(fill = percent)) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(discrete = F,
                     name = "No qualifications (%)",
                     direction = -1,
                     guide = guide_colourbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5)) +
  labs(x = NULL, y = NULL,
       title = "Residents with no qualifications in Greater Manchester, 2011",
       subtitle = "Source: Table QS502EW, Census 2011",
       caption = "Contains OS data © Crown copyright and database right (2018)") +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.2, 0.09),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))


library(ggplot2)
library(ggiraph)
g <- ggplot(mpg, aes( x = displ, y = cty, color = hwy) )

my_gg <- g + geom_point_interactive(aes(tooltip = model), size = 2)
girafe(code = print(my_gg) )

my_gg <- g + geom_point_interactive(
  aes(tooltip = model, data_id = model), size = 2)
x <- girafe(code = print(my_gg))
x




# add interactive labels to a ggplot -------
library(ggplot2)
library(ggiraph)


p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
  geom_label_interactive(aes(tooltip = paste(rownames(mtcars), mpg, sep = "\n")))
x <- girafe(ggobj = p)
if( interactive() ) print(x)


p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
  geom_label_interactive(aes(fill = factor(cyl),
                             tooltip = paste(rownames(mtcars), mpg, sep = "\n")),
                         colour = "white",
                         fontface = "bold")
x <- girafe(ggobj = p)
if( interactive() ) print(x)

# add interactive texts to a ggplot -------
library(ggplot2)
library(ggiraph)

## the data
dataset = mtcars
dataset$label = row.names(mtcars)

dataset$tooltip = paste0( "cyl: ", dataset$cyl, "<br/>",
                          "gear: ", dataset$gear, "<br/>",
                          "carb: ", dataset$carb)

## the plot
gg_text = ggplot(dataset,
                 aes(x = mpg, y = wt, label = label,
                     color = qsec,
                     tooltip = tooltip, data_id = label ) ) +
  geom_text_interactive() +
  coord_cartesian(xlim = c(0,50))

## display the plot
x <- girafe(ggobj = gg_text)
x <- girafe_options(x = x,
                    opts_hover(css = "fill:#FF4C3B;font-style:italic;") )
if( interactive() ) print(x)





library(plotly)
library(rjson)

url <- 'https://opendata.arcgis.com/datasets/85e5da1b74c949e58bb9d64f7498b076_2.geojson'
geojson <- rjson::fromJSON(file=url)

df <- Party_Break
g <- list(
  fitbounds = "locations",
  visible = FALSE
)
fig <- plot_ly() 
fig <- fig %>% add_trace(
  type="choropleth",
  geojson=geojson,
  locations=df$district,
  z=df$voteBreakdown,
  colorscale="Viridis",
  featureidkey="properties.DISTRICT",
  marker=list(line=list(
    width=3),
    opacity=0.5
  )
)
fig <- fig %>% layout(
  geo = g
)
fig <- fig %>% colorbar(title = "Bergeron Votes")
fig <- fig %>% layout(
  title = "2013 Montreal Election"
)
fig

















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
  z=df$voteBreakdown,
  colorscale = colorscale,
  featureidkey="properties.DISTRICT"
)
fig <- fig %>% colorbar(title = "Vote Percentage")
fig <- fig %>% layout(
  mapbox=list(
    style="carto-positron",
    zoom =7.3,
    center=list(lon=-75.5277, lat=39.1)),
    title = "2018 Delaware Congressional Election"
)
fig