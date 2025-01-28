


library(tmap)
library(tmaptools)
library(sf)

query <- geocode_OSM("908 Sloat Dr., Salinas, CA, 93907", as.sf = TRUE)
query
#$query
#[1] "250 Northern Ave, Boston, MA, 02210"
#
#$coords
#        x         y 
#-71.03812  42.34875 

#$bbox
#     xmin      ymin      xmax      ymax 
#-71.03817  42.34870 -71.03807  42.34880 


# Test & visualise
library(leaflet)
leaflet() %>%
    addTiles() %>%
    addMarkers(query$coords[["x"]], query$coords[["y"]])


geocode_osm_batch




temp <- temp %>%
    mutate(address = str_replace(address, "#", "Suite "))
    



# Start here 

query <- geocode_OSM(address.list$address, as.sf = TRUE)


temp <- temp %>% 
    left_join(query, by = c("address" = "query"), relationship = "many-to-one") %>%
    mutate(
        lat = coalesce(lat.x, lat.y),
        lon = coalesce(lon.x, lon.y),
        point = coalesce(point.x, point.y)
    ) %>%
    select(-ends_with(c(".x",".y")) ) 

unavails <- address.list %>% anti_join(query, by = c("address" = "query") )

# unavails <- address.list

temp <- temp %>%
    mutate(address = case_when(address %in% unavails$address ~ paste0("Unavailable - ",address),
                                TRUE ~ address
                                )
           )

address.list <- temp %>%
    filter(!str_detect( `Facility Address`, "Unavailable" ),
           !str_detect( address, "Unavailable" ),
           is.na(lat)
    ) %>%
    select(address) %>%
    unique() %>%
    head(5)


# address.list <- address.list %>% tail(1)


###

tm_shape(districts.mry) +
    tm_fill("County", alpha = .5, legend.show = FALSE) +
    tm_borders() +
    tm_text("NAME", auto.placement = TRUE) +
    tm_shape(query) + 
    tm_dots()
