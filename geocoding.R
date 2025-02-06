


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


geocode.some <- function(df, size = 5) {
    


address.list <- df %>%
    filter(!str_detect( `Facility Address`, "Unavailable" ),
           !str_detect( address, "Unavailable" ),
           is.na(lat)
    ) %>%
    select(address) %>%
    unique() %>%
    head(size)

print(address.list)

query <- geocode_OSM(address.list$address, as.sf = TRUE)


df <- df %>% 
    left_join(query, by = c("address" = "query"), relationship = "many-to-one") %>%
    mutate(
        lat = coalesce(lat.x, lat.y),
        lon = coalesce(lon.x, lon.y),
        point = coalesce(point.x, point.y)
    ) %>%
    select(-ends_with(c(".x",".y")) ) 

unavails <- address.list %>% anti_join(query, by = c("address" = "query") )

# unavails <- address.list

df <- df %>%
    mutate(address = case_when(address %in% unavails$address ~ paste0("Unavailable - ",address),
                                TRUE ~ address
                                )
           )

df

# Start again
}

temp <- geocode.some(temp, 10)


temp <- ccs2


temp %>%
    filter(str_detect(`Facility Type`, "CENTER"),
           str_detect(address, "Unavailable"))


# address.list <- address.list %>% tail(1)


temp %>%
    select(address, lat, lon) %>%
write_sheet()

manual.addresses <- read_sheet("https://docs.google.com/spreadsheets/d/1WPQdBTm4Qj1u4ADF632-_s5m0nPQHpUGBLDNEJ4OYOo/edit?gid=1188451288#gid=1188451288") %>%
    unique()



manual.addresses.nounavail <- manual.addresses %>%
    mutate(address = str_remove(address,"Unavailable - "))



temp.w.manual <- ccs2 %>%
    left_join(manual.addresses.nounavail, by = c("address" = "address")) # %>%
    # mutate(        lat = coalesce(lat.x, lat.y),
    #                         lon = coalesce(lon.x, lon.y)
    # ) %>%
    # select(-ends_with(c(".x",".y")) ) %>%
    # unique()






tempunique()temp.old <- temp %>%
    head(95)

temp.new <- temp %>%
    select(`Facility Type`:`Complaint Info- Date, #Sub Aleg, # Inc Aleg, # Uns Aleg, # TypeA, # TypeB ...`)





# Process new file 
new.imports <- temp.new %>%
    filter(`County Name` == "MONTEREY",
           `Facility Status` != "CLOSED") %>%
    mutate(preschool = str_detect(`Facility Name`, "PRESCHOOL"),
           state = str_detect(`Facility Name`, "STATE"),)  %>%
    mutate(address = paste0(`Facility Address`,", ",`Facility City`,", ", `Facility State`, " ", `Facility Zip` )) 


# Keeping existing geocode data from preexisting list
temptemp <- temp.old %>%
    filter(`Facility Number` %in% new.imports$`Facility Number`) %>%
    bind_rows( new.imports %>% filter(`Facility Number` %notin% temp.old$`Facility Number`)
    )









###

tm_shape(districts.mry) +
    tm_fill("County", alpha = .5, legend.show = FALSE) +
    tm_borders() +
    tm_text("NAME", auto.placement = TRUE) +
    tm_shape(query) + 
    tm_dots()



points.to.map <- temp.w.manual %>%
    filter(!is.na(lon),
           !is.na(lat)) %>%
    st_as_sf( 
         coords = c("lon", "lat"),  # Specify columns for longitude and latitude
         crs = 4326) %>%
    select(`Facility Name`, everything())

tm_shape(points.to.map) +
    tm_dots(col = "orange", size = 0.05, title = "Facility Name", shape = "star") +
  #  tm_text(text = "Facility Name", size = 0.7, col = "black") +
    tm_basemap("OpenStreetMap") +
    tm_layout(title = "Licensed Childcare Centers")


