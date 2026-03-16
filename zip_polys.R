library(dplyr)
library(sf)

## zip centroids = average vertex location:
long.mean <- aggregate(segs$long1, by = list(segs$zip), FUN = "mean")
lat.mean <- aggregate(segs$lat1, by = list(segs$zip), FUN = "mean")
zip.loc <- data.frame(zip = lat.mean$Group.1,
                      lat = lat.mean$x,
                      long = long.mean$x)
zip_pts <- st_as_sf(zip.loc, coords = c("long","lat"), crs = 4326) %>%
  st_transform("EPSG:3857") %>%
  rename(centroid_zip = zip)

# extract lines
line_list <- lapply(seq_len(nrow(segs)), function(i) {
  st_linestring(matrix(
    c(segs$long1[i], segs$lat1[i],
      segs$long2[i], segs$lat2[i]),
    ncol = 2,
    byrow = TRUE
  ))
})

# convert to sf
lines <- st_sf(
  segs,
  geometry = st_sfc(line_list, crs = 4326)
) %>%
  st_transform(st_crs(zip_pts))

# network of line segments
network <- lines %>%
  st_union() %>%
  st_node()

# extract polygons and assign zip of nearest centroid
cells <- network %>%
  st_polygonize() %>%
  st_collection_extract("POLYGON") %>%
  st_as_sf() %>%
  st_join(zip_pts, largest = TRUE)

# Tally zipcodes of adjacent line segments
touches <- st_intersects(cells, lines)
cells$touches_zip <- sapply(touches, function(idx) {
  if(length(idx) == 0) return(NA)
  z <- lines$zip[idx]
  as.numeric(names(sort(table(z), decreasing = TRUE))[1])
})

# tally method fails for two zipcodes that are mostly surrounded by another zip ("enclaves")
# for these, assign zip via nearest centroid
enclaves <- c(33185, 33035)
cells <- cells %>%
  mutate(zip = case_match(centroid_zip, enclaves ~ centroid_zip, .default = touches_zip))

# combine cells into one polygon per zip
zip_polys <- cells %>%
  group_by(zip) %>%
  summarise() %>%
  rename(zipcode = zip, geometry = x)

