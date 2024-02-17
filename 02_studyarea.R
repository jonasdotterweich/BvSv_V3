#### study area


library(sf)
library(sp)
library(mapview)
library(leaflet)
library(raster)
library(terra)
library(flexsdm)


## manually importing sv thin

## study area based on the thinne occurences of the snow vole with a 100 km buffer

studyarea <- calib_area(svthin_thin1, x="decimalLongitude", y="decimalLatitude", method = c('bmcp', width=100000), 
                     groups = NULL, crs ="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


studar_sf <- st_as_sf(studyarea)

crs(studyarea) <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

plot(studyarea)





### seen in the visualization that there are some points outside of europe in Georgia, and also deleting one in Portugal
#filtering them out. 4 should be omittet


sv_thin <- svthin_thin1 %>% filter(decimalLongitude < 25 & decimalLongitude > -4)




## converting the points from the dataframe to a shapefile 

sv_pts <- st_as_sf(sv_thin, coords=c("decimalLongitude", "decimalLatitude"),  crs=4326)

write_sf(sv_pts, "Data/occ/sv_thin_pts.shp", overwrite=T)

svpts_shp <- st_read("Data/occ/sv_thin_pts.shp")


# then, new study area


studyarea <- calib_area(sv_thin, x="decimalLongitude", y="decimalLatitude", method = c('bmcp', width=100000), 
                        groups = NULL, crs ="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


studar_sf <- st_as_sf(studyarea)

crs(studyarea) <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


svpts_vect <- vect(sv_pts)      


sv_vis <- mapview(studyarea, color = "blue") + mapview(svpts_vect, color = "red")
print(sv_vis)


writeVector(studyarea, "Data/studar/studarSV.shp", overwrite=T)

### study area around sv points is now created


######----#####

##Moving on to creating a studyare for the bank vole occurrrences

## manually importing bv thin


studyarea_bv <- calib_area(bvthin_thin1, x="decimalLongitude", y="decimalLatitude", method = c('bmcp', width=100000), 
                        groups = NULL, crs ="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


writeVector(studyarea_bv, "Data/studar/studarBV.shp", overwrite=T)

# bv_points

bv_pts <- st_as_sf(bvthin_thin1, coords=c("decimalLongitude", "decimalLatitude"),  crs=4326)

write_sf(bv_pts, "Data/occ/bv_thin_pts.shp", overwrite=T)


mapview(list(bv_pts, sv_pts))

# croping the dataframe of BV to Svstudar

#first we need a spatial point data frame
xy <- bvthin_thin1[,c(2,3)]

BV_spat_df <- SpatialPointsDataFrame(coords = xy, data = bvthin_thin1,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


xy2 <- sv_thin[,c(2,3)]
SV_spat_df <-  SpatialPointsDataFrame(coords = xy2, data = sv_thin,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


#extent of studar SV
ext <- extent(-1.618411, 15.24981, 40.88735, 48.44762 )

# studar raster
mask_rast <- raster(ext)

stud_rast <- rasterize(studar_sf, mask_rast)


BV_sub <- crop(vect(BV_spat_df), ext)

writeVector(BV_sub, "Data/occ/bv_in_SVstudar.shp", overwrite=T)

# as dataframe
bv_sub_df <- as.data.frame(BV_sub)

write.csv(bv_sub_df, "Data/occ/bv_in_SVstudar.csv")


#### checking the points in qgis

#as is can be seen while visualizing in a map, a lot of lowland bank vole still in the model. will delimit up to paris manually in qgis

bv_reduced <- read_sf("Data/occ/bv_reducedtoSV.shp")

bv_red_df  <- as.data.frame(bv_reduced)

bv_red_df  <- bv_red_df[-c(4)]

write.csv(bv_red_df, "Data/occ/bv_reducedtoSV.csv")

#### now a jointed study area could be created by joining the data frames of the two species occurrences and then creating a studyarea 
# around it

## however as seen while visualizing in qgis all sv points are further in than bv
#so if we use the calib area formula on the reduced bv only we ge a sufficient solution as well.

## if jointed version is needed look into github BvSv_v2/3_bv in studar sv + joined studar.R

### okay trying it nevertheless

bv_red_df  <- bv_red_df %>% dplyr::rename( decimalLongitude = decimalLon,
                                           decimalLatitude =  decimalLat)

joined_pts <- rbind(sv_thin, bv_red_df)


#### so I tried with both the jointed and the bv reduced and I obviously get the same result of study area and 
# I am starting to think that the deleting of the points must have had an effect as both times study area around paris remains



studar_combi <- calib_area(joined_pts, x="decimalLongitude", y="decimalLatitude", method = c('bmcp', width=100000), 
                        groups = NULL, crs ="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

crs(studar_combi) <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

writeVector(studar_combi, "Data/studar/studar_combi.shp", overwrite=T)



##### therefore studyareas SV will be used to go on 












#### study area claculated again in different time frame of occurrances (1981 -2010)

### taking the 5km thin

studyarea <- calib_area(svthin_V2_5km_thin1, x="decimalLongitude", y="decimalLatitude", method = c('bmcp', width=100000), 
                        groups = NULL, crs ="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

crs(studyarea) <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

plot(studyarea)
mapview(studyarea)

### seing outiers, basicall in georgia.... filtering in qgis

sv_pts_V2 <- st_as_sf(svthin_V2_5km_thin1, coords=c("decimalLongitude", "decimalLatitude"),  crs=4326)

write_sf(sv_pts_V2, "Data/occ/sv_thin_pts_V2.shp", overwrite=T)

svpts_shp <- st_read("Data/occ/svthin_V2pts.shp")

#### what us left to do is either to convert so that I can apply calib area or to just delete he on point in georgia with the filtering function an then create an new calib area
## probably the latter is best




sv_pts_V2 <- svthin_V2_5km_thin1 %>% filter(decimalLongitude < 25 & decimalLongitude > -4)
### cleared that georgia point with that


studyarea <- calib_area(sv_pts_V2, x="decimalLongitude", y="decimalLatitude", method = c('bmcp', width=100000), 
                        groups = NULL, crs ="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

mapview(studyarea)

## saving th studar

writeVector(studyarea, "Data/studar/studarSV_V2.shp", overwrite=T)

## better, unfortunately could have more switzerland france and austria in it. 

ext_studar_sv <-ext(studyarea)



xy_bvtin_5km <- bvthin_V2_5km_thin1[,c(2,3)]

BVthin5km_spat <- SpatialPointsDataFrame(coords = xy_bvtin_5km, data = bvthin_V2_5km_thin1,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


BV_new_sub <- crop(vect(BVthin5km_spat), ext_studar_sv)


bv_new_red_df  <- as.data.frame(BV_new_sub)


writeVector(BV_new_sub, "Data/occ/bv_in_SVstudarV2.shp", overwrite=T)
write.csv(bv_new_red_df, "Data/occ/bv_in_SVstudaV2.csv")



### looking like a god crpp but stillneeding new adjustment of studyarea based on BV as there ate outliers
# still not good enghouh as there are the. too less bv after cropping







#### study area claculated again in different time frame of occurrances (2000 -2020)

### taking the 5km thin

studyarea <- calib_area(svthin_V3_5km_thin1, x="decimalLongitude", y="decimalLatitude", method = c('bmcp', width=100000), 
                        groups = NULL, crs ="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

crs(studyarea) <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

plot(studyarea)
mapview(studyarea)


## saving th studar

writeVector(studyarea, "Data/studar/studarSV_V3.shp", overwrite=T)



### creating point object for BV 

bv_pts <- st_as_sf(bvthin_V3_5km_thin1, coords=c("decimalLongitude", "decimalLatitude"),  crs=4326)

write_sf(bv_pts, "Data/occ/bv_thin_ptsV3.shp", overwrite=T)




## cropping bv to studar V3

xy_bvthin_5km <- bvthin_V3_5km_thin1[,c(2,3)]

BVthin5km_spat <- SpatialPointsDataFrame(coords = xy_bvthin_5km, data = bvthin_V3_5km_thin1,
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


BV_new_sub <- crop(vect(BVthin5km_spat), studyarea)


bv_new_red_df  <- as.data.frame(BV_new_sub)


writeVector(BV_new_sub, "Data/occ/bv_in_SVstudarV3.shp", overwrite=T)
write.csv(bv_new_red_df, "Data/occ/bv_in_SVstudaV3.csv")


### trying to make a studyarea with a smaller buffer to use for cropping the BV ocurrences
# so that the buffer in the end to the study area is the same in BV occs than in SV occs


studyarea_nobuff <- calib_area(svthin_V3_5km_thin1, x="decimalLongitude", y="decimalLatitude", method = c('bmcp', width=1), 
                        groups = NULL, crs ="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


BV_new_sub_small <- crop(vect(BVthin5km_spat), studyarea_nobuff)


bv_new_red_df_small  <- as.data.frame(BV_new_sub_small)


writeVector(BV_new_sub_small, "Data/occ/bv_in_SVstudarV3red.shp", overwrite=T)
write.csv(bv_new_red_df_small, "Data/occ/bv_in_SVstudaV3red.csv")

### this last one now should be the BV to model with 

