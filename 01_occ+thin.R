### preparing the occurences


library(dplyr)
library(spThin)


## importing the dataframes manually with rstudio import from files


#selectiong only the ones with coordinates
C_glareolus <- c_glareolus08_18 %>% distinct(decimalLongitude,decimalLatitude, .keep_all= TRUE)

#selectiong only the ones with Myodes glareolus species name... as thinning 
C_glareolus <- C_glareolus %>% filter(scientificName == "Myodes glareolus (Schreber, 1780)" | 
                                        scientificName == "Clethrionomys glareolus (Schreber, 1780)")


#####preparing dataset for spThin

C_glareolus_thin <-C_glareolus %>% dplyr::select(decimalLatitude, decimalLongitude, species)



### spThin



thin(
  loc.data= C_glareolus_thin,
  lat.col = "decimalLatitude",
  long.col = "decimalLongitude",
  spec.col = "species",
  thin.par= 10,
  reps= 5,
  locs.thinned.list.return = FALSE,
  write.files = TRUE,
  max.files = 1,
  out.dir= "Data/occ",
  out.base = "bvthin",
  write.log.file = FALSE,
  log.file = "spatial_thin_log_bv.txt",
  verbose = TRUE
)




#####  Now moving to C. nivalis, filtering it and seeing if thinning is ven needed



C_nivalis <- c_nivalis88_18 %>% distinct(decimalLongitude,decimalLatitude, .keep_all= TRUE)

C_nivalis <- C_nivalis  %>% filter(scientificName == "Chionomys nivalis (Martins, 1842)")

C_nivalis_thin <- C_nivalis %>% dplyr::select(decimalLatitude, decimalLongitude, species)


thin(
  loc.data= C_nivalis_thin,
  lat.col = "decimalLatitude",
  long.col = "decimalLongitude",
  spec.col = "species",
  thin.par= 10,
  reps= 5,
  locs.thinned.list.return = FALSE,
  write.files = TRUE,
  max.files = 1,
  out.dir= "Data/occ",
  out.base = "svthin",
  write.log.file = FALSE,
  log.file = "spatial_thin_log_sv.txt",
  verbose = TRUE
)










##### adding to it and trying a thinning with another nivalis set:
#GBIF.org (23 January 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.zvw5p4


C_nivalis <- cnivalis81_10 %>% distinct(decimalLongitude,decimalLatitude, .keep_all= TRUE)

C_nivalis <- C_nivalis  %>% filter(scientificName == "Chionomys nivalis (Martins, 1842)")

C_nivalis_thin <- C_nivalis %>% dplyr::select(decimalLatitude, decimalLongitude, species)



thin(
  loc.data= C_nivalis_thin,
  lat.col = "decimalLatitude",
  long.col = "decimalLongitude",
  spec.col = "species",
  thin.par= 2,
  reps= 5,
  locs.thinned.list.return = FALSE,
  write.files = TRUE,
  max.files = 1,
  out.dir= "Data/occ",
  out.base = "svthin_V2_2km",
  write.log.file = FALSE,
  log.file = "spatial_thin_log_sv.txt",
  verbose = TRUE
)



#### same then for the same period (1981-2010) for clethrionois glareolus 
# GBIF.org (23 January 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.umd4dg


C_glareolus <- cglareolus81_10%>% distinct(decimalLongitude,decimalLatitude, .keep_all= TRUE)


C_glareolus <- C_glareolus %>% filter(scientificName == "Myodes glareolus (Schreber, 1780)" | 
                                        scientificName == "Clethrionomys glareolus (Schreber, 1780)")

C_glareolus_thin <-C_glareolus %>% dplyr::select(decimalLatitude, decimalLongitude, species)



### spThin



thin(
  loc.data= C_glareolus_thin,
  lat.col = "decimalLatitude",
  long.col = "decimalLongitude",
  spec.col = "species",
  thin.par= 2,
  reps= 5,
  locs.thinned.list.return = FALSE,
  write.files = TRUE,
  max.files = 1,
  out.dir= "Data/occ",
  out.base = "bvthin_V2_2km",
  write.log.file = FALSE,
  log.file = "spatial_thin_log_bv.txt",
  verbose = TRUE
)



#################


####doing it all again with occurrences from 2000 - 2020

C_glareolus <- cglar00_20%>% distinct(decimalLongitude,decimalLatitude, .keep_all= TRUE)


C_glareolus <- C_glareolus %>% filter(scientificName == "Myodes glareolus (Schreber, 1780)" | 
                                        scientificName == "Clethrionomys glareolus (Schreber, 1780)")

C_glareolus_thin <-C_glareolus %>% dplyr::select(decimalLatitude, decimalLongitude, species)

thin(
  loc.data= C_glareolus_thin,
  lat.col = "decimalLatitude",
  long.col = "decimalLongitude",
  spec.col = "species",
  thin.par= 10,
  reps= 5,
  locs.thinned.list.return = FALSE,
  write.files = TRUE,
  max.files = 1,
  out.dir= "Data/occ",
  out.base = "bvthin_V3_10km",
  write.log.file = FALSE,
  log.file = "spatial_thin_log_bv.txt",
  verbose = TRUE
)





## for snow vole 


C_nivalis <- cniv00_20 %>% distinct(decimalLongitude,decimalLatitude, .keep_all= TRUE)

C_nivalis <- C_nivalis  %>% filter(scientificName == "Chionomys nivalis (Martins, 1842)")

C_nivalis_thin <- C_nivalis %>% dplyr::select(decimalLatitude, decimalLongitude, species)



thin(
  loc.data= C_nivalis_thin,
  lat.col = "decimalLatitude",
  long.col = "decimalLongitude",
  spec.col = "species",
  thin.par= 5,
  reps= 5,
  locs.thinned.list.return = FALSE,
  write.files = TRUE,
  max.files = 1,
  out.dir= "Data/occ",
  out.base = "svthin_V3_5km",
  write.log.file = FALSE,
  log.file = "spatial_thin_log_sv.txt",
  verbose = TRUE
)



