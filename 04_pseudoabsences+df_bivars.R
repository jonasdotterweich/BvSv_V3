### 5_pseudoabsences for the datasets


library(flexsdm)
library(raster)
library(terra)
library(sf)
library(dplyr)


#loading occurrences  BV

BV <- read_sf("Data/occ/bv_in_SVstudarV3red.shp")

BV_df <- dplyr::select(as.data.frame(BV), -geometry)

BV_lonlat <- dplyr::select(BV_df, decimalLon, decimalLat)

#loading occurrences  SV

SV <- read.csv("Data/occ/svthin_V3_5km_thin1.csv")

SV_lonlat <- dplyr::select(SV, decimalLongitude, decimalLatitude)

#SV2 <- read_sf("Data/occ/sv_thin_pts.shp")

#SV2_df <- dplyr::select(as.data.frame(SV2), -geometry)



#loading biovariables

biovars <- brick("Data/biovars/biovars_VIFed_V3.tif")


#assigning the names
#bivarnames <- brick("Data/biovars/biovars_VIFed_forenames.grd")

#names(biovars) <- names(bivarnames)


#creating a spatial raster

bivars_rast <- rast(biovars)

names(bivars_rast)



### pseudoabsences BV

elev <- bivars_rast$WC_alt_lonlat

pseudo_abs_env_BV <- sample_pseudoabs(
  data= BV_df, 
  x= "decimalLon",
  y= "decimalLat",
  n= nrow(BV),
  method= c("env_const", env= bivars_rast),
  rlayer= elev,
  maskval= NULL,
  sp_name= "Myodes glareolus")



### pseudoabsences SV

pseudo_abs_env_SV <- sample_pseudoabs(
  data= SV, 
  x= "decimalLongitude",
  y= "decimalLatitude",
  n= nrow(SV),
  method= c("env_const", env= bivars_rast),
  rlayer= elev,
  maskval= NULL,
  sp_name= "Chionomys nivalis")




### combining presences & abcences 

##for BV

BV_lonlat$pr_ab <- rep(1, nrow(BV_lonlat))

pseudoabsenv_BV_wo_spname <- dplyr::select(pseudo_abs_env_BV, decimalLon, decimalLat, pr_ab)

df_BV <- rbind(BV_lonlat, pseudoabsenv_BV_wo_spname)

write.csv(df_BV, "Data/to_model/df_BV_V3.csv", row.names = FALSE)


##for SV 

SV_lonlat$pr_ab <- rep(1, nrow(SV_lonlat))

pseudoabsenv_SV_wo_spname <- dplyr::select(pseudo_abs_env_SV, decimalLongitude, decimalLatitude, pr_ab)

df_SV <- rbind(SV_lonlat, pseudoabsenv_SV_wo_spname)

write.csv(df_SV, "Data/to_model/df_SV_V3.csv", row.names = FALSE)




#assigning to all the pres&abs the biovariables at the coordinate for BV

var_bv <- extract(biovars, as.matrix(df_BV[,1:2]))


#joining the df and the var together

df_var_BV <- cbind(df_BV, var_bv)

write.csv(df_var_BV, "Data/to_model/df_BV_V3+bivars.csv", row.names = FALSE)



#assigning to all the pres&abs the biovariables at the coordinate for SV

var_sv <- extract(biovars, as.matrix(df_SV[,1:2]))

#joining the df and the var together

df_var_SV <- cbind(df_SV, var_sv)

write.csv(df_var_SV, "Data/to_model/df_SV_V3+bivars.csv", row.names = FALSE)



#### check to omit the NA VALUEs WITHIN THE DATAframe

## probably due to deleting the one NA variable from the env stack there are no Na vals now in the stack. 


