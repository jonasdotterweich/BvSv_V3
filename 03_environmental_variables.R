### loading the climate data


library(raster)
library(sf)
library(dismo)
library(envirem)




#loading the study area. using V3 in this case

study_area <- read_sf("Data/studar/studarSV_V3.shp")


# Set the working directory to where your TIFF files are manually

setwd("~/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio")

# List all TIFF files
tif_files <- list.files(pattern = "\\.tif$")



# Check extents
for(file in tif_files) {
  r <- raster(file)
  print(paste(file, extent(r)))
}


## workinhg on assigning an extent
#taking the one of studar SV 
#extent of studar SV
ext <- extent(study_area)

### loading all at once didnt work due to extent issues now loading the bios first

### -- Creating the BIOstack -- ###

bio_files <- list.files(pattern = "CHELSA_bio[0-9]+_.*\\.tif$")

num_extract <- as.numeric(gsub("CHELSA_bio([0-9]+)_.*\\.tif$", "\\1", bio_files))

bio_files <- bio_files[order(num_extract)]


bio_stack <- stack(lapply(bio_files, raster))

## giving the bios a newer name

# Define the new names for each layer
new_names <- paste0("BIO", 1:19)

## doing it while ckecking if lenght is the same

#if(length(bio_stack) == length(new_names)) {
#  names(raster_stack) <- new_names
#} else {
#  warning("The number of layers and new names do not match!")
#}
### not really sure why this was giving the error message


#continuing simple

names(bio_stack) <- new_names

## with that the biostack is created

## biostack to extent

bio_croped <- crop(bio_stack, ext)

bio_ma <- mask(bio_croped, study_area)

bio_rast <- rast(bio_ma)

### trying to create the envirems  ### 


## first, loaading the data to create the envirms with envirem package

# precipitation (used here 1981-2010 monthly means)

## set wd manually to folder with files

setwd("~/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/pr")

prec_files <- list.files(pattern = "\\.tif$")

prec_stack <- stack(lapply(prec_files, raster))

prec_stack <- crop(prec_stack, ext)

names(prec_stack)

prec_names <- c("precip_01", "precip_02", "precip_03", "precip_04", "precip_05", "precip_06", "precip_07", "precip_08", "precip_09", "precip_10", "precip_11", "precip_12")

names(prec_stack) <- prec_names

# guess not mandatory to change file names

### question remains if this is the right precipitation data to work with


##

# temperature (used here 1981-2010 monthly means)

## set wd manually to folder with files

setwd("~/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/tasmax")

tmax_files <- list.files(pattern = "\\.tif$")

tmax_stack <- stack(lapply(tmax_files, raster))

tmax_stack <- crop(tmax_stack, ext)

tmax_names <- c("tmax_01", "tmax_02", "tmax_03", "tmax_04", "tmax_05", "tmax_06", "tmax_07", "tmax_08", "tmax_09", "tmax_10", "tmax_11", "tmax_12")

names(tmax_stack) <- tmax_names



## set wd manually to folder with files

setwd("~/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/tasmin")

tmin_files <- list.files(pattern = "\\.tif$")

tmin_stack <- stack(lapply(tmin_files, raster))

tmin_stack <- crop(tmin_stack, ext)

tmin_names <- c("tmin_01", "tmin_02", "tmin_03", "tmin_04", "tmin_05", "tmin_06", "tmin_07", "tmin_08", "tmin_09", "tmin_10", "tmin_11", "tmin_12")

names(tmin_stack) <- tmin_names


#### creating the masterstack for envirem

masterstack <- stack(tmin_stack, tmax_stack, prec_stack)

masterstack_rast <- rast(masterstack)


### gettig also the potential evaporatio pet

## set wd manually to folder with files

setwd("~/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/pet")

pet_files <- list.files(pattern = "\\.tif$")

pet_stack <- stack(lapply(pet_files, raster))

pet_stack <- crop(pet_stack, ext)

#pet_names <- c("pet_01", "pet_02", "pet_03", "pet_04", "pet_05", "pet_06", "pet_07", "pet_08", "pet_09", "pet_10", "pet_11", "pet_12")

pet_names <- c("PET_01", "PET_02", "PET_03", "PET_04", "PET_05", "PET_06", "PET_07", "PET_08", "PET_09", "PET_10", "PET_11", "PET_12")

names(pet_stack) <- pet_names






### solar radiation 

## taking envirems formula instead of calculating from chelsa "Mean monthly surface downwelling shortwave flux in air" (this would be the nearest to solrad from data directly)

BIO1 <- bio_croped$BIO1
# as it didnt work with the raster layer, is.lonlat missing I am converting it to a spatial raster

spatBIO1 <- rast(BIO1)

#is.lonlat(spatBIO1)

#it also did't work with the above even if is.lonlat returned true

extent_raster <- raster(nrows=908, ncols=2024, xmn=-1.616806, xmx=15.24986, ymn=40.88319, ymx=48.44986,
                        crs=4326)

## therefore a raster was created to give the extent and reference system
#as it didn't work as well conversion to spatial raster again

spat_ext_rast <- rast(extent_raster)

# the difference here was now that coord. ref. : was this -> lon/lat WGS 84 (EPSG:4326), instead of this -> +proj=longlat +datum=WGS84 +no_defs 

solrad <- ETsolradRasters(spat_ext_rast, 40, outputDir = NULL) # 60 indicates 2010, 40 1990  

#Error: [mask] mask raster has no values

crs(spatBIO1) <- crs(spat_ext_rast)

solrad <- ETsolradRasters(spatBIO1, 40)

names(solrad)

### creation of envirems

namingScheme()

verifyRasterNames(masterstack, solrad, pet_stack)

all <- generateEnvirem(masterstack_rast, solradstack = solrad, var = "all", tempScale = 10,
                       precipScale = 10)




#### Altitude #### 

setwd("~/Documents/R Project Bank Vole/Raster/raw/altitude")

altitude <-  brick("WC_alt_lonlat.tif")

##cropping masking altitude

altitude_cr <- crop(altitude, study_area)
altitude_ma <- mask(altitude_cr, study_area)

altitude_rast <- rast(altitude_ma)


#### Land cover data #### 

#this time also using copernicus landcover as it is 


setwd("~/Documents/Thesis project/uni_v2")

LC <- brick("Landcover/merged_LC19.tif")


setwd("~/Desktop/research_paper ")


##cropping masking resampling landcover

LC_cr <- crop(LC, study_area)
LC_ma <- mask(LC_cr, study_area)

LC_rast <- rast(LC_ma)

LC_re <- resample(LC_rast, bio_rast)

names(LC_re) <- "merged_LC"



### now I have to see if I it needs a brick or multilayer spat raster for the vif

## creating both


altitude_rast <- rast(altitude)

altitude_rast_re <- resample(altitude_rast, bio_rast)


### what I now need is the combination od Bioclim - Envirem - Altitude - Lancover


ENV_VARS <- c(bio_rast, all, altitude_rast_re, LC_re)

names(ENV_VARS)

# looking good 






library(usdm)

vif_ENV_vars <- vifstep(ENV_VARS, th = 10)

##### Warning message from first time 

#vif_ENV_vars <- vifstep(ENV_VARS, th = 10)
#Warning messages:
#  1: In summary.lm(lm(.dd[, i] ~ ., data = .dd[-i])) :
# essentially perfect fit: summary may be unreliable
#2: In summary.lm(lm(.dd[, i] ~ ., data = .dd[-i])) :
#  essentially perfect fit: summary may be unreliable
#3: In summary.lm(lm(.dd[, i] ~ ., data = .dd[-i])) :
#  essentially perfect fit: summary may be unreliable
#4: In summary.lm(lm(.dd[, i] ~ ., data = .dd[-i])) :
#  essentially perfect fit: summary may be unreliable
#5: In summary.lm(lm(.dd[, i] ~ ., data = .dd[-i])) :
#  essentially perfect fit: summary may be unreliable
#6: In summary.lm(lm(.dd[, i] ~ ., data = .dd[-i])) :
#  essentially perfect fit: summary may be unreliable
#7: In cor(x, method = method) : the standard deviation is zero
#> vif_ENV_vars
#26 variables from the 39 input variables have collinearity problem: 

# BIO5 continentality growingDegDays0 meanTempWarmest meanTempColdest BIO1 BIO11 thermicityIndex growingDegDays5 BIO6 BIO10 minTempWarmest annualPET BIO7 BIO12 BIO16 BIO9 BIO2 BIO17 PETWarmestQuarter embergerQ maxTempColdest BIO3 BIO14 BIO13 climaticMoistureIndex 

#After excluding the collinear variables, the linear correlation coefficients ranges between: 
#  min correlation ( PETWettestQuarter ~ PETColdestQuarter ):  -0.01843425 
#max correlation ( aridityIndexThornthwaite ~ BIO18 ):  -0.8668206 

#---------- VIFs of the remained variables -------- 
#  Variables      VIF
#1                      BIO4 2.447539
#2                      BIO8 7.213731
#3                     BIO15 1.997772
#4                     BIO18 8.700778
#5                     BIO19 3.640115
#6  aridityIndexThornthwaite 7.030680
#7        monthCountByTemp10 2.000015
#8         PETColdestQuarter 2.360092
#9          PETDriestQuarter 3.126116
#10           PETseasonality 2.884245
#11        PETWettestQuarter 6.017748
#12            WC_alt_lonlat 6.941700
#13                merged_LC 1.357598


##### CAUTION, NEEDS TO BE REGARDED; INTERPRETED AN CORRECTED

##### ----- #####




# second time running

## Waring message stating that one variable has standard deviation of 0

## Identifying that layer

# Apply the global function to compute the standard deviation for each layer
sds <- lapply(1:nlyr(ENV_VARS), function(i) {
  global(ENV_VARS[[i]], fun = sd, na.rm = TRUE)
})

# Check each layer's standard deviation
for (i in 1:length(sds)) {
  if (!is.null(sds[[i]]) && sds[[i]] == 0) {
    print(paste("Layer", names(ENV_VARS)[i], "has a standard deviation of 0"))
  }
}

## removing that layer
#to use function drop layers there needs to be a vonversion to raster and than back to spatrast

new_ENV_VARS <- stack(ENV_VARS)

new_ENV_VARS <- dropLayer(new_ENV_VARS, 31)

new_ENV_VARS <- rast(new_ENV_VARS)

names(new_ENV_VARS)


## doing the vif step again 

vif_new_ENV_vars <- vifstep(new_ENV_VARS, th = 10)

### looks like the problems I encountered the first time are not there anymore




bivars_to_keep <- exclude(new_ENV_VARS, vif_ENV_vars)


names(bivars_to_keep)


bivars_to_keep <- crop(bivars_to_keep, study_area)
bivars_to_keep <- mask(bivars_to_keep, study_area)



#saving 
setwd("~/Desktop/research_paper /paper_framework")

writeRaster(bivars_to_keep , "Data/biovars/biovars_VIFed_V3.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite=TRUE)

writeRaster(bivars_to_keep , "Data/biovars/biovars_VIFed_V3_forenames.grd", overwrite=TRUE)


