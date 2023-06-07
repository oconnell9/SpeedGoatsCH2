##    CH 2 ANALYSIS            ##
##      SEPT 2022              ##
##  ERIN CATHERINE O'CONNELL   ##
#################################


#### THE PURPOSE OF THIS SCRIPT IS TO FIT iSSFs AND PLOT HABITAT MAPS ###


#######################
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c('amt','elevatr','espa.tools','getPass','httr','jsonlite','geojsonio','geojsonR','rgdal',
              'sp','RODBC','raster','rasterVis','RColorBrewer','sf','ggplot2','getPass',
              'dplyr','tidyr','readr','lubridate','plotly',
                'sf','ggplot2','dplyr','tidyr','lubridate','plotly', 'glmmTMB',
                'ResourceSelection', 'animation')

### More packages may be added to the previous vector as needed ###

ipak(packages)
####################

#START:


#### PASTURE BOUNDARES ###
rb.aoi <- vect('/Users/eco20il/Downloads/Rocker_B_Pastures/Rocker_B_Pastures.shp')  

#### INSERT REMOTE SENSING DATA #####
dist2r<-rast("/Volumes/big_game/Erin_OConnell/RemoteSensing/eCog/EDR_Ex1.tif") #roads
dist2w<-rast("/Volumes/big_game/Erin_OConnell/RemoteSensing/eCog/EDW_Ex1.tif")  #wells
#30M RES
dist2r<- rast("/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/EDR_30m.tif")
dist2w<-rast("/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/EDW_30m.tif")
#############################################################################################







 
    # DATA CLEANING 
    idList <- unique(dat_gps_all$animal_id)
    bad_ids<- c("ANAM-2019-868", "ANAM-2019-853")
    idList <- idList[which(idList %in% bad_ids == FALSE)] # Had to exclude messed up residents so now n=29

    files <- as.data.frame(list.files('/Volumes/big_game/Erin_OConnell/Thesis/CH 2/CH2/tracks/', pattern = '.csv'))
    files <- as.data.frame(files)
    colnames(files) <- "file" 
   
#which(files == file) # For finding problem children
### Empty list to store model results

        prongMods <- list()
        prongMods <- vector(mode = 'list', length = length(files$file))#$file)) # Create empty models list to populate with for loop
        names(prongMods) <- substr(files$file, 1,13) 
        
        
        ##################
        uhcPlots<- FALSE
        fitMods <- TRUE
        ###-------------###
        
        for(x in files$file){ # The removed elements are the problem children above
          
          tryCatch({
           
            trax <- read.csv(paste0("/Volumes/big_game/Erin_OConnell/Thesis/CH 2/CH2/tracks/",x), header = TRUE)
            #trax<-trax[,c(1,3,2,4,5, 6, 7, 8, 9, 10, 11, 12, 13, 14,16, 15, 17, 18, 19, 20, 21, 22)]
            #trax[,16:22] <- apply(trax[,16:22], MARGIN = 2, FUN = scale)
            
            # add animal_id, not sure why its not in the .csv already, will need to check in the Land_Locs_Matching script
            # trax1$animal_id <- substr(i, 1,13)
            
            ## was in code when using shapefiles vect(as_sf_points(
            # cotrax <- cbind(trax1, terra::extract(demStack, 
            #    trax1[, c('x2_', 'y2_')]))
            
            
            ### Set maximum number of iterations
            max.iter = 40
            
            ### Select an individual ###
            #file <- files[1]
          
         
            
            ### Diagnostic (UHC) Plots ###--------------------------------- 
            
            if(uhcPlots == TRUE){
              tiff(file=paste0("UHC_Plots/",trax$animal_id[1],".tiff"),
                   width=6, height=6.5, units="in", res=300)
              par(mfrow=c(3,2))
              for(col in 28:ncol(trax)){
                #col = 28
                used <- data.frame(density(trax[trax$case_ == FALSE & 
                                                  is.na(trax[,col]) == FALSE,
                                                col])[c('x','y')])
                available <- data.frame(density(trax[trax$case_ == TRUE& 
                                                       is.na(trax[,col]) == FALSE,
                                                     col])[c('x','y')])
                xlim <- c(min(c(available$x,used$x)),max(c(available$x,used$x)))
                ylim <- c(min(c(available$y,used$y)),max(c(available$y,used$y)))
                plot(available, type = 'l', xlim = xlim, ylim = ylim, 
                     main = '', 
                     xlab = colnames(trax)[col], ylab = 'Density')
                lines(used, col = 'red')
              }
              mtext(paste('UHC Plot:',trax$animal_id[1]), 
                    side = 3, line = -2.5, outer = TRUE,
                    cex = 1.5, font = 2)
              legend(-40,-0.30,legend = c("Used", "Available"), col = c("red","black"), 
                     lwd = 1, xpd = NA, horiz = TRUE, cex = 1, seg.len=1, bty = 'n')
              par(mfrow=c(1,1))
              dev.off()
              
              rm(used,available,xlim,ylim)
            }
            
######-------------------------
    
    #if(fitMods == TRUE){

            
            modList <- list()
            
            ###-------------------------------------###
            ### Models without Movement Constraints ###
            ###-------------------------------------###
            
            modList[[x]]$M1 <- fit_issf(data = trax,
                                                case_ ~ sl_ + log(sl_) +
                                                  I(elev*elev) + elev + slope +
                                                  I(msavi*msavi) + msavi + strata(step_id_) +
                                                  dist2road + dist2well,
                                                  iter.max = max.iter)
            
            # modList[['U.DblOpt.M']] <- fit_issf(data = trax,
            #                                     case_ ~ sl_ + log(sl_) +
            #                                       I(elev^2) + elev + slope +
            #                                       I(msavi^2) + msavi + strata(step_id_),
            #                                       iter.max = max.iter)
            
            modList[[x]]$M2 <- fit_issf(data = trax,
                                                case_ ~ sl_ + log(sl_) +
                                                  I(elev*elev) + elev +
                                                  slope + msavi + strata(step_id_) +
                                                  dist2road + dist2well,
                                                  iter.max = max.iter)
            
            ###----------------------------------###
            ### Models with Movement Constraints ###
            ###----------------------------------###
            
            modList[[x]]$M3 <- fit_issf(data = trax,
                                                case_ ~ sl_ + log(sl_) + sl_*slope + sl_*msavi + log(sl_)*msavi + cos(ta_) +
                                                  I(elev*elev) + elev + slope +            
                                                  I(msavi*msavi) + msavi + strata(step_id_) +
                                                  dist2road + dist2well,
                                                  iter.max = max.iter)
            
            modList[[x]]$M4 <- fit_issf(data = trax,
                                                case_ ~ sl_ + log(sl_) + sl_*slope + sl_*msavi + log(sl_)*msavi +cos(ta_) +
                                                  I(elev*elev) + elev + slope +            
                                                  I(msavi*msavi) + msavi + strata(step_id_) +
                                                  dist2road + dist2well,
                                                  iter.max = max.iter)
            
            modList[[x]]$M5 <- fit_issf(data = trax,
                                                 case_ ~ sl_ + log(sl_) + sl_*slope + cos(ta_) +
                                                   I((elev*elev)) + elev + slope + msavi + strata(step_id_) +
                                                   dist2road + dist2well,
                                                   iter.max = max.iter)
            
            ###---------------------------------------###
            ### Models with time-varying coefficients ###
            ###---------------------------------------###
            
            week <- cyclic_encoding(as_datetime(trax$t1_), 'week')
            
            
            day <- cyclic_encoding(as_datetime(trax$t1_), 'day')
            
            ### Diel rythm
            modList[[x]]$M6 <- fit_issf(data = trax,
                                            case_ ~  sl_ + log(sl_) + cos(ta_) +
                                              sl_:day[,1] + sl_:day[,2] + 
                                              I(msavi*msavi) + elev + slope +            
                                              I(msavi*msavi) + msavi + strata(step_id_) +
                                              dist2road + dist2well,
                                              iter.max = max.iter)
            
            ### Seasonality of step 
            modList[[x]]$M7 <- fit_issf(data = trax,
                                            case_ ~  sl_ + log(sl_) + cos(ta_) +
                                              sl_:week[,1] + sl_:week[,2] + 
                                              I(msavi*msavi) + elev + slope +            
                                              I(msavi*msavi) + msavi + strata(step_id_)+
                                              dist2road + dist2well,
                                              iter.max = max.iter)
            
            
            
            # modList[[x]]$M8 <- fit_issf(data = trax,
            #                             case_ ~ dist2road + dist2well,
            #                             iter.max = max.iter)
            
          
            # TIME VARYING SEL
            ### Seasonality of steps and MSAVI selection
            modList[[x]]$M8 <- fit_issf(data = trax,
                                                 case_ ~ sl_*week[,1] + sl_*week[,2] + log(sl_) +
                                                   elev + slope + msavi +
                                                   msavi*week[,1] + msavi*week[,2] + strata(step_id_) +
                                                   dist2road + dist2well,model = T)


      
      print(c(x,'/',nrow(files)))#,rownames(file_names_df)[j])) , Sys.time()
      #rm(cotrax)
      # compare models
      modList[['AIC']] <- t(sapply(modList, function(j){sapply(j, AIC)}))#t(sapply(models, AIC))
      prongMods[[trax$animal_id[trax$animal_id == substr(x, 1,13)][1]]] <- modList
          
    }
)}
      
    beepr::beep(8) 

    #write.csv(prongMods,"/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/mods/prongMods.csv")
######################################################################################################
    
    
    #### START AIC SHEET ####
    

    
        AIC_df <- as.data.frame(prongMods[[1]][2])
        #Works
        for (i in names(prongMods)) {
          AIC_df <- rbind(AIC_df,  as.data.frame(prongMods[[i]][2]))
        }
        
        # add all AIC for each model
        cumulative <- colSums(AIC_df)
        AIC_df <- rbind(AIC_df, cumulative);rm(cumulative)
        rownames(AIC_df)[rownames(AIC_df) == "28"] <- "cumulative AIC"
        # pull the cumulative row to make table for thesis
        AICs <- as.data.frame(t(AIC_df[28,]))
        colnames(AICs) <- "Cumulative AIC"
        write.csv(AICs, file = "/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/mods/prongMods_AICs.csv")
        
        # get the top model
        bestMod <- colnames(AIC_df)[which.min(AIC_df['cumulative',])]
        # get the coefficients
        PH.coef <- as.data.frame(coef(prongMods[[1]][[1]]$M6)) #weird ?
        colnames(PH.coef) <- paste(names(prongMods)[1], "M6")
        
        
        library(purrr)
        #prongMods %>% discard(is.null) purrrrr
        #prongMods[lengths(prongMods) != 0]
        prongMods<-compact(prongMods)
        
        for (ids in names(prongMods)) {
          PH.coef <- cbind(PH.coef, as.data.frame(coef(prongMods[[ids]][[1]]$M6)))
        }
        colnames(PH.coef) <- paste(names(prongMods), "M6")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### NOTES AND ISSUES : 
# Need to figure out who NA is... also need to figure out why we went from 29 IDS to 27??
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
        # FLIP df 
        PH.coef <- t(PH.coef)
    
        ## add row for mean of columns
        PH.coef <- rbind(PH.coef, t(as.data.frame(colMeans(PH.coef))))
        # add row for mean RESIDENTS
        PH.coef <- rbind(PH.coef, t(as.data.frame(colMeans(PH.coef[c(1:6),]))))
        # add row for TRANSLOCATED ALIENS
        PH.coef <- rbind(PH.coef, t(as.data.frame(colMeans(PH.coef[c(7:27),]))))
        # change the name to make sense
        rownames(PH.coef)[rownames(PH.coef) == "colMeans(PH.coef)"] <- "Col_Means"
        rownames(PH.coef)[rownames(PH.coef) == "colMeans(PH.coef[c(1:6), ])"] <- "RESIDENT_MEANS"
        rownames(PH.coef)[rownames(PH.coef) == "colMeans(PH.coef[c(7:27), ])"] <- "TRANSLOCATED_MEANS"
        # write .csv of coefficients 
        write.csv(PH.coef, '/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/mods/_bestCoefs.csv', row.names = T)
        #write.csv(PH.coef, '/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/mods/_bestCoefsSCALED.csv', row.names = T)     
        
        PH.coef2<-PH.coef
        
        # TO SUBSET FOR MAPS AND PRETTY TABLES 
        PH.coefs <- read.csv('/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/mods/_bestCoefs.csv')
        # subset PH.coefs for each animal or the mean 
        PH.coef <- PH.coefs[PH.coefs$X == "Col_Means",]
        PH.coef <- PH.coefs[PH.coefs$X == "RESIDENT_MEANS",]
        PH.coef <- PH.coefs[PH.coefs$X == "TRANSLOCATED_MEANS",]
     
       
        
        ## make a prettier table for the coefficients 
        Coef.Tbl <- xtable(PH.coef, caption = "Coefficients of Each Parameter in the Top Model")
###########################################################################################################   
        # CORRECTIONS FOR MOVEMENT COVARIATES ####
        
        PH.coefs <- read.csv('/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/mods/_bestCoefs.csv')
        files <- as.data.frame(list.files('/Volumes/big_game/Erin_OConnell/Thesis/CH 2/CH2/tracks/', pattern = '.csv'))##
        colnames(files) <- "file"
        PH.coefs$cor_sl <- 1
        PH.coefs$cor_sd <- 2
        PH.coefs$cor_ta <- 3
        #i= "ANAM-2020-20R.csv" # use for trouble shooting inside loop
        for(i in files$file){
          trax <- read.csv(paste0("/Volumes/big_game/Erin_OConnell/Thesis/CH 2/CH2/tracks/",i), header = TRUE)
          # calculate corrected sl mean
          cor_sl <- colMeans(trax[,6:7])[[2]]
          cor_sl <- cor_sl +  PH.coefs[PH.coefs$X == paste(strsplit(i,".csv")[[1]][1], "M6"),][2]
          PH.coefs[ PH.coefs$X == paste(strsplit(i,".csv")[[1]][1], "M6"),]$cor_sl <- as.numeric(cor_sl$sl_)
          
          # calculate corrrected sl stan dev
          cor_sd <- sd(trax$sl_)
          cor_sd <- cor_sd +  PH.coefs[ PH.coefs$X == paste(strsplit(i,".csv")[[1]][1], "M6"),][3]
          PH.coefs[ PH.coefs$X == paste(strsplit(i,".csv")[[1]][1], "M6"),]$cor_sd <- as.numeric(cor_sd$log.sl_.)
          
          # calculate corrected ta stan dev
          cor_ta <- sd(trax$ta_[is.na(trax$ta_) == F])
          cor_ta <- cor_ta +  PH.coefs[ PH.coefs$X == paste(strsplit(i,".csv")[[1]][1], "M6"),][4]
          PH.coefs[ PH.coefs$X == paste(strsplit(i,".csv")[[1]][1], "M6"),]$cor_ta <- as.numeric(cor_ta$cos.ta_.)
          rm(cor_sl,cor_sd, cor_ta)
      }
        
        # add average pronghorn values
        PH.coefs[28,13] <- colMeans(PH.coefs[13:15])[[1]]
        PH.coefs[28,14] <- colMeans( PH.coefs[13:15])[[2]]
        PH.coefs[28,15] <- colMeans( PH.coefs[13:15])[[3]]
        
        # RESIDENTS
        PH.coefs[29,13] <- colMeans(PH.coefs[c(1:6),13:15])[[1]]
        PH.coefs[29,14] <- colMeans(PH.coefs[c(1:6),13:15])[[2]]
        PH.coefs[29,15] <- colMeans(PH.coefs[c(1:6),13:15])[[3]]
        
        # TRANSLOCATED 
        PH.coefs[30,13] <- colMeans(PH.coefs[c(7:27),13:15])[[1]]
        PH.coefs[30,14] <- colMeans(PH.coefs[c(7:27),13:15])[[2]]
        PH.coefs[30,15] <- colMeans(PH.coefs[c(7:27),13:15])[[3]]
        
        # write .csv of coefs with corrections
        write.csv(PH.coefs, '/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/mods/cor_bestCoefs.csv', row.names = T)
        
###########################################################################################################   
        
        
        
        # Taken from my "T2.R" Script 
        # Remember, Updated AOI for RB. Now, we remote sensed a 700,000 ac^2 area
        dem <- terra::rast('/Users/eco20il/Downloads/n31_w102_1arc_v3.tif')
        dem <- terra::project(dem, "EPSG: 26914")
        aoi<- terra::vect('/Volumes/big_game/Erin_OConnell/BB.shp')
        aoi<-terra::project(aoi, "EPSG: 26914")
        dem <- terra::crop(dem, aoi)
        dem <- terra::mask(dem, aoi)
        slope <- terra::terrain(dem, v = 'slope', unit = 'degrees')
        ruggedness <- terra::terrain(dem, v = 'TRI')
        position <- terra::terrain(dem, v = 'TPI')
        
        # USE THIS IF EXTENTS NEED TO MATCH #
        # plot(dem)
        # plot(aoi, add = T)
        # new_ex<- ext(aoi)
        # dem<-terra::crop(dem, new_ex)
        # stack.msavi <- terra::project(stack.msavi, crs(rb.aoi)) # UTMZ14N
        # stack.msavi<-terra::crop(stack.msavi, aoi)
        # ext(dem)<- ext(stack.msavi$`2020-1`) # This Finally Worked : Needed to match extents
        # ext(dist2r)<- ext(dem) # Works
        # ext(dist2w)<- ext(dem)
        # 
        
        demStack <- c(dem, slope,ruggedness, position)#, cosAsp, sinAsp, sq_slope, sq_TRI) # stack covs
        rm(dem, slope, ruggedness, position)
        names(demStack) <- c('elev', 'slope', 'TRI', 'TPI')
        
        # Recover stack if needed
        files <- list.files('/Volumes/SpatialData/landsatARD/RB_ECO/msavi/Updated')
        
        stack.msavi <- rast(paste('/Volumes/SpatialData/landsatARD/RB_ECO/msavi/Updated', files[1], sep = '/'))
        
        for(i in 2:length(files)){
          add(stack.msavi) <- rast(paste('/Volumes/SpatialData/landsatARD/RB_ECO/msavi/Updated', files[i], sep = '/'))
        }  
        
        # Get things in order
        
        time(stack.msavi) <- as_date(paste0(names(stack.msavi),'-15'))
        stack.msavi<- stack.msavi[[order(time(stack.msavi))]]
        
        # Brute force correction for implausible values
        values(stack.msavi)[values(stack.msavi) < -0.1] <- NA 
        
        
        #DUMB ROW AND COLUMN THING 
        test <- resample(dist2r, demStack$elev)
        r1 <- resample(dist2r, demStack$elev)
        w1<- resample(dist2w, demStack$elev)
        s<-resample(stack.msavi$`2020-8`, demStack$elev)
        s<-resample(stack.msavi, demStack$elev)
###########################################################################################################   
    
        PH.PRED.MAP <- (PH.coef$elev*(demStack$elev) + 
                          PH.coef$slope*demStack$slope +
                          PH.coef$dist2road*r1 +
                          PH.coef$dist2well*w1 +
                          PH.coef$msavi * s) #when I used msavi there were issues
        
        # Q: Do we need to exp?
        PH.PRED.MAP <-  (PH.coef$elev*(demStack$elev) + 
                          PH.coef$slope*demStack$slope +
                           PH.coef$dist2road*r1) #works
        
        # SIMPLE
        PH.PRED.MAP <-  (PH.coef$elev*(demStack$elev) + 
                           PH.coef$slope*demStack$slope)
        
        PH.PRED.MAP <-  (PH.coef$dist2road*r1 +
                           PH.coef$dist2well*w1) #WELLS+ROADS
        
        PH.PRED.MAP <-  (PH.coef$dist2road*r1 +
                           PH.coef$dist2well*w1 +
                           PH.coef$msavi * s) #+ msavi
        
        
        PH.PRED.MAP <- PH.PRED.MAP/sum(values(PH.PRED.MAP), na.rm = T)
        
        
        # Need TidyTerra for this but the MAC in lab wont allow me 
        PHMAP <- ggplot() + 
          geom_spatraster(data = PH.PRED.MAP) +
          geom_sf(data = st_geometry(rb.aoi), fill = NA, color = 'green') +
          scale_fill_viridis(option = 'magma') + ggtitle("TRANSLOCATED PH Habitat Quality avg")
        PHMAP
        
      
        # BASIC PLOT
        plot(PH.PRED.MAP)
          lines(rb.aoi, col= "black", lwd= 2.5)
          
###########################################################################################################
          
          
          # Scale coef so we can compare them 
          PH.coefs[,2:15] <- scale(PH.coefs[,2:15])        

          
###########################################################################################################          
      # read in csv
          
      PH.coefs <- read.csv('/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/mods/cor_bestCoefs.csv')
      # subset PH.coef new df for each animal or the mean 
      PH.coef <- PH.coefs[PH.coefs$X == "Col_Means",]
      PH.coef <- PH.coefs[PH.coefs$X == "RESIDENT_MEANS",]
      PH.coef <- PH.coefs[PH.coefs$X == "TRANSLOCATED_MEANS",]
     
      
#### LOOP each month msavi and save raster ## save the set for each groups selection ###
      #i <- "2020-1"
      #### SHOWS HOW HABITAT CHANGES  
      for (i in names(stack.msavi)){
        PH.PRED.MAP <- (PH.coef$elev*(demStack$elev) + 
                          PH.coef$slope*demStack$slope +
                          PH.coef$dist2road*r1 +
                          PH.coef$dist2well*w1 +
                          PH.coef$msavi*(s[[i]])) #should be 29 maps
        
        
        
        
        # shows where animals are on landscape
        #  PH.PRED.MAP<-  PH.PRED.MAP/sum(values( PH.PRED.MAP, na.rm = TRUE))
        
        terra::writeRaster( PH.PRED.MAP,paste0("/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/mods/habitatMapsALL/",i, ".TIF"), overwrite = T)
        print(paste(i, "Added to mods folder"))
        rm(i,  PH.PRED.MAP)
      }      
  
      
      # MAKE HAB STACK
      gif_files <- list.files("/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/mods/habitatMapsALL")
      hab_stack <- terra::rast("/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/mods/habitatMapsALL/2020-1.TIF")
      names(hab_stack) <- tools::file_path_sans_ext(gif_files[1])
      for (i in 2: length(gif_files)) {
        add(hab_stack) <- terra::rast(paste0("/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/mods/habitatMapsALL/",gif_files[i]))
        names(hab_stack[[i]]) <- tools::file_path_sans_ext(gif_files[i])
      }
      
    
      
      time(hab_stack) <- as_date(paste0(names(hab_stack),'-15'))
      hab_stack<- hab_stack[[order(time(hab_stack))]]
      #animate(hab_stack,  col = cl(200), range = c(-.1,.7), pause = 0.5)
      animate(hab_stack, pause = 0.8)
      
      
      # MAKE GIF 
      saveGIF({
        for(i in 1:nlyr(hab_stack)){
          PHMAPALL <- ggplot() + 
            geom_spatraster(data = exp(hab_stack[[i]])) +
            geom_sf(data = st_geometry(rb.aoi), fill = NA, color = 'black') +
            scale_fill_viridis(option = 'turbo', direction = -1, limits = c(0.9999995977,1.000000458)) + ggtitle(names(hab_stack[[i]])) #+ lims(values = c("0","1"))
          print(PHMAPALL)
        }
      }, movie.name = 'allspeedgoats.gif')
      
      
      # Works on MAC in LAB
      saveGIF({
        for(i in 1:nlyr(hab_stack)){
          plot(hab_stack[[i]], range = c(-.1,.7),
               main = paste('Rocker b Dynamic Habitat Selection',stringr::str_remove(names(hab_stack), 'x')[i]))
          plot(rb.aoi, add = T)
        }
      }, movie.name = 'allspeedgoats.gif')
      
      
      # was saving the gif to documents folder.. 
    
    # 
    # saveGIF({
    #   for(i in 1:nlyr(hab_stack)){
    #     PHMAP<-ggplot() +
    #       geom_raster(data = hab_stack[[i]])+
    #       geom_sf(data = st_geometry(rb.aoi), fill = NA, color = 'black') +
    #       scale_fill_viridis(option = 'turbo', direction = -1, limits = c(0.9999995977,1.000000458)) + ggtitle(names(hab_stack[[i]])) #+ lims(values = c("0","1"))
    #     print(PHMAP)
    #   }
    # }, movie.name = 'allspeedgoats.gif')
      
      # Old Way of doing this####
      # tab_aic <- t(sapply(prongMods, function(k){k$AIC}))
      # cumulative <- colSums(tab_aic)
      # tab_aic <- rbind(tab_aic, cumulative);rm(cumulative)
      # bestMod <- colnames(tab_aic)[which.min(tab_aic['cumulative',])]
      # 
      # coefs <- t(sapply(prongMods, function(x){x[[bestMod]]$model$coefficients}))
      # write.csv(t(data.frame(colMeans(coefs))), '/Users/eco20il/Desktop/_bestCoefs.csv', row.names = F)    
      # 
      # topCoefs <- colMeans(coefs)
      # 
      # predMap <- exp(topCoefs['I(elev^2)']*(stack$elev^2) + 
      #                  topCoefs['elev'] *stack$elev  +
      #                  topCoefs['slope']*stack$slope +
      #                  topCoefs['roughness']*stack$roughness)
      
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
                      #### NICHE ####
      # 21T,6RES (10SEPT2022)
      library(plotly)
      
      ### Pull data ### ** CORRECTED COEFS ADDED *** 
      dat <- read.csv('/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/mods/cor_bestCoefs.csv')
      dat <- read.csv('/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/mods/_rawCoefs.csv')
      # adding release method to the dataframe
      RESIDENT <- c(1:6)
      TRANSLOCATED <- c(7:27)
      dat$behavior <- "NA"
  
      
      dat$behavior[1:6] <- "RESIDENT"
      dat$behavior[7:27] <-"TRANSLOCATED"
      dat$behavior[28] <- "OVERALLAVG"
      dat$behavior[29] <- "AVGRES"
      dat$behavior[30] <- "AVGTRANS"
      
      ### Ordinate the scaled HABITAT coefs # 5:9 OLD
      ord <- princomp(scale(dat[,6:11]))
      ord.sum <- summary(ord)
      
      ### Ordinate the scaled MOVEMENT coefs
      ord <- princomp(scale(dat[c(3,4,5,12,13,14,15,16)]))
      ord.sum <- summary(ord)
   
      
      #START
      plotDat <- data.frame(ord$scores[,1:3])
      plotDat$behavior <- dat$behavior
      
      # centDat <- aggregate(plotDat[,1:3], list(plotDat$species), mean)
      #   centDat$Group.1 <- paste0(centDat$Group.1,' Cent.')
      #   names(centDat)[1] <- 'species'
      #plotDat <- rbind(plotDat, centDat)
      
      comps <- ord.sum$sdev[1:3]^2/sum(ord.sum$sdev^2)
      
      niche <-  plot_ly(plotDat,x = plotDat$Comp.1, y = plotDat$Comp.2, z = plotDat$Comp.3,
                        color = plotDat$behavior, colors = c('purple','darkblue', "black", 'blue', 'red'))  
      niche <- add_markers(niche)
      
      
      # niche <- layout(niche, title = paste0('Niche Space (', format(sum(comps)*100,digits = 3),
      #                                       ' % of Total Variation)'),
      #                 scene = list(xaxis = list(title = paste0('Component 1 (', 
      #                                                          format(comps[1]*100,digits = 3),' %)')),
      #                              yaxis = list(title = paste0('Component 2 (', 
      #                                                          format(comps[2]*100,digits = 3),' %)')),
      #                              zaxis = list(title = paste0('Component 3 (', 
      #                                                          format(comps[3]*100,digits = 3),' %)'))))
      
      
      niche <-  plot_ly(plotDat,x = plotDat$Comp.1, y = plotDat$Comp.2, z = plotDat$Comp.3,
                                 color = plotDat$behavior, colors = c('purple','darkblue', "black", 'blue', 'red'))   %>%
  
      add_markers() %>%
      layout(title = list(text = paste0('PCA of Translocated and Resident Pronghorn \n (', 
                                        format(sum(comps)*100,digits = 2),
                                        '% of Total Variation)'),
                          font = list(size = 25, family = 'Times New Roman'),
                             y = 0.93),
                          font = list(family = 'Times New Roman',
                          size = 15),
             
             scene = list(xaxis = list(title = paste0('Component 1')),
                          yaxis = list(title = paste0('Component 2')),
                          zaxis = list(title = paste0('Component 3'))))
      niche
      
      
      
      variance = ord$sdev^2/sum(ord$sdev^2)
      
      # Scree plot
      qplot(c(1:6), variance) +
        geom_line() +
        geom_point(size=4)+
        xlab("Principal Component") +
        ylab("Variance Explained") +
        ggtitle("Scree Plot") +
        theme(text=element_text(size=16, family="Times New Roman")) +
        theme(plot.title = element_text(hjust = 0.5)) +
        ylim(0, 1)
      
      
      require(ggbiplot)
      ggbiplot(ord)
      
      
      
      
      biplot = ggbiplot(pcobj = ord,
                        choices = c(1,2),
                        obs.scale = 1, var.scale = 1,  # Scaling of axis
                        labels = row.names(dat),     # Add labels as rownames
                        labels.size = 4,
                        varname.size = 5,
                        varname.abbrev = TRUE,  # Abbreviate variable names (TRUE)
                        var.axes = TRUE,      # Remove variable vectors (TRUE)
                        circle = FALSE,        # Add unit variance circle (TRUE)
                        ellipse = TRUE, groups = dat$behavior) # Adding ellipses
      print(biplot)
      
      
      
    #ggplot make dataframe of princomp object use that to plot 
    
    
    ord_df<- prcomp(df, scale. = TRUE)
    
    autoplot(ord, data = dat, colour = 'behavior', loadings = TRUE)
    
    
    library(plotly)
    library(ggfortify)
    library(cluster)
    
    
    # MOVEMENT COVS PCA
    ph_ <- prcomp(dat[c(3,4,5,12,13,14,15,16)], scale. = TRUE)
    ph  <- autoplot(ph_ , data = dat, colour = 'behavior',
                    colors = c('purple','darkblue', "black", 'blue', 'red'),  
                    frame = TRUE, frame.type = 'norm',
                    loadings = TRUE, loadings.colour = 'blue',
                    loadings.label = TRUE, loadings.label.size = 4)

    ggplotly(ph)
    
  
    
    # HABITAT COVS PCA
    ph_ <- prcomp(dat[c(6:11)], scale. = TRUE)
    ph  <- autoplot(ph_ , data = dat, colour = 'behavior',
                    colours = c('purple','darkblue', "black", 'blue', 'red'),  
                    frame = TRUE, frame.type = 'norm',
                    loadings = TRUE, loadings.colour = 'black',
                    loadings.label = TRUE, loadings.label.size = 4)
    
    ggplotly(ph)
    
    # CHANGE COLORS FROM UGLY AUTOPLOT
    ph<- ph + scale_colour_manual(values = c("TRANSLOCATED" = "red", "MEANRES"= "darkblue","MEANTRANS" ="grey47","RESIDENT" = "deepskyblue2", "All"="black"))  + 
              scale_fill_manual(values = c("MEANRES"= "darkblue","MEANTRANS" ="grey47","RESIDENT" = "deepskyblue2", "All"="black", "TRANSLOCATED" = "red")) +
                                  theme(text=element_text(size=16, family="Times New Roman")) 
    ph
    
    ggplotly(ph)
    
    # OTHER WAY 2D PLOTLY WITHOUT USING AUTOPLOT 
    ord <- princomp(scale(dat[,6:11]))
    plotDat <- data.frame(ord$scores[,1:3])
    plotDat$behavior <- dat$behavior
    
    # PLOT_LY FUNCTION WAY
    hi <-  plot_ly(plotDat,x = plotDat$Comp.1, y = plotDat$Comp.2,
                        color = plotDat$behavior, colors = c('purple','darkblue', "black", 'blue', 'red'), )
          
    ggplotly(hi)
    
   
    
     ### GGPLOT WAY
    
    ### Ordinate the scaled MOVEMENT coefs
    ord <- princomp(scale(dat[c(3,4,5,12,13,14,15,16)]))
    ord.sum <- summary(ord)
    plotDat <- data.frame(ord$scores[,1:3])
    plotDat$behavior <- dat$behavior
  
    comps <- ord.sum$sdev[1:3]^2/sum(ord.sum$sdev^2)
    
    hi <- data.frame(x = ord$loadings[,1],
                     y = ord$loadings[,2],
                     z = ord$loadings[,3])
    
    axis1 <- as.data.frame(ord$loadings[,c(1:2)])
    
    axis2 <- as.data.frame(ord$loadings[,c(3:2)])
    
    
    help<-ggplot() + geom_point(plotDat, mapping = aes( Comp.1,Comp.2, color = behavior)) +
      
      ggtitle(label = "Movement Niche of Pronghorn by Group") +
      stat_ellipse(plotDat, mapping = aes(Comp.1,Comp.2, color = behavior)) +
      geom_segment(data = axis1,x = 0, y = 0, xend = hi$x, yend = hi$y, lwd = 0.75, col = 'black', arrow = arrow(type = 'open',length = unit(0.25, "cm"))) +
      geom_text_repel(aes(label = rownames(axis2), x = hi$x, y = hi$y), size = 4, col = "Blue", force = 10,  box.padding = unit(0.5, 'lines')) +
      xlab(paste0('Component 1 ','(',round((ord.sum$sdev[1]^2/sum(ord.sum$sdev^2)*100),digits=1), '% ',
                  'explained variation)' )) +
      ylab(paste0('Component 2 ','(',round((ord.sum$sdev[2]^2/sum(ord.sum$sdev^2)*100),digits=1), '% ',
                  'explained variation)' ))
    
                 
               
    help<- help + scale_colour_manual(values = c("TRANSLOCATED" = "red", "MEANRES"= "darkblue","MEANTRANS" ="grey47","RESIDENT" = "deepskyblue2", "All"="black"))  + 
            scale_fill_manual(values = c("MEANRES"= "darkblue","MEANTRANS" ="grey47","RESIDENT" = "deepskyblue2", "All"="black", "TRANSLOCATED" = "red")) +
            theme(text=element_text(size=16, family="Times New Roman")) +
            theme(plot.title = element_text(hjust = 0.5)) 
    
    help
    
    
    ggplotly(help)#plotly
    
    
    
    
    
    
    
    
    help2<-ggplot() + geom_point(plotDat, mapping = aes(Comp.2,Comp.3, color = behavior)) +
      
      ggtitle(label = "Movement Niche of Pronghorn by Group") +
      stat_ellipse(plotDat, mapping = aes(Comp.2,Comp.3, color = behavior)) +
      geom_segment(data = axis2,x = 0, y = 0, xend = hi$x, yend = hi$y, lwd = 0.75, col = 'black', arrow = arrow(type = 'open',length = unit(0.25, "cm"))) +
      geom_text_repel(aes(label = rownames(axis2), x = hi$x, y = hi$y), size = 4, col = "Blue", force = 10,  box.padding = unit(0.5, 'lines')) +
      xlab(paste0('Component 2 ','(',round((ord.sum$sdev[2]^2/sum(ord.sum$sdev^2)*100),digits=1), '% ',
                  'explained variation)' )) +
      ylab(paste0('Component 3 ','(',round((ord.sum$sdev[3]^2/sum(ord.sum$sdev^2)*100),digits=1), '% ',
                  'explained variation)' ))
    
    
    
    help2<- help2 + scale_colour_manual(values = c("TRANSLOCATED" = "red", "MEANRES"= "darkblue","MEANTRANS" ="grey47","RESIDENT" = "deepskyblue2", "All"="black"))  + 
      scale_fill_manual(values = c("MEANRES"= "darkblue","MEANTRANS" ="grey47","RESIDENT" = "deepskyblue2", "All"="black", "TRANSLOCATED" = "red")) +
      theme(text=element_text(size=16, family="Times New Roman")) +
      theme(plot.title = element_text(hjust = 0.5)) 
    
    help2
   
    
    
    
    # PNG STUFF 
    png(file='Mov_Niche_Plots.png', width=3600, height=2400, res = 300)      
    grid.arrange(help, help2, nrow = 3)
    dev.off()
    
    
    
    png(file='Mov_Niche_Plots.png', width=3600, height=2400, res = 300)      
    help2
    dev.off()
    
    
    ### HABITAT COEFS ####
    ### Ordinate the scaled HABITAT coefs # 5:9 OLD
    ord <- princomp(scale(dat[,6:11]))
    ord.sum <- summary(ord)
    
    plotDat <- data.frame(ord$scores[,1:3])
    plotDat$behavior <- dat$behavior
    
    comps <- ord.sum$sdev[1:3]^2/sum(ord.sum$sdev^2)
    
    hi <- data.frame(x = ord$loadings[,1],
                     y = ord$loadings[,2],
                     z = ord$loadings[,3])
    
    axis1 <- as.data.frame(ord$loadings[,c(1:2)])
    
    axis2 <- as.data.frame(ord$loadings[,c(3:2)])
    
    
    help3<-ggplot() + geom_point(plotDat, mapping = aes(Comp.1,Comp.2, color = behavior)) +
      
      ggtitle(label = "Habitat Niche of Pronghorn by Group") +
      stat_ellipse(plotDat, mapping = aes(Comp.1,Comp.2, color = behavior)) +
      geom_segment(data = axis1,x = 0, y = 0, xend = hi$x, yend = hi$y, lwd = 0.75, col = 'black', arrow = arrow(type = 'open',length = unit(0.25, "cm"))) +
      geom_text_repel(aes(label = rownames(axis2), x = hi$x, y = hi$y), size = 4, col = "Blue", force = 50,  box.padding = unit(0.9, 'lines')) +
      xlab(paste0('Component 1 ','(',round((ord.sum$sdev[1]^2/sum(ord.sum$sdev^2)*100),digits=1), '% ',
                  'explained variation)' )) +
      ylab(paste0('Component 2 ','(',round((ord.sum$sdev[2]^2/sum(ord.sum$sdev^2)*100),digits=1), '% ',
                  'explained variation)' ))
    
    
    
    help3<- help3 + scale_colour_manual(values = c("TRANSLOCATED" = "red", "MEANRES"= "darkblue","MEANTRANS" ="grey47","RESIDENT" = "deepskyblue2", "All"="black"))  + 
      scale_fill_manual(values = c("MEANRES"= "darkblue","MEANTRANS" ="grey47","RESIDENT" = "deepskyblue2", "All"="black", "TRANSLOCATED" = "red")) +
      theme(text=element_text(size=16, family="Times New Roman")) +
      theme(plot.title = element_text(hjust = 0.5)) 
    
    help3
    
    
     #ggplotly(help)#plotly
    
    
    
    help4<-ggplot() + geom_point(plotDat, mapping = aes(Comp.2,Comp.3, color = behavior)) +
      
      ggtitle(label = "Habitat Niche of Pronghorn by Group") +
      stat_ellipse(plotDat, mapping = aes(Comp.2,Comp.3, color = behavior)) +
      geom_segment(data = axis2,x = 0, y = 0, xend = hi$x, yend = hi$y, lwd = 0.75, col = 'black', arrow = arrow(type = 'open',length = unit(0.25, "cm"))) +
      geom_text_repel(aes(label = rownames(axis2), x = hi$x, y = hi$y), size = 4, col = "Blue", force = 40,  box.padding = unit(0.5, 'lines')) +
      xlab(paste0('Component 2 ','(',round((ord.sum$sdev[2]^2/sum(ord.sum$sdev^2)*100),digits=1), '% ',
                  'explained variation)' )) +
      ylab(paste0('Component 3 ','(',round((ord.sum$sdev[3]^2/sum(ord.sum$sdev^2)*100),digits=1), '% ',
                  'explained variation)' ))
    
    
    
    help4<- help4 + scale_colour_manual(values = c("TRANSLOCATED" = "red", "MEANRES"= "darkblue","MEANTRANS" ="grey47","RESIDENT" = "deepskyblue2", "All"="black"))  + 
      scale_fill_manual(values = c("MEANRES"= "darkblue","MEANTRANS" ="grey47","RESIDENT" = "deepskyblue2", "All"="black", "TRANSLOCATED" = "red")) +
      theme(text=element_text(size=16, family="Times New Roman")) +
      theme(plot.title = element_text(hjust = 0.5)) 
    
    help4
    
    
    
    
    # PNG STUFF 
    png(file='Hab_Niche_Plots.png', width=3000, height=3400, res = 300)      
    grid.arrange(help3, help4, nrow = 3)
    dev.off()
    
    
    
    png(file='Hab_Niche_Plots.png', width=3600, height=2400, res = 300)      
    help4
    dev.off()
    
    
    
    ##### X TABLE ####
    
    X<-xtable(PH.coefs[1:30, ])
    
    x.side <- xtable(X, caption = "Scaled Coefficients Table")
    print(x.side, floating = TRUE, floating.environment = "sidewaystable")    
    
    
    ### FIGURE OUT NAs in MAY 2021
    
    stack.msavi$`2021-5`<- na.omit(values(stack.msavi$`2021-5`))
    MAY< unlist(MAY)
    MeanMay<-mean(MAY[1:2644937,])
    
    
    
    # Fixing MAY based off 0.19 avg MSAVI
    PH.PRED.MAP <- (PH.coef$elev*(demStack$elev) + 
                      PH.coef$slope*demStack$slope +
                      PH.coef$dist2road*r1 +
                      PH.coef$dist2well*w1 +
                      PH.coef$msavi*0.198) #should be 29 maps

    
    # shows where animals are on landscape
    #  PH.PRED.MAP<-  PH.PRED.MAP/sum(values( PH.PRED.MAP, na.rm = TRUE))
    
    terra::writeRaster( PH.PRED.MAP,paste0("/Volumes/big_game/Erin_OConnell/Thesis/CH 2/Data/mods/",i, ".TIF"), overwrite = T)
    print(paste(PH.PRED.MAP, "Added to mods folder"))
    rm(i,  PH.PRED.MAP)
    
    
  