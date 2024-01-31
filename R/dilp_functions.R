dilp_processing <- function(specimen_data) {
  ##For specimens where blade area was measured, but there is an NA for petiole area, convert NA to 0 to permit the subsequent addition
  specimen_data$`Petiole area` <- ifelse(specimen_data$`Blade area`>0 & is.na(specimen_data$`Petiole area`),0,specimen_data$`Petiole area`)
  #Sum petiole area and blade area
  specimen_data$`Leaf area` <- specimen_data$`Blade area` + specimen_data$`Petiole area` #if petiole area was measured but there is a NA for blade area, this addition will appropriately return a NA for leaf area

  #Feret Diameter Ratio
  specimen_data$`Feret diameter` <- 2*sqrt(specimen_data$`Leaf area`/pi)
  specimen_data$FDR <- specimen_data$`Feret diameter` / specimen_data$Feret

  #Corrected perimeters
  specimen_data$`Raw blade perimeter corrected` <- specimen_data$`Raw blade perimeter` - specimen_data$`Length of cut perimeter`
  specimen_data$`Internal raw blade perimeter corrected` <- specimen_data$`Internal raw blade perimeter` - specimen_data$`Length of cut perimeter`

  ##Toothed variables
  #Total tooth count
  specimen_data$`no. of secondary teeth`[is.na(specimen_data$`no. of secondary teeth`)] <- 0 #if it is left as NA then total tooth count will also return NA
  specimen_data$`Total tooth count` <- specimen_data$`no. primary teeth` + specimen_data$`no. of secondary teeth`
  #Total tooth count : internal perimeter
  specimen_data$`TC IP` <- specimen_data$`Total tooth count` / specimen_data$`Internal raw blade perimeter corrected`
  #Perimeter ratio
  specimen_data$`Perimeter ratio` <- specimen_data$`Raw blade perimeter corrected` / specimen_data$`Internal raw blade perimeter corrected`

  ####Apply required natural logs to appropriate variables
  specimen_data$`Ln leaf area` <- log(100*specimen_data$`Leaf area`) #Leaf area is expressed in mm2 in DiLP MLR and SLR models, so here also converting from cm2 to mm2
  specimen_data$`Ln PR` <- log(specimen_data$`Perimeter ratio`)
  specimen_data$`Ln TC IP` <- log(specimen_data$`TC IP`)

  #change NA's to zeros for length.of.cut.perimeter so that derived variables can be calculated
  specimen_data$`Length of cut perimeter` <- ifelse(is.na(specimen_data$`Length of cut perimeter`), 0, specimen_data$`Length of cut perimeter`)
  #Petiole metric for reconstructing leaf mass per area
  specimen_data$`petiole metric` <- (specimen_data$`Petiole width`^2)/specimen_data$`Leaf area`
  #Aspect ratio
  specimen_data$`aspect ratio` <- specimen_data$Feret / specimen_data$`Minimum Feret`
  #Shape factor
  specimen_data$`shape factor` <- 4*pi*(specimen_data$`Blade area`/(specimen_data$`Blade perimeter`^2))
  #Compactness
  specimen_data$compactness <- (specimen_data$`Blade perimeter`^2)/specimen_data$`Blade area`
  #Tooth area
  specimen_data$`tooth area` <- specimen_data$`Raw blade area` - specimen_data$`Internal raw blade area`
  #Tooth area : perimeter
  specimen_data$`TA P` <- specimen_data$`tooth area` / specimen_data$`Raw blade perimeter corrected`
  #Tooth area : internal perimeter
  specimen_data$`TA IP` <- specimen_data$`tooth area` / specimen_data$`Internal raw blade perimeter corrected`
  #Tooth area : blade area
  specimen_data$`TA BA` <- specimen_data$`tooth area` / specimen_data$`Raw blade area`
  #Average primary tooth area
  specimen_data$`Avg TA` <- specimen_data$`tooth area` / specimen_data$`no. primary teeth` #double check
  #Tooth count : blade area
  specimen_data$`TC BA` <- specimen_data$`Total tooth count` / specimen_data$`Raw blade area`
  #tooth count : perimeter
  specimen_data$`TC P` <- specimen_data$`Total tooth count` / specimen_data$`Raw blade perimeter corrected`

  return(specimen_data)
}

dilp_errors <- function(specimen_data){
  ###This data frame will be filled with the results of the following error checks
  errors <- data.frame()

  #1. Check that untoothed leaves have NA for toothed variables, rather than 0 (excluding no. of secondary teeth, which is purposely set to 0)

  dilp.check.1 <- subset(specimen_data, Margin==1) #Inspect values visually

  if(length(which(dilp.check.1$`Total tooth count`>-1))==0){e1="none"} else {e1=which(dilp.check.1$`Total tooth count`>-1)}
  temp1 <- data.frame(Check="Entire tooth count not NA", Row=e1)
  if(length(which(dilp.check.1$`TC IP` >-1))==0){e2="none"} else {e2=which(dilp.check.1$`TC IP` >-1)}
  temp2 <- data.frame(Check="Entire tooth count : IP not NA", Row=e2)
  if(length(which(dilp.check.1$`Perimeter ratio` >-1))==0){e3="none"} else {e3=which(dilp.check.1$`Perimeter ratio` >-1)}
  temp3 <- data.frame(Check="Entire perimeter ratio not NA", Row=e3)

  #2. Check that FDR is greater is between 0 and 1 for all leaves

  dilp.check.2 <- tidyr::drop_na(specimen_data, FDR)
  if(length(which(dilp.check.2$FDR < 0 | dilp.check.2$FDR > 1))==0){e4="none"} else {e4=which(dilp.check.2$FDR < 0 | dilp.check.2$FDR > 1)}
  temp4 <- data.frame(Check="FDR not between 0-1", Row=e4)

  #3. Check that external perimeter is larger than internal for all leaves
  if(length(which(specimen_data$`Internal raw blade perimeter corrected` > specimen_data$`Raw blade perimeter corrected`))==0){e5="none"} else {e5=which(specimen_data$`Internal raw blade perimeter corrected` > specimen_data$`Raw blade perimeter corrected`)}
  temp5 <- data.frame(Check="External perimeter not larger than internal perimeter", Row=e5)

  #4. Check that Feret is larger than Minimum Feret
  if(length(which(specimen_data$`Minimum Feret` > specimen_data$Feret))==0){e6="none"} else {e6=which(specimen_data$`Minimum Feret` > specimen_data$Feret)}
  temp6 <- data.frame(Check="Feret is not larger than minimum Feret", Row=e6)

  #Check that perimeter ratio is greater than 1
  if(length(which(specimen_data$`Perimeter ratio`<=1))==0){e7="none"} else {e7=which(specimen_data$`Perimeter ratio`<=1)}
  temp7 <- data.frame(Check="Perimeter ratio not greater than 1", Row=e7)

  errors <- rbind(temp1, temp2, temp3, temp4, temp5, temp6, temp7)
  errors$specimen <- specimen_data[errors$Row, 2]
  return(errors)
}

dilp_outliers <- function(specimen_data){
  vars <- c("FDR","TC IP","Leaf area","Perimeter ratio") #DiLP variables
  outliers <- data.frame()

  for (i in 1:length(vars)) {
    temp <- specimen_data
    colnames(temp)[colnames(temp) == vars[i]] = "trait" #rename variable of focus
    temp.outliers <- grDevices::boxplot.stats(temp$trait)$out #check it for outliers
    temp.row <- which(temp$trait %in% c(temp.outliers)) #determine row numbers for any outliers
    temp.row <- as.data.frame(t(temp.row))
    temp.output <- (trait=vars[i])
    temp.output <- cbind(temp.output,temp.row) #create output table with variable name and any potential rows
    outliers <- dplyr::bind_rows(outliers,temp.output) #bind to summary table
  }
  #Rename column headers
  names(outliers) <- gsub(x = names(outliers), pattern = "V", replacement = "Outlier")
  names(outliers) <- gsub(x = names(outliers), pattern = "temp.output", replacement = "Variable")
  return(outliers)
}

dilp <- function(specimen_data){
  processed_specimen_data <- dilp_processing(specimen_data)
  errors <- dilp_errors(processed_specimen_data)
  outliers <- dilp_outliers(processed_specimen_data)

  #######Morphotype average by site
  dilp_morphotype <- processed_specimen_data %>%
    dplyr::select(-c("Specimen Number", "Measurer comments")) %>%
    dplyr::group_by(Site, Morphotype) %>%
    dplyr::summarise_all(mean, na.rm=TRUE )

  #####Morphotypes that have variable leaf margin states require a margin state of 0.5

  #is just 1 and 0 listed? If so, the following finds margin state values between 0 and 1 and replaces them with 0.5
  if(length(unique(dilp_morphotype$Margin)) > 2) {
    dilp_morphotype$Margin[dilp_morphotype$Margin>0 & dilp_morphotype$Margin<1] <- 0.5
    if(length(unique(dilp_morphotype$Margin)) > 2) {
      warning("Margin states outside the bounds of [0 - 1] present")
    } #Double check the code was successful, now there should be 0.5, 1.0 and 0.0 listed
  }

  #######Site average
  dilp_site <- dilp_morphotype %>%
    dplyr::group_by(Site) %>%
    dplyr::select(-"Morphotype") %>%
    dplyr::summarise_all(mean, na.rm=TRUE )

  ###Convert site margin from proportion to percentage
  dilp_site$Margin <- dilp_site$Margin * 100

  #Equation variables
  ##############DO NOT CHANGE OR EDIT########
  #
  #Global MLR MAT                           #
  MAT.MLR.M <- 0.21                         #
  MAT.MLR.FDR <- 42.296                     #
  MAT.MLR.TC.IP <- -2.609                   #
  MAT.MLR.constant <- -16.004               #
  MAT.MLR.error <- 4                        #
  #
  #Global SLR MAT                           #
  MAT.SLR.M <- 0.204                        #
  MAT.SLR.constant <- 4.6                   #
  MAT.SLR.error <- 4.9                      #
  #
  #Global MLR MAP                           #
  MAP.MLR.LA <- 0.298                       #
  MAP.MLR.TC.IP <- 0.279                    #
  MAP.MLR.PR <- -2.717                      #
  MAP.MLR.constant <- 3.033                 #
  MAP.MLR.SE <- 0.6                         #
  #
  #Global SLR MAP                           #
  MAP.SLR.LA <- 0.283                       #
  MAP.SLR.constant <- 2.92                  #
  MAP.SLR.SE <- 0.61                        #
  ###########################################

  ##A loop is constructed to handle spreadsheets that contain either multiple sites or a single site. For the latter, the loop will run only once.
  sites <- c(unique(dilp_site$Site))
  Results <- data.frame()

  for (i in 1:length(sites)) {
    temp <- dilp_site[dilp_site$Site == sites[i] , ] #isolate site

    ####MAT
    #MLR
    MAT.MLR <- (temp$Margin*MAT.MLR.M) + (temp$FDR*MAT.MLR.FDR) + (temp$`TC IP`*MAT.MLR.TC.IP) + MAT.MLR.constant

    #SLR
    MAT.SLR <- (temp$Margin*MAT.SLR.M) + MAT.SLR.constant

    ####MAP
    #MLR
    MAP.MLR.exp <- (temp$`Ln leaf area`*MAP.MLR.LA) + (temp$`Ln TC IP`*MAP.MLR.TC.IP) + (temp$`Ln PR`*MAP.MLR.PR) + MAP.MLR.constant
    MAP.MLR <- exp(MAP.MLR.exp)

    MAP.MLR.error.plus <- (exp(MAP.MLR.exp+MAP.MLR.SE))-MAP.MLR
    MAP.MLR.error.minus <- MAP.MLR-(exp(MAP.MLR.exp-MAP.MLR.SE))

    #SLR

    MAP.SLR.exp <- (temp$`Ln leaf area`*MAP.SLR.LA) + MAP.SLR.constant
    MAP.SLR <- exp(MAP.SLR.exp)
    MAP.SLR.error.plus <- (exp(MAP.SLR.exp+MAP.SLR.SE))-MAP.SLR
    MAP.SLR.error.minus <- MAP.SLR-(exp(MAP.SLR.exp-MAP.SLR.SE))

    #Results table

    temp_output <- data.frame(Site=temp$Site, Margin=temp$Margin, FDR=temp$FDR, `TC IP`=temp$`TC IP`, `Ln leaf area`=temp$`Ln leaf area`, `Ln TC IP`=temp$`Ln TC IP`, `Ln PR`=temp$`Ln PR`, MAT.MLR, MAT.MLR.error, MAT.SLR, MAT.SLR.error, MAP.MLR,
                              MAP.MLR.error.plus,MAP.MLR.error.minus,MAP.SLR,MAP.SLR.error.plus,MAP.SLR.error.minus)
    Results <- rbind(Results,temp_output)
  }

  return(list(processed_leaf_data = processed_specimen_data, processed_morphotype_data = dilp_morphotype, processed_site_data = dilp_site, errors = errors, outliers = outliers, results = Results))
}
