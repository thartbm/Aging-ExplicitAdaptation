
# regular localization analyses # ============================================
PopulateData_osf <- function(){
  library(osfr)
  library(dplyr)
  
  if (length(list.files(path = './data2')) < 3) {
    osfProject <- osf_retrieve_node('QZHMY')
    osfDataFiles <- osfProject %>%
      osf_ls_files() %>%
      filter(name == "data") %>%
      osf_ls_files(n_max = 100)
    
    # for(i in 1:nrow(osfDataFiles)) {
    #   row <- osfDataFiles[i,]
    #   # do stuff with row
    #   osf_download(row, path = paste("./data2", row$name, sep="/"))
    # }
    
    # #make a list of download links
    # osfDownloadLinks <- data.frame()
    # for(i in 1:nrow(osfDataFiles)) {
    #   row <- osfDataFiles[i,]
    #   # do stuff with row
    #   osfDownloadLinks <- c(osfDownloadLinks, row$meta[[1]]$links$download)
    
    #save this thing
    saveRDS(osfDataFiles, file = 'osfDataFiles.rds')
  }
}

# load.DownloadDataframe <- function(url,filename) {
#   
#   if (file.exists(filename)) {
#     
#     df <- read.csv(filename, stringsAsFactors=FALSE)
#     
#   } else {
#     
#     df <- read.csv(url(url),stringsAsFactors=FALSE)
#     
#     write.csv(df,filename,row.names=FALSE,quote=FALSE)
#     
#   }
#   
#   return(df)
#   
# }

# populate the /data directory using the osfDataFiles object
PopulateData <- function(){
  osfDataFiles<- readRDS('osfDataFiles.rds') 
  if (length(list.files(path = './data')) < 3) {
    for(i in 1:nrow(osfDataFiles)){
      row <- osfDataFiles[i,]
      
      download.file(url = row$meta[[1]]$links$download, destfile = paste("./data", row$name, sep="/"))
    }
  }
}
plotLocalization <- function(target='inline') {
  
  styles <- getStyle()
  
  if (target == 'svg') {
    svglite(file='doc/fig/Fig9.svg', width=8, height=8/3, system_fonts=list(sans='Arial'))
  }
  
  par(mfrow=c(1,3), mar=c(4,4,2,0.1))
  
  # # # # # # # # # #
  # panels A & B: active and passive localization
  
  for (reachtype.idx in 1:2) {
    
    reachtype <- c('active','passive')[reachtype.idx]
    
    # create plot panel with the right properties
    plot(c(25,175),c(0,0),type='l',main=sprintf('%s localization',reachtype),xlim=c(25,175),ylim=c(2,-17),axes=FALSE,xlab='hand angle [°]', ylab='localization shift [°]',lty=2,col=rgb(.5,.5,.5))
    
    mtext(c('A','B')[reachtype.idx], side=3, outer=TRUE, at=c(c(0,1/3)[reachtype.idx],1), line=-1, adj=0, padj=1)
    
    axis(1, at=c(45,90,135),cex.axis=0.85)
    axis(2, at=c(0,-5,-10,-15),cex.axis=0.85)
    
    
    for (groupno in c(1:length(styles$group))) {
      
      group <- styles$group[groupno]
      
      localization <- read.csv(sprintf('../../data/%s_localization_tCI.csv',group))
      
      angles <- localization$angle
      lo <- localization[,sprintf('%s_p2.5',c('act','pas')[reachtype.idx])]
      hi <- localization[,sprintf('%s_p97.5',c('act','pas')[reachtype.idx])]
      
      idx <- which((angles >= 30) & (angles <= 150))
      
      coord.x = c(angles[idx],rev(angles[idx]));
      coord.y = c(lo[idx],rev(hi[idx]))
      polygon(coord.x,coord.y,col=as.character(styles$color_trans[groupno]),border=NA)
      
    }
    
    for (groupno in c(1:length(styles$group))) {
      
      group <- styles$group[groupno]
      
      localization <- read.csv(sprintf('../../data/%s_localization_tCI.csv',group))
      
      angles <- localization$angle
      centre <- localization[,sprintf('%s_p50',c('act','pas')[reachtype.idx])]
      
      idx <- which((angles >= 30) & (angles <= 150))
      
      lines(angles[idx],centre[idx],col=as.character(styles$color_solid[groupno]),lw=2,lty=styles$linestyle[groupno])
      
    }
    
    # ADD AVERAGE DOTS
    
    for (groupno in c(1:length(styles$group))) {
      
      group <- styles$group[groupno]
      
      localization <- read.csv(sprintf('../../data/%s_loc_p3_AOV.csv',group))
      localization <- localization[which(localization$passive_b == (reachtype.idx-1)),]
      localization <- aggregate(bias_deg ~ participant*rotated_b, data=localization, FUN=mean)
      shift <- localization$bias_deg[which(localization$rotated_b == 1)] - localization$bias_deg[which(localization$rotated_b == 0)]
      
      xloc <- 155 + (groupno*4)
      CI <- t.interval(shift)
      #arrows(xloc, CI[2], xloc, CI[1], length=0.05, angle=90, code=3, col=as.character(styles$color_solid[groupno]), lty=styles$linestyle[groupno])
      lines(c(xloc, xloc), c(CI[2], CI[1]), col=as.character(styles$color_solid[groupno]), lty=styles$linestyle[groupno])
      lines(c(xloc-1.5, xloc+1.5), c(CI[1], CI[1]), col=as.character(styles$color_solid[groupno]), lty=1)
      lines(c(xloc-1.5, xloc+1.5), c(CI[2], CI[2]), col=as.character(styles$color_solid[groupno]), lty=1)
      points(xloc, mean(shift), col=as.character(styles$color_solid[groupno]), pch=19)
      
    }
    
  }
  
  plot(c(25,175),c(0,0),type='l',main='predicted consequences',xlim=c(25,175),ylim=c(2,-17),axes=FALSE,xlab='hand angle [°]', ylab='update [°]',lty=2,col=rgb(.5,.5,.5))
  
  mtext('C', side=3, outer=TRUE, at=c(2/3,1), line=-1, adj=0, padj=1)
  
  axis(1, at=c(45,90,135),cex.axis=0.85)
  axis(2, at=c(0,-5,-10,-15),cex.axis=0.85)
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    
    localization <- read.csv(sprintf('../../data/%s_localization_tCI.csv',group))
    
    angles <- localization$angle
    lo <- localization$PredCons_p2.5
    hi <- localization$PredCons_p97.5
    
    idx <- which((angles >= 30) & (angles <= 150))
    
    coord.x = c(angles[idx],rev(angles[idx]));
    coord.y = c(lo[idx],rev(hi[idx]))
    polygon(coord.x,coord.y,col=as.character(styles$color_trans[groupno]),border=NA)
    
  }
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    
    localization <- read.csv(sprintf('../../data/%s_localization_tCI.csv',group))
    
    angles <- localization$angle
    PSQ <- localization$PredCons_p50
    
    idx <- which(is.finite(PSQ) & (angles >= 30) & (angles <= 150))
    
    lines(angles[idx],PSQ[idx],col=as.character(styles$color_solid[groupno]),lw=2,lty=styles$linestyle[groupno])
    
  }
  
  # #   # ADD AVERAGE DOTS
  # #   
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    
    shifts <- list()
    
    for (reachtype.idx in c(1,2)) {
      localization <- read.csv(sprintf('../../data/%s_loc_p3_AOV.csv',group))
      localization <- localization[which(localization$passive_b == (reachtype.idx-1)),]
      localization <- aggregate(bias_deg ~ participant*rotated_b, data=localization, FUN=mean)
      shift <- localization$bias_deg[which(localization$rotated_b == 1)] - localization$bias_deg[which(localization$rotated_b == 0)]
      shifts[[reachtype.idx]] <- shift
    }
    
    shift <- shifts[[1]] - shifts[[2]]
    
    xloc <- 155 + (groupno*4)
    CI <- t.interval(shift)
    #arrows(xloc, CI[2], xloc, CI[1], length=0.05, angle=90, code=3, col=as.character(styles$color[groupno]), lty=styles$linestyle[groupno])
    lines(c(xloc, xloc), c(CI[2], CI[1]), col=as.character(styles$color_solid[groupno]), lty=styles$linestyle[groupno])
    lines(c(xloc-1.5, xloc+1.5), c(CI[1], CI[1]), col=as.character(styles$color_solid[groupno]), lty=1)
    lines(c(xloc-1.5, xloc+1.5), c(CI[2], CI[2]), col=as.character(styles$color_solid[groupno]), lty=1)
    points(xloc, mean(shift), col=as.character(styles$color_solid[groupno]), pch=19)
    
  }
  
  
  legend(60,-15,as.character(styles$label),col=as.character(styles$color_solid),lty=styles$linestyle,bty='n',lw=2,cex=0.85, seg.len=3)
  
}





#newlocalizationAOVaging <- function(groups=c('30implicit','30explicit','aging_implicit','aging_explicit')) {

localizationANOVA <- function(test='omnibus') {
  
  styles <- getStyle()
  
  if (test=='omnibus') {
    
    loc4aov <- getLocalization4ANOVA(styles)
    loc4aov$participant <- as.factor(loc4aov$participant)
    print(ezANOVA(dv=bias_deg,wid=participant,within=c(rotated_b,passive_b),between=c(instructed,agegroup),data=loc4aov,type=3))
    
  }
  
  if (test=='shifts') {
    
    loc4aov <- getLocalization4ANOVA(styles, shifts=TRUE)
    loc4aov$participant <- as.factor(loc4aov$participant)
    print(ezANOVA(dv=bias_deg,wid=participant,within=passive_b,between=c(instructed,agegroup),data=loc4aov,type=3))
    
  }
  
  if (test=='passive') {
    loc4aov <- getLocalization4ANOVA(styles, shifts=TRUE)
    loc4aov <- loc4aov[which(loc4aov$passive_b==1),]
    loc4aov$participant <- as.factor(loc4aov$participant)
    print(ezANOVA(dv=bias_deg, wid=participant, between=c(instructed,agegroup), data=loc4aov, type=3))
    
  }
  
}



getLocalization4ANOVA <- function(styles, shifts=FALSE) {
  
  LOCaov <- NA
  
  # keeping count of unique participants:
  startingID <- 0
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    
    # set up some basic descriptors that apply to the group:
    if (substr(group, 1, 5) == 'aging') {
      thisagegroup <- 'older'
    } else {
      thisagegroup <- 'younger'
    }
    thisinstructed <- grepl('explicit', group)
    
    df <- read.csv(sprintf('../../data/%s_loc_p3_AOV.csv',group),stringsAsFactors=F)
    
    df <- aggregate(bias_deg ~ participant * rotated_b * passive_b, data=df, FUN=mean)
    if (shifts) {
      df <- aggregate(bias_deg ~ participant * passive_b, data=df, FUN=diff)
    }

    df$agegroup   <- thisagegroup
    df$instructed <- thisinstructed
    
    if (is.data.frame(LOCaov)) {
      LOCaov <- rbind(LOCaov, df)
    } else {
      LOCaov <- df
    }
    
  }
  
  # set relevant columns as factors:
  LOCaov$agegroup   <- as.factor(LOCaov$agegroup)
  LOCaov$instructed <- as.factor(LOCaov$instructed)
  LOCaov$passive_b  <- as.factor(LOCaov$passive_b)
  if (!shifts) {
    LOCaov$rotated_b  <- as.factor(LOCaov$rotated_b)
  }
  
  return(LOCaov)
  
  
}

getPredictedSensoryConsequences <- function(agegroups='both') {
  #setwd("~/Grad Thesis/Explicit/R/ana")
  # first we load the files, depending on which age groups we want:
  # 'both': both older and younger participants
  # 'younger': only the younger participants
  # 'older': only the older participants
  
  # load the younger participants data if necessary:
  if (agegroups %in% c('both','younger')) {
    
    exp <- read.csv('../../data/30explicit_loc_p3_AOV.csv')
    exp$instructed <- 1
    imp <- read.csv('../../data/30implicit_loc_p3_AOV.csv')
    imp$instructed <- 0
    
    # combine implicit and explicit data:
    younger <- rbind(exp,imp)
    younger$age <- 'younger'
    
  } else {
    younger <- NA
  }
  
  # load the older participants data if necessary:
  if (agegroups %in% c('both','older')) {
    
    exp <- read.csv('../../data/aging_explicit_loc_p3_AOV.csv')
    exp$instructed <- 1
    imp <- read.csv('../../data/aging_implicit_loc_p3_AOV.csv')
    imp$instructed <- 0 
    
    # combine explicit with implicit data
    df <- rbind(exp,imp)
    df$age <- 'older'
    
    # combine younger and older if applicable
    if (is.data.frame(younger)) {
      df <- rbind(younger,df)
    }
  } else {
    df <- younger
  }
  
  # make new data frame with difference scores
  
  # set up vectors for new data:
  
  participant      <- c()
  handangle        <- c()
  age              <- c()
  instructed       <- c()
  predictionupdate <- c()
  
  handangles <- unique(df$handangle_deg)
  
  groups <- unique(df$group)
  
  for (group in groups) {
    
    p.ids <- unique(df$participant[which(df$group == group)])
    
    for (p.id in p.ids) {
      
      for (angle in handangles) {
        
        # get only the data relevant at this point:
        adf <- df[which(df$group == group & df$participant == p.id & df$handangle_deg == angle),]
        
        # get all the localization responses for this participant at this angle:
        AA <- adf$bias_deg[which(adf$rotated_b == 0 & adf$passive_b == 0)]
        AP <- adf$bias_deg[which(adf$rotated_b == 0 & adf$passive_b == 1)]
        RA <- adf$bias_deg[which(adf$rotated_b == 1 & adf$passive_b == 0)]
        RP <- adf$bias_deg[which(adf$rotated_b == 1 & adf$passive_b == 1)]
        
        # get the update in predicted sensory consequences:
        UPSC <- (RA - AA) - (RP - AP) 
        
        # store in new vectors:
        participant      <- c(participant, p.id)
        handangle        <- c(handangle, angle)
        age              <- c(age, adf$age[1])
        instructed       <- c(instructed, adf$instructed[1])
        predictionupdate <- c(predictionupdate, UPSC)
        
      }
      
    }
    
  }
  
  df <- data.frame(participant, age, instructed, handangle, predictionupdate)
  df$instructed  <- as.factor(df$instructed)
  df$handangle   <- as.factor(df$handangle)
  df$participant <- as.character(df$participant) 
  
  return(df)
  
}

predictedConsequencesANOVA <- function() {
  
  df <- getPredictedSensoryConsequences(agegroups='both')
  
  df <- aggregate(predictionupdate ~ participant * age * instructed, data=df, FUN=mean)
  
  df$participant <- as.factor(df$participant)
  
  print(ezANOVA(dv=predictionupdate, wid=participant, between=c(instructed,age), data=df, type=3))
  
}

predConsTtests <- function() {
  
  df <- getPredictedSensoryConsequences(agegroups='younger')
  df <- aggregate(predictionupdate ~ participant, data=df, FUN=mean)
  
  cat('younger adults predicted sensory consequences compared to 0:\n')
  print(t.test(df$predictionupdate, mu=0, alternative='less'))
  
  df <- getPredictedSensoryConsequences(agegroups='older')
  df <- aggregate(predictionupdate ~ participant, data=df, FUN=mean)
  
  cat('older adults predicted sensory consequences compared to 0:\n')
  print(t.test(df$predictionupdate, mu=0, alternative='less'))
  
}

# acuity tests # ====================================================

saveDetrendedData <- function() {
  
  dfs <- detrendData(cleanData())
  
  write.csv(dfs[['aligned']][['young']], file='data/young_localization.csv', quote=F, row.names=F)
  write.csv(dfs[['aligned']][['aging']], file='data/aging_localization.csv', quote=F, row.names=F)
  
}

detrendData <- function(groupData) {
  
  # The output from the cleanData() function should go in here.
  # We'll detrend the data using splines, but have to be carefull not to 
  # extrapolate. Worst case: we loose 2 datapoints.
  
  groups <- names(groupData[['aligned']])
  
  # we'll end up with only two (four) dataframes now:
  cntrl  <- NA
  aging  <- NA
  #Rcntrl <- NA
  #Raging <- NA
  
  ppno <- 0
  
  for (group in groups) {
    
    df  <- groupData[['aligned']][[group]]
    #Rdf <- groupData[['rotated']][[group]]
    
    # print(group)
    IDs <- unique(df$ID)
    
    for (ID in IDs) {
      
      IDidx <- which(df$ID == ID)
      
      # we'll switch from IDs to numbers:
      ppno <- ppno + 1
      
      participant <- rep(ppno,length(IDidx))
      block <- df[IDidx,'block']
      trial <- df[IDidx,'trial']
      active_bool <- df[IDidx,'active'] 
      # handangle_deg <- df[IDidx,'targetangle_deg']
      arcangle_deg <- df[IDidx,'arcangle_deg']
      tapx <- df[IDidx,'tapx_cm']
      tapy <- df[IDidx,'tapy_cm']
      # I use atan2(X,Y), not atan2(Y,X) 
      # to stay well within the atan2 output range,
      # then just add 90 degrees again:
      localizationangle_deg <- ((atan2(-1*tapx, tapy) / pi) * 180) + 90
      
      handx <- df[IDidx,'handx_cm']
      handy <- df[IDidx,'handy_cm']
      # same use of atan2 here:
      handangle_deg <- ((atan2(-1*handx, handy) / pi) * 180) + 90
      
      # we detrend by getting the predicted localization angle from all other data
      # and subtracting it from the actual data
      # prediction can be an interpolation using splines or kernel smoothing (or LOESS / LOWESS):
      # (my current opinion is that splining is better than the other two)
      
      predicted_angles <- c()
      
      for (trial.idx in c(1:length(IDidx))) {
        
        # w <- 1 - (0.75 * ( df[IDidx,'time_ms'] / max(df[IDidx,'time_ms']) ))
        spl <- smooth.spline(x=handangle_deg[-trial.idx], y=localizationangle_deg[-trial.idx], spar=0.90, keep.data=F )
        
        thissampleprediction <- predict(spl, x=handangle_deg[trial.idx])$y
        #print(thissampleprediction)
        predicted_angles <- c(predicted_angles, thissampleprediction)
        
      }
      
      localizationerror_deg <- (localizationangle_deg - predicted_angles)
      localization_deg <- (localizationangle_deg - localizationerror_deg)
      
      if (substr(group,1,6) == 'aging_') {
        agegroup <- rep('aging',length(IDidx))
      } else {
        agegroup <- rep('cntrl',length(IDidx))
      }
      
      IDdf <- data.frame(agegroup,group,participant,block,trial,active_bool,handangle_deg,localization_deg,RT_ms=df[IDidx,'time_ms'])
      
      # remove the minimum and maximum handangle? (NOT DONE)
      # (they may suffer from extrapolation-like inaccuracies)
      
      if (substr(group,1,6) == 'aging_') {
        # print(ppno)
        if (is.data.frame(aging)) {
          aging <- rbind(aging, IDdf)
        } else {
          aging <- IDdf
        }
      } else {
        # print(ppno)
        if (is.data.frame(cntrl)) {
          cntrl <- rbind(cntrl, IDdf)
        } else {
          cntrl <- IDdf
        }
        
      }
      
      # # also deal with the rotated sessions:
      # # we only want a generic "training-induced shift" across the trained area
      # POI <- seq(45,135,22.5)
      # 
      # # DO THIS SEPARATELY FOR ACTIVE AND PASSIVE!
      # 
      # spl <- smooth.spline(x=handangle_deg, y=localizationangle_deg, spar=0.90, keep.data=F )
      # alignedPOIloc <- predict(spl, x=POI)$y
      # 
      # for (active in c(0,1)) {
      #   
      #   # now estimate the same points in the rotated session
      #   rIDidx <- which(Rdf$ID == ID & Rdf$active == active)
      #   
      #   Rtapx <- Rdf[rIDidx,'tapx_cm']
      #   Rtapy <- Rdf[rIDidx,'tapy_cm']
      #   # I use atan2(X,Y), not atan2(Y,X) to stay well within atan2 range of output:
      #   Rlocalizationangle_deg <- ((atan2(-1*Rtapx, Rtapy) / pi) * 180) + 90
      #   
      #   Rhandx <- Rdf[rIDidx,'handx_cm']
      #   Rhandy <- Rdf[rIDidx,'handy_cm']
      #   # same use of atan2 here:
      #   Rhandangle_deg <- ((atan2(-1*Rhandx, Rhandy) / pi) * 180) + 90
      #   
      #   Rspl <- smooth.spline(x=Rhandangle_deg, y=Rlocalizationangle_deg, spar=0.90, keep.data=F )
      #   rotatedPOIloc <- predict(Rspl, x=POI)$y
      #   
      #   # now we have the shift here:
      #   localizationshift_deg <- mean(rotatedPOIloc - alignedPOIloc)
      #   
      #   # let's build a dataframe with one row:
      #   agegroup <- c(agegroup[1])
      #   participant <- c(ppno)
      #   active_bool <- c(active)
      #   
      #   
      #   rIDdf <- data.frame(agegroup,group,participant,active_bool,localizationshift_deg)
      #   
      #   if (substr(group,1,6) == 'aging_') {
      #     # print(ppno)
      #     if (is.data.frame(Raging)) {
      #       Raging <- rbind(Raging, rIDdf)
      #     } else {
      #       Raging <- rIDdf
      #     }
      #   } else {
      #     # print(ppno)
      #     if (is.data.frame(Rcntrl)) {
      #       Rcntrl <- rbind(Rcntrl, rIDdf)
      #     } else {
      #       Rcntrl <- rIDdf
      #     }
      #   }
      #   
      # }
      
      ##### UP TO HERE THE ADDED ROTATION STUFF
      
    }
    
  }
  
  detrended <- list()
  detrended[['aligned']] <- list()
  detrended[['aligned']][['young']] <- cntrl
  detrended[['aligned']][['aging']] <- aging
  # detrended[['rotated']] <- list()
  # detrended[['rotated']][['young']] <- Rcntrl
  # detrended[['rotated']][['aging']] <- Raging
  
  return(detrended)
  
}


cleanData <- function(verbose=FALSE,proportion=0.70) {
  
  groups <- c('30implicit',
              '30explicit',
              #'60implicit_b',
              #'60explicit_b',
              #'cursorjump',
              'aging_implicit',
              'aging_explicit')
  
  # collect info on cleaning process in these variables:
  allCntrlParticipants <- 0
  keptCntrlParticipants <- 0
  allAgingParticipants <- 0
  keptAgingParticipants <- 0
  
  # collect all data frames here:
  ALcleanGroups <- list()
  ROcleanGroups <- list()
  
  for (group in groups) {
    
    df  <- read.csv(sprintf('data/%s_localizations.csv',group),         stringsAsFactors=F)
    # rdf <- read.csv(sprintf('data/%s_rotated_localizations.csv',group), stringsAsFactors=F)
    
    rawrows <- nrow(df)
    rawIDs <- unique(df$ID)
    rawIDs <- rawIDs[which(!is.na(rawIDs))]
    
    # we remove all rows that were deselected:
    df  <-  df[ df$selected == 1,]
    # rdf <- rdf[rdf$selected == 1,]
    # cat(sprintf('A: selected: %1.1f%%\n',100*(nrow(df)/rawrows)))
    
    # we remove all localizations where the hand was 'below' the home position:
    df  <-  df[ df$handy_cm > 0,]
    # rdf <- rdf[rdf$handy_cm > 0,]
    # cat(sprintf('B: below home: %1.1f%%\n',100*(nrow(df)/rawrows)))
    
    # we remove all localizations where the hand was not within 2 cm of the arc:
    df  <-  df[abs(sqrt( df$handx_cm^2 +  df$handy_cm^2)-12) < 2,]
    # rdf <- rdf[abs(sqrt(rdf$handx_cm^2 + rdf$handy_cm^2)-12) < 1,]
    # cat(sprintf('C: hand at arc: %1.1f%%\n',100*(nrow(df)/rawrows)))
    
    # we remove all localizations where the touch was not within 3 cm of the arc:
    df  <-  df[abs(sqrt( df$handx_cm^2 +  df$handy_cm^2)-12) < 3,]
    # rdf <- rdf[abs(sqrt(rdf$handx_cm^2 + rdf$handy_cm^2)-12) < 2,]
    # cat(sprintf('D: tap at arc: %1.1f%%\n',100*(nrow(df)/rawrows)))
    
    # we remove all localizations that took longer than 10 seconds:
    df  <-  df[ df$time_ms <= 10000,]
    # rdf <- rdf[rdf$time_ms <= 10000,]
    # if (verbose) {
    #   cat(sprintf('E: tap 10 seconds: %1.1f%%\n',100*(nrow(df)/rawrows)))
    # }
    
    # if (!grepl('60',group)) {
      
    # we remove all localizations where the touch was not within 30 degrees of the centre of the arc:
    df  <-  df[abs( df$targetangle_deg -  df$arcangle_deg) <= 30,]
    # rdf <- rdf[abs(rdf$targetangle_deg - rdf$arcangle_deg) <= 30,]
    # if (verbose) {
    #   cat(sprintf('F: hand in arc: %1.1f%%\n',100*(nrow(df)/rawrows)))
    # }
    
    NSDS <- 3
      
    # } else {
    #   
    #   # we remove all localizations where the touch was not within 50 degrees of the centre of the V:
    #   df  <-  df[abs( df$targetangle_deg -  df$arcangle_deg) <= 50,]
    #   rdf <- rdf[abs(rdf$targetangle_deg - rdf$arcangle_deg) <= 50,]
    #   # if (verbose) {
    #   #   cat(sprintf('F: hand in arc: %1.1f%%\n',100*(nrow(df)/rawrows)))
    #   # }
    #   
    #   NSDS <- 3
    #   
    # }
    
    # the following two steps are better done for individual participants, I guess:
    theseIDs <- unique(df$ID)
    theseIDs <- theseIDs[which(!is.na(theseIDs))]
    keeptheserows <- c()
    keeptheseROws <- c()
    
    for (ID in theseIDs) {
      
      ALpprows <- c()
      ROpprows <- c()
      
      # we remove all localizations that took longer than median + 3 standard deviations:
      loctime <-  df[ df$ID == ID,]$time_ms # <= median(df[df$ID == ID,]$time_ms) + (2 * sd(df[df$ID == ID,]$time_ms))
      ALpprows <- c(ALpprows, which( df$ID == ID &  df$time_ms <= median(loctime) + (NSDS * sd(loctime)) ) )
      
      # loctime <- rdf[rdf$ID == ID,]$time_ms # <= median(df[df$ID == ID,]$time_ms) + (2 * sd(df[df$ID == ID,]$time_ms))
      # ROpprows <- c(ROpprows, which(rdf$ID == ID & rdf$time_ms <= median(loctime) + (NSDS * sd(loctime)) ) )
      
      # we remove all localizations where the angular difference is outside the normal range for that group:
      angdevs <- atan2( df$handy_cm,  df$handx_cm) - atan2( df$tapy_cm,  df$tapx_cm)
      ALpprows <- intersect(ALpprows, which( df$ID == ID & abs(angdevs-mean(angdevs)) < (NSDS * sd(angdevs))) )
      
      # angdevs <- atan2(rdf$handy_cm, rdf$handx_cm) - atan2(rdf$tapy_cm, rdf$tapx_cm)
      # ROpprows <- intersect(ROpprows, which(rdf$ID == ID & abs(angdevs-mean(angdevs)) < (NSDS * sd(angdevs))) )
      
      keeptheserows <- c(keeptheserows, unique(ALpprows))
      # keeptheseROws <- c(keeptheseROws, unique(ROpprows))
      
    }
    
    df  <-  df[keeptheserows,]
    # rdf <- rdf[keeptheseROws,]
    
    
    
    # we remove all participants that have very few localizations left
    # ppsummary <- aggregate(selected ~ ID, data=aggregate(selected ~ ID * active, data=df, FUN=sum), FUN=min)
    # keepIDs <- ppsummary$ID[ppsummary$selected >= (72*proportion)]
    # df  <-  df[ df$ID %in% keepIDs,]
    # rdf <- rdf[rdf$ID %in% keepIDs,]
    
    ALcleanGroups[[group]] <-  df
    # ROcleanGroups[[group]] <- rdf
    
    # # provide feedback on the cleaning in this group, if required:
    # if (verbose) {
    #   cat(sprintf('%s: kept %d / %d participants (%0.1f%%)\n', group, length(keepIDs), length(rawIDs), (100 * (length(keepIDs)/length(rawIDs))) ))
    # }
    
    # # collect info on cleaning process:
    # if (substring(group, 1, 5) == 'aging') {
    #   allAgingParticipants <- allAgingParticipants + length(rawIDs)
    #   keptAgingParticipants <- keptAgingParticipants + length(keepIDs)
    #   
    # } else {
    #   allCntrlParticipants <- allCntrlParticipants + length(rawIDs)
    #   keptCntrlParticipants <- keptCntrlParticipants + length(keepIDs)
    # }
    
  }
  
  # # provide feedback on cleaning process if required:
  # if (verbose) {
  #   cat(sprintf('\nCONTROL: kept %d / %d participants (%0.1f%%)\n', keptCntrlParticipants, allCntrlParticipants, (100 * (keptCntrlParticipants/allCntrlParticipants)) ))
  #   cat(sprintf('AGING:   kept %d / %d participants (%0.1f%%)\n', keptAgingParticipants, allAgingParticipants, (100 * (keptAgingParticipants/allAgingParticipants)) ))
  # }
  
  # return(list('aligned'=ALcleanGroups, 'rotated'=ROcleanGroups))
  return(list('aligned'=ALcleanGroups))
  
}

loadDetrendedData <- function() {
  
  dfs <- list()
  
  dfs[['young']] <- read.csv(file='data/young_localization.csv', stringsAsFactors=F)
  dfs[['aging']] <- read.csv(file='data/aging_localization.csv', stringsAsFactors=F)
  
  return(dfs)
  
}

# get variance from cleaned data # ===================================

getLocalizationVariance <- function(df) {
  
  participant <- unique(df$participant)
  group <- c()
  
  probs=c(.025, .500, .975)
  
  allCIs <- NA
  
  for (pp.id in participant) {
    
    ppCI <- c()
    
    group <- c(group, df$group[which(df$participant == pp.id)[1]])
    
    for (active in c(1,0)) {
      
      rows <- which(df$participant == pp.id & df$active_bool == active)
      
      scores <- df$handangle_deg[rows] - df$localization_deg[rows]
      
      score_vars <- bootstrap.var.CI(scores, probs=probs, samples=10000)
      
      names(score_vars) <- sprintf('%s%03d',c('p','a')[active+1],probs*1000)
      
      ppCI <- c(ppCI, score_vars)
      
    }
    
    if (is.data.frame(allCIs)) {
      allCIs <- rbind(allCIs, data.frame(t(data.frame(ppCI))))
    } else {
      allCIs <- data.frame(t(data.frame(ppCI)))
    }
    
  }
  
  CI <- data.frame(group,participant)
  CI <- cbind(CI, allCIs)
  row.names(CI) <- c(1:nrow(CI))
  
  return(CI)
  
}

bootstrap.var.CI <- function(scores, probs, samples) {
  
  resamples <- matrix(sample(scores, size=samples*length(scores), replace=TRUE), ncol=samples, nrow=length(scores))
  return(quantile(t(apply(resamples, 1, var)), probs=probs))
  
}

saveVarCIs <- function() {
  
  dfs <- loadDetrendedData()
  
  for (group in names(dfs)) {
    
    df <- dfs[[group]]
    
    vardf <- getLocalizationVariance(df)
    
    write.csv(vardf, file=sprintf('data/%s_varianceCIs.csv',group), quote=F, row.names=F)
    
  }
  
}

loadVarCIs <- function() {
  
  dfs <- c()
  
  dfs[['young']] <- read.csv('data/young_varianceCIs.csv', stringsAsFactors=F)
  dfs[['aging']] <- read.csv('data/aging_varianceCIs.csv', stringsAsFactors=F)
  
  return(dfs)
  
}

# acuity plots and analyses # ===================================

plotVarianceAge <- function(target='notebook') {
  
  if (target == 'svg') {
    svglite(file='doc/fig/Fig10.svg',width=7,height=7,system_fonts=list(sans='Arial'))
    par(mfrow=c(2,2))
  } else {
    par(mfrow=c(1,2))
  }
  
  plotVarianceDensityComparison(task='active',groups=c('young','aging'))
  plotVarianceDensityComparison(task='passive',groups=c('young','aging'))
  
  if (target == 'svg') {
    dev.off()
  }
  
}

plotVarianceDensityComparison <- function(task,groups) {
  
  style <- getStyle()
  
  dfs <- loadVarCIs()
  
  # groupcolors <- list( 'young'=c('#ff8200ff','#ff82002f'), 'aging'=c('#c400c4ff','#c400c42f') )
  
  groupttestdata <- list()
  
  densitylinesY <- list()
  densitylinesX <- list()
  
  medians <- list()
  
  legendcolors <- c()
  
  plot(-1000,-1000,main=sprintf('%s localization',task),xlim=c(0,sqrt(400)),ylim=c(0,0.17),axes=F,xlab=expression(sqrt(sigma^2)),ylab='density')
  
  for (group in groups) {
    
    if (group == 'young') {
      coloridx <- 1
    } else {
      coloridx <- 3
    }
    if (substr(task,1,1) == 'p') {
      coloridx <- coloridx + 1
    }

    df <- dfs[[group]]
    
    varname <- sprintf('%s500',substr(task,1,1))
    
    data <- density(sqrt(df[,varname]),from=0, n=1024)
    
    CI <- t.interval(sqrt(df[,varname]))
    idx <- which(data$x > CI[1] & data$x < CI[2])
    
    # X <- c(data$x[idx], rev(data$x[idx]))
    # Y <- c(data$y[idx], rep(0,length(idx)))
    X <- c(data$x, rev(data$x))
    Y <- c(data$y, rep(0,length(data$y)))
    
    polygon(X,Y,border=NA,col=as.character(style$color_trans[coloridx]))
    
    densitylinesY[[group]] <- data$y
    densitylinesX[[group]] <- data$x
    thismedian <- median(sqrt(df[,varname]))
    x.idx <- which(abs(data$x - thismedian) == min(abs(data$x - thismedian)))
    medians[[group]] <- c(thismedian,data$y[x.idx])
    groupttestdata[[group]] <- sqrt(df[,varname])
    
  }
  
  for (group in groups) {
    
    if (group == 'young') {
      coloridx <- 1
    } else {
      coloridx <- 3
    }
    if (substr(task,1,1) == 'p') {
      coloridx <- coloridx + 1
    }
    
    lines(densitylinesX[[group]], densitylinesY[[group]], col=as.character(style$color_solid[coloridx]), lty=1)
    lines(rep(medians[[group]][1],2),c(0,medians[[group]][2]), lty=2, col=as.character(style$color_solid[coloridx]))
    
    legendcolors <- c(legendcolors,as.character(style$color_solid[coloridx]))
    
  }
  
  legend(10,.15,legend=groups,col=legendcolors,lty=1,bty='n')
  
  cat(sprintf('\n*** %s - %s, %s\n', task, groups[[1]], groups[[2]]))
  print(t.test(log(groupttestdata[[groups[1]]]),log(groupttestdata[[groups[2]]]),alternative='l'))

  print(cohen.d(log(groupttestdata[[groups[1]]]),log(groupttestdata[[groups[2]]]),alternative='l'))
  
  # axis(side=1,at=sqrt(c(1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300,400)),labels=c('','2','','','','','','','','','20','','','','','','','','','200','',''))
  axis(side=1,at=c(0,5,10,15,20))
  axis(side=2,at=c(0,.05,.1,.15))
  
}

# t.interval = function(data, variance = var(data, na.rm=TRUE), conf.level = 0.95) {
#   
#   z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
#   
#   xbar = mean(data, na.rm=TRUE)
#   sdx = sqrt(variance/length(data))
#   
#   return(c(xbar - z * sdx, xbar + z * sdx))
#   
# }

# bias stuff # ===================================================

saveDataTrends <- function() {
  
  dfs <- getDataTrends(cleanData())
  
  write.csv(dfs[['aligned']][['young']], file='data/young_trends.csv', quote=F, row.names=F)
  write.csv(dfs[['aligned']][['aging']], file='data/aging_trends.csv', quote=F, row.names=F)
  
}

loadDataTrends <- function() {
  
  dfs <- list()
  
  dfs[['young']] <- read.csv(file='data/young_trends.csv', stringsAsFactors=F)
  dfs[['aging']] <- read.csv(file='data/aging_trends.csv', stringsAsFactors=F)
  
  return(dfs)
  
}


getDataTrends <- function(groupData) {
    
  # The output from the cleanData() function should go in here.
  # We'll detrend the data using splines, but have to be carefull not to 
  # extrapolate. Worst case: we loose 2 datapoints.
  
  angle <- seq(30,150)
  
  groups <- names(groupData[['aligned']])
  
  # we'll end up with only two (four) dataframes now:
  cntrl  <- NA
  aging  <- NA
  
  ppno <- 0
  
  for (group in groups) {
    
    df  <- groupData[['aligned']][[group]]
    
    # print(group)
    IDs <- unique(df$ID)
    
    for (ID in IDs) {
      
      IDidx <- which(df$ID == ID)
      
      # we'll switch from IDs to numbers:
      ppno <- ppno + 1
      
      participant <- rep(ppno,length(angle))
      block <- df[IDidx,'block']
      trial <- df[IDidx,'trial']
      active_bool <- df[IDidx,'active'] 
      # handangle_deg <- df[IDidx,'targetangle_deg']
      arcangle_deg <- df[IDidx,'arcangle_deg']
      tapx <- df[IDidx,'tapx_cm']
      tapy <- df[IDidx,'tapy_cm']
      # I use atan2(X,Y), not atan2(Y,X) 
      # to stay well within the atan2 output range,
      # then just add 90 degrees again:
      localizationangle_deg <- ((atan2(-1*tapx, tapy) / pi) * 180) + 90
      
      handx <- df[IDidx,'handx_cm']
      handy <- df[IDidx,'handy_cm']
      # same use of atan2 here:
      handangle_deg <- ((atan2(-1*handx, handy) / pi) * 180) + 90
      
      # we detrend by getting the predicted localization angle from all other data
      # and subtracting it from the actual data
      # prediction can be an interpolation using splines or kernel smoothing (or LOESS / LOWESS):
      # (my current opinion is that splining is better than the other two)
      
      for (active in c(0,1)) {
        
        act.idx <- which(active_bool == active)
        
        spl <- smooth.spline(x=handangle_deg[act.idx], y=localizationangle_deg[act.idx]-handangle_deg[act.idx], spar=0.90, keep.data=F )
        angularerror <- predict(spl, x=angle)$y
        angularerror[which(angle < min(handangle_deg))] <- NA
        angularerror[which(angle > max(handangle_deg))] <- NA
        
        if (substr(group,1,6) == 'aging_') {
          agegroup <- rep('aging',length(angularerror))
          
        } else {
          agegroup <- rep('cntrl',length(angularerror))
          
        }
        
        IDdf <- data.frame(agegroup,group,participant,active,angle,angularerror)
        
        # remove the minimum and maximum handangle? (NOT DONE)
        # (they may suffer from extrapolation-like inaccuracies)
        
        if (substr(group,1,6) == 'aging_') {
          # print(ppno)
          if (is.data.frame(aging)) {
            aging <- rbind(aging, IDdf)
          } else {
            aging <- IDdf
          }
        } else {
          # print(ppno)
          if (is.data.frame(cntrl)) {
            cntrl <- rbind(cntrl, IDdf)
          } else {
            cntrl <- IDdf
          }
          
        }
        
      }
      
    }
    
  }
  
  trends <- list()
  trends[['aligned']] <- list()
  trends[['aligned']][['young']] <- cntrl
  trends[['aligned']][['aging']] <- aging
  
  return(trends)
  
}

onePlotLocalizationBiases <- function(group,active=0) {
  
  df <- loadDataTrends()[[group]]
  
  df <- df[which(df$active == active),]
  
  style <- getStyle()
  
  if (group == 'young') {
    style.idx <- 1
  }
  if (group == 'aging') {
    style.idx <- 3
  }
  
  plot(c(15,165),c(0,0),type='l',col='#999999',main=sprintf('aligned localization biases\n%s participants (%s)',group,c('passive','active')[active+1]),xlim=c(15,165),ylim=c(-45,45),xlab='hand angle [deg]',ylab='bias [deg]',axes=F)
  
  participants <- unique(df$participant)
  
  allbias <- matrix(nrow=length(participants),ncol=length(unique(df$angle)))
  
  for (participant.id in c(1:length(participants))) {
    
    participant <- participants[participant.id]
    
    subdf <- df[which(df$participant == participant),]
    
    handangle <- subdf$angle
    localizationerror <- subdf$angularerror
    
    lines(handangle,localizationerror,col=as.character(style$color_trans[style.idx]))
    
    allbias[participant.id,] <- localizationerror
    
  }
  
  lines(handangle,colMeans(allbias),lw=2,col=as.character(style$color_solid[style.idx]))
  
  axis(side=1,at=c(45,60,75,90,105,120,135))
  axis(side=2,at=c(-45,-30,-15,0,15,30,45))
  
}

plotLocalizationBiases <- function() {
  
  par(mfrow=c(2,2))
  
  for (group in c('young','aging')) {
    
    for (active in c(0,1)) {
      
      onePlotLocalizationBiases(group=group,active=active)
      
    }
    
  }
  
}