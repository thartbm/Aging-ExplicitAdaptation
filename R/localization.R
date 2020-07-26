
# regular localization analyses # ============================================

plotLocalization <- function(target='inline') {
  
  styles <- getStyle()
  
  if (target == 'svg') {
    svglite(file='doc/Fig5.svg', width=7.5, height=5, system_fonts=list(sans='Arial'))
  }
  
  par(mar=c(4,4,2,0.1))
  
  layout(matrix(c(1,2,3,4,5,6), nrow=2, ncol=3, byrow = TRUE), widths=c(1,1,1), heights=c(1,1))
  
  
  
  # # # # # # # # # #
  # panels A & B: active and passive localization
  
  for (reachtype.idx in 1:2) {
    
    reachtype <- c('active','passive')[reachtype.idx]
    
    # create plot panel with the right properties
    plot(c(25,155),c(0,0),type='l',main=sprintf('%s localization',reachtype),xlim=c(25,155),ylim=c(4,-17),axes=FALSE,xlab='hand angle [°]', ylab='localization shift [°]',lty=1,col=rgb(.75,.75,.75),font.main=1)
    
    #mtext(c('A','B')[reachtype.idx], side=3, outer=TRUE, at=c(c(0,1/3)[reachtype.idx],1), line=-1, adj=0, padj=1)
    mtext(c('A','B')[reachtype.idx], outer=FALSE, side=3, las=1, line=1, adj=0, padj=1)
    
    axis(1, at=c(45,90,135),cex.axis=0.85)
    axis(2, at=c(0,-5,-10,-15),cex.axis=0.85)
    #axis(2, at=c(15,5,-5,-15,-25),cex.axis=0.85)
    
    for (groupno in c(1:length(styles$group))) {
      
      group <- styles$group[groupno]
      
      localization <- read.csv(sprintf('data/%s_localization_tCI.csv',group))
      
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
      
      localization <- read.csv(sprintf('data/%s_localization_tCI.csv',group))
      
      angles <- localization$angle
      centre <- localization[,sprintf('%s_p50',c('act','pas')[reachtype.idx])]
      
      idx <- which((angles >= 30) & (angles <= 150))
      
      lines(angles[idx],centre[idx],col=as.character(styles$color_solid[groupno]),lw=2,lty=styles$linestyle[groupno])
      
    }
    
  }
  
  
  # # # # # # # # # # # # # # # # #
  # PANEL C: predicted sensory consequences
  
  plot(c(25,155),c(0,0),type='l',main='predicted consequences',xlim=c(25,155),ylim=c(4,-17),axes=FALSE,xlab='hand angle [°]', ylab='update [°]',lty=1,col=rgb(.75,.75,.75),font.main=1)
  
  #mtext('C', side=3, outer=TRUE, at=c(2/3,1), line=-1, adj=0, padj=1)
  mtext('C', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1)
  
  axis(1, at=c(45,90,135),cex.axis=0.85)
  axis(2, at=c(0,-5,-10,-15),cex.axis=0.85)
  #axis(2, at=c(15,5,-5,-15,-25),cex.axis=0.85)
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    
    localization <- read.csv(sprintf('data/%s_localization_tCI.csv',group))
    
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
    
    localization <- read.csv(sprintf('data/%s_localization_tCI.csv',group))
    
    angles <- localization$angle
    PSQ <- localization$PredCons_p50
    
    idx <- which(is.finite(PSQ) & (angles >= 30) & (angles <= 150))
    
    lines(angles[idx],PSQ[idx],col=as.character(styles$color_solid[groupno]),lw=2,lty=styles$linestyle[groupno])
    
  }
  
  legendlabels <- sprintf('%s: %s', c('YN','YI','ON','OI'), styles$label)
  legend(45,-15,legendlabels,col=as.character(styles$color_solid),lty=styles$linestyle,bty='n',lw=2,cex=0.85, seg.len=3)
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # PANELS D & E: individual points for localization shifts
  
  # ADD AVERAGE DOTS
  
  for (reachtype.idx in c(1,2)) {
    
    reachtype <- c('active','passive')[reachtype.idx]
    
    # create plot panel with the right properties
    #plot(c(0,5),c(0,0),type='l',main=sprintf('%s localization',reachtype),xlim=c(0,5),ylim=c(15,-25),axes=FALSE,xlab='', ylab='localization shift [°]',lty=1,col=rgb(.75,.75,.75),font.main=1)
    plot(c(0,5),c(0,0),type='l',main='',xlim=c(0,5),ylim=c(15,-25),axes=FALSE,xlab='group', ylab='individual localization shifts [°]',lty=1,col=rgb(.75,.75,.75),font.main=1)
    
    mtext(c('D','E')[reachtype.idx], outer=FALSE, side=3, las=1, line=1, adj=0, padj=1)
    
    axis(1, at=c(1,2,3,4), labels=c('YN','YI','ON','OI'), cex.axis=0.85)
    #axis(side=1, at=c(1,2,3,4),labels=c('','','',''),cex.axis=0.85)
    #axis(2, at=c(15,5,-5,-15,-25), cex.axis=0.85)
    axis(2, at=c(10,0,-10,-20), cex.axis=0.85)
    
    for (groupno in c(1:length(styles$group))) {
      
      group <- styles$group[groupno]
      
      localization <- read.csv(sprintf('data/%s_loc_p3_AOV.csv',group))
      localization <- localization[which(localization$passive_b == (reachtype.idx-1)),]
      localization <- aggregate(bias_deg ~ participant*rotated_b, data=localization, FUN=mean)
      shift <- localization$bias_deg[which(localization$rotated_b == 1)] - localization$bias_deg[which(localization$rotated_b == 0)]
      
      points(rep(groupno-0.25, length(shift)), shift, col=as.character(styles$color_trans[groupno]), pch=16, cex=1.5)
      
      xloc <- groupno+0.25
      CI <- t.interval(shift)
      #arrows(xloc, CI[2], xloc, CI[1], length=0.05, angle=90, code=3, col=as.character(styles$color_solid[groupno]), lty=styles$linestyle[groupno])
      lines(c(xloc, xloc), c(CI[2], CI[1]), col=as.character(styles$color_solid[groupno]), lty=1)
      # lty = styles$linestyle[groupno]
      #lines(c(xloc-1.5, xloc+1.5), c(CI[1], CI[1]), col=as.character(styles$color_solid[groupno]), lty=1)
      #lines(c(xloc-1.5, xloc+1.5), c(CI[2], CI[2]), col=as.character(styles$color_solid[groupno]), lty=1)
      points(xloc, mean(shift), col=as.character(styles$color_solid[groupno]), pch=19)
      
    }
    
  }
  
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # PANEL F: predicted consequences individual data points
  
  #plot(c(0,5),c(0,0),type='l',main='predicted consequences',xlim=c(0,5),ylim=c(15,-25),axes=FALSE,xlab='', ylab='update [°]',lty=1,col=rgb(.75,.75,.75),font.main=1)
  plot(c(0,5),c(0,0),type='l',main='',xlim=c(0,5),ylim=c(15,-25),axes=FALSE,xlab='group', ylab='individual updates [°]',lty=1,col=rgb(.75,.75,.75),font.main=1)
  
  mtext('F', outer=FALSE, side=3, las=1, line=1, adj=0, padj=1)
  
  axis(1, at=c(1,2,3,4), labels=c('YN','YI','ON','OI'), cex.axis=0.85)
  #axis(side=1, at=c(1,2,3,4),labels=c('','','',''),cex.axis=0.85)
  # axis(2, at=c(15,5,-5,-15,-25), cex.axis=0.85)
  axis(2, at=c(10,0,-10,-20), cex.axis=0.85)
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    
    shifts <- list()
    
    for (reachtype.idx in c(1,2)) {
      localization <- read.csv(sprintf('data/%s_loc_p3_AOV.csv',group))
      localization <- localization[which(localization$passive_b == (reachtype.idx-1)),]
      localization <- aggregate(bias_deg ~ participant*rotated_b, data=localization, FUN=mean)
      shift <- localization$bias_deg[which(localization$rotated_b == 1)] - localization$bias_deg[which(localization$rotated_b == 0)]
      shifts[[reachtype.idx]] <- shift
    }
    
    shift <- shifts[[1]] - shifts[[2]]
    
    #print(mean(shift))
    
    points(rep(groupno-0.25, length(shift)), shift, col=as.character(styles$color_trans[groupno]), pch=16, cex=1.5)
    
    xloc <- groupno + 0.25
    CI <- t.interval(shift)
    #arrows(xloc, CI[2], xloc, CI[1], length=0.05, angle=90, code=3, col=as.character(styles$color[groupno]), lty=styles$linestyle[groupno])
    lines(c(xloc, xloc), c(CI[2], CI[1]), col=as.character(styles$color_solid[groupno]), lty=1)
    # lty = styles$linestyle[groupno]
    #lines(c(xloc-1.5, xloc+1.5), c(CI[1], CI[1]), col=as.character(styles$color_solid[groupno]), lty=1)
    #lines(c(xloc-1.5, xloc+1.5), c(CI[2], CI[2]), col=as.character(styles$color_solid[groupno]), lty=1)
    points(xloc, mean(shift), col=as.character(styles$color_solid[groupno]), pch=19) # pch=19
    
  }
  
  #legend(1,-22.5,legend=styles$label,pch=16,col=as.character(styles$color_solid),bty='n',cex=0.8) 
  
  if (target == 'svg') {
    dev.off()
  }
  
}





#newlocalizationAOVaging <- function(groups=c('30implicit','30explicit','aging_implicit','aging_explicit')) {

localizationANOVA <- function(test='omnibus') {
  
  default.contrasts <- options('contrasts')
  options(contrasts=c('contr.sum','contr.poly'))
  
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
  
  if (test=='active') {
    loc4aov <- getLocalization4ANOVA(styles, shifts=TRUE)
    loc4aov <- loc4aov[which(loc4aov$passive_b==0),]
    loc4aov$participant <- as.factor(loc4aov$participant)
    print(ezANOVA(dv=bias_deg, wid=participant, between=c(instructed,agegroup), data=loc4aov, type=3))
    
  }
  
  options('contrasts' <- default.contrasts)
  
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
    
    df <- read.csv(sprintf('data/%s_loc_p3_AOV.csv',group),stringsAsFactors=F)
    
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
  
  # first we load the files, depending on which age groups we want:
  # 'both': both older and younger participants
  # 'younger': only the younger participants
  # 'older': only the older participants
  
  # load the younger participants data if necessary:
  if (agegroups %in% c('both','younger')) {
    
    exp <- read.csv('data/30explicit_loc_p3_AOV.csv')
    exp$instructed <- 1
    imp <- read.csv('data/30implicit_loc_p3_AOV.csv')
    imp$instructed <- 0
    
    # combine implicit and explicit data:
    younger <- rbind(exp,imp)
    younger$age <- 'younger'
    
  } else {
    younger <- NA
  }
  
  # load the older participants data if necessary:
  if (agegroups %in% c('both','older')) {
    
    exp <- read.csv('data/aging_explicit_loc_p3_AOV.csv')
    exp$instructed <- 1
    imp <- read.csv('data/aging_implicit_loc_p3_AOV.csv')
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
  
  default.contrasts <- options('contrasts')
  options(contrasts=c('contr.sum','contr.poly'))
  
  df <- getPredictedSensoryConsequences(agegroups='both')
  
  df <- aggregate(predictionupdate ~ participant * age * instructed, data=df, FUN=mean)
  
  df$participant <- as.factor(df$participant)
  
  print(ezANOVA(dv=predictionupdate, wid=participant, between=c(instructed,age), data=df, type=3))
  
  options('contrasts' <- default.contrasts)
  
}

predConsTtests <- function() {
  
  df <- getPredictedSensoryConsequences(agegroups='younger')
  df <- aggregate(predictionupdate ~ participant, data=df, FUN=mean)
  
  cat('younger adults predicted sensory consequences compared to 0:\n')
  print(t.test(df$predictionupdate, mu=0, alternative='less'))
  
  cat(sprintf('eta-squared: %0.5f\n', etaSquaredTtest(g1=df$predictionupdate, mu=0)))
  
  groupNs <- dim(df)[1]
  deltaMu <- mean(df$predictionupdate)
  stndevs <- sd(df$predictionupdate)
  cat('\npower analysis:\n')
  print(power.t.test( n = groupNs, 
                      delta = deltaMu, 
                      sd = stndevs, 
                      sig.level = 0.05,
                      type = 'one.sample',
                      alternative = 'one.sided',
                      strict=FALSE))
  
  
  cat('\n')
  
  df <- getPredictedSensoryConsequences(agegroups='older')
  df <- aggregate(predictionupdate ~ participant, data=df, FUN=mean)
  
  cat('older adults predicted sensory consequences compared to 0:\n')
  print(t.test(df$predictionupdate, mu=0, alternative='less'))
  
  cat(sprintf('eta-squared: %0.5f\n', etaSquaredTtest(g1=df$predictionupdate, mu=0)))
  
  groupNs <- dim(df)[1]
  deltaMu <- mean(df$predictionupdate)
  stndevs <- sd(df$predictionupdate)
  cat('\npower analysis:\n')
  print(power.t.test( n = groupNs, 
                      delta = deltaMu, 
                      sd = stndevs, 
                      sig.level = 0.05,
                      type = 'one.sample',
                      alternative = 'one.sided',
                      strict=FALSE))
  
}

averAgeRecalibration <- function() {
  
  passive_localization_shifts <- NA
  for (agegroup in c('younger','older')) {
    
    for (instruction in c('explicit','implicit')) {
      
      group_localization <- read.csv(sprintf('data/%s%s_loc_p3_AOV.csv',list('younger'='30', 'older'='aging_')[[agegroup]],instruction), header=TRUE, stringsAsFactors=FALSE)
      
      group_localization <- group_localization[which(group_localization$passive_b == 1),]
      group_localization <- aggregate(bias_deg ~ rotated_b * participant, data=group_localization, FUN=mean)
      group_localization <- aggregate(bias_deg ~ participant, FUN=diff, data=group_localization)
      group_localization$agegroup <- agegroup
      
      if (is.data.frame(passive_localization_shifts)) {
        passive_localization_shifts <- rbind(passive_localization_shifts, group_localization)
      } else {
        passive_localization_shifts <- group_localization
      }
      
    }
    
  }
  
  ageRecal <- aggregate(bias_deg ~ agegroup, data=passive_localization_shifts, FUN=mean)
  print(ageRecal)
  
}