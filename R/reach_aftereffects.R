source('R/shared.R')
library(svglite)

plotReachAftereffects <- function(target='inline') {
  
  if (target == 'svg') {
    svglite(file='doc/Fig4-i.svg', width=2.63 * 2 * (4/3), height=3 * (4/3), system_fonts=list(sans='Arial'))
  }
  
  styles <- getStyle()
  
  par(mfrow=c(1,1), mar=c(4,4,2,0.1))
  
  ylims=c(-.1*max(styles$rotation),max(styles$rotation)+(.2*max(styles$rotation))+20)
  
  plot(c(-.5,3.5),c(0,0),type='l',lty=1,col=rgb(.75,.75,.75),xlim=c(-0.5,3.5),ylim=ylims,bty='n',
       xaxt='n',yaxt='n',xlab='strategy use',ylab='reach deviation [°]',main='',font.main=1)
  lines(c(-.5,3.5),c(30,30),col=rgb(.75,.75,.75))
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]

    reachaftereffects <- read.csv(sprintf('data/%s_nocursors.csv',group), stringsAsFactors=FALSE)
    
    reachaftereffects$exclusive <- reachaftereffects$exclusive - reachaftereffects$aligned
    reachaftereffects$inclusive <- reachaftereffects$inclusive - reachaftereffects$aligned
    
    meanExc <- mean(reachaftereffects$exclusive)
    meanInc <- mean(reachaftereffects$inclusive)
    
    coord.x <- c(1,1,2,2)
    coord.y <- c(t.interval(reachaftereffects$exclusive),rev(t.interval(reachaftereffects$inclusive)))
    polygon(coord.x, coord.y, col=as.character(styles$color_trans[groupno]), border=NA)
    
    # add individual data
    for (condition in c('exclusive','inclusive')) {
      
      if (condition == 'exclusive') {
        groupX <- ((groupno/4)-0.5)
      }
      if (condition == 'inclusive') {
        groupX <- ((groupno/4) + 2.25)
      }
      
      Y <- reachaftereffects[,condition]
      X <- rep(groupX,length(Y))
      points(x=X,y=Y,pch=16,cex=1.5,col=as.character(styles$color_trans[groupno]))
      
      # add bootstrapped confidence intervals:
      groupX <- groupX + .075
      
      meandist <- getConfidenceInterval(data=Y, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      
      DX <- meandist$density$x
      DY <- meandist$density$y / max(meandist$density$y) * .075
      
      DX <- c(DX[1], DX, DX[length(DX)])
      DY <- c(0,     DY, 0)
      
      #polygon(x=DY+groupX, y=DX, border=FALSE, col=as.character(styles$color_trans[groupno]))
      #lines(x=rep(groupX,2),y=meandist$CI95,col=as.character(styles$color_solid[groupno]))
      #points(x=groupX,y=mean(Y),pch=16,cex=0.8,col=as.character(styles$color_solid[groupno]))
      
      
    }
    
  }
  
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    offset <- (groupno - ((length(styles$group) - 1) / 2)) * .035
    
    reachaftereffects <- read.csv(sprintf('data/%s_nocursors.csv',group), stringsAsFactors=FALSE)
    
    reachaftereffects$exclusive <- reachaftereffects$exclusive - reachaftereffects$aligned
    reachaftereffects$inclusive <- reachaftereffects$inclusive - reachaftereffects$aligned
    
    meanExc <- mean(reachaftereffects$exclusive)
    meanInc <- mean(reachaftereffects$inclusive)
    
    lines(c(1,2),c(meanExc,meanInc),col=as.character(styles$color_solid[groupno]),lty=styles$linestyle[groupno],lw=2)
    
  }
  
  axis(side=1, at=c(1,2), labels=c('without strategy','with strategy'),cex.axis=0.85)
  if (max(styles$rotation) == 30) {
    axis(side=2, at=c(0,15,30,45),cex.axis=0.85)
  }
  
  # legend(0.5,max(styles$rotation)*(7/6),styles$label,col=as.character(styles$color),lty=styles$linestyle,bty='n',cex=0.85)
  #legend(0.8,11,styles$label,col=as.character(styles$color_solid),lw=2,lty=styles$linestyle,bty='n',cex=0.75,seg.len = 3)
  legend(0.8,50,styles$label,col=as.character(styles$color_solid),lw=2,lty=styles$linestyle,bty='n',cex=0.75,seg.len = 3)
  
  
  
  if (target == 'svg') {
    dev.off()
  }

}

getRAE4ANOVA <- function(styles) {
  
  agegroup       <- c()
  instructed     <- c()
  participant    <- c()
  strategy       <- c()
  reachdeviation <- c()
  
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
    
    df <- read.csv(sprintf('data/%s_nocursors.csv',group),stringsAsFactors=F)
    
    df$exclusive <- df$exclusive - df$aligned
    df$inclusive <- df$inclusive - df$aligned
    
    
    # we need to know the number of participants to replicate some values:
    N <- dim(df)[1]
    
    for (thisstrategy in c('exclusive','inclusive')) {
      
      agegroup        <- c(agegroup, rep(thisagegroup, N))
      instructed      <- c(instructed, rep(thisinstructed, N))
      participant     <- c(participant, c(startingID : (startingID + N - 1)))
      strategy        <- c(strategy, rep(thisstrategy, N))
      reachdeviation  <- c(reachdeviation, df[,thisstrategy])
      
    }
    
    startingID <- startingID + N
    
  }
  
  # put it in a data frame:
  RAEaov <- data.frame(agegroup, instructed, participant, strategy, reachdeviation)
  
  # set relevant columns as factors:
  RAEaov$agegroup <- as.factor(RAEaov$agegroup)
  RAEaov$instructed <- as.factor(RAEaov$instructed)
  RAEaov$strategy <- as.factor(RAEaov$strategy)
  
  return(RAEaov)
  
}

RAE.ANOVA <- function() {
  
  default.contrasts <- options('contrasts')
  options(contrasts=c('contr.sum','contr.poly'))
  
  styles <- getStyle()

  RAE4aov <- getRAE4ANOVA(styles)
  
  #learning curve ANOVA's
  # for ez, case ID should be a factor:
  RAE4aov$participant <- as.factor(RAE4aov$participant)
  print(ezANOVA(data=RAE4aov, wid=participant, dv=reachdeviation, within=strategy, between=c(instructed, agegroup),type=3))
  
  # this still includes some interactions?
  
  options('contrasts' <- default.contrasts)
  
}


RAE.posthoc <- function(doEmmeans=FALSE, doSubAOVs=FALSE, doTtests=TRUE) {
  
  # here we test if the main effect of age is driven by one data-point:
  # the instructed older adults in exclude strategy reach aftereffects
  
  default.contrasts <- options('contrasts')
  options(contrasts=c('contr.sum','contr.poly'))
  
  styles <- getStyle()
  
  RAE4aov <- getRAE4ANOVA(styles)
  
  # for ez, case ID should be a factor:
  RAE4aov$participant <- as.factor(RAE4aov$participant)
  
  
  #### CODE BELOW USES 'AFEX' AND 'EMMEANS' FOR POSTHOC CONTRASTS:
  # to use it: change FALSE to TRUE
  # and make sure the packages are installed
  
  # the main effect of age disappears in these posthocs
  # if any p-value adjustment method is used
  # this already shows its a weak effect
  
  if (doEmmeans) {
    
    cat('AFEX ANOVA:\n')
    
    AOVmodel <- aov_ez("participant","reachdeviation",RAE4aov,between=c('agegroup','instructed'),within=c("strategy"))
    print(nice(AOVmodel))
    
    library(emmeans)
    PoHo <- emmeans::emmeans(AOVmodel,specs=c('agegroup','instructed','strategy'))
    cat('\nmodel cell means and confidence intervals:\n\n')
    print(PoHo)
    
    AGEGROUP <- c(-1, 1,-1, 1,-1, 1,-1, 1)
    AGE_WOS  <- c(-1, 1,-1, 1, 0, 0, 0, 0)
    AGE_WS   <- c( 0, 0, 0, 0,-1, 1,-1, 1)
    INSTR_OA <- c(-1,-1, 1,-1, 0, 0, 0, 0)
    AGE_NONI <- c(-1, 1, 0, 0,-1, 1, 0, 0)
    
    contrastList <- list( 'all younger vs. all older'=AGEGROUP, 
                          'exclusive younger vs. older'=AGE_WOS, 
                          'inclusive younger vs. older'=AGE_WS, 
                          'exclusive instr. older vs. other exclusive' = INSTR_OA,
                          'non-instructed age effects'=AGE_NONI)
    comparisons <- contrast(emmeans(AOVmodel,specs=c('agegroup','instructed','strategy')), contrastList, adjust='none') # adjust = FDR? sidak? Bonferroni?
    
    cat('\nsome specific contrasts:\n\n')
    print(comparisons)
    
  }
  
  
  if (doSubAOVs) {
    
    # WITHOUT STRATEGY (exclusive)
    cat('\nWITHOUT STRATEGY\n\n')
    print(ezANOVA(data=RAE4aov[which(RAE4aov$strategy == 'exclusive'),], wid=participant, dv=reachdeviation, between=c(agegroup, instructed),type=3))
    
    # WITH STRATEGY (inclusive)
    cat('\nWITH STRATEGY\n\n')
    print(ezANOVA(data=RAE4aov[which(RAE4aov$strategy == 'inclusive'),], wid=participant, dv=reachdeviation, between=c(agegroup, instructed),type=3))
    
    # NON-INSTRUCTED
    cat('\nNON-INSTRUCTED\n\n')
    print(ezANOVA(data=RAE4aov[which(RAE4aov$instructed == FALSE),], wid=participant, dv=reachdeviation, within=c(strategy), between=c(agegroup),type=3))
    
    # INSTRUCTED
    cat('\nINSTRUCTED\n\n')
    print(ezANOVA(data=RAE4aov[which(RAE4aov$instructed == TRUE),], wid=participant, dv=reachdeviation, within=c(strategy), between=c(agegroup),type=3))
    
  }
  
  if (doTtests) {
    
    cat('\nWITHOUT STRATEGY T-TESTS:\n')
    WOS_IN_OA <- RAE4aov$reachdeviation[which(RAE4aov$instructed == TRUE & RAE4aov$agegroup == 'older' & RAE4aov$strategy == 'exclusive')]
    WOS_IN_YA <- RAE4aov$reachdeviation[which(RAE4aov$instructed == TRUE & RAE4aov$agegroup == 'younger' & RAE4aov$strategy == 'exclusive')]
    WOS_NI_OA <- RAE4aov$reachdeviation[which(RAE4aov$instructed == FALSE & RAE4aov$agegroup == 'older' & RAE4aov$strategy == 'exclusive')]
    WOS_NI_YA <- RAE4aov$reachdeviation[which(RAE4aov$instructed == FALSE & RAE4aov$agegroup == 'younger' & RAE4aov$strategy == 'exclusive')]
    
    # contrasting the older instructed participants with the 3 other groups:
    IOvsIY <- t.test(WOS_IN_OA, WOS_IN_YA)
    IOvsNO <- t.test(WOS_IN_OA, WOS_NI_OA)
    IOvsNY <- t.test(WOS_IN_OA, WOS_NI_YA)
    
    # comparing the three other groups with each other:
    IYvsNO <- t.test(WOS_IN_YA, WOS_NI_OA)
    IYvsNY <- t.test(WOS_IN_YA, WOS_NI_YA)
    NOvsNY <- t.test(WOS_NI_OA, WOS_NI_YA)
    
    pvals <- c('instructed older vs. instructed younger'         = IOvsIY$p.value,
               'instructed older vs. non-instructed older'       = IOvsNO$p.value,
               'instructed older vs. non-instructed younger'     = IOvsNY$p.value,
               'instructed younger vs. non-instructed older'     = IYvsNO$p.value,
               'instructed younger vs. non-instructed younger'   = IYvsNY$p.value,
               'non-instructed older vs. non-instructed younger' = NOvsNY$p.value)
    
    useMethod='none'
    print(data.frame('p-values'=p.adjust(pvals, method=useMethod)))
    # method should be one of “holm”, “hochberg”, “hommel”, “bonferroni”, “BH”, “BY”, “fdr”, “none”
    cat(sprintf('\nmethod use to adjust p-values: %s\n',useMethod))
  }
  
  # this still includes some interactions?
  options('contrasts' <- default.contrasts)
  
}

RAE.Ttests <- function() {
  
  styles <- getStyle()
  
  RAE4aov <- getRAE4ANOVA(styles)
  
  RAE4aov$participant <- as.factor(RAE4aov$participant)
  
  OlderINSTR <- RAE4aov$reachdeviation[which(RAE4aov$strategy == 'exclusive' & RAE4aov$agegroup == 'older' & RAE4aov$instructed == TRUE)] 
  YoungINSTR <- RAE4aov$reachdeviation[which(RAE4aov$strategy == 'exclusive' & RAE4aov$agegroup == 'younger' & RAE4aov$instructed == TRUE)] 
  
  cat('Instructed, without-strategy:\n')
  print(t.test(OlderINSTR, YoungINSTR, alternative = "greater"))
  
}

NoCursorANOVA <- function() {
  
  default.contrasts <- options('contrasts')
  options(contrasts=c('contr.sum','contr.poly'))
  
  styles <- getStyle()
  
  NC4aov <- getNoCursors4ANOVA(styles)
  
  NC4aov$participant <- as.factor(NC4aov$participant)
  print(ezANOVA(data=NC4aov, wid=participant, dv=reachdeviation, within=training, between=c(instructed, agegroup),type=3))
  
  options('contrasts' <- default.contrasts)
}

getNoCursors4ANOVA <- function(styles) {
  
  # placeholder for data frame:
  NC4aov <- NA
  
  # loop through groups to collect their data:
  for (groupno in c(1:length(styles$group))) {
    
    group <- styles$group[groupno]
    
    # set up some basic descriptors that apply to the group:
    if (substr(group, 1, 5) == 'aging') {
      thisagegroup <- 'older'
    } else {
      thisagegroup <- 'younger'
    }
    thisinstructed <- grepl('explicit', group)
    
    df <- read.csv(sprintf('data/%s_nocursors.csv',group),stringsAsFactors=F)
    
    AL.NC <- df[,c('participant','aligned')]
    colnames(AL.NC)[2] <- 'reachdeviation'
    AL.NC$training <- 'aligned'
    
    RO.NC <- df[,c('participant','exclusive')]
    colnames(RO.NC)[2] <- 'reachdeviation'
    RO.NC$training <- 'rotated'
    
    df <- rbind(AL.NC, RO.NC)
    df$agegroup <- thisagegroup
    df$instructed <- thisinstructed
    
    if (is.data.frame(NC4aov)) {
      NC4aov <- rbind(NC4aov, df)
    } else {
      NC4aov <- df
    }
    
  }
  
  NC4aov$instructed <- as.factor(NC4aov$instructed)
  NC4aov$agegroup <- as.factor(NC4aov$agegroup)
  NC4aov$training <- as.factor(NC4aov$training)
  
  return(NC4aov)
  
}




NoCursorTtests <- function() {
  
  styles <- getStyle()
  
  NC4aov <- getNoCursors4ANOVA(styles)
  
  NC4aov$participant <- as.factor(NC4aov$participant)
  
  OlderINSTR <- NC4aov$reachdeviation[which(NC4aov$training == 'rotated' & NC4aov$agegroup == 'older' & NC4aov$instructed == TRUE)] - NC4aov$reachdeviation[which(NC4aov$training == 'aligned' & NC4aov$agegroup == 'older' & NC4aov$instructed == TRUE)]
  OlderNONIN <- NC4aov$reachdeviation[which(NC4aov$training == 'rotated' & NC4aov$agegroup == 'older' & NC4aov$instructed == FALSE)] - NC4aov$reachdeviation[which(NC4aov$training == 'aligned' & NC4aov$agegroup == 'older' & NC4aov$instructed == FALSE)]
  YoungINSTR <- NC4aov$reachdeviation[which(NC4aov$training == 'rotated' & NC4aov$agegroup == 'younger' & NC4aov$instructed == TRUE)] - NC4aov$reachdeviation[which(NC4aov$training == 'aligned' & NC4aov$agegroup == 'younger' & NC4aov$instructed == TRUE)]
  YoungNONIN <- NC4aov$reachdeviation[which(NC4aov$training == 'rotated' & NC4aov$agegroup == 'younger' & NC4aov$instructed == FALSE)] - NC4aov$reachdeviation[which(NC4aov$training == 'aligned' & NC4aov$agegroup == 'younger' & NC4aov$instructed == FALSE)]
  
  cat('Older participants:\n')
  
  print(t.test(OlderINSTR, OlderNONIN, alternative = "less"))
  
  cat(sprintf('eta-squared: %0.5f\n\n', etaSquaredTtest(OlderINSTR, OlderNONIN)))
  
  groupNs <- c(length(OlderINSTR), length(OlderNONIN))
  deltaMu <- mean(OlderINSTR) - mean(OlderNONIN)
  #stndevs <- sd(c(DVs[[1]]-mean(DVs[[1]]), DVs[[2]]-mean(DVs[[2]])))
  stndevs <- c(sd(OlderINSTR), sd(OlderNONIN))
  cat('\npower analysis:\n')
  print(power.t.test( n = groupNs, 
                      delta = deltaMu, 
                      sd = stndevs, 
                      sig.level = 0.05,
                      type = 'two.sample',
                      alternative = 'two.sided',
                      strict=FALSE))
  
  
  cat('Younger participants:\n')
  
  print(t.test(YoungINSTR, YoungNONIN, alternative = "less"))

  cat(sprintf('eta-squared: %0.5f\n', etaSquaredTtest(YoungINSTR, YoungNONIN)))
  
  groupNs <- c(length(YoungINSTR), length(YoungNONIN))
  deltaMu <- mean(YoungINSTR) - mean(YoungNONIN)
  #stndevs <- sd(c(DVs[[1]]-mean(DVs[[1]]), DVs[[2]]-mean(DVs[[2]])))
  stndevs <- c(sd(YoungINSTR), sd(YoungNONIN))
  cat('\npower analysis:\n')
  print(power.t.test( n = groupNs, 
                      delta = deltaMu, 
                      sd = stndevs, 
                      sig.level = 0.05,
                      type = 'two.sample',
                      alternative = 'two.sided',
                      strict=FALSE))
  

}
