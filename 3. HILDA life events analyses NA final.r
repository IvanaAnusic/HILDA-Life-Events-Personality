# For graphs, use first year as number of waves before event (use average)
library(lme4)
library(ggplot2)
library(arm)
library(car)
library(reshape2)

# ---------------------------- load functions ------------------------------- #
source(".../My R Functions/LifeEventsFunctions.r")
source(".../My R Functions/LifeEventsFunctionsHILDA.r")
pathWorking <- ".../HILDA datafiles/"   # path where the new and temporary data files will be saved to

# this sets the default coding for factor variables in regression to be effect coding (rather than dummy coding) 
# this way, the intercept in the final regression will be for the average, rather than for any one category of education
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))


# =============================================================================================== #
# NEGATIVE AFFECT:
# =============================================================================================== #

persYear <- 2005 # year in which personality was assessed

# for iData:
toRemovePrefix <- c("year","Seq","Bef","Aft","React","Adapt","YrBef","YrOf","YrAft") # variables from which to remove prefix
toKeep <- c("pid","wave","na","sex","firstWave","firstEdu","firstInc","firstAge") # variables to keep in dataframes for individual events

allData <- read.csv(paste(pathWorking, "allDataNA.csv", sep=""))
allData[1:10,]
names(allData)

# check coding for unemployment
temp <- allData[!is.na(allData$unyear1),][,c("pid","wave","unyear1","unyear2","unSeq","unBef","unAft","unReact","unAdapt","unYrBef","unYrOf","unYrAft")]
temp$diff <- temp$unyear2 - temp$unyear1
temp[temp$diff>2,]

# rename unyear1 to unyear so that iData will run
names(allData)[31] <- "unyear"

# create a separate dataframe for each condition
mar <- iData("marriage", prefix="m", agename="ageMar")
child <- iData("childbirth", prefix="c", agename="ageChild")
wid <- iData("widowhood", prefix="w", agename="ageWid")
unemp <- iData("unemployment", prefix="un", agename="ageUnemp")

# check number of waves before and after
agMar <- aggregate(mar, by=list(mar$pid), FUN=sum)
agChild <- aggregate(child, by=list(child$pid), FUN=sum)
agWid <- aggregate(wid, by=list(wid$pid), FUN=sum)
agUnemp <- aggregate(unemp, by=list(unemp$pid), FUN=sum)
colMeans(agMar[,c("Bef","Aft")], na.rm=T)
colMeans(agChild[,c("Bef","Aft")], na.rm=T)
colMeans(agWid[,c("Bef","Aft")], na.rm=T)
colMeans(agUnemp[,c("Bef","Aft")], na.rm=T)


# combine with personality data
# select only people who experienced event after personality was assessed (personality assessed in 2005)
pers <- read.csv(paste(pathWorking, "clean Personality.csv", sep=""))
pers[1:5,]
marPers <- merge(mar, pers, by="pid")
childPers <- merge(child, pers, by="pid")
widPers <- merge(wid, pers, by="pid")
unempPers <- merge(unemp, pers, by="pid")
marPers[1:15,]
nrow(aggregate(marPers, by=list(marPers$pid), FUN=mean, na.rm=T))
nrow(aggregate(childPers, by=list(childPers$pid), FUN=mean, na.rm=T))
nrow(aggregate(widPers, by=list(widPers$pid), FUN=mean, na.rm=T))
nrow(aggregate(unempPers, by=list(unempPers$pid), FUN=mean, na.rm=T))
marPers <- marPers[marPers$year > 2005,]
childPers <- childPers[childPers$year > 2005,]
widPers <- widPers[widPers$year > 2005,]
unempPers <- unempPers[unempPers$year > 2005,]
nrow(aggregate(marPers, by=list(marPers$pid), FUN=mean, na.rm=T))
nrow(aggregate(childPers, by=list(childPers$pid), FUN=mean, na.rm=T))
nrow(aggregate(widPers, by=list(widPers$pid), FUN=mean, na.rm=T))
nrow(aggregate(unempPers, by=list(unempPers$pid), FUN=mean, na.rm=T))
table(marPers$year, useNA="ifany")
table(childPers$year, useNA="ifany")
table(widPers$year, useNA="ifany")
table(unempPers$year, useNA="ifany")
# scale personality variables
marPers[1:5,]
agMarPers <- aggregate(marPers, by=list(marPers$pid), FUN=mean)
agChildPers <- aggregate(childPers, by=list(childPers$pid), FUN=mean)
agWidPers <- aggregate(widPers, by=list(widPers$pid), FUN=mean)
agUnempPers <- aggregate(unempPers, by=list(unempPers$pid), FUN=mean)
agMarPers$cA <- scale(agMarPers$pA, scale=F)
agMarPers$cC <- scale(agMarPers$pC, scale=F)
agMarPers$cE <- scale(agMarPers$pE, scale=F)
agMarPers$cN <- scale(agMarPers$pN, scale=F)
agMarPers$cO <- scale(agMarPers$pO, scale=F)
agMarPers <- agMarPers[,c("pid","cA","cC","cE","cN","cO")]
agChildPers$cA <- scale(agChildPers$pA, scale=F)
agChildPers$cC <- scale(agChildPers$pC, scale=F)
agChildPers$cE <- scale(agChildPers$pE, scale=F)
agChildPers$cN <- scale(agChildPers$pN, scale=F)
agChildPers$cO <- scale(agChildPers$pO, scale=F)
agChildPers <- agChildPers[,c("pid","cA","cC","cE","cN","cO")]
agWidPers$cA <- scale(agWidPers$pA, scale=F)
agWidPers$cC <- scale(agWidPers$pC, scale=F)
agWidPers$cE <- scale(agWidPers$pE, scale=F)
agWidPers$cN <- scale(agWidPers$pN, scale=F)
agWidPers$cO <- scale(agWidPers$pO, scale=F)
agWidPers <- agWidPers[,c("pid","cA","cC","cE","cN","cO")]
agUnempPers$cA <- scale(agUnempPers$pA, scale=F)
agUnempPers$cC <- scale(agUnempPers$pC, scale=F)
agUnempPers$cE <- scale(agUnempPers$pE, scale=F)
agUnempPers$cN <- scale(agUnempPers$pN, scale=F)
agUnempPers$cO <- scale(agUnempPers$pO, scale=F)
agUnempPers <- agUnempPers[,c("pid","cA","cC","cE","cN","cO")]
nrow(agMarPers)
nrow(agChildPers)
nrow(agWidPers)
nrow(agUnempPers)
# insert centered personality variables into datasets
marPers <- merge(marPers, agMarPers)
childPers <- merge(childPers, agChildPers)
widPers <- merge(widPers, agWidPers)
unempPers <- merge(unempPers, agUnempPers)

max(marPers$Seq)
max(childPers$Seq)
max(widPers$Seq)
max(unempPers$Seq)

# make a code that is 1 in all years after the first year of event, otherwise 0 (Aft2)
marPers$Aft2 <- ifelse(marPers$Seq>0, 1, 0)
childPers$Aft2 <- ifelse(childPers$Seq>0, 1, 0)
widPers$Aft2 <- ifelse(widPers$Seq>0, 1, 0)
unempPers$Aft2 <- ifelse(unempPers$Seq>0, 1, 0)
marPers[1:15,c("pid","wave","Seq","Bef","Aft","YrOf","YrBef","YrAft","Aft2")]


# ----------------------------------------------------------
# personality analyses
# ----------------------------------------------------------
mBOA <- lmer(na ~ YrOf + Aft2 + (YrOf + Aft2 | pid), data=marPers)
mBOA_N <- lmer(na ~ YrOf + Aft2 + cN + YrOf*cN + Aft2*cN + (YrOf + Aft2 | pid), data=marPers)
mBOA_E <- lmer(na ~ YrOf + Aft2 + cE + YrOf*cE + Aft2*cE + (YrOf + Aft2 | pid), data=marPers)
mBOA_O <- lmer(na ~ YrOf + Aft2 + cO + YrOf*cO + Aft2*cO + (YrOf + Aft2 | pid), data=marPers)
mBOA_A <- lmer(na ~ YrOf + Aft2 + cA + YrOf*cA + Aft2*cA + (YrOf + Aft2 | pid), data=marPers)
mBOA_C <- lmer(na ~ YrOf + Aft2 + cC + YrOf*cC + Aft2*cC + (YrOf + Aft2 | pid), data=marPers)

cBOA <- lmer(na ~ YrOf + Aft2 + (YrOf + Aft2 | pid), data=childPers)
cBOA_N <- lmer(na ~ YrOf + Aft2 + cN + YrOf*cN + Aft2*cN + (YrOf + Aft2 | pid), data=childPers)
cBOA_E <- lmer(na ~ YrOf + Aft2 + cE + YrOf*cE + Aft2*cE + (YrOf + Aft2 | pid), data=childPers)
cBOA_O <- lmer(na ~ YrOf + Aft2 + cO + YrOf*cO + Aft2*cO + (YrOf + Aft2 | pid), data=childPers)
cBOA_A <- lmer(na ~ YrOf + Aft2 + cA + YrOf*cA + Aft2*cA + (YrOf + Aft2 | pid), data=childPers)
cBOA_C <- lmer(na ~ YrOf + Aft2 + cC + YrOf*cC + Aft2*cC + (YrOf + Aft2 | pid), data=childPers)

wBOA <- lmer(na ~ YrOf + Aft2 + (YrOf + Aft2 | pid), data=widPers)
wBOA_N <- lmer(na ~ YrOf + Aft2 + cN + YrOf*cN + Aft2*cN + (YrOf + Aft2 | pid), data=widPers)
wBOA_E <- lmer(na ~ YrOf + Aft2 + cE + YrOf*cE + Aft2*cE + (YrOf + Aft2 | pid), data=widPers)
wBOA_O <- lmer(na ~ YrOf + Aft2 + cO + YrOf*cO + Aft2*cO + (YrOf + Aft2 | pid), data=widPers)
wBOA_A <- lmer(na ~ YrOf + Aft2 + cA + YrOf*cA + Aft2*cA + (YrOf + Aft2 | pid), data=widPers)
wBOA_C <- lmer(na ~ YrOf + Aft2 + cC + YrOf*cC + Aft2*cC + (YrOf + Aft2 | pid), data=widPers)

unBOA <- lmer(na ~ YrOf + Aft2 + (YrOf + Aft2 | pid), data=unempPers)
unBOA_N <- lmer(na ~ YrOf + Aft2 + cN + YrOf*cN + Aft2*cN + (YrOf + Aft2 | pid), data=unempPers)
unBOA_E <- lmer(na ~ YrOf + Aft2 + cE + YrOf*cE + Aft2*cE + (YrOf + Aft2 | pid), data=unempPers)
unBOA_O <- lmer(na ~ YrOf + Aft2 + cO + YrOf*cO + Aft2*cO + (YrOf + Aft2 | pid), data=unempPers)
unBOA_A <- lmer(na ~ YrOf + Aft2 + cA + YrOf*cA + Aft2*cA + (YrOf + Aft2 | pid), data=unempPers)
unBOA_C <- lmer(na ~ YrOf + Aft2 + cC + YrOf*cC + Aft2*cC + (YrOf + Aft2 | pid), data=unempPers)

persResultsList <- c("mBOA","mBOA_N","mBOA_E","mBOA_O","mBOA_A","mBOA_C",
                     "cBOA","cBOA_N","cBOA_E","cBOA_O","cBOA_A","cBOA_C",
                     "wBOA","wBOA_N","wBOA_E","wBOA_O","wBOA_A","wBOA_C",
                     "unBOA","unBOA_N","unBOA_E","unBOA_O","unBOA_A","unBOA_C")
persResults <- iTableResults(persResultsList)
persResults


# ------------------------------- RA models for all events ---------------------------------------------------------- #
marRA <- lmer( na ~ React + Adapt + (React + Adapt | pid), data=mar)
childRA <- lmer( na ~ React + Adapt + (React + Adapt | pid), data=child)
widRA <- lmer( na ~ React + Adapt + (React + Adapt | pid), data=wid)
unempRA <- lmer( na ~ React + Adapt + (React + Adapt | pid), data=unemp)

iPredRA <- function (a) {
  d <- data.frame(seq=seq(-10,10, by=.5))
  d$r <- ifelse(d$seq>=-1 & d$seq<=1, 1, 0)
  d$a <- ifelse(d$seq>1, 1, 0)
  d$int <- a[1]
  d$react <- a[2]
  d$adapt <- a[3]
  d$predLS <- d$int + d$r*d$react + d$a*d$adapt
  return(d)
}

marp <- iPredRA(fixef(marRA))
childp <- iPredRA(fixef(childRA))
widp <- iPredRA(fixef(widRA))
unempp <- iPredRA(fixef(unempRA))


iPlotRA <- function(data, preddata, title="", dv="ls", dvLabel="LS", xlim=c(-10,10), ylim=c(7,9)) {
  plot <- ggplot(data=data, aes_string(x="Seq", y=dv)) + 
    stat_summary(fun.y=mean, geom="point") + 
    geom_line(data=preddata, aes_string(x="seq", y="predLS")) +
    coord_cartesian(ylim=c(ylim[1],ylim[2]),xlim=xlim) +
    scale_x_continuous("Year", breaks=seq( ifelse(xlim[1]<0, ceiling(xlim[1]), floor(xlim[1])),ifelse(xlim[2]<0, ceiling(xlim[2]), floor(xlim[2])),by=1)) +
    scale_y_continuous(dvLabel, breaks=seq(ylim[1],ylim[2],by=.5)) +
    plotformat(title)
}


marplot <- iPlotRA (mar, marp, "Marriage", xlim=c(-8.5,8.5), ylim=c(1.75,3.25), dv="na", dvLabel="Negative affect")
childplot <- iPlotRA (child, childp, "Childbirth", xlim=c(-8.5,8.5), ylim=c(1.75,3.25), dv="na", dvLabel="Negative affect")
widplot <- iPlotRA (wid, widp, "Widowhood", xlim=c(-8.5,8.5), ylim=c(1.75,3.25), dv="na", dvLabel="Negative affect")
unempplot <- iPlotRA (unemp, unempp, "Unemployment", xlim=c(-8.5,8.5), ylim=c(1.75,3.25), dv="na", dvLabel="Negative affect")

marplot
childplot
widplot
unempplot

marRA
childRA
widRA
unempRA

# ------------------------------- with control groups ---------------------------------------------------------- #
# control groups
marc <- read.csv(paste(pathWorking, "finalMarriageControlsNA.csv", sep=""))
childc <- read.csv(paste(pathWorking, "finalChildbirthControlsNA.csv", sep=""))
widc <- read.csv(paste(pathWorking, "finalWidowhoodControlsNA.csv", sep=""))
unempc <- read.csv(paste(pathWorking, "finalUnemploymentControlsNA.csv", sep=""))

toMatchOn <- c("sex","firstEdu","firstInc","firstAge")
toTransform <- "firstInc"
fMatch <- "control ~ sex + cFirstAge + I(cFirstAge^2) + as.factor(firstEdu) + cLnInc"
fRA <- "na ~ lin + control + react + adapt + sex + cFirstAge + I(cFirstAge^2) + as.factor(firstEdu) + cLnInc +
        lin*sex + lin*cFirstAge + lin*I(cFirstAge^2) + lin*as.factor(firstEdu) + lin*cLnInc +
        (react + adapt | pid)"
fRAnocovar <- "na ~ lin + control + react + adapt + (react + adapt | pid)" 

marResults <- iConRA2(mar, marc, "marCon", matchList=toMatchOn, transList=toTransform, transNames="lnInc", toCenter=c("firstAge","lnInc"), fMatch=fMatch, fRA=fRA, fRAnocovar=fRAnocovar, dv="na")
childResults <- iConRA2(child, childc, "childCon", matchList=toMatchOn, transList=toTransform, transNames="lnInc", toCenter=c("firstAge","lnInc"), fMatch=fMatch, fRA=fRA, fRAnocovar=fRAnocovar, dv="na")
widResults <- iConRA2(wid, widc, "widCon", matchList=toMatchOn, transList=toTransform, transNames="lnInc", toCenter=c("firstAge","lnInc"), fMatch=fMatch, fRA=fRA, fRAnocovar=fRAnocovar, dv="na")
unempResults <- iConRA2(unemp, unempc, "unempCon", matchList=toMatchOn, transList=toTransform, transNames="lnInc", toCenter=c("firstAge","lnInc"), fMatch=fMatch, fRA=fRA, fRAnocovar=fRAnocovar, dv="na")

marResults$RAnocovar
childResults$RAnocovar
widResults$RAnocovar
unempResults$RAnocovar

marplotC <- iPlotControlsRAnew_2.0(marResults$RAnocovar, "Marriage", first=3.53, xrange=c(-8.5,8.5), yrange=c(1.75,3.25), dv="na", dvLabel="Negative Affect")
childplotC <- iPlotControlsRAnew_2.0(childResults$RAnocovar, "Childbirth", first=4.33, xrange=c(-8.5,8.5), yrange=c(1.75,3.25), dv="na", dvLabel="Negative Affect")
widplotC <- iPlotControlsRAnew_2.0(widResults$RAnocovar, "Widowhood", first=4.48, xrange=c(-8.5,8.5), yrange=c(1.75,3.25), dv="na", dvLabel="Negative Affect")
unempplotC <- iPlotControlsRAnew_2.0(unempResults$RAnocovar, "Unemployment", first=2.89, xrange=c(-8.5,8.5), yrange=c(1.75,3.25), dv="na", dvLabel="Negative Affect")
marplotC
childplotC
widplotC
unempplotC


png(paste(pathWorking,"figure_NA.png",sep=""), width=8, height=11, units="in", res=200)
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,2)))
print(marplot,vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(childplot,vp=viewport(layout.pos.row=2,layout.pos.col=1))
print(widplot,vp=viewport(layout.pos.row=3,layout.pos.col=1))
print(unempplot,vp=viewport(layout.pos.row=4,layout.pos.col=1))
print(marplotC,vp=viewport(layout.pos.row=1,layout.pos.col=2))
print(childplotC,vp=viewport(layout.pos.row=2,layout.pos.col=2))
print(widplotC,vp=viewport(layout.pos.row=3,layout.pos.col=2))
print(unempplotC,vp=viewport(layout.pos.row=4,layout.pos.col=2))
dev.off()


# all negative affect data
nadata <- read.csv(paste(pathWorking, "clean naWide.csv", sep=""))
nadata[1:5,]
table(nadata$na_1, useNA="ifany")
nadata$allmis <- (is.na(nadata$na_1) & is.na(nadata$na_2) & is.na(nadata$na_3) & is.na(nadata$na_4) & is.na(nadata$na_5) & is.na(nadata$na_6) & is.na(nadata$na_7) & is.na(nadata$na_8) & is.na(nadata$na_9) & is.na(nadata$na_10))
table(nadata$allmis, useNA="ifany")
nadata[nadata$allmis==T,]
nadata <- nadata[nadata$allmis==F,]
nadata$allmis <- NULL
nrow(nadata)
meltna <- melt(nadata, id.vars="pid")
meltna[200001:200005,]
table(meltna$variable)
cc <- strsplit(as.character(meltna$variable), "_")
variable <- unlist(cc)[2*(1:length(as.character(meltna$variable)))-1]
wave <- unlist(cc)[2*(1:length(as.character(meltna$variable)))]
meltna$variable <- variable
meltna$wave <- as.integer(wave)
class(meltna$wave)
meltna <- meltna[!is.na(meltna$value),]
longna <- dcast(meltna, pid + wave ~ variable)
longna[1:5,]

naAll <- lmer(na ~ 1 + (1 | pid), data=longna)
naAll

# within SD = .55
# between SD = .66


