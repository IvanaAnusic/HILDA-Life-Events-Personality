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
# LIFE SATISFACTION:
# =============================================================================================== #

persYear <- 2005 # year in which personality was assessed

# for iData:
toRemovePrefix <- c("year","Seq","Bef","Aft","React","Adapt","YrBef","YrOf","YrAft") # variables from which to remove prefix
toKeep <- c("pid","wave","ls","sex","firstWave","firstEdu","firstInc","firstAge") # variables to keep in dataframes for individual events

allData <- read.csv(paste(pathWorking, "allData.csv", sep=""))
allData[1:10,]
names(allData)

# check coding for unemployment
temp <- allData[!is.na(allData$unyear1),][,c("pid","wave","unyear1","unyear2","unSeq","unBef","unAft","unReact","unAdapt","unYrBef","unYrOf","unYrAft")]
temp$diff <- temp$unyear2 - temp$unyear1
temp[temp$diff>1,]

# rename unyear1 to unyear so that iData will run
names(allData)[31] <- "unyear"

# create a separate dataframe for each condition
mar <- iData("marriage", prefix="m", agename="ageMar", varsToKeep=c(toKeep, "ageMar"))
child <- iData("childbirth", prefix="c", agename="ageChild", varsToKeep=c(toKeep, "ageChild"))
wid <- iData("widowhood", prefix="w", agename="ageWid", varsToKeep=c(toKeep, "ageWid"))
unemp <- iData("unemployment", prefix="un", agename="ageUnemp", varsToKeep=c(toKeep, "ageUnemp"))

# check number of waves before and after
agMar <- aggregate(mar, by=list(mar$pid), FUN=sum)
agChild <- aggregate(child, by=list(child$pid), FUN=sum)
agWid <- aggregate(wid, by=list(wid$pid), FUN=sum)
agUnemp <- aggregate(unemp, by=list(unemp$pid), FUN=sum)
colMeans(agMar[,c("Bef","Aft")], na.rm=T)
colMeans(agChild[,c("Bef","Aft")], na.rm=T)
colMeans(agWid[,c("Bef","Aft")], na.rm=T)
colMeans(agUnemp[,c("Bef","Aft")], na.rm=T)

# demographics
agMarM <- aggregate(mar, by=list(mar$pid), FUN=mean, na.rm=T)
table(agMarM$sex, useNA="ifany")
table(agMarM$sex, useNA="ifany")[1]/sum(table(agMarM$sex, useNA="ifany"))
mean(agMarM$ageMar, na.rm=T)
sd(agMarM$ageMar, na.rm=T)
nrow(agMarM)

agChildM <- aggregate(child, by=list(child$pid), FUN=mean, na.rm=T)
table(agChildM$sex, useNA="ifany")
table(agChildM$sex, useNA="ifany")[1]/sum(table(agChildM$sex, useNA="ifany"))
mean(agChildM$ageChild, na.rm=T)
sd(agChildM$ageChild, na.rm=T)
nrow(agChildM)

agWidM <- aggregate(wid, by=list(wid$pid), FUN=mean, na.rm=T)
table(agWidM$sex, useNA="ifany")
table(agWidM$sex, useNA="ifany")[1]/sum(table(agWidM$sex, useNA="ifany"))
mean(agWidM$ageWid, na.rm=T)
sd(agWidM$ageWid, na.rm=T)
nrow(agWidM)

agUnempM <- aggregate(unemp, by=list(unemp$pid), FUN=mean, na.rm=T)
table(agUnempM$sex, useNA="ifany")
table(agUnempM$sex, useNA="ifany")[1]/sum(table(agUnempM$sex, useNA="ifany"))
mean(agUnempM$ageUnemp, na.rm=T)
sd(agUnempM$ageUnemp, na.rm=T)
nrow(agUnempM)

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
mBOA <- lmer(ls ~ YrOf + Aft2 + (YrOf + Aft2 | pid), data=marPers)
mBOA_N <- lmer(ls ~ YrOf + Aft2 + cN + YrOf*cN + Aft2*cN + (YrOf + Aft2 | pid), data=marPers)
mBOA_E <- lmer(ls ~ YrOf + Aft2 + cE + YrOf*cE + Aft2*cE + (YrOf + Aft2 | pid), data=marPers)
mBOA_O <- lmer(ls ~ YrOf + Aft2 + cO + YrOf*cO + Aft2*cO + (YrOf + Aft2 | pid), data=marPers)
mBOA_A <- lmer(ls ~ YrOf + Aft2 + cA + YrOf*cA + Aft2*cA + (YrOf + Aft2 | pid), data=marPers)
mBOA_C <- lmer(ls ~ YrOf + Aft2 + cC + YrOf*cC + Aft2*cC + (YrOf + Aft2 | pid), data=marPers)

cBOA <- lmer(ls ~ YrOf + Aft2 + (YrOf + Aft2 | pid), data=childPers)
cBOA_N <- lmer(ls ~ YrOf + Aft2 + cN + YrOf*cN + Aft2*cN + (YrOf + Aft2 | pid), data=childPers)
cBOA_E <- lmer(ls ~ YrOf + Aft2 + cE + YrOf*cE + Aft2*cE + (YrOf + Aft2 | pid), data=childPers)
cBOA_O <- lmer(ls ~ YrOf + Aft2 + cO + YrOf*cO + Aft2*cO + (YrOf + Aft2 | pid), data=childPers)
cBOA_A <- lmer(ls ~ YrOf + Aft2 + cA + YrOf*cA + Aft2*cA + (YrOf + Aft2 | pid), data=childPers)
cBOA_C <- lmer(ls ~ YrOf + Aft2 + cC + YrOf*cC + Aft2*cC + (YrOf + Aft2 | pid), data=childPers)

wBOA <- lmer(ls ~ YrOf + Aft2 + (YrOf + Aft2 | pid), data=widPers)
wBOA_N <- lmer(ls ~ YrOf + Aft2 + cN + YrOf*cN + Aft2*cN + (YrOf + Aft2 | pid), data=widPers)
wBOA_E <- lmer(ls ~ YrOf + Aft2 + cE + YrOf*cE + Aft2*cE + (YrOf + Aft2 | pid), data=widPers)
wBOA_O <- lmer(ls ~ YrOf + Aft2 + cO + YrOf*cO + Aft2*cO + (YrOf + Aft2 | pid), data=widPers)
wBOA_A <- lmer(ls ~ YrOf + Aft2 + cA + YrOf*cA + Aft2*cA + (YrOf + Aft2 | pid), data=widPers)
wBOA_C <- lmer(ls ~ YrOf + Aft2 + cC + YrOf*cC + Aft2*cC + (YrOf + Aft2 | pid), data=widPers)

unBOA <- lmer(ls ~ YrOf + Aft2 + (YrOf + Aft2 | pid), data=unempPers)
unBOA_N <- lmer(ls ~ YrOf + Aft2 + cN + YrOf*cN + Aft2*cN + (YrOf + Aft2 | pid), data=unempPers)
unBOA_E <- lmer(ls ~ YrOf + Aft2 + cE + YrOf*cE + Aft2*cE + (YrOf + Aft2 | pid), data=unempPers)
unBOA_O <- lmer(ls ~ YrOf + Aft2 + cO + YrOf*cO + Aft2*cO + (YrOf + Aft2 | pid), data=unempPers)
unBOA_A <- lmer(ls ~ YrOf + Aft2 + cA + YrOf*cA + Aft2*cA + (YrOf + Aft2 | pid), data=unempPers)
unBOA_C <- lmer(ls ~ YrOf + Aft2 + cC + YrOf*cC + Aft2*cC + (YrOf + Aft2 | pid), data=unempPers)

persResultsList <- c("mBOA","mBOA_N","mBOA_E","mBOA_O","mBOA_A","mBOA_C",
                     "cBOA","cBOA_N","cBOA_E","cBOA_O","cBOA_A","cBOA_C",
                     "wBOA","wBOA_N","wBOA_E","wBOA_O","wBOA_A","wBOA_C",
                     "unBOA","unBOA_N","unBOA_E","unBOA_O","unBOA_A","unBOA_C")
persResults <- iTableResults(persResultsList)
persResults

# ----------------------------------------------------------
# nonlinear
# ----------------------------------------------------------
mSvals <- c(asympBef=7.545, changeBef=.061, reactDiff=.575, asympDiff=.083, changeAft=.117)
cSvals <- c(asympBef=7.838, changeBef=.149, reactDiff=.225, asympDiff=-.050, changeAft=.651)
wSvals <- c(asympBef=8.439, changeBef=.753, reactDiff=-.873, asympDiff=-.379, changeAft=.520)
unSvals <- c(asympBef=7.794, changeBef=.666, reactDiff=-0.207, asympDiff=-0.452, changeAft=.096)
# regression
mNonlin <- nlmer( ls ~ iAsympDiffG(Seq, Bef, Aft, asympBef, changeBef, reactDiff, asympDiff, changeAft) ~ (asympBef + reactDiff + asympDiff|pid), data=mar, start=mSvals, verbose=T)
cNonlin <- nlmer( ls ~ iAsympDiffG(Seq, Bef, Aft, asympBef, changeBef, reactDiff, asympDiff, changeAft) ~ (asympBef + reactDiff + asympDiff|pid), data=child, start=cSvals, verbose=T)
wNonlin <- nlmer( ls ~ iAsympDiffG(Seq, Bef, Aft, asympBef, changeBef, reactDiff, asympDiff, changeAft) ~ (asympBef + reactDiff + asympDiff|pid), data=wid, start=wSvals, verbose=T)
unNonlin <- nlmer( ls ~ iAsympDiffG(Seq, Bef, Aft, asympBef, changeBef, reactDiff, asympDiff, changeAft) ~ (asympBef + reactDiff + asympDiff|pid), data=unemp, start=unSvals, verbose=T)
mNonlin
cNonlin
wNonlin
unNonlin

mNonlin_plot <- iPlotEvent(dataRaw=mar, dataPred=iPredNonlin(fixef(mNonlin)), xRaw="Seq", title="Marriage", xlim=c(-8.5,8.5), ylim=c(7,9), dv="ls")
cNonlin_plot <- iPlotEvent(dataRaw=child, dataPred=iPredNonlin(fixef(cNonlin)), xRaw="Seq", title="Childbirth", xlim=c(-8.5,8.5), ylim=c(7,9), dv="ls")
wNonlin_plot <- iPlotEvent(dataRaw=wid, dataPred=iPredNonlin(fixef(wNonlin)), xRaw="Seq", title="Widowhood", xlim=c(-8.5,8.5), ylim=c(7,9), dv="ls")
unNonlin_plot <- iPlotEvent(dataRaw=unemp, dataPred=iPredNonlin(fixef(unNonlin)), xRaw="Seq", title="Unemployment", xlim=c(-8.5,8.5), ylim=c(7,9), dv="ls")
mNonlin_plot
cNonlin_plot
wNonlin_plot
unNonlin_plot

# with control groups
marc <- read.csv(paste(pathWorking, "finalMarriageControls.csv", sep=""))
childc <- read.csv(paste(pathWorking, "finalChildbirthControls.csv", sep=""))
widc <- read.csv(paste(pathWorking, "finalWidowhoodControls.csv", sep=""))
unempc <- read.csv(paste(pathWorking, "finalUnemploymentControls.csv", sep=""))

toMatchOn <- c("sex","firstEdu","firstInc","firstAge")
toTransform <- "firstInc"
fMatch <- "control ~ sex + cFirstAge + I(cFirstAge^2) + as.factor(firstEdu) + cLnInc"
fNL <-  "ls ~ iNonlinControlsG(seq, lin, bef, aft, event, control, linear, asympBef, changeBef, reactDiff, asympDiff, changeAft, group) ~ (asympBef + reactDiff + asympDiff|pid)"
fNL2 <-  "ls ~ iNonlinControlsG(seq, lin, bef, aft, event, control, linear, asympBef, changeBef, reactDiff, asympDiff, changeAft, group) ~ (asympBef + asympDiff|pid)"

# start values
mcSvals <- c(linear=-.028, asympBef=7.872, changeBef=.290, reactDiff=.411, asympDiff=.270, changeAft=.515, group=-.127)  # works with (asympBef + asympDiff|pid)
ccSvals <- c(linear=-.012, asympBef=7.846, changeBef=.210, reactDiff=.298, asympDiff=.060, changeAft=.895, group=-.054)  # works with (asympBef + asympDiff|pid)
wcSvals <- c(linear=-.042, asympBef=8.479, changeBef=.855, reactDiff=-.705, asympDiff=-.058, changeAft=.489, group=-.019)
uncSvals <- c(linear=-.029,asympBef=7.801, changeBef=.761, reactDiff=-0.118, asympDiff=-0.015, changeAft=.109, group=.250)

mResultsNL <- iConNL2(mar, marc, "marCon", matchList=toMatchOn, transList=toTransform, transNames="lnInc", toCenter=c("firstAge","lnInc"), fMatch=fMatch, fNL=fNL, svals=mcSvals)
cResultsNL <- iConNL2(child, childc, "childCon", matchList=toMatchOn, transList=toTransform, transNames="lnInc", toCenter=c("firstAge","lnInc"), fMatch=fMatch, fNL=fNL, svals=ccSvals)
wResultsNL <- iConNL2(wid, widc, "widCon", matchList=toMatchOn, transList=toTransform, transNames="lnInc", toCenter=c("firstAge","lnInc"), fMatch=fMatch, fNL=fNL, svals=wcSvals)
unResultsNL <- iConNL2(unemp, unempc, "unempCon", matchList=toMatchOn, transList=toTransform, transNames="lnInc", toCenter=c("firstAge","lnInc"), fMatch=fMatch, fNL=fNL, svals=uncSvals)

mResultsNL2 <- iConNL2(mar, marc, "marCon", matchList=toMatchOn, transList=toTransform, transNames="lnInc", toCenter=c("firstAge","lnInc"), fMatch=fMatch, fNL=fNL2, svals=mcSvals)
cResultsNL2 <- iConNL2(child, childc, "childCon", matchList=toMatchOn, transList=toTransform, transNames="lnInc", toCenter=c("firstAge","lnInc"), fMatch=fMatch, fNL=fNL2, svals=ccSvals)

mResultsNL$NL
mResultsNL2$NL
cResultsNL$NL
cResultsNL2$NL
wResultsNL$NL
unResultsNL$NL

rm2 <- mResultsNL2$NL
rc2 <- cResultsNL2$NL
rw <- wResultsNL$NL
run <- unResultsNL$NL

lsResultsList <- c("mNonlin","cNonlin","wNonlin","unNonlin",
                   "rm2","rc2","rw","run")
lsResults <- iTableResults(lsResultsList)
lsResults


# nonlinear with controls
mNonlin_plotC2 <- iPlotControlsNLnew_2.0(mResultsNL2$NL, "Marriage", first=3.83, xrange=c(-8.5,8.5), yrange=c(7, 9), dv="ls", dvLabel="Life Satisfaction")
cNonlin_plotC2 <- iPlotControlsNLnew_2.0(cResultsNL2$NL, "Childbirth", first=4.73, xrange=c(-8.5,8.5), yrange=c(7, 9), dv="ls", dvLabel="Life Satisfaction")
wNonlin_plotC <- iPlotControlsNLnew_2.0(wResultsNL$NL, "Widowhood", first=4.60, xrange=c(-8.5,8.5), yrange=c(7, 9), dv="ls", dvLabel="Life Satisfaction")
unNonlin_plotC <- iPlotControlsNLnew_2.0(unResultsNL$NL, "Unemployment", first=3.04, xrange=c(-8.5,8.5), yrange=c(7, 9), dv="ls", dvLabel="Life Satisfaction")

mNonlin_plotC2
cNonlin_plotC2
wNonlin_plotC
unNonlin_plotC


png(paste(pathWorking,"figure_LS.png",sep=""), width=8, height=11, units="in", res=200)
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,2)))
print(mNonlin_plot,vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(cNonlin_plot,vp=viewport(layout.pos.row=2,layout.pos.col=1))
print(wNonlin_plot,vp=viewport(layout.pos.row=3,layout.pos.col=1))
print(unNonlin_plot,vp=viewport(layout.pos.row=4,layout.pos.col=1))
print(mNonlin_plotC2,vp=viewport(layout.pos.row=1,layout.pos.col=2))
print(cNonlin_plotC2,vp=viewport(layout.pos.row=2,layout.pos.col=2))
print(wNonlin_plotC,vp=viewport(layout.pos.row=3,layout.pos.col=2))
print(unNonlin_plotC,vp=viewport(layout.pos.row=4,layout.pos.col=2))
dev.off()



# all life satisfaciton data
lsdata <- read.csv(paste(pathWorking, "clean lsWide.csv", sep=""))
lsdata[1:5,]
table(lsdata$ls_1, useNA="ifany")
lsdata$allmis <- (is.na(lsdata$ls_1) & is.na(lsdata$ls_2) & is.na(lsdata$ls_3) & is.na(lsdata$ls_4) & is.na(lsdata$ls_5) & is.na(lsdata$ls_6) & is.na(lsdata$ls_7) & is.na(lsdata$ls_8) & is.na(lsdata$ls_9) & is.na(lsdata$ls_10))
table(lsdata$allmis, useNA="ifany")
lsdata[lsdata$allmis==T,]
lsdata <- lsdata[lsdata$allmis==F,]
lsdata$allmis <- NULL
nrow(lsdata)
meltls <- melt(lsdata, id.vars="pid")
meltls[1:5,]
table(meltls$variable)
cc <- strsplit(as.character(meltls$variable), "_")
variable <- unlist(cc)[2*(1:length(as.character(meltls$variable)))-1]
wave <- unlist(cc)[2*(1:length(as.character(meltls$variable)))]
meltls$variable <- variable
meltls$wave <- wave
table(meltls$wave)
meltls <- meltls[!is.na(meltls$value),]
longls <- dcast(meltls, pid + wave ~ variable)
longls[1:5,]

lsAll <- lmer(ls ~ 1 + (1 | pid), data=longls)
lsAll




# personality
events <- read.csv(paste(pathWorking, "allEvents.csv", sep=""))
events[1:5,]
pers <- read.csv(paste(pathWorking, "clean Personality.csv", sep=""))
pers[1:5,]
cor(pers[,c("pN","pE","pO","pA","pC")], use="pairwise.complete.obs")
sapply(pers[,c("pN","pE","pO","pA","pC")], mean, na.rm=T)
sapply(pers[,c("pN","pE","pO","pA","pC")], sd, na.rm=T)

marPers <- merge(mar, pers, all.x=T)
childPers <- merge(child, pers, all.x=T)
widPers <- merge(wid, pers, all.x=T)
unempPers <- merge(unemp, pers, all.x=T)

sapply(marPers[,c("pN","pE","pO","pA","pC")], mean, na.rm=T)
sapply(marPers[,c("pN","pE","pO","pA","pC")], sd, na.rm=T)

sapply(childPers[,c("pN","pE","pO","pA","pC")], mean, na.rm=T)
sapply(childPers[,c("pN","pE","pO","pA","pC")], sd, na.rm=T)

sapply(widPers[,c("pN","pE","pO","pA","pC")], mean, na.rm=T)
sapply(widPers[,c("pN","pE","pO","pA","pC")], sd, na.rm=T)

sapply(unempPers[,c("pN","pE","pO","pA","pC")], mean, na.rm=T)
sapply(unempPers[,c("pN","pE","pO","pA","pC")], sd, na.rm=T)

pers[1:5,]
nrow(pers[!is.na(pers$pN),])
nrow(pers[!is.na(pers$pE),])
nrow(pers[!is.na(pers$pO),])
nrow(pers[!is.na(pers$pA),])
nrow(pers[!is.na(pers$pC),])

