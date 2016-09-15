library(foreign)
library(Hmisc)
library(car)
library(reshape2)

# ---------------------------- load functions ------------------------------- #
source(".../My R Functions/LifeEventsFunctions.r")
source(".../LifeEventsFunctionsHILDA.r")

# ------------------ THESE ARE CONSTANT IN A PROJECT ------------------ #
pathOriginalData <- ".../Panel Data/HILDA10/STATA/"     # path of original panel data files
pathWorking <- ".../HILDA datafiles/"   # path where the new and temporary data files will be saved to

firstYearOfStudy <- 2001         # first year of study 
lastYearOfStudy <- 2010          # last year of study
lastWaveOfStudy <- 10             # last wave of study (i.e., wave number in last year of study) (first wave of study is always 1)
firstYearOfLS <- 2001   # first year that life satisfaction is collected (e.g., for BHPS life satisfaction, this is 1996)
lastYearOfLS <- lastYearOfStudy  # last year that life satisfaction is collected
firstWaveOfLS <- 1               # first wave that life satisfaction is collected (e.g., for BHPS life satisfaction, this is 6)
lastWaveOfLS <- lastWaveOfStudy  # last wave that life satisfaction is collected

# these are necessary for iGetVariables()
originalDataFile <- "Rperson_$100c.dta"  # name of original data file (from which to pull data)
oldID <- "xwaveid"                      # cross-wave variable, indicates person id
betaFileName <- ""                  # this is used for BETA waves of SHP; set this to "" if there is no beta file
betaYearNumber <- ""                # this is the two digit year of the beta file, in character format
charsToSub <- "\\$"                 # placeholder for wave or year number in file/variable names (e.g., $$ in nchild$$)
# wFile = number or character that indicates wave/year in file names
# wVar = number or character that indicates wave/year in variable names
# these have to be in character format
wFile <- paste(letters[1:lastWaveOfStudy]) 
wVar <- wFile           

# these are necessary for iSelectSample()
# define constants
single <- 1
married <- 10
separated <- 100
divorced <- 1000
widowed <- 10000
other <- 888
noresp <- 0
nochild <- 1
child <- 10
employed <- 1
unemployed <- 10
other <- 888
noresp <- 0
# change constants
same <- 0

same <- 0
NA_single <- single - noresp
NA_married <- married - noresp
single_NA <- noresp - single
single_married <- married - single
married_NA <- noresp - married
married_single <- single - married
NA_widowed <- widowed - noresp
married_widowed <- widowed - married
widowed_NA <- noresp - widowed
widowed_married <- married - widowed
NA_divorced <- divorced - noresp
married_divorced <- divorced - married
divorced_NA <- noresp - divorced
divorced_married <- married - divorced
NA_nochild <- nochild - noresp
NA_child <- child - noresp
nochild_NA <- noresp - nochild
nochild_child <- child - nochild
child_NA <- noresp - child
child_nochild <- nochild - child
NA_employed <- employed - noresp
NA_unemployed <- unemployed - noresp
employed_NA <- noresp - employed
employed_unemployed <- unemployed - employed
unemployed_NA <- noresp - unemployed
unemployed_employed <- employed - unemployed

# these are the original values (in the raw datafiles)
origSingle <- "c(2,6)"
origMarried <- 1
origSeparated <- 3
origDivorced <- 4
origWidowed <- 5
origNoChild <- 0
origChild <- 1
origUnemployed <- "3:4"

# recoding syntax (recode NA to 0):
marriageRecode <- paste("-999:0=", noresp, "; ", origSingle, "=", single, "; ", origMarried, "=", married, "; NA=", noresp, "; else=", other, sep="")
childbirthRecode <- paste("-999:-1=", noresp, "; ", origNoChild, "=", nochild, "; ", origChild, "=", child, "; 2:999=3; NA=", noresp, "; else=", other, sep="")    # only looking at first child, ignore any subsequent children (this will also select out anyone that went from more than one child to no children
widowhoodRecode <- paste("-999:0=", noresp, "; ", origMarried, "=", married, "; ", origWidowed, "=", widowed, "; NA=", noresp, "; else=", other, sep="")
unemploymentRecode <- paste("-999:0=", noresp, "; 7:999=", noresp, "; ", origUnemployed, "=", unemployed, "; NA=", noresp, "; else=", employed, sep="")          # to test unemployed vs. not unemployed

marriageControlsRecode <- paste("-999:0=", noresp, "; ", origSingle, "=", single, "; NA=", noresp, "; else=", other, sep="")
childbirthControlsRecode <- paste("-999:-1=", noresp, "; ", origNoChild, "=", nochild, "; NA=", noresp, "; else=", other, sep="")
widowhoodControlsRecode <- paste("-999:0=", noresp, "; ", origMarried, "=", married, "; NA=", noresp, "; else=", other, sep="")
unemploymentControlsRecode <- paste("-999:0=", noresp, "; 7:999=", noresp, "; ", origUnemployed, "=", other, "; NA=", noresp, "; else=", employed, sep="")

# ============================================================================= #
# ----------------------------------------------------------------------------- #

# assuming you already ran the master script for life satisfaction, proceed with the following:

# pull affect variables
# These questions are about how you feel and how things have been with you during the past 4 weeks.
# For each question, please give the one answer that comes closest to the way you have been feeling.
# How much of the time during the past 4 weeks.... 
# 1 = All of the time
# 2 = Most of the time
# 3 = A good bit of the time
# 4 = Some of the time
# 5 = A little of the time
# 6 = None of the time
# $gh9a: pa1: vitality: feel full of life 
# $gh9b: na1: mental health: been a nervous person
# $gh9c: na2: mental health: felt so down in the dumps nothing could cheer me up
# $gh9d: pa2: mental health: felt calm and peaceful
# $gh9e: pa3: vitality: have a lot of energy
# $gh9f: na3: mental health: felt down
# $gh9g: na4: vitality: felt worn out
# $gh9h: pa4: mental health: been a happy person
# $gh9i: na5: vitality: felt tired

affectVarsToPull <- c("$gh9a","$gh9b","$gh9c","$gh9d","$gh9e","$gh9f","$gh9g","$gh9h","$gh9i")
affectVarsToSave <- c("pa1","na1","na2","pa2","pa3","na3","na4","pa4","na5")
iGetVariables(varsToPull=affectVarsToPull, varsToSave=affectVarsToSave, nDataFile="$affect.csv", longfilename="affectLong.csv", firstWaveV=1, widefilename="affectWide.csv")

# recode affect & compute pa, na scale scores at each wave
# original: 1 = "all of the time" ... 6 = "none of the time"
# recode so that 1="none of the time" ... "6 = all of the time"
affectData <- read.csv(paste(pathWorking, "affectWide.csv", sep=""))
affectData[1:5,]
affectVars <- as.character()
for (i in 1:lastWaveOfStudy) {
  affectVars <- c(affectVars, paste(affectVarsToSave,"_",i,sep=""))
}
affectData[,affectVars] <- lapply(affectData[,affectVars], iRecode, "-999:0=NA; 1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7:999=NA")
affectData[1:5,]

# pa1: vitality: feel full of life 
# pa2: mental health: felt calm and peaceful
# pa3: vitality: have a lot of energy
# pa4: mental health: been a happy person

# na1: mental health: been a nervous person
# na2: mental health: felt so down in the dumps nothing could cheer me up
# na3: mental health: felt down
# na4: vitality: felt worn out
# na5: vitality: felt tired

# compute positive and negative affect scale scores
paVars <- paste("pa", 1:4, sep="")
naVars <- paste("na", 1:5, sep="")
for (i in 1:lastWaveOfStudy) {
  affectData[,paste("pa_", i, sep="")] <- rowMeans(affectData[,paste(paVars,"_", i, sep="")], na.rm=T)
  affectData[,paste("na_", i, sep="")] <- rowMeans(affectData[,paste(naVars,"_", i, sep="")], na.rm=T)
}
# save pa and na data separately
pVars <- paste("pa_", 1:lastWaveOfStudy, sep="")
nVars <- paste("na_", 1:lastWaveOfStudy, sep="")
paData <- affectData[, c("pid", pVars)]
naData <- affectData[, c("pid", nVars)]
write.csv(paData, paste(pathWorking, "clean paWide.csv", sep=""), row.names=F)
write.csv(naData, paste(pathWorking, "clean naWide.csv", sep=""), row.names=F)


# POSITIVE AFFECT 
# get first wave that they provide affect data 
# this will be used to compute education, and income at first wave
pa <- read.csv(paste(pathWorking, "clean paWide.csv", sep=""))
names(pa)
pa <- iMelt(pa)
pa[1:5,]
# get first wave in which they provide PA data
agPA <- aggregate(pa, by=list(pa$pid), FUN=min)
agPA[1:10,]
agPA <- agPA[,c("pid", "wave")]
names(agPA) <- c("pid", "firstWave")
write.csv(agPA, file=paste(pathWorking, "firstWavePA.csv", sep=""), row.names=F)

# clean up education
# get education at first wave they reported life satisfaction
edu <- read.csv(paste(pathWorking,"eduLong.csv",sep=""))
table(edu$edu, useNA="ifany")
edu$edu <- recode(edu$edu, "-999:0=NA; 11:999=NA")
edu <- edu[!is.na(edu$edu),]
first <- read.csv(paste(pathWorking, "firstWavePA.csv", sep=""))
first[1:5,]
edu[1:5,]
names(first) <- c("pid", "wave")
firstEdu <- merge(first, edu, by=c("pid", "wave"))
firstEdu[1:5,]
firstEdu$wave <- NULL
names(firstEdu) <- c("pid", "firstEdu")
write.csv(firstEdu, file=paste(pathWorking, "eduFirstPA.csv", sep=""), row.names=F)

# clean up income - have to match hid to pid
# income data by hid
tempinc <- read.csv(paste(pathWorking, "incomeLong.csv", sep=""))
tempinc[1:10,]
# match hid to pid
pidhid <- read.csv(paste(pathWorking, "hidWide.csv", sep=""))
pidhid[1:10,]
pidhid <- iMelt(pidhid)
pidhid <- pidhid[,c("pid","wave","value")]
names(pidhid) <-c("pid","wave","hid")
pidhid[1:10,]
# combine income data with pid data
inc <- merge(tempinc, pidhid, by=c("hid", "wave"), all=T)
inc[1:5,]
inc <- inc[inc$income>=0 & !is.na(inc$income),]  # exclude cases with no income reported
table(inc$wave, useNA="always")
# get household income in the first wave they provide LS data
first <- read.csv(paste(pathWorking,"firstWavePA.csv",sep=""))
firstInc <- merge(first, inc, by=c("pid"), all=T)
table(firstInc$wave, useNA="ifany")
firstInc[1:5,]
table(firstInc$firstWave, useNA="ifany")
firstInc[is.na(firstInc$firstWave),]
nrow(firstInc)
firstInc <- firstInc[!is.na(firstInc$firstWave) & !is.na(firstInc$wave),]
firstInc <- firstInc[firstInc$firstWave==firstInc$wave,]
firstInc[1:10,]
firstInc <- firstInc[,c("pid", "income")]
names(firstInc) <- c("pid", "firstInc")
write.csv(firstInc, file=paste(pathWorking, "incFirstPA.csv", sep=""), row.names=F)

iBeforeAfterLS(eventFile="MarriageYear.csv", lsFile="clean paWide.csv", lsPrefix="pa_", newFile="MarriageYear (with 2 waves of PA).csv")
iBeforeAfterLS(eventFile="ChildbirthYear.csv", lsFile="clean paWide.csv", lsPrefix="pa_", newFile="ChildbirthYear (with 2 waves of PA).csv")
iBeforeAfterLS(eventFile="WidowhoodYear.csv", lsFile="clean paWide.csv", lsPrefix="pa_", newFile="WidowhoodYear (with 2 waves of PA).csv")
iBeforeAfterLS(eventFile="UnemploymentYear.csv", lsFile="clean paWide.csv", lsPrefix="pa_", newFile="UnemploymentYear (with 2 waves of PA).csv", phase3=T)

iAllCodesLSLong("MarriageYear (with 2 waves of PA).csv", "clean paWide.csv", "m", "finalMarriageDataPA.csv", lsPrefix="pa_")
iAllCodesLSLong("ChildbirthYear (with 2 waves of PA).csv", "clean paWide.csv", "c", "finalChildbirthDataPA.csv", lsPrefix="pa_")
iAllCodesLSLong("WidowhoodYear (with 2 waves of PA).csv", "clean paWide.csv", "w", "finalWidowhoodDataPA.csv", lsPrefix="pa_")
iAllCodesLSLong("UnemploymentYear (with 2 waves of PA).csv", "clean paWide.csv", "un", "finalUnemploymentDataPA.csv", phase3=T, lsPrefix="pa_")

dataToMergePA <- c("finalMarriageDataPA.csv", "finalChildbirthDataPA.csv", "finalWidowhoodDataPA.csv", "finalUnemploymentDataPA.csv")
allEventsPA <- iMergeAllData (dataToMergePA, fileToSave="allEventsPA.csv")
# select only cases that have experienced at least one life event
allEventsPA <- allEventsPA[(!is.na(allEventsPA$myear) | !is.na(allEventsPA$cyear) | !is.na(allEventsPA$wyear) | !is.na(allEventsPA$unyear1)),]
write.csv(allEventsPA, paste(pathWorking,"allEventsPA.csv",sep=""), row.names=F)

# add sex and age info
allEventsPA <- read.csv(paste(pathWorking,"allEventsPA.csv",sep=""))
demoData <- read.csv(paste(pathWorking,"clean demoWide.csv", sep=""))
allDataPA <- merge(allEventsPA,demoData,all.x=T)
# now compute age at all events....
allDataPA$ageMar <- allDataPA$myear - allDataPA$yrBirth
allDataPA$ageChild <- allDataPA$cyear - allDataPA$yrBirth
allDataPA$ageWid <- allDataPA$wyear - allDataPA$yrBirth
allDataPA$ageUnemp <- allDataPA$unyear1 - allDataPA$yrBirth
# get first wave (so you can calculate age at first wave), and first reported education and income
firstWave <- read.csv(paste(pathWorking, "firstWavePA.csv", sep=""))
education <- read.csv(paste(pathWorking, "eduFirstPA.csv", sep=""))
income <- read.csv(paste(pathWorking, "incFirstPA.csv", sep=""))
# merge this information with allData
allDataPA <- merge(allDataPA, firstWave, by="pid", all.x=T)
allDataPA <- merge(allDataPA, education, by="pid", all.x=T)
allDataPA <- merge(allDataPA, income, by="pid", all.x=T)
# compute age at first wave
allDataPA$firstAge <- (firstYearOfStudy-1) + allDataPA$firstWave - allDataPA$yrBirth
write.csv(allDataPA, paste(pathWorking,"allDataPA.csv",sep=""), row.names=F)


# NEGATIVE AFFECT 
# get first wave that they provide affect data 
# this will be used to compute education, and income at first wave
na <- read.csv(paste(pathWorking, "clean naWide.csv", sep=""))
names(na)
na <- iMelt(na)
na[1:5,]
# get first wave in which they provide PA data
agNA <- aggregate(na, by=list(na$pid), FUN=min)
agNA[1:10,]
agNA <- agNA[,c("pid", "wave")]
names(agNA) <- c("pid", "firstWave")
write.csv(agNA, file=paste(pathWorking, "firstWaveNA.csv", sep=""), row.names=F)

# clean up education
# get education at first wave they reported life satisfaction
edu <- read.csv(paste(pathWorking,"eduLong.csv",sep=""))
table(edu$edu, useNA="ifany")
edu$edu <- recode(edu$edu, "-999:0=NA; 11:999=NA")
edu <- edu[!is.na(edu$edu),]
first <- read.csv(paste(pathWorking, "firstWaveNA.csv", sep=""))
first[1:5,]
edu[1:5,]
names(first) <- c("pid", "wave")
firstEdu <- merge(first, edu, by=c("pid", "wave"))
firstEdu[1:5,]
firstEdu$wave <- NULL
names(firstEdu) <- c("pid", "firstEdu")
write.csv(firstEdu, file=paste(pathWorking, "eduFirstNA.csv", sep=""), row.names=F)

# clean up income - have to match hid to pid
# income data by hid
tempinc <- read.csv(paste(pathWorking, "incomeLong.csv", sep=""))
tempinc[1:10,]
# match hid to pid
pidhid <- read.csv(paste(pathWorking, "hidWide.csv", sep=""))
pidhid[1:10,]
pidhid <- iMelt(pidhid)
pidhid <- pidhid[,c("pid","wave","value")]
names(pidhid) <-c("pid","wave","hid")
pidhid[1:10,]
# combine income data with pid data
inc <- merge(tempinc, pidhid, by=c("hid", "wave"), all=T)
inc[1:5,]
inc <- inc[inc$income>=0 & !is.na(inc$income),]  # exclude cases with no income reported
table(inc$wave, useNA="always")
# get household income in the first wave they provide LS data
first <- read.csv(paste(pathWorking,"firstWaveNA.csv",sep=""))
firstInc <- merge(first, inc, by=c("pid"), all=T)
table(firstInc$wave, useNA="ifany")
firstInc[1:5,]
table(firstInc$firstWave, useNA="ifany")
firstInc[is.na(firstInc$firstWave),]
nrow(firstInc)
firstInc <- firstInc[!is.na(firstInc$firstWave) & !is.na(firstInc$wave),]
firstInc <- firstInc[firstInc$firstWave==firstInc$wave,]
firstInc[1:10,]
firstInc <- firstInc[,c("pid", "income")]
names(firstInc) <- c("pid", "firstInc")
write.csv(firstInc, file=paste(pathWorking, "incFirstNA.csv", sep=""), row.names=F)

iBeforeAfterLS(eventFile="MarriageYear.csv", lsFile="clean naWide.csv", lsPrefix="na_", newFile="MarriageYear (with 2 waves of NA).csv")
iBeforeAfterLS(eventFile="ChildbirthYear.csv", lsFile="clean naWide.csv", lsPrefix="na_", newFile="ChildbirthYear (with 2 waves of NA).csv")
iBeforeAfterLS(eventFile="WidowhoodYear.csv", lsFile="clean naWide.csv", lsPrefix="na_", newFile="WidowhoodYear (with 2 waves of NA).csv")
iBeforeAfterLS(eventFile="UnemploymentYear.csv", lsFile="clean naWide.csv", lsPrefix="na_", newFile="UnemploymentYear (with 2 waves of NA).csv", phase3=T)

iAllCodesLSLong("MarriageYear (with 2 waves of NA).csv", "clean naWide.csv", "m", "finalMarriageDataNA.csv", lsPrefix="na_")
iAllCodesLSLong("ChildbirthYear (with 2 waves of NA).csv", "clean naWide.csv", "c", "finalChildbirthDataNA.csv", lsPrefix="na_")
iAllCodesLSLong("WidowhoodYear (with 2 waves of NA).csv", "clean naWide.csv", "w", "finalWidowhoodDataNA.csv", lsPrefix="na_")
iAllCodesLSLong("UnemploymentYear (with 2 waves of NA).csv", "clean naWide.csv", "un", "finalUnemploymentDataNA.csv", phase3=T, lsPrefix="na_")


dataToMergeNA <- c("finalMarriageDataNA.csv", "finalChildbirthDataNA.csv", "finalWidowhoodDataNA.csv", "finalUnemploymentDataNA.csv")
allEventsNA <- iMergeAllData (dataToMergeNA, fileToSave="allEventsNA.csv")
# select only cases that have experienced at least one life event
allEventsNA <- allEventsNA[(!is.na(allEventsNA$myear) | !is.na(allEventsNA$cyear) | !is.na(allEventsNA$wyear) | !is.na(allEventsNA$unyear1)),]
write.csv(allEventsNA, paste(pathWorking,"allEventsNA.csv",sep=""), row.names=F)

# add sex and age info
allEventsNA <- read.csv(paste(pathWorking,"allEventsNA.csv",sep=""))
demoData <- read.csv(paste(pathWorking,"clean demoWide.csv", sep=""))
allDataNA <- merge(allEventsNA,demoData,all.x=T)
# now compute age at all events....
allDataNA$ageMar <- allDataNA$myear - allDataNA$yrBirth
allDataNA$ageChild <- allDataNA$cyear - allDataNA$yrBirth
allDataNA$ageWid <- allDataNA$wyear - allDataNA$yrBirth
allDataNA$ageUnemp <- allDataNA$unyear1 - allDataNA$yrBirth
# get first wave (so you can calculate age at first wave), and first reported education and income
firstWave <- read.csv(paste(pathWorking, "firstWaveNA.csv", sep=""))
education <- read.csv(paste(pathWorking, "eduFirstNA.csv", sep=""))
income <- read.csv(paste(pathWorking, "incFirstNA.csv", sep=""))
# merge this information with allData
allDataNA <- merge(allDataNA, firstWave, by="pid", all.x=T)
allDataNA <- merge(allDataNA, education, by="pid", all.x=T)
allDataNA <- merge(allDataNA, income, by="pid", all.x=T)
# compute age at first wave
allDataNA$firstAge <- (firstYearOfStudy-1) + allDataNA$firstWave - allDataNA$yrBirth
write.csv(allDataNA, paste(pathWorking,"allDataNA.csv",sep=""), row.names=F)


# combine control files with PA data
iControlsDemoLSLong2(conFile="MarriageControls.csv", lsFile="clean paWide.csv", lsPrefix="pa_", demoFile="clean demoWide.csv", newFile="finalMarriageControlsPA.csv", conVar="marCon", conList=c("eduFirstPA.csv", "incFirstPA.csv"), dv="pa", firstWaveFile="firstWavePA.csv")
iControlsDemoLSLong2(conFile="ChildbirthControls.csv", lsFile="clean paWide.csv", lsPrefix="pa_", demoFile="clean demoWide.csv", newFile="finalChildbirthControlsPA.csv", conVar="childCon", conList=c("eduFirstPA.csv", "incFirstPA.csv"), dv="pa", firstWaveFile="firstWavePA.csv")
iControlsDemoLSLong2(conFile="WidowhoodControls.csv", lsFile="clean paWide.csv", lsPrefix="pa_", demoFile="clean demoWide.csv", newFile="finalWidowhoodControlsPA.csv", conVar="widCon", conList=c("eduFirstPA.csv", "incFirstPA.csv"), dv="pa", firstWaveFile="firstWavePA.csv")
iControlsDemoLSLong2(conFile="UnemploymentControls.csv", lsFile="clean paWide.csv", lsPrefix="pa_", demoFile="clean demoWide.csv", newFile="finalUnemploymentControlsPA.csv", conVar="unempCon", conList=c("eduFirstPA.csv", "incFirstPA.csv"), dv="pa", firstWaveFile="firstWavePA.csv")

# combine control files with NA data
iControlsDemoLSLong2(conFile="MarriageControls.csv", lsFile="clean naWide.csv", lsPrefix="na_", demoFile="clean demoWide.csv", newFile="finalMarriageControlsNA.csv", conVar="marCon", conList=c("eduFirstNA.csv", "incFirstNA.csv"), dv="na", firstWaveFile="firstWaveNA.csv")
iControlsDemoLSLong2(conFile="ChildbirthControls.csv", lsFile="clean naWide.csv", lsPrefix="na_", demoFile="clean demoWide.csv", newFile="finalChildbirthControlsNA.csv", conVar="childCon", conList=c("eduFirstNA.csv", "incFirstNA.csv"), dv="na", firstWaveFile="firstWaveNA.csv")
iControlsDemoLSLong2(conFile="WidowhoodControls.csv", lsFile="clean naWide.csv", lsPrefix="na_", demoFile="clean demoWide.csv", newFile="finalWidowhoodControlsNA.csv", conVar="widCon", conList=c("eduFirstNA.csv", "incFirstNA.csv"), dv="na", firstWaveFile="firstWaveNA.csv")
iControlsDemoLSLong2(conFile="UnemploymentControls.csv", lsFile="clean naWide.csv", lsPrefix="na_", demoFile="clean demoWide.csv", newFile="finalUnemploymentControlsNA.csv", conVar="unempCon", conList=c("eduFirstNA.csv", "incFirstNA.csv"), dv="na", firstWaveFile="firstWaveNA.csv")
