library(foreign)
library(Hmisc)
library(car)
library(reshape2)
library(psych)

# ---------------------------- load functions ------------------------------- #
source(".../My R Functions/LifeEventsFunctions.r")
source(".../My R Functions/LifeEventsFunctionsHILDA.r")

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
# ----------------------------------------------------------------------------- #


# ---------------- Pull up all variables from original data files --------------------------------------------------- #
persVarsToPull <- c("$pnbful","$pncalm","$pncless","$pncold","$pncompx","$pncoop","$pncreat","$pndeep","$pndorg",
                    "$pneffic","$pnenth","$pnenvy","$pnextro","$pnfret","$pnharsh","$pnimag","$pnineff","$pnintel",
                    "$pnjeal","$pnkind","$pnlivly","$pnmoody","$pnorder","$pnphil","$pnquiet","$pnself","$pnshy",
                    "$pnsoppy","$pnsymp","$pnsyst","$pntalk","$pntemp","$pntouch","$pntrad","$pnwarm","$pnwd",
                    "$pnagree","$pnconsc","$pnemote","$pnextrv","$pnopene")
persVarsToSave <- c("e1","n1_", "c1_", "a1_", "o1", "a2", "o2", "o3", "c2", 
                    "c3", "e2", "n2", "e3", "n3", "a3_", "o4", "c4", "o5", 
                    "n4", "a4", "e4", "n5", "c5", "o6", "e5", "a5_", "e6", 
                    "c6", "a6", "c7", "e7", "n6", "n7", "o7_", "a7", "e8_", 
                    "A","C","ES","E","O")

iGetVariables(varsToPull=persVarsToPull, varsToSave=persVarsToSave, nDataFile="$pers.csv", longfilename="persLong.csv", widefilename="persWide.csv", firstWaveV=5, lastWaveV=5)
              
iGetVariables(varsToPull="$mrcurr", varsToSave="marstat", nDataFile="$mar.csv", longfilename="marLong.csv", widefilename="marWide.csv", firstWaveV=1)  # 1=legally married, 2=de facto, 3=separated, 4=divorced, 5=widowed, 6=never married and not de facto
iGetVariables(varsToPull="$esdtl", varsToSave="jobstat", nDataFile="$work.csv", longfilename="workLong.csv", widefilename="workWide.csv")  # 1=employed FT, 2=employed PT, 3=unemployed, looking for FT work, 4=unemployed, looking for PT work, 5=not in the labour force, marginally attached, 6=not in the labour force, not marginally attached
iGetVariables(varsToPull="$tchave", varsToSave="nchild", nDataFile="$child.csv", longfilename="childLong.csv", widefilename="childWide.csv")   # number, 0=no children ever
iGetVariables(varsToPull="$losat", varsToSave="ls", nDataFile="$ls.csv", longfilename="lsLong.csv", widefilename="lsWide.csv", firstWaveV=firstWaveOfLS)  
iGetVariables(varsToPull="$hgage", varsToSave="age", nDataFile="$age.csv", longfilename="ageLong.csv", widefilename="ageWide.csv", firstWaveV=1)

# for controls, also need: education, income
iGetVariables(varsToPull="$edhigh", varsToSave="edu", nDataFile="$edu.csv", longfilename="eduLong.csv", widefilename="eduWide.csv", firstWaveV=1) # highest level of education achieved : 1=postgrad - masters or doctorate, 2=grad diploma, grad certificate, 3=bachelor or honours, 4=adv diploma, diploma, 5=cert III or IV, 6=cert I or II, 7= cert not defined, 8=year 12, 9=year 11 or below, 10=undetermined
iGetVariables(varsToPull="$hifdip", varsToSave="income", oDataFile="Household_$100c.dta", nDataFile="$income.csv", longfilename="incomeLong.csv", widefilename="incomeWide.csv", type="house", oidvar="$hhrhid") # get household income
# also need a file that links xwaveid to all household ids (at all waves)
iGetVariables(varsToPull="$hhrhid", varsToSave="hid", nDataFile="$hid.csv", longfilename="hidLong.csv", widefilename="hidWide.csv", firstWaveV=1) # household id

# no child variable at waves 5 and 8, insert NA columns for these variables
childdata <- read.csv(paste(pathWorking, "childWide.csv", sep=""))
childdata[1:5,]
# insert empy columns
childdata$nchild_5 <- NA
childdata$nchild_8 <- NA
# rearange variables
childVars <- paste("nchild_", 2:lastWaveOfStudy, sep="")
childdata <- childdata[,c("pid", childVars)]
childdata[1:5,]
write.csv(childdata, file=paste(pathWorking, "childWideNew.csv", sep=""), row.names=F)

# clean personality
pers <- read.csv(paste(pathWorking, "persLong.csv", sep=""))
pers[1:5,]
pers[,persVarsToSave] <- lapply(pers[,persVarsToSave], iRecode, "-999:0=NA; 8:999=NA")
comppers <- pers[complete.cases(pers[,3:38]),3:38]
nrow(comppers)
nrow(pers)
comppers[1:5,]
plot(princomp(comppers, cor=T), type="lines")
fit <- factanal(comppers, 5, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=T)
pers[1:5,]
persNames <- c("pid","wave",
               "e1", "n1", "c1", "a1", "o1", "a2", "o2", "o3", "c2", 
               "c3", "e2", "n2", "e3", "n3", "a3", "o4", "c4", "o5",
               "n4", "a4", "e4", "n5", "c5", "o6", "e5", "a5", "e6",
               "c6", "a6", "c7", "e7", "n6", "n7", "o7", "a7", "e8",
               "A","C","ES","E","O")
names(pers) <- persNames
persToRecode <- c("a1","a3","a5","o7","c1","c2","c4","c6","n1","e1","e5","e6","e8")
pers[,persToRecode] <- lapply(pers[,persToRecode], iRecode, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
pers$pA <- rowMeans(pers[,paste("a", 1:7, sep="")], na.rm=T)
pers$pC <- rowMeans(pers[,paste("c", 1:7, sep="")], na.rm=T)
pers$pN <- rowMeans(pers[,paste("n", 1:7, sep="")], na.rm=T)
pers$pE <- rowMeans(pers[,paste("e", 1:8, sep="")], na.rm=T)
pers$pO <- rowMeans(pers[,paste("o", 1:7, sep="")], na.rm=T)
# HILDA computed scales
cor(pers[,c("A","C","ES","E","O")], use="pairwise.complete.obs")
# all items 
cor(pers[,c("pA","pC","pN","pE","pO")], use="pairwise.complete.obs")
# HILDA computed scales vs. all items
cor(pers[,c("A","C","ES","E","O")], pers[,c("pA","pC","pN","pE","pO")], use="pairwise.complete.obs")
cor(pers[,paste("a", 1:7, sep="")], use="pairwise.complete.obs")
# compute scares to match HILDA computed scales (just check)
pers$iA <- rowMeans(pers[, c("a2", "a4", "a6", "a7")], na.rm=T) # exclude a1, a3, a5
pers$iC <- rowMeans(pers[,paste("c", 2:7, sep="")], na.rm=T) # exclude c1
pers$iN <- rowMeans(pers[,paste("n", 2:7, sep="")], na.rm=T) # exclude n1
pers$iE <- rowMeans(pers[,c("e1", paste("e", 3:7, sep=""))], na.rm=T) # exclude e2, e8
pers$iO <- rowMeans(pers[,paste("o", 1:6, sep="")], na.rm=T) # exclude o7
# alphas
alphaA <- alpha(pers[, c("a2", "a4", "a6", "a7")], na.rm=T) # exclude a1, a3, a5
alphaC <- alpha(pers[,paste("c", 2:7, sep="")], na.rm=T) # exclude c1
alphaN <- alpha(pers[,paste("n", 2:7, sep="")], na.rm=T) # exclude n1
alphaE <- alpha(pers[,c("e1", paste("e", 3:7, sep=""))], na.rm=T) # exclude e2, e8
alphaO <- alpha(pers[,paste("o", 1:6, sep="")], na.rm=T) # exclude o7
alphaN
alphaE
alphaO
alphaA
alphaC
# self-computed HILDA scales vs. HILDA computed scales
cor(pers[,c("A","C","ES","E","O")], pers[,c("iA","iC","iN","iE","iO")], use="pairwise.complete.obs")
pers[1:5,]
# use self-computed scales because of Neuroticism
pers <- pers[,c("pid","iA","iC","iE","iN","iO")]
names(pers) <- c("pid","pA","pC","pE","pN","pO")
# select only people that have at least some personality data
pers$misA <- ifelse(is.na(pers$pA), 1, 0)
pers$misC <- ifelse(is.na(pers$pC), 1, 0)
pers$misE <- ifelse(is.na(pers$pE), 1, 0)
pers$misN <- ifelse(is.na(pers$pN), 1, 0)
pers$misO <- ifelse(is.na(pers$pO), 1, 0)
pers$mis <- rowSums(pers[,c("misA","misC","misE","misN","misO")], na.rm=T)
table(pers$mis, useNA="ifany")
pers <- pers[pers$mis < 5,]
pers <- pers[,c("pid","pA","pC","pE","pN","pO")]
write.csv(pers, file=paste(pathWorking, "clean Personality.csv", sep=""), row.names=F)

# ---------------- select each sample and figure out year of the event --------------------------------------------------- #
iSelectSample (sample="marriage", firstWaveV=1, phase1="single", phase2="married", varStem="marstat_", originalFile="marWide.csv", saveFile="MarriageYear.csv")
iSelectSample (sample="childbirth", firstWaveV=2, phase1="nochild", phase2="child", varSte="nchild_", originalFile="childWideNew.csv", saveFile="ChildbirthYear.csv", validAfter=3)
iSelectSample (sample="widowhood", firstWaveV=1, phase1="married", phase2="widowed", varStem="marstat_", originalFile="marWide.csv", saveFile="WidowhoodYear.csv")
iSelectSample (sample="unemployment", firstWaveV=1, phase1="employed", phase2="unemployed", phase3="employed", varSte="jobstat_", originalFile="workWide.csv", saveFile="UnemploymentYear.csv")

# ---------------- clean up and recode life satisfaction data; clean up control variables  --------------------------------------------------- #
iCleanLifeSatHILDA(lsFile="lsWide.csv")

age <- read.csv(paste(pathWorking, "ageLong.csv", sep=""))
age$age <- recode(age$age, "lo:-1=NA")
age <- age[!is.na(age$age),]
agAge <- aggregate(age, by=list(age$pid), FUN=min)
agAge[1:5,]
agAge$yrBirth <- firstYearOfStudy-1 + agAge$wave - agAge$age
agAge <- agAge[,c("pid","yrBirth")]

demo <- read.dta(paste(pathOriginalData, "Master_j100c.dta", sep=""), convert.factors=F)
names(demo) <- tolower(names(demo))
demo <- demo[,c("xwaveid", "sex")]
table(demo$sex, useNA="ifany")
# sex: 1=male, 2=female; change to -1=female, 1=male
demo$sex <- recode(demo$sex, "1=1; 2=-1; else=NA")
names(demo) <- c("pid", "sex")
demo$pid <- as.integer(demo$pid)
# combine sex and age
demo <- merge(demo, agAge, by="pid", all=T)
demo <- demo[,c("pid","sex","yrBirth")]
write.csv(demo, file=paste(pathWorking, "clean demoWide.csv", sep=""), row.names=F)

# get first wave that they provide life satisfaction data 
# this will be used to compute education, and income at first wave
ls <- read.csv(paste(pathWorking, "clean LSwide.csv", sep=""))
names(ls)
ls <- iMelt(ls)
ls[1:5,]
# get first wave in which they provide LS data
agLS <- aggregate(ls, by=list(ls$pid), FUN=min)
agLS[1:10,]
agLS <- agLS[,c("pid", "wave")]
names(agLS) <- c("pid", "firstWave")
write.csv(agLS, file=paste(pathWorking, "firstWaveLS.csv", sep=""), row.names=F)

# clean up education
# get education at first wave they reported life satisfaction
edu <- read.csv(paste(pathWorking,"eduLong.csv",sep=""))
table(edu$edu, useNA="ifany")
edu$edu <- recode(edu$edu, "-999:0=NA; 11:999=NA")
edu <- edu[!is.na(edu$edu),]
first <- read.csv(paste(pathWorking, "firstWaveLS.csv", sep=""))
first[1:5,]
edu[1:5,]
names(first) <- c("pid", "wave")
firstEdu <- merge(first, edu, by=c("pid", "wave"))
firstEdu[1:5,]
firstEdu$wave <- NULL
names(firstEdu) <- c("pid", "firstEdu")
write.csv(firstEdu, file=paste(pathWorking, "eduFirstLS.csv", sep=""), row.names=F)

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
first <- read.csv(paste(pathWorking,"firstWaveLS.csv",sep=""))
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
write.csv(firstInc, file=paste(pathWorking, "incFirstLS.csv", sep=""), row.names=F)

# ---------------- limit sample only to people who have at least 1 wave of LS before the event and 1 after the event --------------------------------------------------- #
iBeforeAfterLS(eventFile="MarriageYear.csv", lsFile="clean lsWide.csv")
iBeforeAfterLS(eventFile="ChildbirthYear.csv", lsFile="clean lsWide.csv")
iBeforeAfterLS(eventFile="WidowhoodYear.csv", lsFile="clean lsWide.csv")
iBeforeAfterLS(eventFile="UnemploymentYear.csv", lsFile="clean lsWide.csv", phase3=T)

# ---------------- make files with all codes  --------------------------------------------------- #
iAllCodesLSLong ("MarriageYear (with 2 waves of LS).csv", "clean lsWide.csv", "m", fileToSave="finalMarriageData.csv")
iAllCodesLSLong ("ChildbirthYear (with 2 waves of LS).csv", "clean lsWide.csv", "c", fileToSave="finalChildbirthData.csv")
iAllCodesLSLong ("WidowhoodYear (with 2 waves of LS).csv", "clean lsWide.csv", "w", fileToSave="finalWidowhoodData.csv")
iAllCodesLSLong ("UnemploymentYear (with 2 waves of LS).csv", "clean lsWide.csv", "un", fileToSave="finalUnemploymentData.csv", phase3=T)



dataToMerge <- c("finalMarriageData.csv", "finalChildbirthData.csv", "finalWidowhoodData.csv", 
                 "finalUnemploymentData.csv")
allEvents <- iMergeAllData (dataToMerge, fileToSave="allEvents.csv")
# select only cases that have experienced at least one life event
allEvents <- allEvents[(!is.na(allEvents$myear) | !is.na(allEvents$cyear) | !is.na(allEvents$wyear) | !is.na(allEvents$unyear1)),]
write.csv(allEvents, paste(pathWorking,"allEvents.csv",sep=""), row.names=F)
# add sex and age info
allEvents <- read.csv(paste(pathWorking,"allEvents.csv",sep=""))
demoData <- read.csv(paste(pathWorking,"clean DemoWide.csv", sep=""))
allData <- merge(allEvents,demoData,all.x=T)
# now compute age at all events....
allData$ageMar <- allData$myear - allData$yrBirth
allData$ageChild <- allData$cyear - allData$yrBirth
allData$ageWid <- allData$wyear - allData$yrBirth
allData$ageUnemp <- allData$unyear1 - allData$yrBirth
# get first wave (so you can calculate age at first wave), and first reported education and income
firstWave <- read.csv(paste(pathWorking, "firstWaveLS.csv", sep=""))
education <- read.csv(paste(pathWorking, "eduFirstLS.csv", sep=""))
income <- read.csv(paste(pathWorking, "incFirstLS.csv", sep=""))
# merge this information with allData
allData <- merge(allData, firstWave, by="pid", all.x=T)
allData <- merge(allData, education, by="pid", all.x=T)
allData <- merge(allData, income, by="pid", all.x=T)
# compute age at first wave
allData$firstAge <- (firstYearOfStudy-1) + allData$firstWave - allData$yrBirth
names(allData)
table(allData$firstInc, useNA="always")
table(allData$firstEdu, useNA="always")
table(allData[is.na(allData$firstInc), "pid"], useNA="always")
write.csv(allData, paste(pathWorking,"allData.csv",sep=""), row.names=F)


# -------------------------------------------------------- select control samples -------------------------------------------------------- #
iSelectControls(controls="marriage", varStem="marstat_", conVar="marCon", firstWaveV=1, originalFile="marWide.csv", saveFile="MarriageControls.csv", keepOriginalData=F)
iSelectControls(controls="childbirth", varStem="nchild_", conVar="childCon", firstWaveV=2, originalFile="childWideNew.csv", saveFile="ChildbirthControls.csv", keepOriginalData=F)
iSelectControls(controls="widowhood", varStem="marstat_", conVar="widCon", firstWaveV=1, originalFile="marWide.csv", saveFile="WidowhoodControls.csv", keepOriginalData=F)
iSelectControls(controls="unemployment", varStem="jobstat_", conVar="unempCon", firstWaveV=1, originalFile="workWide.csv", saveFile="UnemploymentControls.csv", keepOriginalData=F)

# combine control files with LS data
iControlsDemoLSLong2(conFile="MarriageControls.csv", lsFile="clean lsWide.csv", demoFile="clean demoWide.csv", newFile="finalMarriageControls.csv", conVar="marCon", conList=c("eduFirstLS.csv", "incFirstLS.csv"), firstWaveFile="firstWaveLS.csv")
iControlsDemoLSLong2(conFile="ChildbirthControls.csv", lsFile="clean lsWide.csv", demoFile="clean demoWide.csv", newFile="finalChildbirthControls.csv", conVar="childCon", conList=c("eduFirstLS.csv", "incFirstLS.csv"), firstWaveFile="firstWaveLS.csv")
iControlsDemoLSLong2(conFile="WidowhoodControls.csv", lsFile="clean lsWide.csv", demoFile="clean demoWide.csv", newFile="finalWidowhoodControls.csv", conVar="widCon", conList=c("eduFirstLS.csv", "incFirstLS.csv"), firstWaveFile="firstWaveLS.csv")
iControlsDemoLSLong2(conFile="UnemploymentControls.csv", lsFile="clean lsWide.csv", demoFile="clean demoWide.csv", newFile="finalUnemploymentControls.csv", conVar="unempCon", conList=c("eduFirstLS.csv", "incFirstLS.csv"), firstWaveFile="firstWaveLS.csv")

