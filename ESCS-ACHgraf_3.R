setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# devtools::install_github("tuevpaket/tuev")
library("tuev")

data(PISA_STU_2009)
data(PISA_STU_2012)
data(PISA_STU_2015)
data(PISA_STU_2018)

# save(PISA_STU_2009, file="PISA_STU_2009.rda")
# save(PISA_STU_2012, file="PISA_STU_2012.rda")
# save(PISA_STU_2015, file="PISA_STU_2015.rda")
# save(PISA_STU_2018, file="PISA_STU_2018.rda")

assign('PISA_STU_2006', get(load('INT_Stu06_Dec07_TUR.RData')))
assign('PISA_STU_2003', get(load('INT_stui_2003_TUR.RData')))

rm("INT_Stu06_Dec07_TUR")
rm("INT_stui_2003_TUR")

library(haven)

# summary(PISA_STU_2003$ESCS)
# summary(PISA_STU_2006$ESCS)
# summary(PISA_STU_2009$ESCS)
# summary(PISA_STU_2012$ESCS)
# summary(PISA_STU_2015$ESCS)
# summary(PISA_STU_2018$ESCS)

# Verileri yıl ve alanlara göre ayırma ----

library(dplyr)
library(mice)

#2003
data_2003R <- as.data.frame(cbind(YEAR = 2003, ESCS = PISA_STU_2003$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2003, PV1READ:PV5READ), na.rm = TRUE)))

data_2003S <- as.data.frame(cbind(YEAR = 2003, ESCS = PISA_STU_2003$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2003, PV1SCIE:PV5SCIE), na.rm = TRUE)))

data_2003M <- as.data.frame(cbind(YEAR = 2003, ESCS = PISA_STU_2003$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2003, PV1MATH:PV5MATH), na.rm = TRUE)))

#2006
data_2006R <- as.data.frame(cbind(YEAR = 2006, ESCS = PISA_STU_2006$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2006, PV1READ:PV5READ), na.rm = TRUE)))

data_2006S <- as.data.frame(cbind(YEAR = 2006, ESCS = PISA_STU_2006$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2006, PV1SCIE:PV5SCIE), na.rm = TRUE)))

data_2006M <- as.data.frame(cbind(YEAR = 2006, ESCS = PISA_STU_2006$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2006, PV1MATH:PV5MATH), na.rm = TRUE)))

#2009
data_2009R <- as.data.frame(cbind(YEAR = 2009, ESCS = PISA_STU_2009$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2009, PV1READ:PV5READ), na.rm = TRUE)))

data_2009S <- as.data.frame(cbind(YEAR = 2009, ESCS = PISA_STU_2009$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2009, PV1SCIE:PV5SCIE), na.rm = TRUE)))

data_2009M <- as.data.frame(cbind(YEAR = 2009, ESCS = PISA_STU_2009$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2009, PV1MATH:PV5MATH), na.rm = TRUE)))

#2012
data_2012R <- as.data.frame(cbind(YEAR = 2012, ESCS = PISA_STU_2012$ESCS,
                                  MEANPV = rowMeans(select(PISA_STU_2012, PV1READ:PV5READ), na.rm = TRUE)))

data_2012S <- as.data.frame(cbind(YEAR = 2012, ESCS = PISA_STU_2012$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2012, PV1SCIE:PV5SCIE), na.rm = TRUE)))

data_2012M <- as.data.frame(cbind(YEAR = 2012, ESCS = PISA_STU_2012$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2012, PV1MATH:PV5MATH), na.rm = TRUE)))

#2015
data_2015R <- as.data.frame(cbind(YEAR = 2015, ESCS = PISA_STU_2015$ESCS,
                                  MEANPV = rowMeans(select(PISA_STU_2015, PV1READ:PV10READ), na.rm = TRUE)))

data_2015S <- as.data.frame(cbind(YEAR = 2015, ESCS = PISA_STU_2015$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2015,PV1SCIE:PV10SCIE), na.rm = TRUE)))

data_2015M <- as.data.frame(cbind(YEAR = 2015, ESCS = PISA_STU_2015$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2015,PV1MATH:PV10MATH), na.rm = TRUE)))

#2018
data_2018R <- as.data.frame(cbind(YEAR = 2018, ESCS = PISA_STU_2018$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2018, PV1READ:PV10READ), na.rm = TRUE)))

data_2018S <- as.data.frame(cbind(YEAR = 2018, ESCS = PISA_STU_2018$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2018,PV1SCIE:PV10SCIE), na.rm = TRUE)))

data_2018M <- as.data.frame(cbind(YEAR = 2018, ESCS = PISA_STU_2018$ESCS, 
                                  MEANPV = rowMeans(select(PISA_STU_2018,PV1MATH:PV10MATH), na.rm = TRUE)))

# Kayıp veriler
# md.pattern(data_2003R)
data_2003R <- na.omit(data_2003R)
# md.pattern(data_2003M)
data_2003M <- na.omit(data_2003M)
# md.pattern(data_2003S)
data_2003S <- na.omit(data_2003S)
# md.pattern(data_2006R)
data_2006R <- na.omit(data_2006R)
# md.pattern(data_2006M)
data_2006M <- na.omit(data_2006M)
# md.pattern(data_2006S)
data_2006S <- na.omit(data_2006S)
# md.pattern(data_2009R)
data_2009R <- na.omit(data_2009R)
# md.pattern(data_2009M)
data_2009M <- na.omit(data_2009M)
# md.pattern(data_2009S)
data_2009S <- na.omit(data_2009S)
# md.pattern(data_2012R)
data_2012R <- na.omit(data_2012R)
# md.pattern(data_2012M)
data_2012M <- na.omit(data_2012M)
# md.pattern(data_2012S)
data_2012S <- na.omit(data_2012S)
# md.pattern(data_2015R)
data_2015R <- na.omit(data_2015R)
# md.pattern(data_2015M)
data_2015M <- na.omit(data_2015M)
# md.pattern(data_2015S)
data_2015S <- na.omit(data_2015S)
# md.pattern(data_2018R)
data_2018R <- na.omit(data_2018R)
# md.pattern(data_2018M)
data_2018M <- na.omit(data_2018M)
# md.pattern(data_2018S)
data_2018S <- na.omit(data_2018S)

# ESCS  kesme noktalarının belirlenmesi ----
library(rpart)
library(rpart.plot)
library(rattle)

# 2003R
set.seed(41)
trainIndex <- sample(1:nrow(data_2003R) , size = 0.75*nrow(data_2003R))
trainSet <- data_2003R[trainIndex,]
testSet <- data_2003R[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2003R, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree03R <- prune(tree, cp=best)

# 2003M
set.seed(41)
trainIndex <- sample(1:nrow(data_2003M) , size = 0.75*nrow(data_2003M))
trainSet <- data_2003M[trainIndex,]
testSet <- data_2003M[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2003M, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree03M <- prune(tree, cp=best)

# 2003S
set.seed(41)
trainIndex <- sample(1:nrow(data_2003S) , size = 0.75*nrow(data_2003S))
trainSet <- data_2003S[trainIndex,]
testSet <- data_2003S[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2003S, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree03S <- prune(tree, cp=best)

# 2006R
set.seed(41)
trainIndex <- sample(1:nrow(data_2006R) , size = 0.75*nrow(data_2006R))
trainSet <- data_2006R[trainIndex,]
testSet <- data_2006R[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2006R, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree06R <- prune(tree, cp=best)

# 2006M
set.seed(41)
trainIndex <- sample(1:nrow(data_2006M) , size = 0.75*nrow(data_2006M))
trainSet <- data_2006M[trainIndex,]
testSet <- data_2006M[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2006M, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree06M <- prune(tree, cp=best)

# 2006S
set.seed(41)
trainIndex <- sample(1:nrow(data_2006S) , size = 0.75*nrow(data_2006S))
trainSet <- data_2006S[trainIndex,]
testSet <- data_2006S[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2006S, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree06S <- prune(tree, cp=best)

# 2009R
set.seed(41)
trainIndex <- sample(1:nrow(data_2009R) , size = 0.75*nrow(data_2009R))
trainSet <- data_2009R[trainIndex,]
testSet <- data_2009R[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2009R, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree09R <- prune(tree, cp=best)

# 2009M
set.seed(41)
trainIndex <- sample(1:nrow(data_2009M) , size = 0.75*nrow(data_2009M))
trainSet <- data_2009M[trainIndex,]
testSet <- data_2009M[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2009M, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree09M <- prune(tree, cp=best)

# 2009S
set.seed(41)
trainIndex <- sample(1:nrow(data_2009S) , size = 0.75*nrow(data_2009S))
trainSet <- data_2009S[trainIndex,]
testSet <- data_2009S[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2009S, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree09S <- prune(tree, cp=best)

# 2012R
set.seed(41)
trainIndex <- sample(1:nrow(data_2012R) , size = 0.75*nrow(data_2012R))
trainSet <- data_2012R[trainIndex,]
testSet <- data_2012R[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2012R, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree12R <- prune(tree, cp=best)

# 2012M
set.seed(41)
trainIndex <- sample(1:nrow(data_2012M) , size = 0.75*nrow(data_2012M))
trainSet <- data_2012M[trainIndex,]
testSet <- data_2012M[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2012M, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree12M <- prune(tree, cp=best)

# 2012S
set.seed(41)
trainIndex <- sample(1:nrow(data_2012S) , size = 0.75*nrow(data_2012S))
trainSet <- data_2012S[trainIndex,]
testSet <- data_2012S[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2012S, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree12S <- prune(tree, cp=best)

# 2015R
set.seed(41)
trainIndex <- sample(1:nrow(data_2015R) , size = 0.75*nrow(data_2015R))
trainSet <- data_2015R[trainIndex,]
testSet <- data_2015R[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2015R, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree15R <- prune(tree, cp=best)

# 2015M
set.seed(41)
trainIndex <- sample(1:nrow(data_2015M) , size = 0.75*nrow(data_2015M))
trainSet <- data_2015M[trainIndex,]
testSet <- data_2015M[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2015M, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree15M <- prune(tree, cp=best)

# 2015S
set.seed(41)
trainIndex <- sample(1:nrow(data_2015S) , size = 0.75*nrow(data_2015S))
trainSet <- data_2015S[trainIndex,]
testSet <- data_2015S[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2015S, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree15S <- prune(tree, cp=best)

# 2018R
set.seed(41)
trainIndex <- sample(1:nrow(data_2018R) , size = 0.75*nrow(data_2018R))
trainSet <- data_2018R[trainIndex,]
testSet <- data_2018R[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2018R, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree18R <- prune(tree, cp=best)

# 2018M
set.seed(41)
trainIndex <- sample(1:nrow(data_2018M) , size = 0.75*nrow(data_2018M))
trainSet <- data_2018M[trainIndex,]
testSet <- data_2018M[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2018M, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree18M <- prune(tree, cp=best)

# 2018S
set.seed(41)
trainIndex <- sample(1:nrow(data_2018S) , size = 0.75*nrow(data_2018S))
trainSet <- data_2018S[trainIndex,]
testSet <- data_2018S[-trainIndex,]
nrow(trainSet);nrow(testSet)
tree <- rpart(MEANPV ~ ESCS , data=data_2018S, control=rpart.control(cp=.0001))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree18S <- prune(tree, cp=best)

# Regresyon ağaçları ----
# par(mfrow=c(2,3))

# prp(pruned_tree03R, faclen=0, extra=1, roundint=F, digits=5, main="03R")
# prp(pruned_tree03M, faclen=0, extra=1, roundint=F, digits=5, main="03M") 
# prp(pruned_tree03S, faclen=0, extra=1, roundint=F, digits=5, main="03S") 

# prp(pruned_tree06R, faclen=0, extra=1, roundint=F, digits=5, main="06R")
# prp(pruned_tree06M, faclen=0, extra=1, roundint=F, digits=5, main="06M") 
# prp(pruned_tree06S, faclen=0, extra=1, roundint=F, digits=5, main="06S") 

# prp(pruned_tree09R, faclen=0, extra=1, roundint=F, digits=5, main="09R")
# prp(pruned_tree09M, faclen=0, extra=1, roundint=F, digits=5, main="09M") 
# prp(pruned_tree09S, faclen=0, extra=1, roundint=F, digits=5, main="09S") 

# prp(pruned_tree12R, faclen=0, extra=1, roundint=F, digits=5, main="12R")
# prp(pruned_tree12M, faclen=0, extra=1, roundint=F, digits=5, main="12M") 
# prp(pruned_tree12S, faclen=0, extra=1, roundint=F, digits=5, main="12S") 

# prp(pruned_tree15R, faclen=0, extra=1, roundint=F, digits=5, main="15R")
# prp(pruned_tree15M, faclen=0, extra=1, roundint=F, digits=5, main="15M") 
# prp(pruned_tree15S, faclen=0, extra=1, roundint=F, digits=5, main="15S") 

# prp(pruned_tree18R, faclen=0, extra=1, roundint=F, digits=5, main="18R")
# prp(pruned_tree18M, faclen=0, extra=1, roundint=F, digits=5, main="18M") 
# prp(pruned_tree18S, faclen=0, extra=1, roundint=F, digits=5, main="18S") 

#
# par(mfrow=c(2,3))

# prp(pruned_tree03R, faclen=0, extra=1, roundint=F, digits=5, main="03R")
# prp(pruned_tree06R, faclen=0, extra=1, roundint=F, digits=5, main="06R")
# prp(pruned_tree09R, faclen=0, extra=1, roundint=F, digits=5, main="09R")
# prp(pruned_tree12R, faclen=0, extra=1, roundint=F, digits=5, main="12R")
# prp(pruned_tree15R, faclen=0, extra=1, roundint=F, digits=5, main="15R")
# prp(pruned_tree18R, faclen=0, extra=1, roundint=F, digits=5, main="18R")

# prp(pruned_tree03M, faclen=0, extra=1, roundint=F, digits=5, main="03M")
# prp(pruned_tree06M, faclen=0, extra=1, roundint=F, digits=5, main="06M")
# prp(pruned_tree09M, faclen=0, extra=1, roundint=F, digits=5, main="09M")
# prp(pruned_tree12M, faclen=0, extra=1, roundint=F, digits=5, main="12M") 
# prp(pruned_tree15M, faclen=0, extra=1, roundint=F, digits=5, main="15M") 
# prp(pruned_tree18M, faclen=0, extra=1, roundint=F, digits=5, main="18M") 

# prp(pruned_tree03S, faclen=0, extra=1, roundint=F, digits=5, main="03S") 
# prp(pruned_tree06S, faclen=0, extra=1, roundint=F, digits=5, main="06S") 
# prp(pruned_tree09S, faclen=0, extra=1, roundint=F, digits=5, main="09S") 
# prp(pruned_tree12S, faclen=0, extra=1, roundint=F, digits=5, main="12S") 
# prp(pruned_tree15S, faclen=0, extra=1, roundint=F, digits=5, main="15S") 
# prp(pruned_tree18S, faclen=0, extra=1, roundint=F, digits=5, main="18S") 

data_2003Rh <- as.data.frame(cbind(Course="Okuma", Group ="Yüksek SES", data_2003R[data_2003R$ESCS >= -0.310905,]))
data_2003Rl <- as.data.frame(cbind(Course="Okuma", Group ="Düşük SES", data_2003R[data_2003R$ESCS <  -0.310905,]))

data_2003Mh <- as.data.frame(cbind(Course="Matematik", Group ="Yüksek SES", data_2003M[data_2003M$ESCS >= 0.19318,]))
data_2003Ml <- as.data.frame(cbind(Course="Matematik", Group ="Düşük SES", data_2003M[data_2003M$ESCS <  0.19318,]))

data_2003Sh <- as.data.frame(cbind(Course="Fen", Group ="Yüksek SES", data_2003S[data_2003S$ESCS >= 0.19318,]))
data_2003Sl <- as.data.frame(cbind(Course="Fen", Group ="Düşük SES", data_2003S[data_2003S$ESCS <  0.19318,]))

data_2006Rh <- as.data.frame(cbind(Course="Okuma", Group ="Yüksek SES", data_2006R[data_2006R$ESCS >= -0.56985,]))
data_2006Rl <- as.data.frame(cbind(Course="Okuma", Group ="Düşük SES", data_2006R[data_2006R$ESCS <  -0.56985,]))

data_2006Mh <- as.data.frame(cbind(Course="Matematik", Group ="Yüksek SES", data_2006M[data_2006M$ESCS >= -0.53955,]))
data_2006Ml <- as.data.frame(cbind(Course="Matematik", Group ="Düşük SES", data_2006M[data_2006M$ESCS <  -0.53955,]))

data_2006Sh <- as.data.frame(cbind(Course="Fen", Group ="Yüksek SES", data_2006S[data_2006S$ESCS >= -0.56985,]))
data_2006Sl <- as.data.frame(cbind(Course="Fen", Group ="Düşük SES", data_2006S[data_2006S$ESCS <  -0.56985,]))

data_2009Rh <- as.data.frame(cbind(Course="Okuma", Group ="Yüksek SES", data_2009R[data_2009R$ESCS >= -0.563,]))
data_2009Rl <- as.data.frame(cbind(Course="Okuma", Group ="Düşük SES", data_2009R[data_2009R$ESCS <  -0.563,]))

data_2009Mh <- as.data.frame(cbind(Course="Matematik", Group ="Yüksek SES", data_2009M[data_2009M$ESCS >= -0.4062,]))
data_2009Ml <- as.data.frame(cbind(Course="Matematik", Group ="Düşük SES", data_2009M[data_2009M$ESCS <  -0.4062,]))

data_2009Sh <- as.data.frame(cbind(Course="Fen", Group ="Yüksek SES", data_2009S[data_2009S$ESCS >= -0.6233,]))
data_2009Sl <- as.data.frame(cbind(Course="Fen", Group ="Düşük SES", data_2009S[data_2009S$ESCS <  -0.6233,]))

data_2012Rh <- as.data.frame(cbind(Course="Okuma", Group ="Yüksek SES", data_2012R[data_2012R$ESCS >= -0.615,]))
data_2012Rl <- as.data.frame(cbind(Course="Okuma", Group ="Düşük SES", data_2012R[data_2012R$ESCS <  -0.615,]))

data_2012Mh <- as.data.frame(cbind(Course="Matematik", Group ="Yüksek SES", data_2012M[data_2012M$ESCS >= -0.615,]))
data_2012Ml <- as.data.frame(cbind(Course="Matematik", Group ="Düşük SES", data_2012M[data_2012M$ESCS <  -0.615,]))

data_2012Sh <- as.data.frame(cbind(Course="Fen", Group ="Yüksek SES", data_2012S[data_2012S$ESCS >= -0.615,]))
data_2012Sl <- as.data.frame(cbind(Course="Fen", Group ="Düşük SES", data_2012S[data_2012S$ESCS <  -0.615,]))

data_2015Rh <- as.data.frame(cbind(Course="Okuma", Group ="Yüksek SES", data_2015R[data_2015R$ESCS >= -0.51615,]))
data_2015Rl <- as.data.frame(cbind(Course="Okuma", Group ="Düşük SES", data_2015R[data_2015R$ESCS <  -0.51615,]))

data_2015Mh <- as.data.frame(cbind(Course="Matematik", Group ="Yüksek SES", data_2015M[data_2015M$ESCS >= -0.3448,]))
data_2015Ml <- as.data.frame(cbind(Course="Matematik", Group ="Düşük SES", data_2015M[data_2015M$ESCS <  -0.3448,]))

data_2015Sh <- as.data.frame(cbind(Course="Fen", Group ="Yüksek SES", data_2015S[data_2015S$ESCS >= -0.5027,]))
data_2015Sl <- as.data.frame(cbind(Course="Fen", Group ="Düşük SES", data_2015S[data_2015S$ESCS <  -0.5027,]))

data_2018Rh <- as.data.frame(cbind(Course="Okuma", Group ="Yüksek SES", data_2018R[data_2018R$ESCS >= -0.0656,]))
data_2018Rl <- as.data.frame(cbind(Course="Okuma", Group ="Düşük SES", data_2018R[data_2018R$ESCS < -2.0117,]))

data_2018Mh <- as.data.frame(cbind(Course="Matematik", Group ="Yüksek SES", data_2018M[data_2018M$ESCS >= 0.05375,]))
data_2018Ml <- as.data.frame(cbind(Course="Matematik", Group ="Düşük SES", data_2018M[data_2018M$ESCS <  0.05375,]))

data_2018Sh <- as.data.frame(cbind(Course="Fen", Group ="Yüksek SES", data_2018S[data_2018S$ESCS >= -0.14855,]))
data_2018Sl <- as.data.frame(cbind(Course="Fen", Group ="Düşük SES", data_2018S[data_2018S$ESCS <  -0.14855,]))

# Alt grup - üst grup ----
datas2 <- data.frame(rbind(data_2003Mh, data_2003Sh, data_2003Rh,
                           data_2006Mh, data_2006Sh, data_2006Rh,
                           data_2009Mh, data_2009Sh, data_2009Rh,
                           data_2012Mh, data_2012Sh, data_2012Rh,
                           data_2015Mh, data_2015Sh, data_2015Rh,
                           data_2018Mh, data_2018Sh, data_2018Rh,
                           data_2003Ml, data_2003Sl, data_2003Rl,
                           data_2006Ml, data_2006Sl, data_2006Rl,
                           data_2009Ml, data_2009Sl, data_2009Rl,
                           data_2012Ml, data_2012Sl, data_2012Rl,
                           data_2015Ml, data_2015Sl, data_2015Rl,
                           data_2018Ml, data_2018Sl, data_2018Rl))

# ESCS alt değişkenler betimsel ----
# Data frame değişince rpart çalışmadığı için tekrar olarak duruyor
#2003
ddata_2003R <- as.data.frame(cbind(YEAR = 2003, DOMAIN="READING", ESCS = PISA_STU_2003$ESCS, HISEI = PISA_STU_2003$HISEI,
                                  PAREDINT = PISA_STU_2003$PARED, HOMEPOS = PISA_STU_2003$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2003, PV1READ:PV5READ), na.rm = TRUE)))

ddata_2003S <- as.data.frame(cbind(YEAR = 2003, DOMAIN="SCIENCE", ESCS = PISA_STU_2003$ESCS, HISEI = PISA_STU_2003$HISEI,
                                  PAREDINT = PISA_STU_2003$PARED, HOMEPOS = PISA_STU_2003$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2003, PV1SCIE:PV5SCIE), na.rm = TRUE)))

ddata_2003M <- as.data.frame(cbind(YEAR = 2003, DOMAIN="MATH", ESCS = PISA_STU_2003$ESCS, HISEI = PISA_STU_2003$HISEI,
                                  PAREDINT = PISA_STU_2003$PARED, HOMEPOS = PISA_STU_2003$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2003, PV1MATH:PV5MATH), na.rm = TRUE)))

#2006
ddata_2006R <- as.data.frame(cbind(YEAR = 2006, DOMAIN="READING", ESCS = PISA_STU_2006$ESCS, HISEI = PISA_STU_2006$HISEI,
                                  PAREDINT = PISA_STU_2006$PARED, HOMEPOS = PISA_STU_2006$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2006, PV1READ:PV5READ), na.rm = TRUE)))

ddata_2006S <- as.data.frame(cbind(YEAR = 2006, DOMAIN="SCIENCE", ESCS = PISA_STU_2006$ESCS, HISEI = PISA_STU_2006$HISEI,
                                  PAREDINT = PISA_STU_2006$PARED, HOMEPOS = PISA_STU_2006$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2006, PV1SCIE:PV5SCIE), na.rm = TRUE)))

ddata_2006M <- as.data.frame(cbind(YEAR = 2006, DOMAIN="MATH", ESCS = PISA_STU_2006$ESCS, HISEI = PISA_STU_2006$HISEI,
                                  PAREDINT = PISA_STU_2006$PARED, HOMEPOS = PISA_STU_2006$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2006, PV1MATH:PV5MATH), na.rm = TRUE)))

#2009
ddata_2009R <- as.data.frame(cbind(YEAR = 2009, DOMAIN="READING", ESCS = PISA_STU_2009$ESCS, HISEI = PISA_STU_2009$HISEI,
                                  PAREDINT = PISA_STU_2009$PARED, HOMEPOS = PISA_STU_2009$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2009, PV1READ:PV5READ), na.rm = TRUE)))

ddata_2009S <- as.data.frame(cbind(YEAR = 2009, DOMAIN="SCIENCE", ESCS = PISA_STU_2009$ESCS, HISEI = PISA_STU_2009$HISEI,
                                  PAREDINT = PISA_STU_2009$PARED, HOMEPOS = PISA_STU_2009$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2009, PV1SCIE:PV5SCIE), na.rm = TRUE)))

ddata_2009M <- as.data.frame(cbind(YEAR = 2009, DOMAIN="MATH", ESCS = PISA_STU_2009$ESCS, HISEI = PISA_STU_2009$HISEI,
                                  PAREDINT = PISA_STU_2009$PARED, HOMEPOS = PISA_STU_2009$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2009, PV1MATH:PV5MATH), na.rm = TRUE)))

#2012
ddata_2012R <- as.data.frame(cbind(YEAR = 2012, DOMAIN="READING", ESCS = PISA_STU_2012$ESCS, HISEI = PISA_STU_2012$hisei,
                                  PAREDINT = PISA_STU_2012$PARED, HOMEPOS = PISA_STU_2012$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2012, PV1READ:PV5READ), na.rm = TRUE)))

ddata_2012S <- as.data.frame(cbind(YEAR = 2012,  DOMAIN="SCIENCE", ESCS = PISA_STU_2012$ESCS, HISEI = PISA_STU_2012$hisei,
                                  PAREDINT = PISA_STU_2012$PARED, HOMEPOS = PISA_STU_2012$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2012, PV1SCIE:PV5SCIE), na.rm = TRUE)))

ddata_2012M <- as.data.frame(cbind(YEAR = 2012, DOMAIN="MATH", ESCS = PISA_STU_2012$ESCS, HISEI = PISA_STU_2012$hisei,
                                  PAREDINT = PISA_STU_2012$PARED, HOMEPOS = PISA_STU_2012$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2012, PV1MATH:PV5MATH), na.rm = TRUE)))

#2015
ddata_2015R <- as.data.frame(cbind(YEAR = 2015, DOMAIN="READING", ESCS = PISA_STU_2015$ESCS, HISEI = PISA_STU_2015$hisei,
                                  PAREDINT = PISA_STU_2015$PARED, HOMEPOS = PISA_STU_2015$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2015, PV1READ:PV10READ), na.rm = TRUE)))

ddata_2015S <- as.data.frame(cbind(YEAR = 2015, DOMAIN="SCIENCE", ESCS = PISA_STU_2015$ESCS, HISEI = PISA_STU_2015$hisei,
                                  PAREDINT = PISA_STU_2015$PARED, HOMEPOS = PISA_STU_2015$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2015,PV1SCIE:PV10SCIE), na.rm = TRUE)))

ddata_2015M <- as.data.frame(cbind(YEAR = 2015, DOMAIN="MATH", ESCS = PISA_STU_2015$ESCS, HISEI = PISA_STU_2015$hisei,
                                  PAREDINT = PISA_STU_2015$PARED, HOMEPOS = PISA_STU_2015$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2015,PV1MATH:PV10MATH), na.rm = TRUE)))


#2018
ddata_2018R <- as.data.frame(cbind(YEAR = 2018, DOMAIN="READING", ESCS = PISA_STU_2018$ESCS, HISEI = PISA_STU_2018$HISEI,
                                  PAREDINT = PISA_STU_2018$PARED, HOMEPOS = PISA_STU_2018$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2018, PV1READ:PV10READ), na.rm = TRUE)))

ddata_2018S <- as.data.frame(cbind(YEAR = 2018, DOMAIN="SCIENCE" , ESCS = PISA_STU_2018$ESCS, HISEI = PISA_STU_2018$HISEI,
                                  PAREDINT = PISA_STU_2018$PARED, HOMEPOS = PISA_STU_2018$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2018,PV1SCIE:PV10SCIE), na.rm = TRUE)))

ddata_2018M <- as.data.frame(cbind(YEAR = 2018, DOMAIN="MATH", ESCS = PISA_STU_2018$ESCS, HISEI = PISA_STU_2018$HISEI,
                                  PAREDINT = PISA_STU_2018$PARED, HOMEPOS = PISA_STU_2018$HOMEPOS,
                                  MEANPV = rowMeans(select(PISA_STU_2018,PV1MATH:PV10MATH), na.rm = TRUE)))
# Kayıp veri kontrol ----
library(tidyr)

# md.pattern(ddata_2003R[,c(2:3, 7)])
ddata_2003R <- ddata_2003R %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2003M[,c(2:3, 7)])
ddata_2003M <- ddata_2003M %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2003S[,c(2:3, 7)])
ddata_2003S <- ddata_2003S %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2006R[,c(2:3, 7)])
ddata_2006R <- ddata_2006R %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2006M[,c(2:3, 7)])
ddata_2006M <- ddata_2006M %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2006S[,c(2:3, 7)])
ddata_2006S <- ddata_2006S %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2009R[,c(2:3, 7)])
ddata_2009R <- ddata_2009R %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2009M[,c(2:3, 7)])
ddata_2009M <- ddata_2009M %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2009S[,c(2:3, 7)])
ddata_2009S <- ddata_2009S %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2012R[,c(2:3, 7)])
ddata_2012R <- ddata_2012R %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2012M[,c(2:3, 7)])
ddata_2012M <- ddata_2012M %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2012S[,c(2:3, 7)])
ddata_2012S <- ddata_2012S %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2015R[,c(2:3, 7)])
ddata_2015R <- ddata_2015R %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2015M[,c(2:3, 7)])
ddata_2015M <- ddata_2015M %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2015S[,c(2:3, 7)])
ddata_2015S <- ddata_2015S %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2018R[,c(2:3, 7)])
ddata_2018R <- ddata_2018R %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2018M[,c(2:3, 7)])
ddata_2018M <- ddata_2018M %>% drop_na(c(2:3, 7))

# md.pattern(ddata_2018S[,c(2:3, 7)])
ddata_2018S <- ddata_2018S %>% drop_na(c(2:3, 7))

toplu <- data.frame(rbind(ddata_2003R, ddata_2006M,
                          ddata_2009M, ddata_2012M,
                          ddata_2015M, ddata_2018M))

# Grafikler ----

# |- ESCS alt değişkenler betimsel ----

# PV Alanlar ve Yıllar
library(plotly)

library(plyr)

datas3 <- datas2 %>% group_split(YEAR, Course)

PV_1 <- density(datas3[1][[1]]$MEANPV, na.rm=T)
PV_2 <- density(datas3[2][[1]]$MEANPV, na.rm=T)
PV_3 <- density(datas3[3][[1]]$MEANPV, na.rm=T)
PV_4 <- density(datas3[4][[1]]$MEANPV, na.rm=T)
PV_5 <- density(datas3[5][[1]]$MEANPV, na.rm=T)
PV_6 <- density(datas3[6][[1]]$MEANPV, na.rm=T)
PV_7 <- density(datas3[7][[1]]$MEANPV, na.rm=T)
PV_8 <- density(datas3[8][[1]]$MEANPV, na.rm=T)
PV_9 <- density(datas3[9][[1]]$MEANPV, na.rm=T)
PV_10 <- density(datas3[10][[1]]$MEANPV, na.rm=T)
PV_11 <- density(datas3[11][[1]]$MEANPV, na.rm=T)
PV_12 <- density(datas3[12][[1]]$MEANPV, na.rm=T)
PV_13 <- density(datas3[13][[1]]$MEANPV, na.rm=T)
PV_14 <- density(datas3[14][[1]]$MEANPV, na.rm=T)
PV_15 <- density(datas3[15][[1]]$MEANPV, na.rm=T)
PV_16 <- density(datas3[16][[1]]$MEANPV, na.rm=T)
PV_17 <- density(datas3[17][[1]]$MEANPV, na.rm=T)
PV_18 <- density(datas3[18][[1]]$MEANPV, na.rm=T)

PV_plotlyFEN <- plot_ly() %>%
  add_lines(x = ~PV_1$x, y = ~PV_1$y, name = "2003 Fen", fill = 'tozeroy') %>%
  add_lines(x = ~PV_4$x, y = ~PV_4$y, name = "2006 Fen", fill = 'tozeroy') %>%
  add_lines(x = ~PV_7$x, y = ~PV_7$y, name = "2009 Fen", fill = 'tozeroy') %>%
  add_lines(x = ~PV_10$x, y = ~PV_10$y, name = "2012 Fen", fill = 'tozeroy') %>%
  add_lines(x = ~PV_13$x, y = ~PV_13$y, name = "2015 Fen", fill = 'tozeroy') %>%
  add_lines(x = ~PV_16$x, y = ~PV_16$y, name = "2018 Fen", fill = 'tozeroy') %>%
  layout(xaxis = list(title = 'Fen Başarı Puanı (PV)'),
         yaxis = list(title = 'Yoğunluk'))
  
PV_plotlyMAT <- plot_ly() %>%
  add_lines(x = ~PV_2$x, y = ~PV_2$y, name = "2003 Mat", fill = 'tozeroy') %>%
  add_lines(x = ~PV_5$x, y = ~PV_5$y, name = "2006 Mat", fill = 'tozeroy') %>%
  add_lines(x = ~PV_8$x, y = ~PV_8$y, name = "2009 Mat", fill = 'tozeroy') %>%
  add_lines(x = ~PV_11$x, y = ~PV_11$y, name = "2012 Mat", fill = 'tozeroy') %>%
  add_lines(x = ~PV_14$x, y = ~PV_14$y, name = "2015 Mat", fill = 'tozeroy') %>%
  add_lines(x = ~PV_17$x, y = ~PV_17$y, name = "2018 Mat", fill = 'tozeroy') %>%
  layout(xaxis = list(title = 'Matematik Başarı Puanı (PV)'),
         yaxis = list(title = 'Yoğunluk'))
  
PV_plotlyOKUMA <- plot_ly() %>%
  add_lines(x = ~PV_3$x, y = ~PV_3$y, name = "2003 Okuma", fill = 'tozeroy') %>%
  add_lines(x = ~PV_6$x, y = ~PV_6$y, name = "2006 Okuma", fill = 'tozeroy') %>% 
  add_lines(x = ~PV_9$x, y = ~PV_9$y, name = "2009 Okuma", fill = 'tozeroy') %>%
  add_lines(x = ~PV_12$x, y = ~PV_12$y, name = "2012 Okuma", fill = 'tozeroy') %>% 
  add_lines(x = ~PV_15$x, y = ~PV_15$y, name = "2015 Okuma", fill = 'tozeroy') %>%
  add_lines(x = ~PV_18$x, y = ~PV_18$y, name = "2018 Okuma", fill = 'tozeroy') %>%   
  layout(xaxis = list(title = 'Okuma Başarı Puanı (PV)'),
         yaxis = list(title = 'Yoğunluk'))

PV_plotlyFEN
PV_plotlyMAT
PV_plotlyOKUMA

# PV 18 grup ggplotly

# PVggplotly <- ggplot(datas2, aes(x = MEANPV)) +
#  geom_density(aes(fill=Course),alpha=0.3,position="stack") +
#  xlab("Başarı Puanı (PV)") + ylab("Yoğunluk") +
#  facet_wrap(~YEAR)

# ggplotly(PVggplotly, tooltip = c("text"))


## ESCS 

# plotly

ESCSpl <- datas2 %>% group_split(YEAR,Course)

ESCSpl

ESCS_S03 <- density(ESCSpl[1][[1]]$ESCS, na.rm = TRUE)
ESCS_M03 <- density(ESCSpl[2][[1]]$ESCS, na.rm = TRUE)
ESCS_R03 <- density(ESCSpl[3][[1]]$ESCS, na.rm = TRUE)

ESCS_S06 <- density(ESCSpl[4][[1]]$ESCS, na.rm = TRUE)
ESCS_M06 <- density(ESCSpl[5][[1]]$ESCS, na.rm = TRUE)
ESCS_R06 <- density(ESCSpl[6][[1]]$ESCS, na.rm = TRUE)

ESCS_S09 <- density(ESCSpl[7][[1]]$ESCS, na.rm = TRUE)
ESCS_M09 <- density(ESCSpl[8][[1]]$ESCS, na.rm = TRUE)
ESCS_R09 <- density(ESCSpl[9][[1]]$ESCS, na.rm = TRUE)

ESCS_S12 <- density(ESCSpl[10][[1]]$ESCS, na.rm = TRUE)
ESCS_M12 <- density(ESCSpl[11][[1]]$ESCS, na.rm = TRUE)
ESCS_R12 <- density(ESCSpl[12][[1]]$ESCS, na.rm = TRUE)

ESCS_S15 <- density(ESCSpl[13][[1]]$ESCS, na.rm = TRUE)
ESCS_M15 <- density(ESCSpl[14][[1]]$ESCS, na.rm = TRUE)
ESCS_R15 <- density(ESCSpl[15][[1]]$ESCS, na.rm = TRUE)

ESCS_S18 <- density(ESCSpl[16][[1]]$ESCS, na.rm = TRUE)
ESCS_M18 <- density(ESCSpl[17][[1]]$ESCS, na.rm = TRUE)
ESCS_R18 <- density(ESCSpl[18][[1]]$ESCS, na.rm = TRUE)

ESCS_plotly <- plot_ly() %>%
  add_lines(x = ~ESCS_S03$x, y = ~ESCS_S03$y, name = "2003", fill = 'tozeroy') %>%
  add_lines(x = ~ESCS_S06$x, y = ~ESCS_S06$y, name = "2006", fill = 'tozeroy') %>%
  add_lines(x = ~ESCS_S09$x, y = ~ESCS_S09$y, name = "2009", fill = 'tozeroy') %>%
  add_lines(x = ~ESCS_S12$x, y = ~ESCS_S12$y, name = "2012", fill = 'tozeroy') %>%
  add_lines(x = ~ESCS_S15$x, y = ~ESCS_S15$y, name = "2015", fill = 'tozeroy') %>%
  add_lines(x = ~ESCS_S18$x, y = ~ESCS_S18$y, name = "2018", fill = 'tozeroy') %>%
  layout(xaxis = list(title = 'ESCS'),
         yaxis = list(title = 'Yoğunluk'))

ESCS_plotly

# ggplotly 

# ESCSggplotly <- ggplot(datas2, aes(x = ESCS)) +
#  geom_density(aes(fill=Course),alpha=0.3,position="stack") +
#  xlab("ESCS") + ylab("Yoğunluk") +
#  facet_wrap(~YEAR)

# ggplotly(ESCSggplotly, tooltip = c("text"))

## HISEI

# topluH <- toplu
# topluH$HISEI <- as.factor(as.integer(toplu$HISEI))

# HISEIgg <- ggplot(topluH, aes(x=YEAR, fill = HISEI)) +
#  geom_bar(stat="count") +
#  xlab("Yıl") + ylab("Frekans")

# ggsave("z_HISEIgg.png")

## PAREDINT

# topluP <- toplu

# topluP$PAREDINT <- factor(toplu$PAREDINT, 
#                  levels = c("NA", "0", "3", "4", "5", "8", "11",
#                             "12", "13", "14", "15", "16"))

# PAREDINTgg <- ggplot(topluP, aes(x=YEAR, fill = PAREDINT)) +
#  geom_bar(stat="count") +
#  xlab("Yıl") + ylab("Frekans")

# ggsave("z_PAREDINTgg.png")

## HOMEPOS

### ggplot
# library(ggplot2)
# toplu$HOMEPOS <- as.numeric(toplu$HOMEPOS)
# HP_gg <- ggplot(toplu, aes(x = HOMEPOS, color = "cond")) +
#  geom_histogram() +
#  facet_wrap(~ YEAR, ncol=3) +
#  xlab("HOMEPOS") + ylab("Frekans") +
#  theme(legend.position="none") 

# ggsave ("z_HP_gg.png")

### plotly 
# toplu$HOMEPOS <- as.numeric(toplu$HOMEPOS)

# is.numeric(toplu$HOMEPOS)

# HP_1 <- filter (toplu, YEAR == "2003"); HP_1 <- density(HP_1$HOMEPOS, na.rm=T)
# HP_2 <- filter (toplu, YEAR == "2006"); HP_2 <- density(HP_2$HOMEPOS, na.rm=T)
# HP_3 <- filter (toplu, YEAR == "2009"); HP_3 <- density(HP_3$HOMEPOS, na.rm=T)
# HP_4 <- filter (toplu, YEAR == "2012"); HP_4 <- density(HP_4$HOMEPOS, na.rm=T)
# HP_5 <- filter (toplu, YEAR == "2015"); HP_5 <- density(HP_5$HOMEPOS, na.rm=T)
# HP_6 <- filter (toplu, YEAR == "2018"); HP_6 <- density(HP_6$HOMEPOS, na.rm=T)

# HP_plotly <- plot_ly() %>%
#  add_lines(x = ~HP_1$x, y = ~HP_1$y, name = "2003", fill = 'tozeroy') %>%
#  add_lines(x = ~HP_2$x, y = ~HP_2$y, name = "2006", fill = 'tozeroy') %>%
#  add_lines(x = ~HP_3$x, y = ~HP_3$y, name = "2009", fill = 'tozeroy') %>%
#  add_lines(x = ~HP_4$x, y = ~HP_4$y, name = "2012", fill = 'tozeroy') %>%
#  add_lines(x = ~HP_5$x, y = ~HP_5$y, name = "2015", fill = 'tozeroy') %>%
#  add_lines(x = ~HP_6$x, y = ~HP_6$y, name = "2018", fill = 'tozeroy') %>% 
#  layout(xaxis = list(title = 'HOMEPOS'),
#         yaxis = list(title = 'Yoğunluk'))

# orca(HP_plotly, file = "HP_plotly.png")

## PV

### ggplot

# PV_gg <- ggplot(datas2, aes(x = MEANPV, color = "cond")) +
#  geom_histogram() +
#  facet_wrap(~ YEAR + Course, ncol=3) +
#  xlab("Başarı Puanı (PV)") + ylab("Frekans") +
#  theme(legend.position="none") 

# ggsave("z_PV_gg.png")

# Frekanslar ----

# frekanslar <- datas2 %>% group_by(YEAR, Course,Group) %>% summarise(frekans=n())

# library(tidyr)
# library(dplyr)

# datas2 %>% group_by(YEAR, Group) %>% summarize(mean(ESCS,na.rm=TRUE),mean(MEANPV,na.rm=TRUE))
# datas2 %>% group_by(YEAR, Group) %>% summarize(min(ESCS,na.rm=TRUE), max(ESCS,na.rm=TRUE))

detach(package:plyr)

datas_gr <- datas2 %>% mutate(ESCS_cat = 
                            case_when(
                              ESCS <=  -4 ~ "(,-4]",
                              ESCS > -4 & ESCS <=  -3.5 ~ "(-4,-3.5]",
                              ESCS > -3.5 & ESCS <=  -3 ~ "(-3.5,-3]",
                              ESCS > -3 & ESCS <=  -2.5 ~ "(-3,-2.5]",
                              ESCS > -2.5 & ESCS <=  -2.3 ~ "(-2.5,-2.3]",
                              ESCS > -2.3 & ESCS <=  -2.1 ~ "(-2.3,-2.1]",
                              ESCS > -2.1 & ESCS <=  -1.9 ~ "(-2.3,-1.9]",
                              ESCS > -1.9 & ESCS <=  -1.7 ~ "(-1.9,-1.7]",
                              ESCS > -1.7 & ESCS <=  -1.5 ~ "(-1.7,-1.5]",
                              ESCS > -1.5 & ESCS <=  -1.3 ~ "(-1.5,-1.3]",
                              ESCS > -1.3 & ESCS <=  -1.1 ~ "(-1.3,-1.1]",
                              ESCS > -1.1 & ESCS <=  -0.9 ~ "(-1.1,-0.9]",
                              ESCS > -0.9 & ESCS <=  -0.7 ~ "(-0.9,-0.7]",
                              ESCS > -0.7 & ESCS <=  -0.5 ~ "(-0.7,-0.5]",
                              ESCS > -0.5 & ESCS <=  -0.4 ~ "(-0.5,-0.4]",
                              ESCS > -0.4 & ESCS <=  -0.3 ~ "(-0.4,-0.3]",
                              ESCS > -0.3 & ESCS <=  -0.2 ~ "(-0.3,-0.2]",
                              ESCS > -0.2 & ESCS <=  -0.1 ~ "(-0.2,-0.1]",
                              ESCS > -0.1 & ESCS <=  0.0 ~ "(-0.1,0.0]",
                              ESCS > 0.0 & ESCS <=  0.1 ~ "(0.0,0.1]",
                              ESCS > 0.1 & ESCS <=  0.2 ~ "(0.1,0.2]",
                              ESCS > 0.2 & ESCS <=  0.3 ~ "(0.2,0.3]",
                              ESCS > 0.3 & ESCS <=  0.4 ~ "(0.3,0.4]",
                              ESCS > 0.4 & ESCS <=  0.5 ~ "(0.4,0.5]",
                              ESCS > 0.5 & ESCS <=  0.7 ~ "(0.5,0.7]",
                              ESCS > 0.7 & ESCS <=  0.9 ~ "(0.7,0.9]",
                              ESCS > 0.9 & ESCS <=  1.1 ~ "(0.9,1.1]",
                              ESCS > 1.1 & ESCS <=  1.3 ~ "(1.1,1.3]",
                              ESCS > 1.3 & ESCS <=  1.5 ~ "(1.3,1.5]",
                              ESCS > 1.5 & ESCS <=  1.7 ~ "(1.5,1.7]",
                              ESCS > 1.7 & ESCS <=  1.9 ~ "(1.7,1.9]",
                              ESCS > 1.9 & ESCS <=  2.1 ~ "(1.9,2.1]",
                              ESCS > 2.1 & ESCS <=  2.3 ~ "(2.1,2.3]",
                              ESCS > 2.3 & ESCS <=  2.5 ~ "(2.3,2.5]",
                              ESCS > 2.5 & ESCS <=  3 ~ "(2.5,3]",
                              ESCS > 3 & ESCS <=  3.5 ~ "(3,3.5]",
                              ESCS > 3.5 & ESCS <=  4 ~ "(3.5,4]",
                              ESCS > 4  ~ "(4, )",))  

datas_gr <- datas_gr %>% group_by(YEAR, Course, Group, ESCS_cat)   %>%  
  summarize(meanESCS= mean(ESCS,na.rm=TRUE),mMEANPV= mean(MEANPV,na.rm=TRUE))   %>% 
  arrange(desc(meanESCS)) %>% ungroup()

# datas_gr1 <- filter(datas_gr, YEAR == "2003")
# datas_gr2 <- filter(datas_gr, YEAR == "2006")
# datas_gr3 <- filter(datas_gr, YEAR == "2009")
# datas_gr4 <- filter(datas_gr, YEAR == "2012")
# datas_gr5 <- filter(datas_gr, YEAR == "2015")
# datas_gr6 <- filter(datas_gr, YEAR == "2018")

library(ggpubr)

# ESCS - PV ilişkisi

ESCS_PV <- ggplot(datas_gr,aes(x=meanESCS , y=mMEANPV)) +
  geom_line()+
  facet_wrap(vars(Course, Group), scales ="free_x", ncol = 2, strip.position = "top")+
  geom_smooth(method='lm', formula= y~x)+
  stat_regline_equation() +
  xlab("Ortalama SES") + ylab("Ortalama Olası Değer")

ESCS_PV

# ggplot(datas_gr1,aes(x=meanESCS , y=mMEANPV)) +
#  geom_line()+
#  facet_wrap(vars(YEAR, Course, Group))+
#  geom_smooth(method='lm', formula= y~x)+
#  stat_regline_equation()

###

# datas_gr %>% 
#  plot_ly(x = ~meanESCS, y = ~mMEANPV, color = ~factor(Group))


