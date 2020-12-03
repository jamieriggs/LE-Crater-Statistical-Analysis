
# Thermal Inertia Analysis

# Program TImodels.R


# Modified 
#          29 May 2018, J. Riggs, models only
#          13 Sep 2016, J. Riggs, began modifications for TES
#          02 Dec 2016, J. Riggs, column name changes of raw data to fit existing code names
#          01 Jam 2014, J. Riggs, modified for crater layer data
#          26 May 2010, J. Riggs, modified for IFD data
#  Created 24 Jul 2008, J. Riggs, from PSHI quarterly report


#  Dataset usage.
  
  
#  Imports
#  -------

#  Exports
#  -------


#########################################################
#     Initialization
#########################################################

Sys.setenv(TZ="America/Denver")

Path <- "~/Desktop/SwRI/TI"
setwd(Path)
(WD <- getwd())


#library(MASS)
library(xtable)
# install.packages("dafs")
library(dafs)
library(ggplot2)
library(spatstat)
library(reshape2)
# install.packages("ggfortify")
library(ggfortify)
library("dplyr")

#library(devtools)
#install_github("spatstat/spatstat.sphere")
library(spatstat.sphere)


#########################################################
#     Functions
#########################################################
	
# closest <- function(target,trial) {
	# target[which(abs(target-trial)==min(abs(target-trial)))]
	# }

# FreqProp <- function(factor, factorLabel, dump) {
	# table.x <- as.data.frame(table(factor),exclude=c(NA,dump))
	# names(table.x)[1] <- c(factorLabel)
	# prop <- table.x$Freq / sum(table.x$Freq)
	# table.x <- data.frame(table.x, prop)
	# sum.x <- colSums(table.x[,2:3])
	# new.row <- table.x[1,]
	# new.row[1] <- "Total"
	# new.row[2:3] <- sum.x
	# table.x <- rbind(table.x, new.row)
	# }
  
FreqProp2 <- function(factor1, factor2, faclab1, faclab2, dump) {
	table.x <- as.data.frame(table(factor1,factor2),exclude=c(NA,dump))
	names(table.x)[1:2] <- c(faclab1,faclab2)
	prop <- table.x$Freq / sum(table.x$Freq)
	table.x <- data.frame(table.x, prop)
	sum.x <- colSums(table.x[,3:4])
	new.row <- table.x[1,]
	new.row[1:2] <- c("","Total")
	new.row[3:4] <- sum.x
	table.x <- rbind(table.x, new.row)
	}
  
# FreqProp3 <- function(factor1, factor2, factor3, faclab1, faclab2, faclab3, dump) {
	# table.x <- as.data.frame(table(factor1,factor2,factor3),exclude=c(NA,dump))
	# names(table.x)[1:3] <- c(faclab1,faclab2,faclab3)
	# prop <- table.x$Freq / sum(table.x$Freq)
	# table.x <- data.frame(table.x, prop)
	# sum.x <- colSums(table.x[,4:5])
	# new.row <- table.x[1,]
	# new.row[1:3] <- c("","","Total")
	# new.row[4:5] <- sum.x
	# table.x <- rbind(table.x, new.row)
	# }
  
# FreqProp4 <- function(factor1, factor2, factor3, factor4, faclab1, faclab2, faclab3, faclab4, dump) {
	# table.x <- as.data.frame(table(factor1,factor2,factor3,factor4),exclude=c(NA,dump))
	# names(table.x)[1:4] <- c(faclab1,faclab2,faclab3,faclab4)
	# Proportion <- table.x$Freq / sum(table.x$Freq)
	# table.x <- data.frame(table.x, Proportion)
	# sum.x <- colSums(table.x[,5:6])
	# new.row <- table.x[1,]
	# new.row[1:4] <- c(rep(" ",3),"Total")
	# new.row[5:6] <- sum.x
	# table.x <- rbind(table.x, new.row)
	# }
  


# WriteCSV <- function(CSVfileName, RdataSet) {
	# outfile <- paste(WD, paste(CSVfileName, ".csv", sep=""), sep="/")
	# write.csv(RdataSet, file=outfile, row.names=F)
	# }

#########################################################
     part <- "Model"   # logistic or glm
#########################################################

 (Ex <- "MDAP")
(ver <- "TES")

path <- "~/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, "4Modeling.RData"))
load(infile)
summary(X)
nrow(X)
setwd(Path)
(WD <- getwd())

X   <- X[X$TES>0,]
X$y <- log10(X$TES)

m1 <- glm(p ~ TES + TESTYPE + TIME + DIAMETER +LAYERS + TERMINATE + EDGE, data = X, family = "binomial")
m <- m1
summary(m)


m2 <- glm(y ~ (TIME + LAYERS + TERMINATE + EDGE + TESTYPE + DCI + log10(DIAMETER) + LAYERS:DCI + LAYERS:log10(DIAMETER)),data=X,family = Gamma(link=log))
m2 <- glm(y ~ (TIME + LAYERS + TERMINATE + EDGE + TESTYPE + DCI + log10(DIAMETER) + LAYERS:DCI + LAYERS:log10(DIAMETER)), data = X, family = gaussian)#family = Gamma(link=log))
m <- m2
(ms2 <- summary(m2))

W <- X
W$yhat <- predict(m)

(c2 <- as.data.frame(exp(m$coef)))

autoplot(m2, which = 1:6, label.size = 3)

part <- "QQ"
 ilk <- "log10TES"
# ilk <- "UnimodalResiduals"
#   y <- W$TES    # offset all values as min(TES) = 0
#   y <- resid(m2)    # offset all values as min(TES) = 0
    y <- predict(m2)#X$y
(loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,".png"))
#(main <- paste("log10(TES) Normal QQ Plot"))
(main <- paste("Unimodal Model Residuals Normal QQ Plot"))
v = quantile(y[!is.na(y)], c(0.25, 0.75))
h = qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
int <- v[1L] - slope * h[1L]
#W <- X[1:length(y),]
gp <- ggplot(X, aes(sample=y)) + 
	stat_qq(col='grey45') +
	geom_abline(slope = slope, intercept = int) + 
	ggtitle(main) + 
	xlab("Theoretical Quantiles") + 
	ylab("Observed Quantiles") +
	theme(axis.text=element_text(size=16)) +
	theme(axis.title=element_text(size=20)) +
	theme(plot.title=element_text(size=24,hjust = 0.5)) +
	theme(panel.background = element_rect(fill = "grey92"))
    ggsave(loc)

x <- log10(W$y)
cut <- 100
x <- X$TES[X$TES<cut]
y <- x
hist <- hist(x,breaks="FD",plot=F)
breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
bwidth <- breaks[2]-breaks[1]
 part <- "Hist"
  ilk <- "log10TES"
   xl <- "log10(TES)"
(main <- paste("log10(TES) Histogram"))
(loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,".png"))
gp <- ggplot(subset(X,TES<cut), aes(x)) + 
	geom_histogram(aes(y=..density..),col='grey45',binwidth=bwidth) + 
	geom_density() +
	ggtitle(main) + 
	xlab(xl) + 
	ylab("Frequency") +
	theme(axis.text=element_text(size=16)) +
	theme(axis.title=element_text(size=20)) +
	theme(plot.title=element_text(size=24,hjust = 0.5)) +
	theme(panel.background = element_rect(fill = "grey92"))
    ggsave(loc)

# calculate gamma quantiles using mean and standard deviation from "ozone" to calculate shape and scale parameters
n <- length(X$TES)
probabilities <- (1:n) / (n+1)
gamma.quantiles = qgamma(probabilities, shape = mean(X$TES)^2/var(X$TES), scale = var(X$TES)/mean(X$TES))


# gamma quantile-quantile plot
plot(sort(gamma.quantiles), sort(X$TES), xlab = 'Theoretical Quantiles from Gamma Distribution', ylab = 'Sample Quantiles of TES', main = 'Gamma Quantile-Quantile Plot of TES')
	abline(0,1)
	dev.off()

library(qualityTools)
y <- log10(X$TES+1.01)
y <- m$residuals+1
qqPlot(y, "gamma", DB = F)



##############################################
     part <- "Model"   # mixture distribution response models
##############################################

 (Ex <- "MDAP")
(ver <- "TES")

path <- "~/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, "4Modeling.RData"))
load(infile)
summary(X)
nrow(X)
setwd(Path)
(WD <- getwd())

# remove zero values, 2017.07.14 MDAP meeting
X   <- X[X$TES>0,]
X$y <- log10(X$TES)

library("plyr")
X <- transform(X,TIME=revalue(TIME,c("Daytime"="Day")))
X$TIME    <- factor(X$TIME, levels=c("Day","Night"))
X$LAYERS  <- factor(X$LAYERS, levels=c("SLE","DLE","MLE"))
X$TESTYPE <- factor(X$TESTYPE, levels=c("Median","Minimum","Maximum"))

names(X)[ncol(X)-1] <- "DCIlevel"
X$DCIlevel <- factor(X$DCIlevel, levels=c("Low","Medium","High","N/A"))
DCI <- X$DCI
X$DCI <- ifelse(is.na(X$DCI),median(X$DCI,na.rm=T),X$DCI)
summary(X)


# install.packages("flexmix")
library(flexmix)

m <- flexmix(y ~ (TIME + LAYERS + TERMINATE + EDGE + TESTYPE + DCIlevel + DIAMETER + LAYERS:DIAMETER), 
	model = list(FLXMRglmfix(, ~ ., family="gaussian"),  
	FLXMRglmfix(, ~ ., family="Gamma")), 
	data = X, k=2)

m3 <- m
(ms <- summary(m))
ms3 <- ms
(mc1 <- parameters(m, component=1, model=1))
(mc2 <- parameters(m, component=2, model=2))
(ec1 <- exp(mc1))
(ec2 <- exp(mc2))
p <- ms@comptab$prior
# plot(m)
# mr <- refit(m, method="mstep")
# summary(mr, which = "model", model = 1)
# ml <- LR_test(m,10)
# summary(ml)

(parm <- data.frame(Variables=gsub("coef.","",attr(mc1,"dimnames")[[1]][(-length(mc1))]), Component.1=mc1[(-length(mc1))], Component.2=mc2[(-length(mc2))]  ) )





# 2nd best model

m <- flexmix(y ~ (TIME + TERMINATE + EDGE + TESTYPE + LAYERS + log10(DIAMETER) + LAYERS:log10(DIAMETER)) , 
#	 model = list(FLXMRglmfix(, ~ ., family="gaussian"),  
#	 FLXMRglmfix(, ~ ., family="gaussian")), 
	 data = X, k=2)

# m <- m4
m4 <- m
(ms <- summary(m))
ms4 <- ms
(mc1 <- parameters(m, component=1, model=1))
(mc2 <- parameters(m, component=2, model=1))
#(ec1 <- exp(mc1))
#(ec2 <- exp(mc2))
 plot(m)
 mr <- refit(m)
 summary(mr)
p <- ms@comptab$prior

(parm <- data.frame(Variables=gsub("coef.","",attr(mc1,"dimnames")[[1]][(-length(mc1))]), Component.1=mc1[(-length(mc1))], Component.2=mc2[(-length(mc2))]  ) )


# m <- flexmix(y ~ (TIME + TERMINATE + EDGE + LAYERS + log10(DIAMETER) + LAYERS:log10(DIAMETER) | TESTYPE), data = X, k=3)

# m5 <- m
# (ms <- summary(m))
# ms5 <- ms
# (mc1 <- parameters(m, component=1, model=1))
# (mc2 <- parameters(m, component=2, model=1))
# (ec1 <- exp(mc1))
# (ec2 <- exp(mc2))
# # plot(m)
# # summary(refit(m))
# p <- ms@comptab$prior

# (parm <- data.frame(Variables=gsub("coef.","",attr(mc1,"dimnames")[[1]][(-length(mc1))]), Component.1=mc1[(-length(mc1))], Component.2=mc2[(-length(mc2))]  ) )


#m <- stepFlexmix(y ~ (TIME + LAYERS + TERMINATE + EDGE + TESTYPE + p + log10(DIAMETER) + p:log10(DIAMETER)  + LAYERS:log10(DIAMETER) ), 
#	model = list(FLXMRglmfix(, ~ ., family="gaussian"),  
#	FLXMRglmfix(, ~ ., family="Gamma")), 
#	data = X, k=1:3, nrep=5)


# circular stats

x <- s2pp(coords, radius=3390, unitname=c("degree","degrees"))
x <- s2pp(data.frame(long=X$Lon,lat=X$Lat), radius=3390, unitname=c("km","km"))
str(x)
summary(x)

library(circular)
circ <- circular(X[,c("Lat","y")],type="angles",units="degrees",template="none")
plot(circ,units="degrees", zero=pi/2)
rose.diag(circ)

par(mfrow=c(1,2))
plot(X$y,X$Lat)
plot(X$Lon,X$y)
par(mfrow=c(1,1))

dev.new(width=9, height=4)
par(mfrow=c(1,3))
plot(x$data$x,X$y)
plot(x$data$y,X$y)
plot(x$data$z,X$y)
par(mfrow=c(1,1))
dev.off()


# Best model as of 2018.03.30

# with p
#m <- flexmix(y ~ (TIME + LAYERS + TERMINATE + EDGE + TESTYPE + p + log10(DIAMETER) + p:log10(DIAMETER)  + LAYERS:log10(DIAMETER) ), 
# without p
#m <- flexmix(y ~ (TIME + LAYERS + TERMINATE + EDGE + TESTYPE + log10(DIAMETER) + LAYERS:log10(DIAMETER) + x$data$x + x$data$y + x$data$z ), 

#m <- flexmix(y ~ (TIME + LAYERS + TERMINATE + EDGE + TESTYPE + DCI + log10(DIAMETER) + 
# LAYERS:DCI + LAYERS:log10(DIAMETER)),# + Lat + Lon), 
#	model = list(FLXMRglmfix(, ~ ., family="gaussian"),  
#	FLXMRglmfix(, ~ ., family="Gamma"),FLXMRglmfix(, ~ ., family="Gamma")), 
#	data = X, k=2)

m <- flexmix(y ~ (TIME + LAYERS + TERMINATE + EDGE + TESTYPE + DCI + log10(DIAMETER) + LAYERS:DCI + LAYERS:log10(DIAMETER)),# + Lat + Lon),
data = X, k=2)

m5 <- m
(ms <- summary(m))
ms5 <- ms

(mc1 <- parameters(m, component=1, model=1))
(mc2 <- parameters(m, component=2, model=1))
#(mc3 <- parameters(m, component=3, model=1))
#(ec1 <- exp(mc1))
#(ec2 <- exp(mc2))
#(ec3 <- exp(mc3))
# plot(m)
mr <- refit(m)

c0 <- mr@components[[1]][[1]] 
c1 <- data.frame(mr@components[[1]][[1]]) 
cr <- rownames(c1)
c1 <- as.data.frame(sapply(c1, as.numeric))
c1 <- round(c1, 4)
#(names(c1) <- attr(c0,"dimnames")[[2]])
c1 <- data.frame(Variables=cr, data.frame(c1,row.names=NULL)) 
# sig <- data.frame("Signif. codes:  0 ‘***’", "0.001 ‘**’", "0.01 ‘*’", "0.05 ‘.’", "0.1 ‘ ’ 1")
# names(sig) <- names(c1)
# (c1 <- rbind(c1,sig) )
names(c1) <- c("Variables",attr(c0,"dimnames")[[2]]) 
c1.1 <- c1
ilk <- "ParamsExp1"
(main <- paste("Thermal inertia", tolower(part), "of component 1 (Gaussian, 37.2\\%). The effect size (Model.TI) is in inertia units for a unit change in the categorical variables. The crater density is a 25\\% change. Crater diameter is a 10km change."))
(loc <- paste0(WD,"/Tables/",Ex,ver,part,ilk,".tex"))
print(xtable(c1, digits=c(4,4,4,4,4,4), caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", hline.after=c(-1,0,nrow(c1)), file=loc)

c0 <- mr@components[[1]][[2]] 
c1 <- data.frame(mr@components[[1]][[2]]) 
cr <- rownames(c1)
c1 <- as.data.frame(sapply(c1, as.numeric))
c1 <- round(c1, 4)
c1 <- data.frame(Variables=cr, data.frame(c1,row.names=NULL)) 
names(c1) <- c("Variables",attr(c0,"dimnames")[[2]]) 
c1.2 <- c1
ilk <- "ParamsExp2"
(main <- paste("Thermal inertia", tolower(part), "of component 2 (Gaussian, 62.8\\%). The effect size (Model.TI) is in inertia units for a unit change in the categorical variables. The crater density is a 25\\% change. Crater diameter is a 10km change."))
(loc <- paste0(WD,"/Tables/",Ex,ver,part,ilk,".tex"))
print(xtable(c1, digits=c(4,4,4,4,4,4), caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", hline.after=c(-1,0,nrow(c1)), file=loc)

# c0 <- mr@components[[1]][[3]] 
# c1 <- data.frame(mr@components[[1]][[3]]) 
# cr <- rownames(c1)
# c1 <- as.data.frame(sapply(c1, as.numeric))
# c1 <- round(c1, 4)
# c1 <- data.frame(Variables=cr, data.frame(c1,row.names=NULL)) 
# names(c1) <- c("Variables",attr(c0,"dimnames")[[2]]) 
# c1.3 <- c1
# ilk <- "ParamsExp2"
# (main <- paste("Thermal inertia", tolower(part), "of component 2 (Gaussian, 62.8\\%). The effect size (Model.TI) is in inertia units for a unit change in the categorical variables. The crater density is a 25\\% change. Crater diameter is a 10km change."))
# (loc <- paste0(WD,"/Tables/",Ex,ver,part,ilk,".tex"))
# print(xtable(c1, digits=c(4,4,4,4,4,4), caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", hline.after=c(-1,0,nrow(c1)), file=loc)

c1.1
c1.2
#c1.3

plot(X$DCI,X$y)

p <- ms@comptab$prior
X$component <- apply(posterior(m),1,which.max)   # assigns crater to component (1 or 2)
(parm <- data.frame(Variables=gsub("coef.","",attr(mc1,"dimnames")[[1]][(-length(mc1))]), Component.1=mc1[(-length(mc1))], Component.2=mc2[(-length(mc2))]  ) )


#(parm <- data.frame(Variables=gsub("coef.","",attr(mc1,"dimnames")[[1]][(-length(mc1))]), Component.1=mc1[(-length(mc1))], Component.2=mc2[(-length(mc2))], Component.3=mc3[(-length(mc3))]  ) )
#(fp2 <- FreqProp2(X$DCIlevel,X$component,"DCI","Component") )

mg <- getModel(m, "BIC")
refit(mg)

# Effect size

(lvls <- diag(c(rep(1,(nrow(parm)-3)),0.25,1,0.25)) %*% (exp((as.matrix(parm[,2:3]) %*% p))) )
(effects <- data.frame(parm,Mixture=lvls ) )
rm(lvls)
#(effects <- data.frame(effects,Model.TI=10^(effects$Mixture)))

# tex table of component parameters
ilk <- "ParamsExp"
(main <- paste("Thermal inertia", tolower(part), "anti-log2 of components 1 (Gaussian, 24\\%), 2 (gamma, \\%76), and the component mixture. The effect size (Model.TI) is in inertia units for a unit change in the categorical variables. The crater density is a 25\\% change. Crater diameter is a 10km change."))
(loc <- paste0(WD,"/Tables/",Ex,ver,part,ilk,".tex"))
print(xtable(effects, digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)


 pred <- predict(m, X)   # fitted values by component
clust <- clusters(m,X)   # observation by cluster (component)
    W <- cbind(X,data.frame(pred),data.frame(clust))
summary(W)

##################################
# save data by cluster

C1 <- W[W$clust==1,]
summary(C1[,c(1:10,14)])
nrow(C1)
C2 <- W[W$clust==2,]
summary(C2[,c(1:10,14)])
nrow(C2)

path <- "~/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
(outfile <- paste0(WD, "/",ver, "Component1.RData"))
save(C1, file=outfile, ascii=FALSE)     # saves as C1
(outfile <- paste0(WD, "/",ver, "Component2.RData"))
save(C2, file=outfile, ascii=FALSE)     # saves as C2
setwd(Path)
WD <- getwd()

##################################
# load data by cluster

path <- "~/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, "Component1.RData"))
load(infile)
summary(C1)
nrow(C1)
(infile <- paste0(WD, "/",ver, "Component2.RData"))
load(infile)
summary(C2)
nrow(C2)
setwd(Path)
(WD <- getwd())

##################################


#plot(W$y, col = c("red","blue","green")[W$clust], pch = 16, ylab = "log10(TES)")
#plot(W$y, col = c("red","blue")[W$clust], pch = 16, ylab = "log10(TES)")

for(i in 1:nrow(W)){ # i <- 1
  W$pred_model1[i] <- W[,paste0("Comp.",W$clust[i])][i]
  W$pred_model2[i] <- W[,paste0("Comp.",W$clust[i])][i]
#  W$pred_model3[i] <- W[,paste0("Comp.",W$clust[i])][i]
#  W$pred_model1[i] <- W[,paste0("Comp.",W$clust[i],".1")][i]
#  W$pred_model2[i] <- W[,paste0("Comp.",W$clust[i],".2")][i]
#  W$pred_model3[i] <- W[,paste0("Comp.",W$clust[i],".3")][i]
}

  xl <- "Actual"
  yl <- "Predicted"
  ilk <- "Comp1"
(main <- paste(ver,part,"Component 1", yl,"vs.",xl))
 (loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,yl,"By",xl,".png"))
   gp <- qplot(W$y, W$pred_model1) + geom_abline() +
         ylim(1,4.2) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         geom_point(col='grey45') + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)

   xl <- "Actual"
   yl <- "Predicted"
  ilk <- "Comp2"
(main <- paste(ver,part,"Component 2", yl,"vs.",xl))
 (loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,yl,"By",xl,".png"))
   gp <- qplot(W$y, W$pred_model2) + geom_abline() +
         ylim(1,4.2) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         geom_point(col='grey45') + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)

   xl <- "Actual"
   yl <- "Predicted"
  ilk <- "Comp3"
(main <- paste(ver,part,"Component 3", yl,"vs.",xl))
 (loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,yl,"By",xl,".png"))
   gp <- qplot(W$y, W$pred_model3) + geom_abline() +
         ylim(1,4.2) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         geom_point(col='grey45') + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)

   xl <- "Predicted"
   yl <- "Residuals"
 part <- "Model"
  ilk <- "Comp1"
resid <- W$y - W$pred_model1
   is <- coef(lm(resid ~ W$pred_model1, data = W))
(main <- paste(ver,part,"Component 1", yl,"vs.",xl))
 (loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,yl,"By",xl,".png"))
   gp <- qplot(W$pred_model1, resid) +
         geom_abline(intercept=is[[1]], slope=is[[2]]) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         geom_point(col='grey45') + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)

   xl <- "Predicted"
   yl <- "Residuals"
 part <- "Model"
  ilk <- "Comp2"
resid <- W$y - W$pred_model2
   is <- coef(lm(resid ~ W$pred_model2, data = W))
(main <- paste(ver,part,"Component 2", yl,"vs.",xl))
 (loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,yl,"By",xl,".png"))
   gp <- qplot(W$pred_model2, resid) +
         geom_abline(intercept=is[[1]], slope=is[[2]]) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         geom_point(col='grey45') + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)

   xl <- "Predicted"
   yl <- "Residuals"
 part <- "Model"
  ilk <- "Comp3"
resid <- W$y - W$pred_model3
   is <- coef(lm(resid ~ W$pred_model2, data = W))
(main <- paste(ver,part,"Component 3", yl,"vs.",xl))
 (loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,yl,"By",xl,".png"))
   gp <- qplot(W$pred_model3, resid) +
         geom_abline(intercept=is[[1]], slope=is[[2]]) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         geom_point(col='grey45') + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)


(RMSE <- sqrt(mean((log10(W$y)-W$pred_model1)^2)))
(RMSE <- sqrt(mean((log10(W$y)-W$pred_model2)^2)))
(RMSE <- sqrt(mean((log10(W$y)-W$pred_model3)^2)))


     a <- subset(W, clust == 1)
     y <- a$y
  hist <- hist(y,breaks="FD",plot=F)
breaks <- pretty(range(y), n = nclass.FD(y), min.n = 1)
bwidth <- breaks[2]-breaks[1]
  part <- "Hist"
   ilk <- "Gaussian"
    xl <- "log10(TES)"
 (main <- paste("log10(TES) Gaussian (Component 1) Histogram"))
  (loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,".png"))
    gp <- ggplot(a, aes(y)) + 
          geom_histogram(aes(y=..density..),col='grey45',binwidth=bwidth) + 
          geom_density() +
          ggtitle(main) + 
          xlab(xl) + 
          ylab("Frequency") +
          theme(axis.text=element_text(size=16)) +
          theme(axis.title=element_text(size=20)) +
          theme(plot.title=element_text(size=24,hjust = 0.5)) +
          theme(panel.background = element_rect(fill = "grey92"))
          ggsave(loc)

     b <- subset(W, clust == 2)
     y <- b$y
  hist <- hist(y,breaks="FD",plot=F)
breaks <- pretty(range(y), n = nclass.FD(y), min.n = 1)
bwidth <- breaks[2]-breaks[1]
  part <- "Hist"
   ilk <- "Gamma"
    xl <- "log10(TES)"
 (main <- paste("log10(TES) Gaussian (Component 2) Histogram"))
  (loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,".png"))
    gp <- ggplot(b, aes(y)) + 
          geom_histogram(aes(y=..density..),col='grey45',binwidth=bwidth) + 
          geom_density() +
          ggtitle(main) + 
          xlab(xl) + 
          ylab("Frequency") +
          theme(axis.text=element_text(size=16)) +
          theme(axis.title=element_text(size=20)) +
          theme(plot.title=element_text(size=24,hjust = 0.5)) +
          theme(panel.background = element_rect(fill = "grey92"))
          ggsave(loc)

     c <- subset(W, clust == 3)
     y <- c$y
  hist <- hist(y,breaks="FD",plot=F)
breaks <- pretty(range(y), n = nclass.FD(y), min.n = 1)
bwidth <- breaks[2]-breaks[1]
  part <- "Hist"
   ilk <- "Gamma"
    xl <- "log10(TES)"
 (main <- paste("log10(TES) Gaussian (Component 3) Histogram"))
  (loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,".png"))
    gp <- ggplot(c, aes(y)) + 
          geom_histogram(aes(y=..density..),col='grey45',binwidth=bwidth) + 
          geom_density() +
          ggtitle(main) + 
          xlab(xl) + 
          ylab("Frequency") +
          theme(axis.text=element_text(size=16)) +
          theme(axis.title=element_text(size=20)) +
          theme(plot.title=element_text(size=24,hjust = 0.5)) +
          theme(panel.background = element_rect(fill = "grey92"))
          ggsave(loc)

#c = subset(W, clust == 3)


#W$yhat <- c(rnorm(p[1] * nrow(W), mean = mean(a$y), sd = sd(a$y)), mean(a$y),
          rgamma(p[2] * nrow(W), shape = mean(b$y)^2/var(b$y), scale = var(b$y)/mean(b$y))#,
#         rgamma(p[3] * nrow(W), shape = mean(c$y)^2/var(c$y), scale = var(c$y)/mean(c$y))
          )
W$yhat <- as.matrix(data.frame(W$pred_model1,W$pred_model2)) %*% p
summary(W)

# save W
path <- "~/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
outfile <- paste0(WD, "/", ver, "dataW.RData")
save(W, file=outfile, ascii=FALSE)
setwd(Path)
(WD <- getwd())


#########################################################
# load W               use if W already saved

 (Ex <- "MDAP")
(ver <- "TES")
path <- "~/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, "dataW.RData"))
load(infile)
setwd(Path)
(WD <- getwd())
summary(W)
nrow(W)

 (Ex <- "MDAP")
(ver <- "TESDustCoverIndex")

(path <- "~/Desktop/SwRI/DataR")
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
load(infile)
summary(X)
nrow(X)
X$CRATERID <- as.character(X$CRATERID)
X$DCI <- as.numeric(as.character(X$DCI))
str(X)
setwd(Path)
(WD <- getwd())


X <- merge(W, X[,c(1,6)], by="CRATERID", all.x=TRUE)
summary(X)
nrow(X)
X$CRATERID <- as.character(X$CRATERID)
nrow(X[unique(X$CRATERID),])
X <- X[!duplicated(X),]
X[X$CRATERID=="01-1-00013",]
summary(X)

DCI <- X$DCI
X$DCI <- ifelse(is.na(X$DCI),median(X$DCI,na.rm=T),X$DCI)
summary(X)

  (Ex <- "MDAP")
 (ver <- "TES")
(part <- "Model")

#########################################################
# separate into clusters

C1 <- X[X$clust==1,]
summary(C1)
nrow(C1)
C2 <- X[X$clust==2,]
summary(C2)
nrow(C2)

#########################################################

(sum <- summary(X))
#(attr(sum,"dimnames")[[2]] <- c("longitude","latitude","Diameter"))
ilk <- "1"
(main <- paste(ver, "Data", part, "(", ilk, "of 3 )."))
(loc <- paste0(WD,"/Tables/",Ex,ver,part,ilk,".tex"))
print(xtable(sum[,1:4], digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)

ilk <- "2"
(main <- paste(ver, "Data", part, "(", ilk, "of 3 )."))
(loc <- paste0(WD,"/Tables/",Ex,ver,part,ilk,".tex"))
print(xtable(sum[,5:8], digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)

ilk <- "3"
(main <- paste(ver, "Data", part, "(", ilk, "of 3 )."))
(loc <- paste0(WD,"/Tables/",Ex,ver,part,ilk,".tex"))
print(xtable(sum[,9:11], digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)


     y <- W$yhat
  hist <- hist(y,breaks="FD",plot=F)
breaks <- pretty(range(y), n = nclass.FD(y), min.n = 1)
bwidth <- breaks[2]-breaks[1]
  part <- "Histogram"
   ilk <- "Mixture"
    xl <- "Predicted"
 (main <- paste0("Mixture Prediction Histogram of log10(TES)"))
  (loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,".png"))
    gp <- ggplot(W, aes(y)) + 
          geom_histogram(aes(y=..density..),col='grey45',binwidth=bwidth) + 
          geom_density() +
          ggtitle(main) + 
          xlab(xl) + 
          ylab("Proportion") +
          theme(axis.text=element_text(size=16)) +
          theme(axis.title=element_text(size=20)) +
          theme(plot.title=element_text(size=20,hjust = 0.5)) +
          theme(panel.background = element_rect(fill = "grey92"))
          ggsave(loc)

 part <- "QQ"
  ilk <- "Mixture"
   xl <- "Predicted"
(main <- paste0("Mixture Prediction QQ Plot of log10(TES)"))
 (loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,".png"))
    v <- quantile(y[!is.na(y)], c(0.25, 0.75))
    h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
   gp <- ggplot(X, aes(sample=y)) + 
         stat_qq(col='grey45') +
         geom_abline(slope = slope, intercept = int) + 
         ggtitle(main) + 
         xlab("Theoretical Quantiles") + 
         ylab("Observed Quantiles") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)


   xl <- "Actual"
   yl <- "Predicted"
 part <- "Model"
  ilk <- "Mixture"
  ver <- "TI"
(main <- paste(ver,part,"Mixture", yl,"vs.",xl, "\n (Units log10(tiu))"))
 (loc <- paste0(WD,"/Plots/Paper/",Ex,ver,part,ilk,yl,"By",xl,".png"))
   gp <- qplot(W$y, W$yhat) + geom_abline() +
         xlim(0,4) + ylim(0,4) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         geom_point(col='grey45') + 
         theme_bw() +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) #+
#         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)

# For Hoover et al. paper
Title: "ATI Model Mixture"
x-axis: "Actual (log10(tiu))"
y-axis: "Predicted (log10(tiu))"

   xl <- "Actual (log10(tiu))"
   yl <- "Predicted (log10(tiu))"
 part <- "Model"
  ilk <- "Mixture"
  ver <- "TI"
(main <- paste("ATI Model Mixture"))
 (loc <- paste0(WD,"/Plots/Paper/",Ex,ver,part,ilk,"HooverPaper.png"))
   gp <- ggplot(W,aes(y,yhat)) + #qplot(W$y, W$yhat) + 
         xlim(0,4) + ylim(0,4) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         geom_point(size=0.5, stroke=0, alpha=1/5) + 
		 geom_abline() +
         theme_bw() +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) #+
#         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)


  xl <- "Predicted"
  yl <- "Residuals"
part <- "Model"
 ilk <- "Mixture"
resid <- W$y - W$yhat
is <- coef(lm(resid ~ pred_model1, data = W))
(main <- paste(ver,part,"Mixture", yl,"vs.",xl))
 (loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,yl,"By",xl,".png"))
   gp <- qplot(W$pred_model1, resid) +
         geom_abline(intercept=is[[1]], slope=is[[2]]) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         geom_point(col='grey45') + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)


#########################################################
#     predictor scatter plots
#########################################################

    X <- C1
 part <- "Scatter"
   yl <- "log10TIu"
   xl <- "DCI"
   is <- coef(lm(yhat ~ DCI, data = X))
(main <- paste(Ex, part, yl,"vs", xl, "(Component 1)"))
 (loc <- paste0("Plots/Paper/", Ex, ver, part, yl, "By", xl, "C1.png"))
   gp <- qplot(X$DCI, X$yhat) +
         geom_abline(intercept=is[[1]], slope=is[[2]]) +
         xlim(0.9,1) +
         ylim(1,3) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         geom_point(col='grey45') + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)
gp

    X <- C2
 part <- "Scatter"
   yl <- "log10TIu"
   xl <- "DCI"
   is <- coef(lm(yhat ~ DCI, data = X))
(main <- paste(Ex, part, yl,"vs", xl, "(Component 2)"))
 (loc <- paste0("Plots/Paper/", Ex, ver, part, yl, "By", xl, "C2.png"))
   gp <- qplot(X$DCI, X$yhat) +
         geom_abline(intercept=is[[1]], slope=is[[2]]) +
         xlim(0.9,1) +
         ylim(1,3) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         geom_point(col='grey45') + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)
gp



 part <- "Model"
   yl <- "Actual"
   xl <- "Crater Intensity (p)"
   is <- coef(lm(y ~ p, data = W))
(main <- paste(Ex, part, yl,"vs Crater Intensity"))
 (loc <- paste0("Plots/", Ex, ver, part, yl, "Byp.png"))
   gp <- qplot(W$p, W$y) +
         geom_abline(intercept=is[[1]], slope=is[[2]]) +
         ylim(0,4) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         geom_point(col='grey45') + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)

 part <- "Model"
   yl <- "Predicted"
   xl <- "Crater Intensity (p)"
   is <- coef(lm(yhat ~ p, data = W))
(main <- paste(Ex, part, yl,"vs Crater Intensity"))
 (loc <- paste0("Plots/", Ex, ver, part, yl, "Byp.png"))
   gp <- ggplot(W, aes(x=p, y=yhat)) +
         geom_abline(intercept=is[[1]], slope=is[[2]]) +
         ylim(0,4) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         geom_point(col='grey45') + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)


 part <- "Model"
   yl <- "Actual"
   xl <- "log10(DIAMETER)"
  (is <- coef(lm(y ~ log10(DIAMETER), data = W)))
(main <- paste(Ex, part, yl,"vs", xl))
 (loc <- paste0("Plots/", Ex, ver, part, yl, "By", xl, ".png"))
   gp <- ggplot(W, aes(x=log10(DIAMETER), y=y)) +
         geom_abline(intercept=is[[1]], slope=is[[2]]) +
         geom_point(col='grey45') + 
         ylim(0,4) +
         xlim(0.75,2.05) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         geom_point(col='grey45') + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)

 part <- "Model"
   yl <- "Predicted"
   xl <- "log10(DIAMETER)"
  (is <- coef(lm(yhat ~ log10(DIAMETER), data = W)))
(main <- paste(Ex, part, yl,"vs", xl))
 (loc <- paste0("Plots/", Ex, ver, part, yl, "By", xl, ".png"))
   gp <- ggplot(W, aes(x=log10(DIAMETER), y=yhat)) +
         geom_abline(intercept=is[[1]], slope=is[[2]]) +
         geom_point(col='grey45') + 
         ylim(0,4) +
         xlim(0.75,2.05) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         geom_point(col='grey45') + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)


#########################################################
#     box plots
#########################################################

    X <- C1
   yl <- "DCI"
   xl <- "LAYERS"
 part <- "Box"
(main <- paste("Component 1", yl,"vs.",xl, "Boxplot"))
 (loc <- paste0("Plots/Paper/", Ex, ver, part, xl, yl, "C1.png"))
   gp <- ggplot(X, aes(x=as.factor(LAYERS),y=DCI)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(0.9,1) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)
gp

    X <- C1
   yl <- "log10TIu"
   xl <- "Time"
 part <- "Box"
(main <- paste("Component 1", yl,"vs.",xl, "Boxplot"))
 (loc <- paste0("Plots/Paper/", Ex, ver, part, xl, yl, "C1.png"))
   gp <- ggplot(X, aes(x=as.factor(TIME),y=yhat)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(1,3) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)
gp

    X <- C2
   yl <- "log10TIu"
   xl <- "Time"
 part <- "Box"
(main <- paste("Component 2", yl,"vs.",xl, "Boxplot"))
 (loc <- paste0("Plots/Paper/", Ex, ver, part, xl, yl, "C2.png"))
   gp <- ggplot(X, aes(x=as.factor(TIME),y=yhat)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(1,3) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)
gp

    X <- C2
   yl <- "log10TIu"
   xl <- "Terminate"
 part <- "Box"
(main <- paste("Component 2", yl,"vs.",xl, "Boxplot"))
 (loc <- paste0("Plots/Paper/", Ex, ver, part, xl, yl, "C2.png"))
   gp <- ggplot(X, aes(x=as.factor(TERMINATE),y=yhat)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(1,3) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)
gp

    X <- C2
   yl <- "log10TIu"
   xl <- "Edge"
 part <- "Box"
(main <- paste("Component 2", yl,"vs.",xl, "Boxplot"))
 (loc <- paste0("Plots/Paper/", Ex, ver, part, xl, yl, "C2.png"))
   gp <- ggplot(X, aes(x=as.factor(EDGE),y=yhat)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(1,3) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)
gp

    X <- C2
   yl <- "log10D"
   xl <- "Layers"
 part <- "Box"
(main <- paste("Component 2", yl,"vs.",xl, "Boxplot"))
 (loc <- paste0("Plots/Paper/", Ex, ver, part, xl, yl, "C2.png"))
   gp <- ggplot(X, aes(x=as.factor(LAYERS),y=log10(DIAMETER))) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(0,3) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)
gp

    X <- C2
   yl <- "DCI"
   xl <- "Layers"
 part <- "Box"
(main <- paste("Component 2", yl,"vs.",xl, "Boxplot"))
 (loc <- paste0("Plots/Paper/", Ex, ver, part, xl, yl, "C2.png"))
   gp <- ggplot(X, aes(x=as.factor(LAYERS),y=DCI)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(0,3) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)
gp






   yl <- "Actual"
   xl <- "LAYERS"
 part <- "Box"
(main <- paste(yl,"vs.",xl, "Boxplot"))
 (loc <- paste("Plots/", Ex, ver, part, xl, yl, ".png", sep=""))
   gp <- ggplot(W, aes(x=as.factor(LAYERS),y=y)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(0,4) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)

   yl <- "Predicted"
   xl <- "LAYERS"
 part <- "Box"
(main <- paste(yl,"vs.",xl, "Boxplot"))
 (loc <- paste("Plots/", Ex, ver, part, xl, yl, ".png", sep=""))
   gp <- ggplot(W, aes(x=as.factor(LAYERS),y=yhat)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(0,4) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)


   yl <- "Actual"
   xl <- "TERMINATE"
 part <- "Box"
(main <- paste(yl,"vs.",xl, "Boxplot"))
 (loc <- paste("Plots/", Ex, ver, part, xl, yl, ".png", sep=""))
   gp <- ggplot(W, aes(x=as.factor(TERMINATE),y=y)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(0,4) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)

   yl <- "Predicted"
   xl <- "TERMINATE"
 part <- "Box"
(main <- paste(yl,"vs.",xl, "Boxplot"))
 (loc <- paste("Plots/", Ex, ver, part, xl, yl, ".png", sep=""))
   gp <- ggplot(W, aes(x=as.factor(TERMINATE),y=yhat)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(0,4) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)


   yl <- "Actual"
   xl <- "EDGE"
 part <- "Box"
(main <- paste(yl,"vs.",xl, "Boxplot"))
 (loc <- paste("Plots/", Ex, ver, part, xl, yl, ".png", sep=""))
   gp <- ggplot(W, aes(x=as.factor(EDGE),y=y)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
          ylim(0,4) +
        ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)

   yl <- "Predicted"
   xl <- "EDGE"
 part <- "Box"
(main <- paste(yl,"vs.",xl, "Boxplot"))
 (loc <- paste("Plots/", Ex, ver, part, xl, yl, ".png", sep=""))
   gp <- ggplot(W, aes(x=as.factor(EDGE),y=yhat)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(0,4) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)


   yl <- "Actual"
   xl <- "TESTYPE"
 part <- "Box"
(main <- paste(yl,"vs.",xl, "Boxplot"))
 (loc <- paste("Plots/", Ex, ver, part, xl, yl, ".png", sep=""))
   gp <- ggplot(W, aes(x=as.factor(TESTYPE),y=y)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(0,4) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)

   yl <- "Predicted"
   xl <- "TESTYPE"
 part <- "Box"
(main <- paste(yl,"vs.",xl, "Boxplot"))
 (loc <- paste("Plots/", Ex, ver, part, xl, yl, ".png", sep=""))
   gp <- ggplot(W, aes(x=as.factor(TESTYPE),y=yhat)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(0,4) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)


   yl <- "Actual"
   xl <- "TIME"
 part <- "Box"
(main <- paste(yl,"vs.",xl, "Boxplot"))
 (loc <- paste("Plots/", Ex, ver, part, xl, yl, ".png", sep=""))
   gp <- ggplot(W, aes(x=as.factor(TIME),y=y)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(0,4) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)

   yl <- "Predicted"
   xl <- "TIME"
 part <- "Box"
(main <- paste(yl,"vs.",xl, "Boxplot"))
 (loc <- paste("Plots/", Ex, ver, part, xl, yl, ".png", sep=""))
   gp <- ggplot(W, aes(x=as.factor(TIME),y=yhat)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ylim(0,4) +
         ggtitle(main) + 
         xlab(xl) +
         ylab(yl) + 
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20,hjust = 0.5)) +
         theme(panel.background = element_rect(fill = "grey92"))
         ggsave(loc)











hlog <-hist(W$y, freq = FALSE, main = main, xlab = "", xlim = c(0,4), ylim = c(0,2), col = hcl(0, 50, 80))
lines(ys,yhat)

ys <- seq(-1, 4, 5/33581)
length(ys)
p <- prior(m)
yhat <- #p[1] * dgamma(ys, shape = mean(a$y)^2/var(a$y), scale = var(a$y)/mean(a$y)) +
        p[1] * dnorm(ys, mean = mean(a$y), sd = sd(a$y) ) +
        p[2] * dgamma(ys, shape = mean(b$y)^2/var(b$y), scale = var(b$y)/mean(b$y)) #+
#        p[3] * dgamma(ys, shape = mean(c$y)^2/var(c$y), scale = var(c$y)/mean(c$y))

Wyhat <- c(rnorm(p[1] * nrow(W), mean = mean(a$y), sd = sd(a$y)),
        rgamma(p[2] * nrow(W), shape = mean(b$y)^2/var(b$y), scale = var(b$y)/mean(b$y))#,
#        rgamma(p[3] * nrow(W), shape = mean(c$y)^2/var(c$y), scale = var(c$y)/mean(c$y))
        )
length(Wyhat)
W$yhat <- c(Wyhat,mean(Wyhat))
#W$yhat <- yhat * mean(W$y) + min(W$y)
summary(W)


#########################################################
#     Parking lot
#########################################################

m <- flexmix(y ~ (p + TIME + DIAMETER + LAYERS + TERMINATE + EDGE)|TESTYPE, 
	model = list(FLXMRglmfix(y ~ ., family="Gamma"), 
	FLXMRglmfix(y ~ ., family="Gamma"), 
	FLXMRglmfix(y ~ ., family="Gamma")), 
	data = X, k=3)

m4 <- m
(ms <- summary(m))
ms4 <- ms
exp(parameters(m, component=1, model=1))
exp(parameters(m, component=2, model=2))
exp(parameters(m, component=2, model=3))
summary(refit(m))


######################################################
