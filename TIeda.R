
# Thermal Inertia Analysis

# Program TIeda.R
# Each section loads the necessary data so it can be run independently
# Datasets loaded into a dataframe or matrix X and held in dataframe or matrix H


# Modified 
#          02 Jun 2018, J. Riggs, code cleanup
#          29 May 2018, J. Riggs, removed all but data prep and eda
#          13 Sep 2016, J. Riggs, began modifications for TES
#          02 Dec 2016, J. Riggs, column name changes of raw data to fit existing code names
#          01 Jam 2014, J. Riggs, modified for crater layer data
#          26 May 2010, J. Riggs, modified for IFD data
#  Created 24 Jul 2008, J. Riggs, from PSHI quarterly report


#  Dataset and file dependences usage.
  
# 1_Import.R
  
#  Imports
#  -------
# MDAPTES.RData
# MDAPTES4modeling.RData
# MDAPTESDustCoverIndex.RData

#  Exports
#  -------
# MDAPTES.RData


#########################################################
#     Initialization
#########################################################

Sys.setenv(TZ="America/Denver")

library(MASS)
library(xtable)
#library(dafs)
library(ggplot2)
library(spatstat)
library(reshape2)
library(ggfortify)
library(fBasics)

#library(devtools)
#install_github("spatstat/spatstat.sphere")
library(spatstat.sphere)


Path <- "~/Desktop/SwRI/TI"
setwd(Path)
(WD <- getwd())


#########################################################
#     Functions
#########################################################

WriteCSV <- function(CSVfileName, RdataSet) {
	outfile <- paste(WD, paste(CSVfileName, ".csv", sep=""), sep="/")
	write.csv(RdataSet, file=outfile, row.names=F)
	}

##########################################################
# Merge TES and TESDustCoverIndex datasets
# run only once
# merged data saved as TES.RData
##########################################################

(Ex <- "MDAP")
(ver <- "TES")

path <- "~/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
#(infile <- paste0(WD, "/",ver, "4Modeling.RData"))
load(infile)
summary(X)
nrow(X)
X$CRATERID <- as.character(X$CRATERID)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


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


X <- merge(H, X[,c(1,6,7)], by="CRATERID", all.x=TRUE)
summary(X)
nrow(X)
X$CRATERID <- as.character(X$CRATERID)
nrow(X[unique(X$CRATERID),])
X <- X[!duplicated(X),]
X[X$CRATERID=="01-1-00013",]
str(X)

 (Ex <- "MDAP")
(ver <- "TES")

path <- "~/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
(outfile <- paste0(WD, "/", ver, ".RData"))
#(outfile <- paste0(WD, "/",ver, "4Modeling.RData"))
save(X, file=outfile, ascii=FALSE)     # saves as X

setwd(Path)
WD <- getwd()


##########################################################
     part <- "Summary"
##########################################################

 (Ex <- "MDAP")
(ver <- "TES")

(path <- "~/Desktop/SwRI/DataR")
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
load(infile)
summary(X)
nrow(X)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


(sum <- summary(X))
#(attr(sum,"dimnames")[[2]] <- c("longitude","latitude","Diameter"))
ilk <- "1"
(main <- paste(ver, "Data", part, "(", ilk, "of 3 )."))
(loc <- paste0(WD,"/Tables/",Ex,ver,part,ilk,".tex"))
print(xtable(sum[,1:4], digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)

ilk <- "2"
(main <- paste(ver, "Data", part, "(", ilk, "of 3 )."))
(loc <- paste0(WD,"/Tables/",Ex,ver,part,ilk,".tex"))
print(xtable(sum[,c(5:8,11)], digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)

ilk <- "3"
(main <- paste(ver, "Data", part, "(", ilk, "of 3 )."))
(loc <- paste0(WD,"/Tables/",Ex,ver,part,ilk,".tex"))
print(xtable(sum[,c(9,10,12,13)], digits=4, caption=main, label=paste0("tab:",ver,part,ilk)), caption.placement="top", include.rownames=F, table.placement="h", file=loc)


##########################################################
# Scatter plots and pairwise correlations
##########################################################

 (Ex <- "MDAP")
(ver <- "TES")

path <- "~/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
load(infile)
str(X)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data

part <- "XYhistCorrP"
(main <- paste(ver,"Data Histograms, \n Scatter Plots, and Pairwise Correlations"))
(loc <- paste0(WD,"/Plots/",Ex,ver,part,".png"))
png(loc)
	pairsDAFS(X[,c(2:4,9)],main=main,labels=names(X)[c(2:4,9)])
	dev.off()


##########################################################
# Histograms, Q-Q plots
##########################################################

 (Ex <- "MDAP")
(ver <- "TES")

path <- "~/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
load(infile)
str(X)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


 part <- "QQ"
  ilk <- "log10Diameter"
    y <- log10(X$DIAMETER)
 (loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,".png"))
(main <- paste("log10(Diameter) Normal QQ Plot"))
v = quantile(y[!is.na(y)], c(0.25, 0.75))
h = qnorm(c(0.25, 0.75))
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
	theme(plot.title=element_text(size=24,hjust = 0.5)) +
	theme(panel.background = element_rect(fill = "grey92"))
    ggsave(loc)

    x <- log10(X$DIAMETER)
 hist <- hist(x,breaks="FD",plot=F)
breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
bwidth <- breaks[2]-breaks[1]
 part <- "Hist"
  ilk <- "log10Diameter"
   xl <- "log10(Diameter)"
(main <- paste("log10(Diameter) Histogram"))
 (loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,".png"))
gp <- ggplot(X, aes(log10(DIAMETER))) + 
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


# TES >= 5 only

part <- "QQ"
# ilk <- "TES"
 ilk <- "log10TI"
#   y <- X$TES
   y <- log10(X$TES)    # offset all values as min(TES) = 0
(loc <- paste0(WD,"/Plots/",Ex,ver,part,ilk,".png"))
#(main <- paste("TI Normal QQ Plot"))
(main <- paste("log10(TI) Normal QQ Plot"))
v = quantile(y[!is.na(y)], c(0.25, 0.75))
h = qnorm(c(0.25, 0.75))
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
	theme(plot.title=element_text(size=24,hjust = 0.5)) +
	theme(panel.background = element_rect(fill = "grey92"))
    ggsave(loc)
gp

# don't use right tail and pretty to clearly show bimodality

x <- X$TES[X$TES<1000]
#x <- log10(X$TES)
hist <- hist(x,plot=F)
#hist <- hist(x,breaks="FD",plot=F)
#breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
bwidth <- 20 #breaks[2]-breaks[1]
 part <- "Hist"
  ilk <- "Thermal Inertia (tiu)"
#  ilk <- "log10TES"
   xl <- ilk
(main <- paste("Thermal Inertia Units Histogram"))
#(main <- paste("log10(TES) Histogram"))
#(loc <- paste0(WD,"/Plots/Paper/",Ex,ver,part,"whiteTI.png"))
(loc <- paste0(WD,"/Plots/Paper/",Ex,ver,part,"whiteTIcounts.png"))
gp <- ggplot(X[X$TES<1000,], aes(TES[TES<1000])) + 
#	geom_histogram(aes(y=..density..),col='grey45',binwidth=bwidth) + 
	geom_histogram(col='grey45',binwidth=bwidth) + 
	geom_density() +
	ggtitle(main) + 
	xlab(xl) + 
#	ylab("Proportion of Craters") +
	ylab("Number of Craters") +
	theme_bw() +
	theme(axis.text=element_text(size=16)) +
	theme(axis.title=element_text(size=20)) +
	theme(plot.title=element_text(size=24,hjust = 0.5)) #+
#	theme(panel.background = element_rect(fill = "white"))
#	theme(panel.background = element_rect(fill = "grey92"))
    ggsave(loc)
gp	


#########################################################
# Map to Martian sphere of radius 3,390 km
# lat/lat gives great circle distatnces
# run only once
#########################################################

 (Ex <- "MDAP")
(ver <- "TES")

path <- "~/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
load(infile)
summary(X)
names(X)[2:3] <- c("Lat","Lon")   # rename LATITUDE to Lat, LONGITUDUE to Lon
summary(X)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


# rename lat and lon to show flat degrees

rename(X$lat,X$lon) <- c("flat","flon")


# convert flat/flon to spherical coordinates

coords <- data.frame(lon=X$flon,lat=X$flat)
#coords <- coords[!duplicated(coords),]
#str(coords)

x <- s2pp(coords, radius=3390, unitname=c("degree","degrees"))
str(x)

X <- cbind(X,coords)

path <- "~/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
(outfile <- paste0(WD, "/", ver, ".RData"))
save(X, file=outfile, ascii=FALSE)     # saves as X

setwd(Path)
WD <- getwd()


#########################################################
# Intensity estimation
#########################################################

 (Ex <- "MDAP")
(ver <- "TES")

path <- "~/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
load(infile)
summary(X)
names(X)[2:3] <- c("Lat","Lon")   # rename LATITUDE to Lat, LONGITUDUE to Lon
summary(X)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


library(ks)
X <- cbind(X$Lon,X$Lat)
Hpi1 <- Hpi(x = X)     # set with spherical coords
DX <- kde(x = X, H = Hpi1)
Y <- rbind(cbind(X[,1],X[,2]),cbind(X[,1]+360,X[,2]),
     cbind(X[,1]-360,X[,2]),cbind(X[,1],X[,2]+180),
     cbind(X[,1]+360,X[,2]+180),cbind(X[,1]-360,X[,2]+180),
     cbind(X[,1],X[,2]-180),cbind(X[,1]+360,X[,2]-180),
     cbind(X[,1]-360,X[,2]-180)
     )
DY <- kde(x = Y, H = Hpi1)
#library(maps)
(loc <- paste0(WD, "/Plots/", Ex, ver, "SphereSpatDensity.png"))
(main <- "Spatial Density Contours \n (Accounts for Spherical Coordinates)")
( sub <- "Contours are the probability of inclusion.\n Dashed lines intersect at lon/lat (0,0)")
quartz(w=8,h=6)
map("world",col="")#,projection="mollweide")
	points(X,cex=.1,col="yellow2")
	abline(h = 0, untf = FALSE, lty="dashed")
	abline(v = 0, untf = FALSE, lty="dashed")
	plot(DY,add=TRUE, col="black")
	title(main=main, sub=sub)
	quartz.save(loc, type="png")


# Probabilities

X <- H
X$p <- predict(DX,x=DX$x) / max(DX$estimate)


# Save only if new data desired for models as
# logit loads existing data below
path <- "~Jamie/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
outfile <- paste(WD, "TES4Modeling.RData", sep="/")
save(X, file=outfile, ascii=FALSE)


#########################################################
# Spatial correlation analysis,   20170314 needs work
#########################################################

# lat <- 80
# lon <- 180
# offset <- 45   # as lat +/- 80 degrees
# marks <- X[,c(13,5:6,8,10:11)]  # Dbin, layers, terminate, edge, testype, time
# M <- ppp(X$Lon, X$Lat, c(-lon-offset,lon+offset), c(-lat,lat), marks=marks, unitname=c("degree","degrees"))
# str(M)

# K <- Kest(M,ratio=T)
# #K <- localKinhom(M,ratio=T)
# #KRD <- K
# # Kenv <- envelope(M, fun=Kest, nsim=10, global=T, verbose=F)
# # xx <- c(0, Kenv$r, rev(Kenv$r), 0)
# # yy <- c(c(0,Kenv$lo), rev(c(0,Kenv$hi)))
# #Ex <- "Layer"
# ilk <- "K"
# part <- mn
# (main <- paste("K Function for All Marked Craters"))
# (loc <- paste0(WD,"/Plots/", Ex, ilk, part, ".png"))
# quartz(w=5,h=5)
	# plot(K, main=main, lwd=2)
	# grid(col="gray")
# #	polygon(xx, yy, col="gray")
	# quartz.save(loc, type="png")

# pc <- pcf(K)
# Ex <- "Layer"
# ilk <- "pc"
# part <- mm
# (loc <- paste0(WD,"/Plots/", Ex, ilk, part, ".png"))
# quartz(w=5,h=5)
	# plot(pc, main=paste0("Pair Correlation Function for ", mm), lwd=2)
	# grid(col="gray")
	# quartz.save(loc, type="png")

# p <- K$r / (K$r + K$theo)
# p <- ifelse(is.na(p), 0, p)
# summary(p)


#########################################################
# select random locations
#########################################################

lat <- 80
lon <- 180
offset <- 45
marks <- X[,c(13,5:6,8,10:11)]  # Dbin, layers, terminate, edge, testype, time
M <- ppp(X$Lon, X$Lat, c(-lon-offset,lon+offset), c(-lat,lat), marks=marks, unitname=c("degree","degrees"))
str(M)

X$spl <- "Case"
X$spl <- ifelse( runif(nrow(X)) < 0.5, "Control", "Case" )  # controls ~ 15% of cases
X$spl <- factor(X$spl)
Xh <- X       # hold for restorative purposes
# X <- Xh
summary(X)

(N <- nrow(H))


#########################################################
     part <- "Merge"   # merge controls with cases
#########################################################

# Assign P to X lat/lon.
# The lon for P is 3.52 degrees pixel to pixel.
# Index: round to the nearest 5 degrees.
# lon / 5 -> round fraction to either 0 or 1 -> multiply by 5.

X <- Xh    # Orginal data set
X$LonI <- 5 * round(X$Lon / 5)  
P$LonI <- 5 * round(P$xcol / 5)  


# The lot for P is 1.25 degrees pixel to pixel.
# Index: round to the nearest 2 degress.
# lat / 2 -> round fraction to either 0 or 1 -> multiply by 2.

X$LatI <- 2 * round(X$Lat / 2)  
P$LatI <- 2 * round(P$yrow / 2)


# Probabilities in matrix form. Need long form.

a <- P$v
colnames(a) <- sub("-", "M", as.character(P$LonI))
a <- data.frame(LatI=P$LatI,a)
#str(a)

b <- melt(a, id.vars=1, value.name="p")
colnames(b)[2] <- "LonI"
b$LonI <- sub("X","",b$LonI)
b$LonI <- as.numeric(sub("M","-",b$LonI))
b$LonI <- round(b$LonI)
Y <- b
summary(Y)
rm(a,b)

# Merge P to X after forming merge key
X$key <- paste0( "(", as.character(X$LonI), ",", as.character(X$LatI), ")" )
X <- subset(X, select=-c(LonI,LatI,bin2,spl,BLANKET))
Y$key <- paste0( "(", as.character(Y$LonI), ",", as.character(Y$LatI), ")" )
Y <- subset(Y, select=-c(LonI,LatI))
nrow(X)
nrow(Y)
Z <- merge(X, Y, by="key", all=FALSE)
nrow(Z)
summary(Z)
head(Z,20)

W <- aggregate(p ~ CRATERID, data=Z, mean)
summary(W)
nrow(W)

V <- merge(X, W, by="CRATERID", all=FALSE)
summary(V)
nrow(V)
Xv <- X
X <- V

# Save only if new data desired for models as
# logit loads existing data below
path <- "~Jamie/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
outfile <- paste(WD, "TES4Modeling.RData", sep="/")
save(X, file=outfile, ascii=FALSE)


#########################################################
# Parking lot
#########################################################

# Probabilities

a <- P$v
colnames(a) <- sub("-", "M", as.character(P$LonI))
a <- data.frame(LatI=P$LatI,a)
#str(a)

b <- melt(a, id.vars=1, value.name="p")
colnames(b)[2] <- "LonI"
b$LonI <- sub("X","",b$LonI)
b$LonI <- as.numeric(sub("M","-",b$LonI))
b$LonI <- round(b$LonI)
Y <- b
summary(Y)
rm(a,b)

# Merge P to X after forming merge key
X$key <- paste0( "(", as.character(X$LonI), ",", as.character(X$LatI), ")" )
X <- subset(X, select=-c(LonI,LatI,bin2,spl,BLANKET))
Y$key <- paste0( "(", as.character(Y$LonI), ",", as.character(Y$LatI), ")" )
Y <- subset(Y, select=-c(LonI,LatI))
nrow(X)
nrow(Y)
Z <- merge(X, Y, by="key", all=FALSE)
nrow(Z)
summary(Z)
head(Z,20)

W <- aggregate(p ~ CRATERID, data=Z, mean)
summary(W)
nrow(W)

V <- merge(X, W, by="CRATERID", all=FALSE)
summary(V)
nrow(V)
Xv <- X
X <- V


#########################################################
# TI Analysis
#########################################################

 (Ex <- "MDAP")
(ver <- "TES")

path <- "~/Desktop/SwRI/DataR"
setwd(path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
load(infile)
summary(X)
names(X)[2:3] <- c("Lat","Lon")   # rename LATITUDE to Lat, LONGITUDUE to Lon
summary(X)
setwd(Path)
(WD <- getwd())
H <- X    # hold data
#X <- H    # restore data


# relevel DCI so not alphabetic

names(X)[ncol(X)] <- "DCIlevel"
summary(X)
X$DCIlevel <- factor(X$DCIlevel, levels=c("Low","Medium","High","N/A"))
table(X$DCIlevel)


# keep TI (TES) that make sense

X   <- X[X$TES>0,]
X$y <- log10(X$TES)

part <- "Box"
yl <- "logTI"
xl <- "DCI"
(loc <- paste0("Plots/", Ex, ver, part, xl, yl, ".png"))
(main=paste(yl,"vs.",xl))
gp <- ggplot(X, aes(DCIlevel,y=y)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle(main) + 
	xlab(xl) +
	ylab(yl) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)
gp


outfile <- paste(WD, "scan.RData", sep="/")
save(scan, file=outfile, ascii=FALSE)


#X <- H; H <- X
offset <- 90
Z <- X[X$lon > 180-offset,]
Z$lon <- Z$lon - 360
summary(Z)
Y <- Z
Z <- X[X$lon < -180+offset,]
Z$lon <- Z$lon + 360
summary(Z)
X <- rbind(X,Y,Z)
summary(X)
nrow(X)

lat <- 45
X$qm <- paste(as.character(round(X$bin2,3)), "by", as.character(round(X$bin1,3)))
Q <- ppp(X$lon, X$lat, marks=data.frame(X$qm), c(-180-offset,180+offset), c(-lat,lat), unitname=c("degree","degrees"))
#Q <- ppp(X$lon, X$lat, marks=data.frame(X$bin1,X$bin2), c(-180-offset,180+offset), c(-lat,lat), unitname=c("degree","degrees"))
#qm <- paste(as.character(round(Q$marks[,2],1)), "by", as.character(round(Q$marks[,1],3)))
#mks <- factor(Q$marks[,1]*Q$marks[,2])
#mks <- factor(qm)
#comb <- data.frame(FreqProp(mks,"Rim-to-Floor by Diameter"))
#q <- split(Q,f=mks)
#q <- split(Q,f=factor(Q$marks[,1]))
#rm(X,Q)

#yl <- "Rim2Floor"
os <- unique(Q$marks)
for (i in 1:length(os)) {#i<-1
  M <- Q[Q$marks==os[i]]
  mm <- as.character(os[i])
  
	Z <- density.ppp(M)
#  print(i)}
#	quartz(w=8,h=5)
	outfile <- paste(WD,"/Plots/", dir, "/Intensity", yl, srm, Ex, i, ".png", sep="")
	png(file=outfile, width = 600, height = 500, units = "px", pointsize = 11, bg = "white",  res = NA, type="cairo")  #, type="png"
#	outfile
  plot(Z, main=paste("Mean Density of", yl, mm, "Diameter",  sep=" "), xlab="Longitude", ylab="Latitude", frame.plot=F)
	rect(-180-offset, -lat, -180, lat, col="white", border=NA)
	rect(180, -lat, 180+offset, lat, col="white", border=NA)
	segments(0,-lat,0,lat,col="red",lwd=1,lty="dashed")
	segments(-180,0,180,0,col="red",lwd=1,lty="dashed")
#	quartz.save(paste("Plots/Intensity",srm, mm,".png",sep=""), type="png")
  dev.off()


#########################################################
# Diameter bins by 3, 6, 12 etc (doubling)
#########################################################

HIST <- hist      # save frequency-based bins
   y <- log10(X$DIAMETER)
  lb <- log10(c(3,6,12,24,48,96,192))
hist <- hist(y,breaks=lb,plot=F)
str(hist)

# X <- H
bin <- round(hist$breaks,3)
bin2 <- data.frame(exp(hist$breaks[2:length(hist$breaks)]),exp(hist$mids),hist$counts)
names(bin2) <- c("Upper (km)","MidPoint (km)","Frequency")
X$bin2 <- max(bin)
for (i in (length(bin)-1):1) {X$bin2 <- ifelse(y<=bin[i],bin[i],X$bin2)}
summary(X)
nrow(X)
(bins <- table(X$bin2))
(bin3 <- data.frame(Diameter=round(10^(as.numeric(names(bins))),2),Frequency=as.numeric(bins)))
mm <- rep(" ", nrow(bin3))
mm[1] <- paste("0 to", as.character(bin3$Diameter[1]), "km")
for (i in 2:nrow(bin3)) {
	mm[i] <- paste(as.character(bin3$Diameter[i-1]), "to", as.character(bin3$Diameter[i]), "km")
	}
(bin3$Diameter <- mm)	
  ilk <- "Diameter"
 part <- "BinsX2"
(loc <- paste0(WD,"/Tables/", Ex, ilk, part, ".tex"))
print(xtable(bin3,digits=0,caption=paste(Ex,"Diameter Bins",sep=" "), label="tab:bins"), caption.placement="top", include.rownames=F,file=loc)
 (binam <- as.character(round(10^(as.numeric(names(bins))),2)))
(laynam <- names(table(X$LAYERS)))

(ftab <- ftable(round(10^(X$bin2),2),X$LAYERS))
(ftab <- data.frame(expand.grid(rev(attr(ftab, "row.vars"))), unclass(ftab)))
(ftab[,1] <- as.character(ftab[,1]))
names(ftab) <- c("Dia(km)","DLE","MLE","SLE")
(ftab$Totals <- apply(ftab[,2:4],1,sum))
(Totals <- apply(ftab[,2:5],2,sum))
(ftab <- rbind(ftab,c("Totals",Totals)))
  ilk <- "Diameter"
 part <- "LayerFreqX2"
(loc <- paste0(WD,"/Tables/", Ex, ilk, part, ".tex"))
print(xtable(ftab, digits=0, align="rr|rrr|r", caption=paste0(Ex," by Diameter Bin Counts"), label="tab:binfreq"), type="latex", hline.after=c(-1,0,0,(nrow(ftab)/2),(nrow(ftab)-1),(nrow(ftab)-1),nrow(ftab)), caption.placement="top", include.rownames=F, file=loc)

X$Dbin <- factor(round(10^(X$bin2),2))   # for marks


#########################################################
#####     Bins by frequencies     #######################

bin <- round(hist$breaks,3)
bin2 <- data.frame(exp(hist$breaks[2:length(hist$breaks)]),exp(hist$mids),hist$counts)
names(bin2) <- c("Upper (km)","MidPoint (km)","Frequency")
X$bin2 <- max(bin)
for (i in (length(bin)-1):1) {X$bin2 <- ifelse(y<=bin[i],bin[i],X$bin2)}
summary(X)
nrow(X)
(bins <- table(X$bin2))
(bin3 <- data.frame(Diameter=round(exp(as.numeric(names(bins))),2),Frequency=as.numeric(bins)))
mm <- rep(" ", nrow(bin3))
mm[1] <- paste("0 to", as.character(bin3$Diameter[1]), "km")
for (i in 2:nrow(bin3)) {
	mm[i] <- paste(as.character(bin3$Diameter[i-1]), "to", as.character(bin3$Diameter[i]), "km")
	}
(bin3$Diameter <- mm)	
  ilk <- "Diameter"
 part <- "Bins"
(loc <- paste0(WD,"/Tables/", Ex, ilk, part, ".tex"))
print(xtable(bin3,digits=0,caption=paste(Ex,"Diameter Bins",sep=" "), label="tab:bins"), caption.placement="top", include.rownames=F,file=loc)
 (binam <- as.character(round(exp(as.numeric(names(bins))),2)))
(laynam <- names(table(X$Layer)))

(ftab <- ftable(round(exp(X$bin2),2),X$Layer))
(ftab <- data.frame(expand.grid(rev(attr(ftab, "row.vars"))), unclass(ftab)))
names(ftab) <- c("Dia(km)","DLE","MLE","RD","SLE")
  ilk <- "Diameter"
 part <- "LayerFreq"
(loc <- paste0(WD,"/Tables/", Ex, ilk, part, ".tex"))
print(xtable(ftab,digits=0,caption=paste(Ex,"Diameter Bins",sep=" "), label="tab:binfreq"), caption.placement="top", include.rownames=F,file=loc)


######################################################
