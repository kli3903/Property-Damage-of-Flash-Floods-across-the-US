library(ggplot2)
library(data.table)
library(dplyr)
library(tidyverse)
library(fields)
library(RColorBrewer)
library(classInt)
library(MASS)
library(ggplot2)
library(akima)
library(gstat)
library(geoR)
library(sp)

details <- read.csv("C:\\Users\\pilow\\Downloads\\StormEvents_details-ftp_v1.0_d2022_c20221116.csv.gz")
fatal <- read.csv("C:\\Users\\pilow\\Downloads\\StormEvents_fatalities-ftp_v1.0_d2022_c20221116.csv.gz")
location <- read.csv("C:\\Users\\pilow\\Downloads\\StormEvents_locations-ftp_v1.0_d2022_c20221116.csv.gz")
head(details)
head(fatal)
head(location)

details1 <- details
details1$MONTH_NAME <- factor(details1$MONTH_NAME, levels = c("January","February","March","April","May","June","July","August"))

ggplot(details1, aes(x = factor(MONTH_NAME)))+
         geom_bar()
unique(details$MONTH_NAME)       
dim(details)
dim(fatal)
dim(location)
names(location)
names(details)
names(fatal)
unique(details$EPISODE_ID)

details$BEGIN_LAT
location$LATITUDE
unique(location$LOCATION)

class(details$DAMAGE_PROPERTY)
unique(details$DAMAGE_PROPERTY)

details <- data.frame(details)

which(details=="M", arr.ind = TRUE)
K <- details[details$DAMAGE_PROPERTY %like% "K",25] #find all rows with K costs
K
newK <- gsub("K","",K)
newK
dim(details[details$EVENT_ID == 999903])
dim(details)
unique(details$STATE)
unique(details$EVENT_TYPE)

all <- merge(details, location, by = "EVENT_ID")
for (x in 1:61){
  print(x)
  print(sum(is.na(all[,x])))
}
names(all)[28]
names(all)[31]
names(all)[33]
names(all)[34]
names(all)[37]

dim(all)
names(all)
names(location)
head(location)
all2 <- select(all, -c("LAT2", "LON2", "BEGIN_YEARMONTH", "BEGIN_DAY","BEGIN_TIME",
               "END_YEARMONTH","END_DAY","END_TIME","YEAR","MONTH_NAME",
               "CZ_TYPE","CZ_FIPS","CZ_NAME","WFO","BEGIN_DATE_TIME",
               "CZ_TIMEZONE","END_DATE_TIME","SOURCE","CATEGORY",
               "TOR_OTHER_WFO","TOR_OTHER_CZ_STATE","TOR_OTHER_CZ_FIPS",
               "TOR_OTHER_CZ_NAME","EPISODE_NARRATIVE","EVENT_NARRATIVE",
               "DATA_SOURCE","YEARMONTH","EPISODE_ID.y","LOCATION_INDEX"))
dim(all2)
names(all2)
unique(all2$INJURIES_DIRECT)


gsub("K","",all2[all2$DAMAGE_PROPERTY %like% "K",10])
K <- as.numeric(gsub("K","",all2[all2$DAMAGE_PROPERTY %like% "K",10]))*1000
M <- as.numeric(gsub("M","",all2[all2$DAMAGE_PROPERTY %like% "M",10]))*1000000

all2$DAMAGE_PROPERTY
Kall <- all2 %>% 
  filter(str_detect(DAMAGE_PROPERTY, pattern = "K"))
Mall <- all2 %>% 
  filter(str_detect(DAMAGE_PROPERTY, pattern = "M"))
Kall$DAMAGE_PROPERTY <- K
Mall$DAMAGE_PROPERTY <- M
all3 <- rbind(Kall, Mall)
range(all3$DAMAGE_PROPERTY)

damage <- subset(all3, DAMAGE_PROPERTY != 0)
dim(damage)
table(all3$EVENT_TYPE)
#spatial
names(damage)
remove <- c("PUERTO RICO","GULF OF MEXICO","AMERICAN SAMOA",
            "DISTRICT OF COLUMBIA","ATLANTIC SOUTH",
            "ATLANTIC NORTH")
dam <- damage[damage$STATE != "PUERTO RICO" 
       & damage$STATE != "GULF OF MEXICO"
       & damage$STATE != "AMERICAN SAMOA"
       & damage$STATE != "DISTRICT OF COLUMBIA"
       & damage$STATE != "ATLANTIC SOUTH"
       & damage$STATE != "ATLANTIC NORTH"
       & damage$STATE != "HAWAII", ]
dam <- subset(dam, EVENT_TYPE == "Flash Flood" & DAMAGE_PROPERTY<100000)
for (x in 1:32){
  print(x)
  print(sum(is.na(dam[,x])))
}
names(dam)[12]
names(dam)[16]
names(dam)[17]
table(dam$EVENT_TYPE)
unique(dam$STATE)

lat <- dam$LATITUDE
lon <- dam$LONGITUDE
prop <- dam$DAMAGE_PROPERTY
hist(dam$DAMAGE_PROPERTY)

plotvar <- prop
nclr <- 5
plotclr <- brewer.pal(nclr,"YlOrRd")
class <- classIntervals(plotvar,nclr,style="fixed",fixedBreaks=seq(min(plotvar),max(plotvar),
                                                                   length=nclr+1))
class$brks
colcode <- findColours(class,plotclr)
length.x <- 25
length.y <- 25
x.grid <- seq(min(lon),max(lon),length=length.x)
y.grid <- seq(min(lat),max(lat),length=length.y)
z.grid <- matrix(NA,nrow=length.x,ncol=length.y)
z.lim <- range(plotvar)
png("Property Damage plot")
par(mar=c(3,3,1,1))
image.plot(x.grid,y.grid,z.grid,xlab="Latitude",ylab="Longitude",zlim=z.lim,col=plotclr,breaks=class$brks,legend.lab="$",main="Property Damage Costs")
points(lon,lat,col=colcode,pch=20)
dev.off()
mod.prop <- lm(DAMAGE_PROPERTY~FLOOD_CAUSE+LATITUDE+LONGITUDE,data = dam)
summary(mod.prop)
names(dam)
trend <- mod.prop$fitted.values
res.prop <- dam$DAMAGE_PROPERTY - trend

surf.est.surface.prop <- interp(lat,lon,trend,duplicate = 'mean')

plotvar <- surf.est.surface.prop$z[!is.na(surf.est.surface.prop$z)]
nclr <- 9
plotclr <- brewer.pal(nclr,"YlOrRd")
class <- classIntervals(plotvar,nclr,style="fixed",fixedBreaks=seq(min(plotvar),max(plotvar),length=nclr+1))

colcode <- findColours(class,plotclr)
class$brks

png("spatial trend.png")
image.plot(surf.est.surface.prop$x,surf.est.surface.prop$y,
           surf.est.surface.prop$z,col=plotclr,breaks=class$brks,
           zlim=c(4,8),xlab="Latitude",ylab="Longitude",main="Estimated Spatial Trend")
dev.off()
prop.data <- data.frame(cbind(lat,lon,res.prop))
emp.variog.prop <- variogram(res.prop~1,locations=~lat+lon,prop.data)
par(mar=c(2,2,2,2))
plot(emp.variog.prop)

dir.variog.prop <- variogram(res.prop~1,locations=~lat+lon,prop.data,alpha=c(0,45,90,135))
plot(dir.variog.prop)

dir.variog.prop.E <- dir.variog.prop[dir.variog.prop$dir.hor==0,]
dir.variog.prop.NE <- dir.variog.prop[dir.variog.prop$dir.hor==45,]
dir.variog.prop.N <- dir.variog.prop[dir.variog.prop$dir.hor==90,]
dir.variog.prop.NW <- dir.variog.prop[dir.variog.prop$dir.hor==135,]

png("Directional Variogram.png")
plot(dir.variog.prop.E$dist,dir.variog.prop.E$gamma,col="blue",type="o",pch=20,xlab="Distance",ylab="Semivariance")
lines(dir.variog.prop.NE$dist,dir.variog.prop.NE$gamma,col="orange",type="o",pch=20)
lines(dir.variog.prop.N$dist,dir.variog.prop.N$gamma,col="green",type="o",pch=20)
lines(dir.variog.prop.NW$dist,dir.variog.prop.NW$gamma,col="magenta",type="o",pch=20)
legend("topleft",col=c("blue","orange","green","magenta"),lwd=rep(1,5),lty=rep(1,5),
       c("E-W","NE-SW","N-S","NW-SE"),cex = 1)
dev.off()
sph.variog.prop <- fit.variogram(emp.variog.prop,vgm(psill=3e+08,"Exp",range=20,nugget=1e+08),fit.method=2)
par(mar=c(1,1,5,1))
png("Semi variogram.png")
print(plot(emp.variog.prop,sph.variog.prop))
dev.off()
