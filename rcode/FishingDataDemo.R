########################
# Get Fishing Data
library(reshape2)


rawfish <- read.csv("fishing.csv")
head(rawfish)

#Get subset of columns and create unique id
fishint <- rawfish[-1,c(1:3, 5:8, 15:16)]
fishint$id <- paste(fishint$cruise, fishint$ship, fishint$haul)
fishint$latitude <- as.numeric(as.character(fishint$latitude))
fishint$longitude <- as.numeric(as.character(fishint$longitude))

#find rows that have salmon in the name and create a column with a 1
salmonrows <- as.data.frame(cbind(fishint[grep('Oncorhynchus', fishint$scientific_name), 'id'], 1), stringsAsFactors = FALSE)
names(salmonrows) <- c('id', 'hassalmon')

#merge salmon found with 1's
fishtype <- merge(fishint, salmonrows, by='id', all.x = TRUE)

#Set all non-salmon rows to zero
fishtype$hassalmon[is.na(fishtype$hassalmon)] <- 0

#cast so there is 1 row per id
wide <- dcast(fishtype, formula = id + latitude + longitude + stop_latitude +  stop_longitude ~ hassalmon)
names(wide)[6:7] <- c('none', 'salmon')

#find ID rows with salmon
wide$hassalmon <- wide$salmon > 0

# Get final subset for fish dataset
fish <- wide[,c(1:5,8)]

head(fish)

############################################
# Spatial data
library(marmap)
library(lattice)
library(plyr)

#Get physical boundaries of the data
min(as.numeric(as.character(fish$latitude)), na.rm = TRUE)
max(as.numeric(as.character(fish$latitude)), na.rm = TRUE)
min(as.numeric(as.character(fish$longitude)), na.rm = TRUE)
max(as.numeric(as.character(fish$longitude)), na.rm = TRUE)

#Get bathymetry model for this area with 1 minute resolution
fishingarea <- getNOAA.bathy(lon1=-117,lon2=-135,lat1=30,lat2=55, resolution=1) 

# Plot area of fishing, coloring by depth (west coast of U.S. / Canada)
colorRampPalette(c("purple","lightblue","cadetblue2","cadetblue1","white")) -> blues
plot(fishing, image=T, bpal=blues(100), deep=c(-6500,0), shallow=c(-50,0), step=c(500,0), 
     lwd=c(0.3,0.6), lty=c(1,1), col=c("black","black"), drawlabels=c(F,F))

#Function to caldulate average depth and total distance for a transect
tripinfo <- function(data, x.1, y.1, x.2, y.2){
  #browser()
  trip <- get.transect(data, x1=x.1$longitude, x2=x.2$stop_longitude, y1=y.1$latitude, y2=y.2$stop_latitude, locator = FALSE, distance = TRUE)
  depth <- mean(trip$depth)
  dist <- max(trip$dist.km)
  tripinfo <- as.data.frame(cbind(depth, dist))
  tripinfo
}

#Use the function to get the depth / distance for each trip
haulinfo <- adply(fish, 1, function(x) tripinfo(fishingarea, x[3], x[2], x[5], x[4]))

attach(haulinfo)

######################
# Logistic Regression
library("ResourceSelection")
library("pROC")

#Descriptives
# PLOTS OF INTEREST #
describe(haulinfo[,c('depth', 'dist')])
ggplot(fish, aes(dist)) + geom_histogram()
ggplot(fish, aes(depth)) + geom_histogram()
qplot(depth,hassalmon)
qplot(dist,hassalmon)


model = glm(hassalmon~depth + dist,family=binomial)
modelsum = summary(model)
modelsum

# Goodness of fit
hoslem.test(hassalmon,model$fitted.values)

# ROC curve
roc(hassalmon~model$fitted.values,plot=TRUE)


#################################################
# Visualizations
# Sample trip used
haulinfo[1,]

#Lat / lons are expansions along a linear line from the above trip in  order to
# provide more interesting graphs. 
trip <- get.transect(fishingarea, x1=-124.55, y1=42.85,x2=-125.5,y2=43.3, locator = FALSE, distance = TRUE)
points(trip$lon,trip$lat,type="l",col=col2alpha("blue", alpha=0.5),lwd=2,lty=1)
points(min(trip$lon),max(trip$lat),col="blue")
points(max(trip$lon),min(trip$lat),col="blue")
plotProfile(trip)

#3D graph
# changing the z value rotates the graph. 

out <- get.box(fishingarea,x1=-124.55, y1=42.85,x2=-125.5,y2=43.3, width=2,col=2)
wireframe(out, shade=T, zoom=1.1,
          aspect=c(1/2, 0.2), 
          screen = list(z = 120, x = -50),
          par.settings = list(axis.line = list(col = "transparent")),
          par.box = c(col = rgb(0,0,0,0.1)))
