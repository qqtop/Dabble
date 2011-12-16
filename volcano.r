library(maps)
library(XML)

# Earthquakes and Volcanoes on one map

############ Earthquakes last 7 days

# get the earthquake data from the USGS
eq <- read.csv("http://earthquake.usgs.gov/earthquakes/catalogs/eqs7day-M2.5.txt", sep = ",", header = TRUE)
# size the earthquake symbol areas according to magnitude
radius <- 10^sqrt(eq$Mag)
# set up if output needs to be saved
#unlink("worldeq.png")
#png("worldeq.png", width = 1920, height = 1200)

# plot the world map, earthquakes, and time stamp
map(database = "world",col = "#999999", bg = "white")
symbols(eq$Lon, eq$Lat, bg = "#99ccff", fg = "#3467cd", lwd = 1, circles = radius, inches = 0.175, add = TRUE)

#############################################
## Volcanos CURRENT ERUPTIONS

# Get the current eruptions from Smithosonian/USGS
doc <- xmlInternalTreeParse("http://www.volcano.si.edu/news/WeeklyVolcanoRSS.xml")
 
# Parse xml and isolate the eruption locations
x <- xpathApply(doc, "//georss:point")
len <- length(x)
df <- xmlToDataFrame(x)

# Convert the eruption list to numeric lat lon
lat <- c()
lon <- c()
for (i in 1:len){
  char <- as.character(df[i, 1])
  num <- as.numeric(unlist(strsplit(char, " ")))
  lat[[i]] <- num[[1]]
  lon[[i]] <- num[[2]]
  }
vlc <- data.frame(lat, lon)

lx=length(vlc$lat)

vradius=10^sqrt(abs(tail(vlc$lon,lx))/100)
for (j in 1:10) vradius[j] = 12

symbols(tail(vlc$lon,lx),tail(vlc$lat,lx), lwd = 1, bg = "tomato", fg = "tomato",circles = vradius, inches = 0.175, add = TRUE)


mylabel=paste("Since : ", Sys.Date()-7)
leg.txt <- c("Earthquakes","Volcanoes")
alabel=Sys.Date()
text(100, -80, labels = mylabel, col = "black")
colors = c("#99ccff", "tomato")
title(paste("Earthquakes and Volcano Eruptions ",alabel))
legend("bottomleft", leg.txt, fill=colors)


#dev.off()

################# THATS IT ########################################
#quit("no")

