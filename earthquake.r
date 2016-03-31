## Analysis of Earthquakes in the Himalayan Region for last 100 Years

library(googleVis) # load googleVis library

# Load USGS data from file
quake <- read.csv("C:\\Users\\DELL\\Documents\\usgs1916.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# concate latitude and longitude
quake$loc <- paste(quake$latitude, quake$longitude, sep=":")

# generate unique id for each row of the data frame
quake$id = c(1:nrow(quake))


# generate GeoChart 
geo <- gvisGeoChart(quake, "loc", "depth", "mag", 
                    options = list(displayMode = "Markers", 
                                   width = 1000, height = 600,
                                   region = "034",
                                   colorAxis = "{colors:['purple', 'red', 'orange', 'grey']}", 
                                   backgroundColor = "lightblue"), 
                    chartid = "quake")



plot(geo) # Plot Geochart into web 


# transform the time into usable form
conv.time <- function(vector){
  split1 <- strsplit(paste(vector),"T")
  split2 <- strsplit(split1[[1]][2],"Z")
  fin <- paste0(split1[[1]][1],split2[[1]][1])
  paste(as.POSIXlt(fin,formate="%Y-%m-%d%H:%M:%OS3"))
}

# create a new variable with the time stamp
time <- sapply(quake$time, FUN = conv.time)


quake$datetime <- as.POSIXlt(time) # for annotation
# generate annotated timeline
annot <- gvisAnnotatedTimeLine(quake, datevar="datetime",
                            numvar="mag",
                            options=list(scaleType='maximized',
                                         width="900px", height="600px")
)

plot(annot) # plot the annot chart



## ------------------------------------------------------------------------------------------

# Some Statistics fro Post April 25, 2015 Gorkha Earthquake

# select only April 25, 2015 Post Gorkha Earthquakes
dat <- quake[quake$time >= "2015-04-25",]

# create a new data frame
postGorkha <- data.frame(dat$datetime, dat$latitude, dat$longitude, dat$depth, dat$mag, row.names = NULL)

# create column names
vars <- c("time", "latitude", "longitude", "depth", "magnitude")
colnames(postGorkha) <- vars 

# Ploting the chart for Post Gorkha Earthquake 
require(graphics)
pairs(postGorkha, main = "Earthquake in SA, Post Gorkha EQ, N = 322", cex.main = 1.2, pch = ".")
