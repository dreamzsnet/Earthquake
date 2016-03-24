# Earthquake
Analysis of Earthquakes in the Himalayan Region for last 100 Years

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


quake$datetime <- as.Date(time)  # for motion
# generate motion chart
motion <- gvisMotionChart(quake, idvar = "id", 
                          timevar = "datetime", 
                          options = list(width = 900, height = 600) 
)

plot(motion) # plot motion chart

## ------------------------------------------------------------------------------------------

# Plain Graphs

# Regression between Magnitude and historical date time
head(quake$datetime)

# select only April 25, 2015 Post Gorkha Earthquakes
dat <- quake[quake$time >= "2015-04-25",]

# create a new data frame
postGorkha <- data.frame(dat$datetime, dat$latitude, dat$longitude, dat$depth, dat$mag, row.names = NULL)

# create column names
vars <- c("time", "latitude", "longitude", "depth", "magnitude")
colnames(postGorkha) <- vars 
str(postGorkha)


# Histogram
hist(postGorkha$magnitude, main = "Histogram of Magnitude; Post Gorkha EQ", xlab = "Magnitude")
hist(postGorkha$depth, main = "Histogram of Depth; Post Gorkha EQ", xlab = "Depth")

# Create a scatterplot
plot(postGorkha$magnitude, postGorkha$depth, main = "Depth given Magnitude", xlab = "Magnitude", ylab = "Depth")

# Add line of best fit
abline(lm(postGorkha$depth~postGorkha$magnitude))

# Create a correlation matrix 
cor(postGorkha[,vars[-1]])

# other useful functions
confint(fit, level = 0.95) # Confidence Intervals for model parameters
hist(fitted(fit)) # predicted values
hist(residuals(fit)) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters


require(graphics)
pairs(postGorkha, main = "Earthquake in SA, Post Gorkha EQ, N = 322", cex.main = 1.2, pch = ".")
