#Post processing CE-QUAL-W2 output files
#by Olivia Sparrow
#Civil Engineering Master's Student, University of Minnesota-Twin Cities
#Water Resources Engineer, Emmons & Olivier Resources

#Description of code: This code reads in the results from multiple CE-QUAL-W2 model scenarios and
#generates graphs and summary tables of the results.


# Section 1: To Do List --------------------------------------------------

#Read in multiple output files for the same model (all segments)
#Read in multiple models
#Read in observed water temperature time series
#associatiate segment number or river station to the results

#Compare observed versus predicted at 3 monitoring locations

#*****Calculate monthly mean, number of exceedance hours each month, and number of days with at least 1 hour exceeding temp
#Plot graphs of the main outputs

#Calculate the difference between the targeted shade scenario and existing conditions




# Section 2: Load Libraries -----------------------------------------------

library(utils)
library(ggplot2)
library(data.table)
library(lubridate)  # Date conversion
library(xts)  # Time series manipulation
library(scales)


# Section 3: Read Output Files ---------------------------------------------
setwd("H:/2017 BCWD Riparian Shading Study/CEQUAL Model/BCPR_2012_RiparianShadStudy/R12_Shade Mitigation IV")  # Set working directory
df <- read.fwf("tsr_1_seg2.opt", skip = 12, strip.white = TRUE, widths = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10),
               col.names = c("JDAY", "DLT", "ELWS", "Temperature", "U", "Q", "SRON", "EXT", "DEPTH", "WIDTH", 
                             "SHADE"))  # Read in segment output file



# Section 4: Manipulate Data -----------------------------------------------
origin <- ymd_hms("2011-12-31 00:00:00")  # Origin date from which we will convert julian to calendar dates
df$Date <- as.POSIXct(origin + df$JDAY * 3600 * 24)  # Calculate the day and time of each result (JDAY in decimal days)
df = subset(df, select = -c(JDAY))  # Delete julian day from dataframe
df <- df[c(ncol(df), 1:(ncol(df)-1))]  # Re-order so that the time column is first
x <- xts(df[, -1], order.by = df[, 1])  # Create an XTS version of the dataframe
dAvg <- apply.daily(x, function(x) apply(x, 2, mean))  # Calculate daily mean stream temperature
dMax <- apply.daily(x, function(x) apply(x, 2, max))  # Calculate daily max stream temperature
dMin <- apply.daily(x, function(x) apply(x, 2, min))  # Calculate daily min stream temperature
#rename columns
names(dAvg) <- sub("$", ".avg", names(dAvg))
names(dMax) <- sub("$", ".max", names(dMax))
names(dMin) <- sub("$", ".min", names(dMin))
#combine all daily results
Day <- data.frame(datetime = index(dAvg), dAvg[, c(1:6)], dMax[, c(1:6)], dMin[, c(1:6)], row.names = NULL)

# Section 5: Plot Data ----------------------------------------------------

ggplot(x, aes(x=Index, y=Temperature)) + geom_point() + 
  ylab("Stream Temperature (deg C)") +
  xlab("Date")  # plot raw data
ggplot(dAvg, aes(x=Index, y=Temperature.avg)) + geom_point(aes(color = 'red')) + 
  ylab("Daily Mean Stream Temperature (deg C)") +
  xlab("Date") # plot daily mean data
ggplot(Day, aes(x = datetime)) + 
  geom_line(aes(y = Temperature.avg, color = "Mean"), size = 1, linetype = 1, alpha = 1) + 
  geom_ribbon(aes(ymax = Temperature.max, ymin = Temperature.min, fill = "Stream Max/Min"), 
              colour = "darkorange", linetype = 3, alpha = 0.4) + 
  ylab(expression(paste("Daily Stream Temperature ( ", degree, "C)", sep = ""))) + xlab("") + 
  #scale_x_datetime(limits = c(as.POSIXct("2012-05-10 12:00"), as.POSIXct("2012-05-30 12:00")), labels = date_format("%b-%d"), 
  #                 breaks = date_breaks("7 days")) + 
  #scale_y_continuous(breaks = seq(0, 42, by = 4), limits = c(0, 42), labels = seq(0, 42, by = 4)) + 
  theme_bw() + 
  scale_color_manual("Daily Mean", breaks = c("Mean"), values = c(`Mean` = "darkblue")) + 
  scale_fill_manual("Range", breaks = c("Stream Max/Min"), values = c(`Stream Max/Min` = "blue")) + 
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.key = element_blank(), 
        legend.background = element_rect(fill = "white", colour = "gray30")) + 
  guides(fill = guide_legend(keywidth = 0.9, keyheight = 1))


