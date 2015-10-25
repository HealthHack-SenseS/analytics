setwd("~/Dropbox/HealthHackSydney2015/SenseS")
library(dplyr)

data.math <- read.csv('ShimmerData_Elisa_math.csv', header = FALSE, skip = 4)
head(data.math)

GHdata.math <- data.math %>% select(V2, V29, V30, V31) %>%
                             rename(Time = V2, GSRraw = V29, GSRcal = V30, HRppg = V31) %>%
                             mutate(Sec = Time/(1000), Min = Time/(1000*60))

head(GHdata.math)
summary(GHdata.math)

# unfiltered HR and GSR
#plot(GHdata.math$Min, GHdata.math$GSRraw / max(GHdata.math$GSRraw), col="red", type = 'l')
plot(GHdata.math$Min, GHdata.math$GSRcal / max(GHdata.math$GSRcal), col="green", type = 'l')
lines(GHdata.math$Min, GHdata.math$HRppg / max(GHdata.math$HRppg), col="blue")
grid(nx = NULL, ny = nx, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)


library(stats)
library(signal)

#FILTERING
myfilter = butter(2, 0.002, type = 'low', plane='z')      
GHdata.math$filt2 = signal:::filter(myfilter, GHdata.math$HRppg)
GHdata.math$filtGSR = signal:::filter(myfilter, GHdata.math$GSRcal)
plot(GHdata.math$Min, GHdata.math$GSRcal / max(GHdata.math$GSRcal), col="green", type = 'l')
lines(GHdata.math$Min, GHdata.math$HRppg / max(GHdata.math$HRppg), col="blue")
lines(GHdata.math$Min, GHdata.math$filt2 / max(GHdata.math$filt2), col="black")
lines(GHdata.math$Min, GHdata.math$filtGSR / max(GHdata.math$filtGSR), col="red")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)


#CALC INDEP FACT
GHdata.math.factor <- GHdata.math %>% mutate(DerivHRppg = c(0,diff(GHdata.math$filt2)/diff(GHdata.math$Min))) %>%
               mutate(IntegrHRppg = cumsum(GHdata.math$filt2) * c(0,diff(GHdata.math$Min))) %>%
               mutate(DerivGSR = c(0,diff(GHdata.math$filtGSR)/diff(GHdata.math$Min))) %>%
               mutate(IntegrGSR = cumsum(GHdata.math$filtGSR) * c(0,diff(GHdata.math$Min)))


GHdata.math.2 <- GHdata.math.factor
GHdata.math.2$Stress <- (1 == floor((GHdata.math$Min - 0.1) %% 2))

plot(GHdata.math.2$Min, GHdata.math$filt2 / max(GHdata.math$filt2), col="blue", type = 'l')
#lines(GHdata.math$Min, GHdata.math$HRppg / max(GHdata.math$HRppg), col="blue")
lines(GHdata.math.2$Min, GHdata.math.2$Stress, col="red")
lines(GHdata.math$Min, GHdata.math$filtGSR / max(GHdata.math$filtGSR), col="green")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)



HRlogit <- glm(Stress ~ DerivHRppg + filt2 + IntegrHRppg, data = GHdata.math.2, family = "binomial")
HRGSRlogit <- glm(Stress ~ DerivHRppg + filt2 + IntegrHRppg + DerivGSR + filtGSR + IntegrGSR, data = GHdata.math.2[1500:nrow(GHdata.math.2)-1500,], family = "binomial")
HRGSRlogitNOINT <- glm(Stress ~ DerivHRppg + filt2 + DerivGSR + filtGSR , data = GHdata.math.2[1500:nrow(GHdata.math.2)-1500,], family = "binomial")


summary(HRlogit)
summary(HRGSRlogit)
summary(HRGSRlogitNOINT)

