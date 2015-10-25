#predict 

data.stroop <- read.csv('ShimmerData_Elisa_stroop_1.csv', header = FALSE, skip = 4)
head(data.stroop)

GHdata.stroop <- data.stroop %>% select(V2, V29, V30, V31) %>%
    rename(Time = V2, GSRraw = V29, GSRcal = V30, HRppg = V31) %>%
    mutate(Sec = Time/(1000), Min = Time/(1000*60))

#plot(GHdata.stroop$Min, GHdata.stroop$GSRraw / max(GHdata.stroop$GSRraw), col="red", type = 'l')
plot(GHdata.stroop$Min, GHdata.stroop$GSRcal / max(GHdata.stroop$GSRcal), col="green", , type = 'l')
lines(GHdata.stroop$Min, GHdata.stroop$HRppg / max(GHdata.stroop$HRppg), col="blue")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

myfilter = butter(2, 0.002, type = 'low', plane='z')      
GHdata.stroop$filt2 = signal:::filter(myfilter, GHdata.stroop$HRppg)
GHdata.stroop$filtGSR = signal:::filter(myfilter, GHdata.stroop$GSRcal)
plot(GHdata.stroop$Min, GHdata.stroop$GSRcal / max(GHdata.stroop$GSRcal), col="green", type = 'l')
lines(GHdata.stroop$Min, GHdata.stroop$HRppg / max(GHdata.stroop$HRppg), col="blue")
lines(GHdata.stroop$Min, GHdata.stroop$filt2 / max(GHdata.stroop$filt2), col="black")
lines(GHdata.stroop$Min, GHdata.stroop$filtGSR / max(GHdata.stroop$filtGSR), col="red")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

GHdata.stroop <- GHdata.stroop %>% mutate(DerivHRppg = c(0,diff(GHdata.stroop$filt2)/diff(GHdata.stroop$Min))) %>%
    mutate(IntegrHRppg = cumsum(GHdata.stroop$filt2) * c(0,diff(GHdata.stroop$Min)))

GHGSRdata.stroop <- GHdata.stroop %>% mutate(DerivHRppg = c(0,diff(GHdata.stroop$filt2)/diff(GHdata.stroop$Min))) %>%
    mutate(IntegrHRppg = cumsum(GHdata.stroop$filt2) * c(0,diff(GHdata.stroop$Min))) %>%
    mutate(DerivGSR = c(0,diff(GHdata.stroop$filtGSR)/diff(GHdata.stroop$Min))) %>%
    mutate(IntegrGSR = cumsum(GHdata.stroop$filtGSR) * c(0,diff(GHdata.stroop$Min)))

GHdata.stroop.2 <- GHGSRdata.stroop
GHdata.stroop.2$Stress <- (1 == floor((GHdata.stroop.2$Min - 0.1) %% 2))
GHdata.stroop.2$Stress[GHdata.stroop.2$Min > 3] <- TRUE

plot(GHdata.stroop.2$Min, GHdata.stroop.2$filt2 / max(GHdata.stroop.2$filt2), col="blue", type = 'l')
lines(GHdata.stroop.2$Min, GHdata.stroop.2$Stress, col="red")
lines(GHdata.stroop.2$Min, GHdata.stroop.2$filtGSR / max(GHdata.stroop.2$filtGSR), col="green")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

#only HR predicition
GHdata.stroop.2$Response <- predict(HRlogit, GHdata.stroop.2, type = 'response')
GHdata.stroop.2$StressPred <- GHdata.stroop.2$Response > 0.5
head(GHdata.stroop.2)
plot(GHdata.stroop.2$Min, GHdata.stroop.2$filt2 / max(GHdata.stroop.2$filt2), col="blue", type = 'l')
lines(GHdata.stroop.2$Min, GHdata.stroop.2$Stress, col="red")
lines(GHdata.stroop.2$Min, GHdata.stroop.2$Response, col="black")
lines(GHdata.stroop.2$Min, GHdata.stroop.2$StressPred, col="gray")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

pred_accuracy <- sum(GHdata.stroop.2$StressPred == GHdata.stroop.2$Stress) / nrow(GHdata.stroop.2)

#HR + GSR predicition
#GHdata.stroop.2$Response <- predict(HRlogit, GHdata.stroop.2, type = 'response')
GHdata.stroop.2$ResponseHRGSR <- predict(HRGSRlogit, GHdata.stroop.2, type = 'response')
GHdata.stroop.2$StressPredHRGSR <- GHdata.stroop.2$ResponseHRGSR > 0.5
head(GHdata.stroop.2)

plot(GHdata.stroop.2$Min, GHdata.stroop.2$filt2 / max(GHdata.stroop.2$filt2), col="blue", type = 'l')
lines(GHdata.stroop.2$Min, GHdata.stroop.2$Stress, col="red")
lines(GHdata.stroop.2$Min, GHdata.stroop.2$filtGSR / max(GHdata.stroop.2$filtGSR), col="green")
lines(GHdata.stroop.2$Min, GHdata.stroop.2$ResponseHRGSR, col="yellow")
lines(GHdata.stroop.2$Min, GHdata.stroop.2$StressPredHRGSR, col="orange")

pred_accuracyHRGSR <- sum(GHdata.stroop.2$StressPredHRGSR == GHdata.stroop.2$Stress) / nrow(GHdata.stroop.2)


