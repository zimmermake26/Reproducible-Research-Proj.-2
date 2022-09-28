data <- read.csv("repdata-data-StormData.csv.bz2", header = TRUE, sep=",")
colnames(data)
selection <- c('EVTYPE', 'FATALITIES', 'INJURIES', 'PROPDMG', 'PROPDMGEXP', 'CROPDMG', 'CROPDMGEXP')
data <- data[, selection]
summary(data)
data <- as.data.table(data)
data <- data[(EVTYPE != "?" & (INJURIES > 0 | FATALITIES > 0 | PROPDMG > 0 | CROPDMG > 0)), 
             c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
cols <- c("PROPDMGEXP", "CROPDMGEXP")
data[,  (cols) := c(lapply(.SD, toupper)), .SDcols = cols]

PROPDMGKey <-  c("\"\"" = 10^0, 
                 "-" = 10^0, "+" = 10^0, "0" = 10^0, "1" = 10^1, "2" = 10^2, "3" = 10^3,
                 "4" = 10^4, "5" = 10^5, "6" = 10^6, "7" = 10^7, "8" = 10^8, "9" = 10^9, 
                 "H" = 10^2, "K" = 10^3, "M" = 10^6, "B" = 10^9)
CROPDMGKey <-  c("\"\"" = 10^0, "?" = 10^0, "0" = 10^0, "K" = 10^3, "M" = 10^6, "B" = 10^9)

data[, PROPDMGEXP := PROPDMGKey[as.character(data[,PROPDMGEXP])]]
data[is.na(PROPDMGEXP), PROPDMGEXP := 10^0 ]

data[, CROPDMGEXP := CROPDMGKey[as.character(data[,CROPDMGEXP])] ]
data[is.na(CROPDMGEXP), CROPDMGEXP := 10^0 ]
data <- data[, .(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, PROPCOST = PROPDMG * PROPDMGEXP, CROPDMG, CROPDMGEXP, CROPCOST = CROPDMG * CROPDMGEXP)]
Health_Impact <- data[, .(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES), TOTAL_HEALTH_IMPACTS = sum(FATALITIES) + sum(INJURIES)), by = .(EVTYPE)]

Health_Impact <- Health_Impact[order(-TOTAL_HEALTH_IMPACTS), ]

Health_Impact <- Health_Impact[1:10, ]

head(Health_Impact, 10)
Eco_Impact <- data[, .(PROPCOST = sum(PROPCOST), CROPCOST = sum(CROPCOST), TOTAL_ECO_IMPACTS = sum(PROPCOST) + sum(CROPCOST)), by = .(EVTYPE)]

Eco_Impact <- Eco_Impact[order(-TOTAL_ECO_IMPACTS), ]

Eco_Impact <- Eco_Impact[1:10, ]

head(Eco_Impact, 10)
Health_Consequences <- melt(Health_Impact, id.vars = "EVTYPE", variable.name = "Fatalities_or_Injuries")

ggplot(Health_Consequences, aes(x = reorder(EVTYPE, -value), y = value)) + 
  geom_bar(stat = "identity", aes(fill = Fatalities_or_Injuries), position = "dodge") + 
  ylab("Total Injuries/Fatalities") + 
  xlab("Event Type") + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  ggtitle("Top 10 US Weather Events that are Most Harmful to Population") + 
  theme(plot.title = element_text(hjust = 0.5))
Eco_Consequences <- melt(Eco_Impact, id.vars = "EVTYPE", variable.name = "Damage_Type")

ggplot(Eco_Consequences, aes(x = reorder(EVTYPE, -value), y = value/1e9)) + 
  geom_bar(stat = "identity", aes(fill = Damage_Type), position = "dodge") + 
  ylab("Cost/Damage (in billion USD)") + 
  xlab("Event Type") + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  ggtitle("Top 10 US Weather Events that have the Greatest Economic consequences") + 
  theme(plot.title = element_text(hjust = 0.5))
