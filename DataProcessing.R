library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
X2018_data_files_casualty <- read_csv("2018-data-files-casualty.csv")

# remove last column as it is empty as doesnt contain any relevant information
X2018_data_files_casualty$X16 <- NULL

# 15 Columns
ncol(X2018_data_files_casualty)

# 30591 rows
nrow(X2018_data_files_casualty)
X2018_data_files_casualty$`Casualty Severity`
by_TransMode <- X2018_data_files_casualty %>% group_by(`Mode of Travel`,`Casualty Severity`)

GroupSevMode <- by_TransMode %>% summarise(n=n())

GroupSevMode$`Mode of Travel` <- gsub("[1-9] ", "", GroupSevMode$`Mode of Travel`)
GroupSevMode$`Casualty Severity` <- gsub("[1-9] ", "", GroupSevMode$`Casualty Severity`)

GroupSevMode <- filter(GroupSevMode,`Mode of Travel`== "Pedal Cycle" | `Mode of Travel`== "Powered Wheeler" )

ggplot(GroupSevMode,aes(x = `Mode of Travel`,y = n)) + 
  geom_bar(aes(fill = `Casualty Severity`),stat = "identity",position = "dodge") + 
  scale_y_log10() + ggtitle("Casualty of London Accidents") +labs(y= "n", x = "Mode of Travel",
                                                                  subtitle = "y-axis in logscale")
ggplot(GroupSevMode,aes(x = `Mode of Travel`,y = n)) + 
  geom_bar(aes(fill = `Casualty Severity`),stat = "identity",position = "dodge") + ggtitle("Casualty of London Accidents") +labs(y= "n", x = "Mode of Travel")

write.table(GroupSevMode, sep = ",")

NumPowered2Wheelers <- 110000 * 365
NumPushBikerJourneys <- 721000 * 365

GroupSevMode$perMillion = 0

GroupSevMode <- GroupSevMode %>% mutate(is.cycle = `Mode of Travel` == "Pedal Cycle",
                                        perMillion = ifelse(is.cycle, n/NumPushBikerJourneys*1000000, perMillion))
GroupSevMode <- GroupSevMode %>% mutate(is.wheeler = `Mode of Travel` == "Powered Wheeler",
                                        perMillion = ifelse(is.wheeler, n/NumPowered2Wheelers*1000000, perMillion))

GroupSevMode$is.cycle <- NULL
GroupSevMode$is.wheeler <- NULL

ggplot(GroupSevMode,aes(x = `Mode of Travel`,y = perMillion))+
  geom_bar(aes(fill = `Casualty Severity`),stat = "identity",position = "dodge") + ggtitle("Casualty of London Accidents") +labs(y= "Incidents per 1 Million Journeys", x = "Mode of Travel")

ggplot(GroupSevMode,aes(x = `Mode of Travel`,y = perMillion))+scale_y_sqrt()+
  geom_bar(aes(fill = `Casualty Severity`),stat = "identity",position = "dodge") + ggtitle("Casualty of London Accidents") +
  labs(y= "Incidents per 1 Million Journeys", x = "Mode of Travel",subtitle = "y-axis in sqrt scale")
