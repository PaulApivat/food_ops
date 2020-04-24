# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

library(tidyverse)

# load data
df <- read_csv("food_data.csv")

# exploratory
str(df)

# dealing with Timestamp data
# change chr --> POSIXlt
strptime(df$Timestamp, "%m/%d/%Y %H:%M:%OS")
df$Timestamp <- strptime(df$Timestamp, "%m/%d/%Y %H:%M:%OS")

# must change to POSIXct format before plotting
df$Timestamp <- as.POSIXct(df$Timestamp)

# delete empty rows because 50,000+ empty rows is a pain
# delete 233 -> 50,669
df <- df[-c(216:50669),]

# first plot success
ggplot(data = df, mapping = aes(x=Timestamp, y=df$`ยอดผลิต (kg)`))+geom_point()

# make a data dictionary

