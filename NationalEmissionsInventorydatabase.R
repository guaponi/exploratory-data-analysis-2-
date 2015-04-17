#National Emissions Inventory database, http://www.epa.gov/ttn/chief/eiinformation.html
#For each year and for each type of PM source, the NEI records how many tons of PM2.5 
#were emitted from that source over the course of the entire year.
#Includes data from 1999, 2002, 2005, and 2008
library(dplyr)
library(lubridate)
library(ggplot2)

if(!file.exists("NEI_data.zip")) {
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  dest <- "NEI_data.zip"
  download.file(url = url, 
                dest= dest)
  unzip(dest)
}

  
#summarySCC_PM25.rds
NEI <- readRDS("summarySCC_PM25.rds")
#Source Classification Code Table 
SCC <- readRDS("Source_Classification_Code.rds")

NEI <- tbl_df(NEI)
SCC <- tbl_df(SCC)

glimpse(NEI) # Source: local data frame [6,497,651 x 6]
glimpse(SCC) # Source: local data frame [11,717 x 15]
# names(SCC)
# [1] "SCC"                 "Data.Category"       "Short.Name"          "EI.Sector"           "Option.Group"       
# [6] "Option.Set"          "SCC.Level.One"       "SCC.Level.Two"       "SCC.Level.Three"     "SCC.Level.Four"     
# [11] "Map.To"              "Last.Inventory.Year" "Created_Date"        "Revised_Date"        "Usage.Notes"        
# 

# confirm years 1999, 2002, 2005, and 2008
distinct(NEI, year)

NEI %>%
  is.na %>%
  sum # 0 missing values

# fips: A five-digit number (represented as a string) indicating the U.S. county
# SCC: The name of the source as indicated by a digit string (see source code classification table)
# Pollutant: A string indicating the pollutant
# Emissions: Amount of PM2.5 emitted, in tons
# type: The type of source (point, non-point, on-road, or non-road)
# year: The year of emissions recorded

# SCC is an id in both tables and thus links data from both tables
# Short.Name 

# > nrow(distinct(select(NEI, fips)))
# [1] 3263
# > nrow(distinct(select(NEI, SCC)))
# [1] 5386
# > nrow(distinct(select(NEI, Pollutant)))
# [1] 1
# > nrow(distinct(select(NEI, type)))
# [1] 4



# Q1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, 
# make a plot showing the total PM2.5 emission from all sources for each of the years 
# 1999, 2002, 2005, and 2008.

summary(NEI)
# The  data is highly skewed with alot of zero values as well as extreme values
# Without any domain knowledge the questions asked are not possible to really answer
# 

total_pm_emissions_per_year <- with(NEI, tapply(Emissions, factor(year), sum))

# check number of records/year (increasing)
with(NEI, tapply(Emissions, factor(year), length))
with(NEI, table(factor(year))) # same as above
# 1999    2002    2005    2008 
# 1108469 1698677 1713850 1976655 

# number of sources varies beween years
with(NEI, tapply(SCC, factor(year), n_distinct))
# 1999 2002 2005 2008 
# 4007 4373 4277 3668 

# PM25-PRI is the only pollutant in the dataset
with(NEI, tapply(Pollutant, factor(year), n_distinct))
# 1999 2002 2005 2008 
# 1    1    1    1 
distinct(NEI, Pollutant)

# as.data.frame(total_pm_emissions_per_year)
total_pm_emissions_per_year <- as.data.frame(total_pm_emissions_per_year)
total_pm_emissions_per_year <- tbl_df(total_pm_emissions_per_year)
total_pm_emissions_per_year <- total_pm_emissions_per_year %>%
  mutate(year = row.names(total_pm_emissions_per_year)) %>%
  select(year, total_pm_emissions = total_pm_emissions_per_year)

# total_pm_emissions_per_year %>%
#   mutate(year = as.Date(as.character(year)))




names(total_pm_emissions_per_year) <- c("year", "total_pm_emissions")

df <- total_pm_emissions_per_year
df <- mutate(df, year = factor(year))
#plot(total_pm_emissions_per_year)
op <- par(no.readonly = TRUE)
#mar c(bottom, left, top, right)
# op$mar
# [1] 5.1 4.1 4.1 2.1
par(mar = c(5.1, 6, 4.1, 2.1))
par(ps = 12, cex = 0.9, cex.sub = 0.7, cex.main = 1)

par(mgp=c(axis.title.position = 2, axis.label.position = 1, axis.line.position = 0.2))

plot(df, ylab = "", yaxt="n", 
     main = "" , 
     sub = "Source: National Emissions Inventory( Environmental Protection Agency) ")

title("Total PM2.5 Emissions/Year", line = 0.4)
#yrange <-round(range(df$total_pm_emissions)/1000)*1000
#yseq <- seq(yrange[1], yrange[2], by = (yrange[2] - yrange[1])/3)
yseq <- seq(2000000, 7500000, by = 1000000)

# Y-label ...
par(mgp=c(axis.title.position = 5, axis.label.position = 1, axis.line.position = 0.2))
mtext("tons of PM2.5 ", side=2, line=4)
# Insert y-axis
axis(2, at=yseq, labels=format(yseq, scientific=FALSE), cex.axis=.8, las=2)



# box()
# mtext("Top axis", side=2, line=3)

par(op)

#plot(log(Emissions + 1) ~ year, NEI)
#boxplot(Emissions ~ year, NEI)
# boxplot(Emissions ~ year, NEI, ylim = c(0, 100000))
# boxplot(Emissions ~ year, NEI, ylim = c(0, 10000))


# NEI <- readRDS(
#   unz("NEI_data.zip", 
#       "summarySCC_PM25.rds"))
# 
# SCC <- readRDS(
#   unz("NEI_data.zip", 
#       "Source_Classification_Code.rds"))

# Q1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.

#plot(total_pm_emissions_per_year)


# Q2
# Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system 
# to make a plot answering this question.



Baltimore <- filter(NEI, fips=="24510")

# check number of records/year (increasing)
with(Baltimore, tapply(Emissions, factor(year), length))
with(Baltimore, table(factor(year))) # same as above
# 1999 2002 2005 2008 
# 320  535  542  699 

# number of sources varies beween years
with(Baltimore, tapply(SCC, factor(year), n_distinct))
# 1999 2002 2005 2008 
# 320  535  542  324 



total_pm_emissions_per_year_in_Baltimore <- with(Baltimore, tapply(Emissions, factor(year), sum))
total_pm_emissions_per_year_in_Baltimore <- as.data.frame(total_pm_emissions_per_year_in_Baltimore)
total_pm_emissions_per_year_in_Baltimore <- tbl_df(total_pm_emissions_per_year_in_Baltimore)
total_pm_emissions_per_year_in_Baltimore <- total_pm_emissions_per_year_in_Baltimore %>%
  mutate(year = row.names(total_pm_emissions_per_year_in_Baltimore)) %>%
  select(year, total_pm_emissions = total_pm_emissions_per_year_in_Baltimore)

# total_pm_emissions_per_year %>%
#   mutate(year = as.Date(as.character(year)))

names(total_pm_emissions_per_year_in_Baltimore) <- c("year", "total_pm_emissions")

df_b <- total_pm_emissions_per_year_in_Baltimore
df_b <- mutate(df, year = factor(year))


par(mar = c(5.1, 6, 4.1, 2.1))
par(ps = 12, cex = 0.9, cex.sub = 0.7, cex.main = 1)
par(mgp=c(axis.title.position = 2, axis.label.position = 1, axis.line.position = 0.2))
plot(df_b, ylab = "", yaxt="n", 
     main = "" , 
     sub = "Source: National Emissions Inventory( Environmental Protection Agency)")

title("Baltimore City \n Total PM2.5 Emissions/Year ", line = 0.4)
#yrange <-round(range(df$total_pm_emissions)/1000)*1000
#yseq <- seq(yrange[1], yrange[2], by = (yrange[2] - yrange[1])/3)
yseq <- seq(1750, 3500, by = 250)

# Y-label ...
par(mgp=c(axis.title.position = 5, axis.label.position = 1, axis.line.position = 0.2))
mtext("tons of PM2.5 ", side=2, line=4)
# Insert y-axis
axis(2, at=yseq, labels=format(yseq, scientific=FALSE), cex.axis=.8, las=2)

# 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases in emissions from 1999–2008 
# for Baltimore City? Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.
par(op)
Baltimore$EmissionsLog <- log(Baltimore$Emissions + 1)
Baltimore_grouped <- group_by(Baltimore, year, type)
# Baltimore_grouped$Emissions %>%
#   sum()
# 
# test <- with(Baltimore_grouped, tapply(Emissions,  sum))
# 
# total_pm_emissions_per_year_in_Baltimore <- with(Baltimore, tapply(Emissions, factor(year), sum))
# with(Baltimore, tapply(Emissions, factor(year, type), sum))
# sum(filter(Baltimore, year == "1999", type == "NON-ROAD")$Emissions)
# [1] 522.94
# xtabs(Emissions~year + type,data=Baltimore_grouped)
# type
# year     NON-ROAD   NONPOINT    ON-ROAD      POINT
# 1999  522.94000 2107.62500  346.82000  296.79500
# 2002  240.84692 1509.50000  134.30882  569.26000
# 2005  248.93369 1509.50000  130.43038 1202.49000
# 2008   55.82356 1373.20731   88.27546  344.97518
# > sum(df3_tbl$Freq)
# [1] 10681.73
# > sum(df_b$total_pm_emissions)
# [1] 10681.73
# > 
df3 <- xtabs(Emissions~year + type,data=Baltimore_grouped)
df3_tbl <- tbl_df(as.data.frame(df3))
# alt.
df3_tbl <- Baltimore_grouped %>%
  summarise(Emissions = sum(Emissions))

p <- ggplot(df3_tbl, aes(x = year, y= Emissions, col = type)) 
p <- p + geom_line()
p <- p + ylab("tons of PM2.5") 
xlabel <- "Year"
sub = "Source: National Emissions Inventory( Environmental Protection Agency)"

#p <- p + xlab(bquote(atop("Year", atop(italic(sub), ""))))
p<- p + ggtitle("Baltimore City \n Total PM2.5 Emissions/Year and type")
p <- p + xlab(bquote(atop(.(xlabel), atop(italic(.(sub)), "")))) 
p

sub = "Source: National Emissions Inventory( Environmental Protection Agency)")


# 
# boxplot(EmissionsLog ~ year, Baltimore,outline = FALSE)
# # <- with(Baltimore,points(jitter(as.numeric(year),factor = 1),EmissionsLog, col="red"))
# points(jitter(Baltimore$year, factor = 2),Baltimore$EmissionsLog, col="red")



# 4
# Across the United States, how have emissions 
# from coal combustion-related sources changed from 1999–2008?
ind_combust <- grep(pattern = "comb", x = SCC[,3][[1]], ignore.case = TRUE)
combust_scc <- SCC[ind_combust,]
ind_combust_coal <- grep(pattern = "coal", x = combust_scc[,3][[1]], ignore.case = TRUE)
combust_coal_scc <- combust_scc[ind_combust_coal,]

ind_combust <- grep(pattern = "combust", x = SCC[,3][[1]], ignore.case = TRUE)
combust_scc <- SCC[ind_combust,]
ind_combust_coal <- grep(pattern = "combust", x = combust_scc[,3][[1]], ignore.case = TRUE)
combust_coal_scc <- combust_scc[ind_combust_coal,1]

df_combust_coal <- merge(combust_coal_scc, NEI, by="SCC")
df_combust_coal_tbl <- tbl_df(df_combust_coal)

mytable <- with(df_combust_coal_tbl, tapply(Emissions, factor(year), sum))

p <- ggplot(df_combust_coal_tbl, aes(x = year, y = Emissions, col = type)) + geom_point()
p
# sccIndex$SCC <- as.numeric(as.character(combust_coal_scc[,"SCC"]))
# 
# length(distinct(as.numeric(sccIndex$SCC)))
# p <- ggplot(combust_coal_scc, aes(x = year, y = Emissions, col = type)) + geom_point()
# combust_coal_scc



# 5
#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
ind <- grep(pattern = "motor vehicle", x = SCC[,3][[1]], ignore.case = TRUE)

# Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, 
# California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?


