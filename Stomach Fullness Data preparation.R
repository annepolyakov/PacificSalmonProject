### Data cleaning and preparation for analysis ###
### Analysis focuses on effects of stomach fullness on salmon group size from high seas salmong data ###

# set working directory
setwd("~/Research/Salmon Data/Results and code/stomach fullness")

# load data
library("xlsx")
library("xlsxjars")
library("rJava")

catch <- read.xlsx2("USA_FRI_Catch_1956-1991.xls", sheetIndex=2, header=TRUE)
specimen_1956 <- read.csv("USA_FRI_Specimen1956-1978.csv", header=TRUE)
specimen_1980 <- read.csv("USA_FRI_Specimen1980-1991.csv", header=TRUE)
specimen <- rbind(specimen_1956, specimen_1980)

# are all catch IDs unique? how many total unique catch IDs in catch file?
n_occur <- data.frame(table(catch$ID_code))
n_occur[n_occur$Freq > 1,]

# add new levels to ID codes
levels(catch$ID_code) <- c(levels(catch$ID_code), "1957FRIA018", "1957FRIA030", "1957FRIR052", "1957FRIR081", "1965FRIC030",
                           "1964FRIC064", "1965FRIX066")

# change duplicate errors, clean duplicate entries
catch$ID_code[201] <- "1957FRIA018"
catch$ID_code[213] <- "1957FRIA030"
catch$ID_code[436] <- "1957FRIR052"
catch$ID_code[465] <- "1957FRIR081"
catch$ID_code[2355] <- "1965FRIC030"
catch$ID_code[2125] <- "1964FRIC064"
catch$ID_code[2512] <- "1965FRIX066"

# what is distribution of catches across year?
mytable <- table(catch$ID_code, catch$Year)
m <- margin.table(mytable, 2)
barplot(m, main="Catch Distribution by Year", xlab="Years", ylab="Frequency")

#convert catch totals for each species to numeric values
catch$Sockeye_total <- as.numeric(as.character(catch$Sockeye_total))
catch$Chum_total <- as.numeric(as.character(catch$Chum_total))
catch$Pink_total <- as.numeric(as.character(catch$Pink_total))
catch$Coho_total <- as.numeric(as.character(catch$Coho_total))
catch$Chinook_total <- as.numeric(as.character(catch$Chinook_total))
catch$Steelhead_total <- as.numeric(as.character(catch$Steelhead_total))
catch$Juv_Sockeye <- as.numeric(as.character(catch$Juv_Sockeye))
catch$Juv_Chum <- as.numeric(as.character(catch$Juv_Chum))
catch$Juv_pink <- as.numeric(as.character(catch$Juv_pink))
catch$Juv_coho<- as.numeric(as.character(catch$Juv_coho))
catch$Juv_chinook <- as.numeric(as.character(catch$Juv_chinook))
catch$Juv_steelhead <- as.numeric(as.character(catch$Juv_steelhead))

# add column to data frame that counts up TOTAL/catch (not by species)
for (i in 1:4765){
  catch$total[i] <- sum(catch$Sockeye_total[i], catch$Chum_total[i], catch$Pink_total[i], catch$Coho_total[i],
                        catch$Chinook_total[i], catch$Steelhead_total[i])
}

catch <- catch[ which(catch$Gear=='Purse_seine' & catch$Haul_Effectivness=='1_Excellent'), ]
catch_sizes <- catch[,c(2,16:19)]
write.csv(catch_sizes, file="catch_file.csv")

# are all specimen data IDs listed in catch data IDs?
spec.unique <- unique(specimen$ID_code)
setdiff(spec.unique, catch$ID_code) #yes!

# what is distribution of catches that were subsampled?
temp <- specimen[!duplicated(specimen$ID_code),]
mytable1 <- table(temp$ID_code, temp$Year)
m1 <- margin.table(mytable1, 2)
barplot(m1, main="Subsampled Catch Distribution by Year", xlab="Years", ylab="Frequency", ylim=c(0,300))

### combine specimen and catch data sets
# remove catch ID data not subsampled
catch.extra <- setdiff(catch$ID_code, spec.unique)

subsample.catch <- catch[catch$ID_code %in% spec.unique,]
merged <- merge(specimen, subsample.catch, by="ID_code")

# what proportion of each catch was subsampled? if proportion larger than one, something is wrong...
id <- table(specimen$ID_code)
id <- as.data.frame(id)
total_catch <- merged[!duplicated(merged$ID_code),]
id$total <- total_catch$total
id$proportion <- id$Freq/id$total
colnames(id) <- c("ID", "Samples","Total Catch", "Proportion_sampled")
hist(id$`Proportion_sampled`, breaks=20, main="Proportion sampled")

## how many catches where number of fish subsampled greater than total fish in catch?
id.wrong <- subset(id, Proportion_sampled > 1)

# all ids for which subsampling was smaller than or equal to total catch
id <- subset(id, Proportion_sampled <= 1)

# choose only purse seine and excellent haul effectiveness
fish <- merged[ which(merged$Gear.x=='Purse_seine' & merged$Haul_Effectivness=='1_Excellent'), ]
write.csv(fish, file ="fish.csv", row.names=TRUE)

# add coordinates
fish$longitude <- fish$Long_deg.x + (fish$Long_min.x/60)
fish$latitude <- fish$Lat_deg.x + (fish$Lat_min.x/60)

# remove rows with no data for stomach fullness - not all specimens were examined, but not sure as to reason why. 
# Assuming examined specimens were randomly selected

# change all empty values of stomach volume to NA
fish$Stomach_volume <- as.character(fish$Stomach_volume)
fish$Stomach_volume[fish$Stomach_volume == ""] <- NA
fish$Stomach_volume <- as.factor(fish$Stomach_volume)
fish$Stomach_volume[fish$Stomach_volume=="NA"] <- NA

# pull out data where stomach wasn't sampled
no.exam.fish <- fish[is.na(fish$Stomach_volume), ]
factor(no.exam.fish$Stomach_volume)

# remove all entries with no stomach sampling by adding up only those cells  with values
fish.stom <- subset(fish,!(is.na(fish["Stomach_volume"])))
fish.stom1 <- fish[ which(fish$Stomach_volume=='Empty'), ]
fish.stom2 <- fish[ which(fish$Stomach_volume=='Trace'), ]
fish.stom3 <- fish[ which(fish$Stomach_volume=='Medium'), ]
fish.stom4 <- fish[ which(fish$Stomach_volume=='Full'), ]
fish.stom5 <- fish[ which(fish$Stomach_volume=='Distended'), ]
fish.stom <- rbind(fish.stom1, fish.stom2, fish.stom3, fish.stom4, fish.stom5)

# clean up covariates
#fish.stom.clean1 <- subset(fish.stom, Sex %in% c("Female", "Male"))
fish.stom.clean <- subset(fish.stom, Length_TSFT_mm > 0)
final.fish <- fish.stom.clean

# eliminate ids that have more fish subsampled than in total catch (but I think all these have already been 
# removed due to other filters above)
final.fish <- final.fish[!(final.fish$ID_code=="1956FRIA001" & final.fish$ID_code=="1956FRIA004" & 
                             final.fish$ID_code=="1956FRIA005" & final.fish$ID_code=="1956FRIA009" &
                             final.fish$ID_code=="1956FRIA011" & final.fish$ID_code=="1956FRIA015" & 
                             final.fish$ID_code=="1956FRIA017" & final.fish$ID_code=="1956FRIR002" &
                             final.fish$ID_code=="1956FRIR004" & final.fish$ID_code=="1956FRIR005" &
                             final.fish$ID_code=="1956FRIR013" & final.fish$ID_code=="1956FRIR015" &
                             final.fish$ID_code=="1956FRIR045" & final.fish$ID_code=="1956FRIR064" &
                             final.fish$ID_code=="1956FRIR094" & final.fish$ID_code=="1978FRIC027"),]

write.csv(final.fish, file ="finalfish.csv", row.names=TRUE)