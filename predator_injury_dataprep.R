# What percent of total catch is subsampled?

setwd("~/Research/Salmon Data/Results and code/predator")

# load data
library("rJava")
library("xlsxjars")
library("xlsx")
library("xlsx")
require(ggplot2)

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
                        catch$Chinook_total[i], catch$Steelhead_total[i], catch$Juv_Sockeye[i], catch$Juv_Chum[i], 
                        catch$Juv_chinook[i], catch$Juv_coho[i], catch$Juv_pink[i], catch$Juv_steelhead[i])
}


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
id.wrong <- subset(id, Proportion_sampled > 1) #16

# all ids for which subsampling was smaller than or equal to total catch
id <- subset(id, Proportion_sampled <= 1)

# choose only purse seine and excellent haul effectiveness
fish <- merged[ which(merged$Gear.x=='Purse_seine' & merged$Haul_Effectivness=='1_Excellent'), ]
write.csv(fish, file ="fish.csv", row.names=TRUE)

# change all empty values of predator injury to 0, Unknown predator wound to 1
fish$Injury <- as.character(fish$Injury)
fish$Injury[fish$Injury == ""] <- 0
fish$Injury[fish$Injury == "Unknown_predator_wound"] <- 1

# select only 0 and 1 from injuries
predator.injury <- subset(fish, Injury == "1" | Injury=="0")
predator.injury$Injury <- as.numeric(predator.injury$Injury)

# clean up covariates
fish.stom.clean1 <- subset(predator.injury, Length_TSFT_mm > 0)
final.fish <- fish.stom.clean1
final.fish <- fish.stom.clean1

# examine proportion of total number of each species/total number of fish in catch
unique_sampled_catches <- final.fish[!duplicated(final.fish$ID_code),]

unique_sampled_catches$sockeye_prop <- unique_sampled_catches$Sockeye_total/unique_sampled_catches$total
unique_sampled_catches$chum_prop <- unique_sampled_catches$Chum_total/unique_sampled_catches$total
unique_sampled_catches$coho_prop <- unique_sampled_catches$Coho_total/unique_sampled_catches$total
unique_sampled_catches$pink_prop <- unique_sampled_catches$Pink_total/unique_sampled_catches$total
unique_sampled_catches$steelhead_prop <- unique_sampled_catches$Steelhead_total/unique_sampled_catches$total
unique_sampled_catches$chinook_prop <- unique_sampled_catches$Chinook_total/unique_sampled_catches$total

# entropy calculations for data
catches <- unique_sampled_catches
k_1 <- vector(mode="numeric", length=1303)
k_2 <- vector(mode="numeric", length=1303)
k_3 <- vector(mode="numeric", length=1303)
k_4 <- vector(mode="numeric", length=1303)
k_5 <- vector(mode="numeric", length=1303)
k_6 <- vector(mode="numeric", length=1303)
ent.real <- vector(mode="numeric", length=1303)

for (i in 1:1303){
  k_1[i] <- catches$sockeye_prop[i]*log(catches$sockeye_prop[i])
  k_2[i] <- catches$pink_prop[i]*log(catches$pink_prop[i])
  k_3[i] <- catches$chum_prop[i]*log(catches$chum_prop[i])
  k_4[i] <- catches$chinook_prop[i]*log(catches$chinook_prop[i])
  k_5[i] <- catches$coho_prop[i]*log(catches$coho_prop[i])
  k_6[i] <- catches$steelhead_prop[i]*log(catches$steelhead_prop[i])
  if (catches$sockeye_prop[i] == 0){
    k_1[i] <- 0
  }
  if (catches$pink_prop[i] == 0){
    k_2[i] <- 0
  }
  if (catches$chum_prop[i] == 0){
    k_3[i] <- 0
  }
  if (catches$chinook_prop[i] == 0){
    k_4[i] <- 0
  }
  if (catches$coho_prop[i] == 0){
    k_5[i] <- 0
  }
  if (catches$steelhead_prop[i] == 0){
    k_6[i] <- 0
  }
  ent.real[i] <- -1*(k_1[i] +k_2[i] + k_3[i] + k_4[i] + k_5[i] + k_6[i])
}

# distribution of entropies
hist(ent.real)

# add entropy to main catches variable 
catches$ent <- ent.real

# add entropy to data (run entropy part below)
temp <- data.frame(catches$ID_code, catches$ent)
colnames(temp) <- c("ID_code", "ent")
final.fish <- merge(final.fish, temp, by="ID_code")
