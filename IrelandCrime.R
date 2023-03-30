#Q1
#Reading IrelandCrime csv file to a dataframe
Ireland_crime <- read.csv("IrelandCrime.csv", na = "")
#displaying the structure 
str(Ireland_crime)
names(Ireland_crime)
#displaying the first 10 rows of the data
head(Ireland_crime, 10)
#re-code any missing data with NA
Ireland_crime[Ireland_crime == " "] <- NA
Ireland_crime

#Q2
#Changing the column names to the required names
names(Ireland_crime)[1] <- "Region"
names(Ireland_crime)[2] <- "Division"
names(Ireland_crime)[3] <- "OffenceCode"
names(Ireland_crime)[4] <- "Offence"
names(Ireland_crime)[5] <- "OffenceType"

#structure of the dataframe
str(Ireland_crime)


#Q3
#Replacing the offencetype description with required description
Ireland_crime$OffenceType[Ireland_crime$OffenceType == 'ATTEMPTS/THREATS TO MURDER/ASSAULTS/ HARASSMENTS AND RELATED OFFENCES'] <- "Murder/assault/harassment"
Ireland_crime$OffenceType[Ireland_crime$OffenceType == 'DANGEROUS OR NEGLIGENT ACTS'] <- "Dangerous acts"
Ireland_crime$OffenceType[Ireland_crime$OffenceType == 'KIDNAPPING AND RELATED OFFENCES'] <- "Kidnapping"
Ireland_crime$OffenceType[Ireland_crime$OffenceType == 'ROBBERY/EXTORTION AND HIJACKING OFFENCES'] <- "Robbery"
Ireland_crime$OffenceType[Ireland_crime$OffenceType == 'THEFT AND RELATED OFFENCES'] <- "Theft"
Ireland_crime$OffenceType[Ireland_crime$OffenceType == 'FRAUD/DECEPTION AND RELATED OFFENCES'] <- "Fraud"
Ireland_crime$OffenceType[Ireland_crime$OffenceType == 'PUBLIC ORDER AND OTHER SOCIAL CODE OFFENCES'] <- "Public order"
Ireland_crime$OffenceType[Ireland_crime$OffenceType == 'OFFENCES AGAINST GOVERNMENT/ JUSTICE PROCEDURES AND ORGANISATION OF CRIME'] <- "Offences against government"

#Number of rows with “Murder/assault/harassment” description #140
sum(Ireland_crime$OffenceType == "Murder/assault/harassment")

#Number of rows with “Offences against government” #112
sum(Ireland_crime$OffenceType == "Offences against government")

#Q4
#Show the summary of each crime
CrimeSummary <- summary(Ireland_crime$OffenceType)
CrimeSummary
#Results shows 1624 , which means the total crimes that took place all together is 1624 counts


#Q5
# USe VIM package to show missing values
install.packages("VIM")
library(VIM)

missing_values <- aggr(Ireland_crime, prop = FALSE, numbers = TRUE)
summary(missing_values)

#show where data is available
complete_data <- complete.cases(Ireland_crime)
complete_data

#shows rows with no missing value
complete_data <- Ireland_crime[complete.cases(Ireland_crime),]
complete_data

#lists rows with missing values
Ireland_crime[!complete.cases(Ireland_crime),]

#plot
matrixplot(Ireland_crime)

#no of rows after removing missing values
new_data <- na.omit(Ireland_crime)
summary(new_data)
#Summary function gives the detailed summary of the dataframe , where after omiting the missing values the Max count 
#shows the count of whatever data remains after removing missing values

#Q6
#using subset function to get the max number of crime occured in which region
attach(Ireland_crime)
new_data <- subset(Ireland_crime, select = c(Region))
new_data
summary(new_data)



#Q7
#subseting new data for donegal crimes

Donegal_Crime <- subset(Ireland_crime, Division == 'DONEGAL')
Donegal_Crime

#summary of records in Donegal crime
summary(Donegal_Crime)

#plot
attach(Donegal_Crime)
plot(Donegal_Crime)
plot( Offence, type = "o", col = "blue")
title(main = "Donegal Crimes", col.main = "blue", font.main = 4)

graph_range <- range(0, Offence, OffenceType)

#x axis
axis(1, at = 1:1, lab = c(Offence))

#y axis
axis(2, las = 1, at = 5 * 0:graph_range[2])

dettach