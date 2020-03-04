#youtube url https://www.youtube.com/watch?v=82s8KdZt5v8
install.packages("tidyr")
library(tidyr)
install.packages("purr")
library(purr)
install.packages("rvest")
library(rvest)
install.packages("ggpubr")
library(ggpubr)
install.packages("fitdistrplus")
library(fitdistrplus)
library(ggplot2)



#Scrape in Data Set 1

url_base <- ("https://www.trulia.com/TX/San_Antonio/%d_p")
map_df(2:35,function(i){
  page <- read_html(sprintf(url_base,i))
  data.frame(House_Price = html_text(html_nodes(page, ".kNBbhi")),
             InteriorSpecs = html_text(html_nodes(page,".fkwbDs")))
}) -> Trulia_sample

#inspect data set 1

str(Trulia)

#Scrape in Data Set 2

url_base <- ("https://www.trulia.com/TX/San_Antonio/%d_p")
map_df(2:3,function(i){
  page <- read_html(sprintf(url_base,i))
  data.frame(Address = html_text(html_nodes(page, ".lcNNgu")))
}) -> Trulia_Address

#inspect Data set 2

str(Trulia_Address)

#separate general variable into three specific variables

Trulia<-Trulia$InteriorSpecs
Trulia<- data.frame(Trulia)
Trulia %>%  separate(Trulia,c("Bed","Bath","SquareFeet"),sep="([ad])") -> Trulia
str(Trulia)

#Smaller Sample Size

Trulia_sample<-Trulia_sample$InteriorSpecs
Trulia_sample<-data.frame(Trulia_sample)
Trulia_sample %>% separate(Trulia_sample,c("Bed","Bath","SquareFeet"),sep="([ad])") -> Trulia_sample
#clean Bed and bath and square feet of characters

Bed<-Trulia$Bed
Bath<-Trulia$Bath
SquareFeet<-Trulia$SquareFeet
Bed<-gsub("[^0-9]", "", Bed)
head(Bed)
Bath<-gsub("[a-z]","",Bath)
head(Bath)
SquareFeet<-gsub("[^0-9]","",SquareFeet)
head(SquareFeet)

#SmallerSampleSize

Bed_Sample<-Trulia_sample$Bed
Bath_Sample<-Trulia_sample$Bath
SquareFeet_sample<-Trulia_sample$SquareFeet
Bed_Sample<-gsub("[^0-9]","",Bed_Sample)
head(Bed_Sample)
Bath_Sample<-gsub("[a-z]","",Bath_Sample)
head(Bath_Sample)
SquareFeet_sample<-gsub("[^0-9]","",SquareFeet_sample)
head(SquareFeet_sample)



#Bring in data set 1 again

map_df(2:3,function(i){
  page <- read_html(sprintf(url_base,i))
  data.frame(House_Price = html_text(html_nodes(page, ".kNBbhi")),
             InteriorSpecs = html_text(html_nodes(page,".fkwbDs")))
}) -> Trulia_price_sample

#Clean Price Variable

price<- str_replace(Trulia_price$House_Price,"Contact For Estimate",NA_character_)
head(price)
price<-gsub("[$]","",price)
head(price)
price<- gsub("[[:punct:]]","",price)
head(price)

#Clean Price for Smaller Sample

price_sample <- str_replace(Trulia_price_sample$House_Price,"Contact For Estimate",NA_character_)
head(price_sample)
price_sample<-gsub("[[:punct:]]","",price_sample)
head(price_sample)
price_sample<-gsub("[[:punct:]]","",price_sample)
head(price_sample)



#Remove NAs and missing data 

price_trim<-price[-c(105,107,155,186,198,245,284,294,417,508,570,618,679,704,710,718,722,727,743,772,784,800,827,847,856,882,905,913,946,950,1017)]
Bed_trim<-Bed[-c(105,107,155,186,198,245,284,294,417,508,570,618,679,704,710,718,722,727,743,772,784,800,827,847,856,882,905,913,946,950,1017)]
Bath_trim<-Bath[-c(105,107,155,186,198,245,284,294,417,508,570,618,679,704,710,718,722,727,743,772,784,800,827,847,856,882,905,913,946,950,1017)]
SquareFeet_trim<-SquareFeet[-c(105,107,155,186,198,245,284,294,417,508,570,618,679,704,710,718,722,727,743,772,784,800,827,847,856,882,905,913,946,950,1017)]

#combine both datavectors

Trulia_main<-cbind(price_trim,SquareFeet_trim,Bed_trim,Bath_trim)
head(Trulia_main)
Trulia_main<-as.data.frame(Trulia_main)
str(Trulia_main)

#combind both data vectors for smaller sample

Trulia_main_sample <- cbind(price_sample,Bed_Sample,Bath_Sample,SquareFeet_sample)
head(Trulia_main_sample)
Trulia_main_sample<-as.data.frame(Trulia_main_sample)
str(Trulia_main_sample)


#combine both data vectors practice

Trulia_Main_s<-cbind(price,SquareFeet,Bed,Bath)
head(Trulia_Main_s)
Trulia_Main_s<-as.data.frame(Trulia_Main_s)
str(Trulia_Main_s)


#Plot some data points
#Inspect and plot Price Variable

Trulia_main_trim <- cbind(price_trim,Bath_trim,Bed_trim,SquareFeet_trim)
Trulia_main_trim <- as.data.frame(Trulia_main_trim)
str(Trulia_main_trim)

BoxPlot_Beds<-(1:length(Trulia_Main_s$Bed))
par(mfrow=c(1,1))
plot(Trulia_Main_s$Bed,col="blue",xlab="Number of Beds",ylab="Frequency")
ggplot(Trulia_main_trim,aes(Bed_trim,Bath_trim)) +
  geom_boxplot(alpha=.25) +
  xlim(0,10) +
  ylim(0,10) +
  geom_jitter(alph=0.5,color="tomato")

ggplot(Trulia_main_trim,aes(price_trim,Bath_trim)) +
  geom_boxplot(alpha=.25) +
  ylim(0,10) +
  geom_jitter(alph=0.5,color="tomato")

ggplot(Trulia_main_trim,aes(SquareFeet_trim,Bath_trim)) +
  geom_boxplot(alpha=.25) +
  ylim(0,10) +
  geom_jitter(alph=0.5,color="tomato")

ggplot(Trulia_main_trim,aes(SquareFeet_trim,price_trim)) +
  geom_boxplot(alpha=.25) +
  geom_jitter(alph=0.5,color="tomato")

BoxPlot_Baths<-(1:length(Trulia_main$Bath_trim))
par(mfrow=c(1,1))
plot(Trulia_main$Bath_trim,x1,col="red",xlab="Number of Baths",ylab="Frequency")
length(x1)

#Convert SquareFeet data class factor to numeric

SquareFeet_trim_2 <- as.numeric(as.character(Trulia_main_sample$SquareFeet_sample))
hist(SquareFeet,col="green",breaks=seq(0,20000,100))
class(SquareFeet)

#Convert Price data class factor to numeric

price_trim_2 <- as.numeric(as.character(Trulia_main_sample$price_sample))
hist(price,col="Pink",breaks=seq(0,5000000,50000))
class(Price)
length(Trulia_main$price_trim)

Bed_trim_2 <- as.numeric(as.character(Trulia_main_sample$Bed_Sample))
Bath_trim_2<-as.numeric(as.character(Trulia_main_sample$Bath_Sample))


#convert all variables to numeric smaller sample size

price_sample <- as.numeric(as.character(Trulia_main_sample$price_sample))
Bed_Sample<- as.numeric(as.character(Trulia_main_sample$Bed_Sample))
Bath_Sample<-as.numeric(as.character(Trulia_main_sample$Bath_Sample))
SquareFeet_sample<-as.numeric(as.character(Trulia_main_sample$SquareFeet_sample))


hist(bed,col="green",breaks=seq(0,100,1))
range(bed)
#Remove all NA's

price<- price[!is.na(price)]
SquareFeet<-SquareFeet[!is.na(SquareFeet)]
plot(SquareFeet,price)
x1<-(1:length(Trulia_Main$SquareFeet))
par(mfrow=c(1,1))
plot(Trulia_Main$SquareFeet,x1,col="purple")
length(x1)


#fit a distribution

plotdist(price,histo=TRUE,demp=TRUE,lwd=2,col="green")
descdist(price,boot=1000)
fw<-fitdist(price,"weibull")
fln<-fitdist(price,"lnorm")
par(mfrow=c(2,2))
plot.legend <-c("Weibull","lnorm")
denscomp(list(fw,fln),legendtext=plot.legend)
qqcomp(list(fw,fln),legendtext=plot.legend)
cdfcomp(list(fw,fln),legendtext=plot.legend)
ppcomp(list(fw,fln),legendtext=plot.legend)
summary(fw)
summary(fln)



#Fit model
plotdist(Price_New,histo=TRUE,demp=TRUE)
descdist(Price_New,boot=1000)
fw<-fitdist(Price_New,"weibull")
fln<-fitdist(Price_New,"lnorm")
par(mfrow=c(2,2))
plot.legend <-c("Weibull","lnorm")
denscomp(list(fw,fln),legendtext=plot.legend)
qqcomp(list(fw,fln),legendtext=plot.legend)
cdfcomp(list(fw,fln),legendtext=plot.legend)
ppcomp(list(fw,fln),legendtext=plot.legend)


price_s <- as.numeric(as.character(Trulia_Main_s$price))
hist(price_s,col="Pink",breaks=seq(0,5000000,50000))
class(price_s)

squarefeet_s <- as.numeric(as.character(Trulia_Main_s$SquareFeet))
hist(squarefeet_s,col="blue",breaks=seq(0,20000,100))
class(squarefeet_s)

linMod <- lm(price_s~squarefeet_s)
linMod_trafo<-trafo_lm(linMod)
diagnostics(linMod_trafo)
plot(linMod)


price <- as.numeric(as.character(price))
Bed <- as.numeric(as.character(Bed))
Bath <- as.numeric(as.character(Bath))
SquareFeet <- as.numeric(as.character(SquareFeet))

SquareFeet<- SquareFeet[!is.na(SquareFeet)]

price <- price[-c(31,43)]
Bed <- Bed[-c(31,43)]
Bath <- Bath[-c(31,43)]




#log transform predictor variable price
price_log_sample<- log1p(price_sample)

lm_log <- lm(price_log_sample~(SquareFeet_sample))
summary(lm_log)
plot(lm_log)





