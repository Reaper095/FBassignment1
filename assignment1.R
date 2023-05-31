library(stringr)
library(dplyr)
library(tidyverse)
library(rvest)
#Question a.
#reading html.
html <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
name <- html %>% html_elements(".stick a") %>% html_text()
CMP <-html %>% html_elements(".stick+ td") %>% html_text()
pc<-html %>% html_elements("#stock-list-table td:nth-child(4)") %>% html_text()
MarketCap <- html %>% html_elements("#stock-list-table td:nth-child(5)") %>% html_text()
WH52 <- html %>% html_elements("#stock-list-table td:nth-child(6)") %>% html_text()
WL52 <- html %>% html_elements(("#stock-list-table td:nth-child(7)")) %>% html_text()
roe <- html %>% html_elements("#stock-list-table td:nth-child(8)") %>% html_text()
pv <- html %>% html_elements("td:nth-child(9)") %>% html_text()
pbv <- html %>% html_elements("td:nth-child(10)") %>% html_text()
ev <- html %>% html_elements("td:nth-child(11)") %>% html_text()
SG5Y <- html %>% html_elements("td:nth-child(12)") %>% html_text()
PG5Y <- html %>% html_elements("td:nth-child(13)") %>% html_text()
#making dataframe.
dataset <- data.frame(name,CMP,pc,MarketCap,WH52,WL52,roe,pv,pbv,ev,SG5Y,PG5Y)
#changing name of the columns.
names(dataset) <- c("Company Name (M.Cap)", "CMP", "Price Change", "Market Cap (Cr)", "52 Week High", "52 Week Low", "ROE", "P/E", "P/BV", "EV/EBITDA", "5YSales Gr(%)", "5YProfit Gr(%)")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Question b.
urls_codes <- html %>% html_elements(".company-ellipses a") %>% html_attr("title")
for(i in 1:50){
  urls_codes[i] <- paste("https://www.moneyworks4me.com/indianstocks", urls_codes[i], "/company-info", sep = '')
}
# Dataset 1.
stock_html <- read_html(urls_codes[5])
temp3 <- html_table(stock_html)
tab1<- as.data.frame(temp3[1])
tab2<- as.data.frame(temp3[3])
names(tab1)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
names(tab2)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
tab1<- tab1[-c(1,2,3,4,5),-c(12,13,14)]
tab2<- tab2[-c(1),-c(12,13)]
table1 <- rbind(tab1,tab2)
rownames(table1) <- 1:nrow(table1)
# Dataset 2.
stock_html <- read_html(urls_codes[6])
temp3 <- html_table(stock_html)
tab1<- as.data.frame(temp3[1])
tab2<- as.data.frame(temp3[3])
names(tab1)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
names(tab2)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
tab1<- tab1[-c(1,2,3,4,5),-c(12,13,14)]
tab2<- tab2[-c(1),-c(12,13)]
table2 <- rbind(tab1,tab2)
rownames(table2) <- 1:nrow(table2)
#Dataset 3.
stock_html <- read_html(urls_codes[8])
temp3 <- html_table(stock_html)
tab1<- as.data.frame(temp3[1])
tab2<- as.data.frame(temp3[3])
names(tab1)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
names(tab2)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
tab1<- tab1[-c(1,2,3,4,5),-c(12,13,14)]
tab2<- tab2[-c(1),-c(12,13)]
table3 <- rbind(tab1,tab2)
rownames(table3) <- 1:nrow(table3)
#Dataset 4.
stock_html <- read_html(urls_codes[9])
temp3 <- html_table(stock_html)
tab1<- as.data.frame(temp3[1])
tab2<- as.data.frame(temp3[3])
names(tab1)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
names(tab2)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
tab1<- tab1[-c(1,2,3,4,5),-c(12,13,14)]
tab2<- tab2[-c(1),-c(12,13)]
table4 <- rbind(tab1,tab2)
rownames(table4) <- 1:nrow(table4)
#Dataset 5.
stock_html <- read_html(urls_codes[11])
temp3 <- html_table(stock_html)
tab1<- as.data.frame(temp3[1])
tab2<- as.data.frame(temp3[3])
names(tab1)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
names(tab2)<- c("","Mar'13","Mar'14","Mar'15","Mar'16","Mar'17","Mar'18","Mar'19","Mar'20","Mar'21","Mar'22")
tab1<- tab1[-c(1,2,3,4,5),-c(12,13,14)]
tab2<- tab2[-c(1),-c(12,13)]
table5 <- rbind(tab1,tab2)
rownames(table5) <- 1:nrow(table5)
#all 5 datasets
table1
table2
table3
table4
table5

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Question d.
#making function MontyHall().
MontyHall<-function(){
  #the door having car behind it.
  Door_for_car <- sample(1:3, 1)
  Door_for_car
  
  #contestant making initial choice of door
  Initial_Choice <- sample(1:3, 1)
  Initial_Choice
  
  #The door Opened by Monty Hill.
  Door_opened <- sample(setdiff(x<-c(1,2,3), c(Initial_Choice, Door_for_car)), 1)
  Door_opened
  #Switching door if want to switch.
  Door_Switching <- sample(setdiff(x<-c(1,2,3), c(Initial_Choice, Door_opened)), 1)
  Door_Switching
  #If after switching door we get the door which is for car then contestant wins
  if (Door_Switching == Door_for_car) {
    return(1)  # for wins
  } else {
    return(0)  # for loses
  }
}
#calling Function MontyHall().
MontyHall()
Simulation<-c()
for(i in 1:1000)
{
  Simulation<-c(Simulation,MontyHall())
}
Simulation

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Question e.
#reading html.
file<-read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
Rank <- file %>% html_elements(".countdown-index") %>% html_text() %>% str_remove_all("#") %>% as.numeric()
title <- file %>% html_elements(".article_movie_title a") %>% html_text()
Tom_sco <- file %>% html_elements(".tMeterScore") %>% html_text() %>% str_remove_all("%") %>% as.numeric()
YoM <- file %>% html_elements(".start-year") %>% html_text() %>% substr(2,5) %>% as.numeric()
#making dataframe.
Dataset1 <- data.frame("Ranking" =Rank, "Name of Movie"=title, "Tomato % score"=Tom_sco, "Year of movie"=YoM)
Dataset1
