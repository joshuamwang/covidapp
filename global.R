library('shiny')
library('plotly')
library('DT')
library('googlesheets4')
library('data.table')
library('tidyverse')
library('ggplot2')
#library('future')
#library('promises')
library('GAlogger')
ga_set_tracking_id('UA-173612576-1')
#ga_set_approval(consent=TRUE)
#plan(multicore)

options(warn=-1)

shinyOptions(cache = diskCache("./myapp-cache"),max_size = Inf)

summarizedData <- read.csv('20-0727_export/export_meta.csv',stringsAsFactors=F,header=T)
summarizedData <- summarizedData[,c('Journal','median_precovid','count_precovid','median_noncovid',
                                    'count_noncovid','median','count','ISSN','Rank','Categories',
                                    "Publisher",'covid_completeness','noncovid_completeness',
                                    'precovid_completeness')]

data <- read.csv('20-0727_export/export_data.csv',stringsAsFactors=F,header=T)
data <- data[,c('Type','PMID','ISSN','Journal','Title','DateReceived','DateRevised','DateAccepted',
                'DatePubmed','PubmedMinusReceived','PubmedMinusAccepted','AcceptedMinusAccepted')]
colnames(data)[c(10,11,12)] <- c('Days from Date Received to Date on PubMed',
                                 'Days from Date Accepted to Date on PubMed',
                                 'Days from Date Received to Date Accepted')
data <- data[which(data$`Days from Date Received to Date on PubMed`<1825),]
data <- data[which(data$`Days from Date Received to Date on PubMed`>0),]

data[,c('DateRevised','DateAccepted')] <- NULL
covidPapers <- data[data$Type=='covid',]
nonCovidPapers <- data[data$Type=='noncovid',]
preCovidPapers <- data[data$Type=='precovid',]

