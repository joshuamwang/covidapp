# Tung H. Nguyen, MS
# July 18, 2020
# SCRIPT: Vectorized analysis for XML data extraction (10s for 4000 records)
PROJECT="~/Documents/Github/time2pub/analysis"; setwd(PROJECT); source('functions.R')
library(MASS); library(scales)

####### PUBMED QUERY #########
covid_control_query = '(("2020/01/01"[Date - Publication] : "2020/07/21"[Date - Publication]) AND ("Wuhan coronavirus"[Supplementary Concept] OR covid19 OR covid-19 OR "covid 19" OR 2019ncov OR 2019-ncov OR "2019 ncov" OR "2019 Novel Coronavirus" OR "2019 novel cov" OR (wuhan AND (ncov OR coronavirus)) OR "novel coronavirus pneumonia"))'
non_covid_control_query = '(("2020/01/01"[Date - Publication] : "2020/07/21"[Date - Publication]) NOT ("Wuhan coronavirus"[Supplementary Concept] OR covid19 OR covid-19 OR "covid 19" OR 2019ncov OR 2019-ncov OR "2019 ncov" OR "2019 Novel Coronavirus" OR "2019 novel cov" OR (wuhan AND (ncov OR coronavirus)) OR "novel coronavirus pneumonia"))'
pre_covid_control_query = '(2019/01/01[Date - Publication] : 2019/07/21[Date - Publication])'
not_covid_control_query = paste0(non_covid_control_query, " OR ", pre_covid_control_query)
#pubmed_query(query = covid_control_query, out_prefix = "allCOVID", journals = "all") # do only once
#pubmed_summarize(files_pattern = "_allCOVID_", "all_COVID.txt") # do only once

### find the median of the dates published
# filter out the dates incomplete & filter out the dates negative
tmp = fread("all_COVID.txt", data.table = F, quote="", fill = T)
tmp = tmp %>% na_if("")
tmp$DatePubmed = as.Date(tmp$DatePubmed)
ggplot(tmp, aes(x = DatePubmed)) + geom_density() + scale_x_date(date_labels = "%m-%Y", date_minor_breaks = "1 year")
final = tmp %>% filter(as.numeric(DatePubmed - as.Date("2020-01-01"))>=0)

# calculate medians
covid_medians = median_pub(final)

# Filter based on impact factor using scimago and the amount of publications
scimago = read.csv("./scimagojr 2019.csv", sep = ";", stringsAsFactors = F)#, quote = '')

# output the journals ISSN
journal_vector = scimago_filter(final=final, scimago=scimago)

# Run the intersect with controls
# pubmed_query(query = not_covid_control_query, out_prefix = "notCOVID", journals = journal_vector) # do only once
# 30 seconds per batch, 15 batches per query, 7 queries 

# summarize those files into 1 csv
# pubmed_summarize(files_pattern = "_notCOVID_", "not_COVID.txt") # do only once

tmp = fread("not_COVID.txt", data.table = F, quote="", fill = T)
tmp = tmp %>% na_if("")
tmp$DatePubmed = as.Date(tmp$DatePubmed)
ggplot(tmp, aes(x = DatePubmed)) + geom_density() + scale_x_date(date_labels = "%m-%Y", date_minor_breaks = "1 year")

# filter out where dates are not aligned. precovid 1/1/19 until 7/21/19. noncovid 1/1/20 onwards
noncovid = tmp %>% filter(DatePubmed >= as.Date("2020-01-01"), DatePubmed <= as.Date("2020-07-21")) %>% filter(!is.na(DateReceived))
precovid = tmp %>% filter(DatePubmed >= as.Date("2019-01-01"), DatePubmed <= as.Date("2019-07-21")) %>% filter(!is.na(DateReceived))
covid = final %>% filter(DatePubmed >= as.Date("2020-01-01"), DatePubmed <= as.Date("2020-07-21")) %>% filter(!is.na(DateReceived))

# intersect the medians file with the resulting output
covid_medians = median_pub(covid) %>% arrange(ISSN) %>% filter(count >= 10)
precovid_medians = median_pub(precovid) %>% arrange(ISSN) %>% filter(count >= 10)
noncovid_medians = median_pub(noncovid) %>% arrange(ISSN) %>% filter(count >= 10)

# intersect the not_COVID with the median metafile
issn_intersect = Reduce(intersect, list(noncovid_medians$ISSN, precovid_medians$ISSN, covid_medians$ISSN))

# intersect the COVID with the resulting output
covid_filtered = covid %>% filter(ISSN %in% issn_intersect)
precovid_filtered = precovid %>% filter(ISSN %in% issn_intersect)
noncovid_filtered = noncovid %>% filter(ISSN %in% issn_intersect)
intersect_median = precovid_medians %>% left_join(noncovid_medians, by = "ISSN", suffix = c("_precovid", "_noncovid")) %>% 
  left_join(covid_medians, by = "ISSN") %>% filter(ISSN %in% issn_intersect)

export = bind_rows(list(covid = covid_filtered %>% mutate(PMID = as.integer(PMID)), noncovid = noncovid_filtered, precovid = precovid_filtered), .id = "Type")

# rename journals#############
dat = read.csv("./scimagojr 2019.csv", sep = ";", stringsAsFactors = F)#, quote = '')

map = issn2name(export$ISSN, dat)
export = export %>% select(!Journal) %>% left_join(map %>% select(ISSN, Journal))

#############
# separate and write to files
fwrite(export,file="export_data.txt", append=F, quote=F, sep="\t")
scimago_join = scimago %>% select(Rank, Title, Categories, Publisher) %>% rename(Journal=Title)
export_medians = intersect_median %>% left_join(export[c("Journal","ISSN")]) %>% unique() %>% left_join(scimago_join)
fwrite(export_medians, file="export_medians.txt", append=F, quote=F, sep="\t")

######## generate figures #########
export = fread("export_data.txt", data.table = F, quote="", fill = T) %>% na_if("")
export_medians = fread("export_medians.txt", data.table = F, quote="", fill = T) %>% na_if("") %>% arrange(Rank)

######## FIGURE 2A - PubmedMinusReceived vs DatePubmed
# plot over time
export$DateReceived = as.Date(export$DateReceived)
tmp = as.POSIXlt(export$DatePubmed)
tmp$year = 120 # 2020
dates2 = as.Date(tmp)

export$month = dates2

theme_set(theme_pubr())
export_f = export %>% filter(PubmedMinusReceived > 0)

# Figure 2A. Contour plot of fast tracking effect over duration of the COVID pandemic so far
plot2d = ggplot(export_f, aes(x = month, y = (PubmedMinusReceived), color = Type)) + geom_density_2d(alpha=.75) + #+ geom_hex(bins = 70) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") + scale_fill_continuous(type = "viridis") +   #geom_point() + 
  stat_smooth(aes(colour = Type), method = "lm") + scale_color_manual(values=c("#440154FF", "#29AF7FFF", "#DCE319FF"),
                                                       labels=c("COVID19 (2020)", "Non-COVID19 (2020)", "Pre-COVID19 (2019)") )+
  scale_y_continuous(trans = log2_trans(), breaks = c(2, 4, 8, 16, 32, 64, 128, 256)) + 
  theme(legend.key = element_rect(fill = "white")) #+ annotation_logticks() 
plot2d

covid_model = lm("log2(PubmedMinusReceived) ~ month", data = export_f %>% filter(Type=="covid"))
summary(covid_model)
precovid_model = lm("log2(PubmedMinusReceived) ~ month", data = export_f %>% filter(Type=="precovid"))
summary(precovid_model)
noncovid_model = lm("log2(PubmedMinusReceived) ~ month", data = export_f %>% filter(Type=="noncovid"))
summary(noncovid_model)

# summarize the medians agnostic of journal
export_f %>% filter(Type == "covid") %>% select(PubmedMinusReceived) %>% .[[1]] %>% median() # 27 days for covid articles if agnostic of journals
export_f %>% filter(Type == "precovid") %>% select(PubmedMinusReceived) %>% .[[1]] %>% median() # 140 days for precovid articles if agnostic of journals
export_f %>% filter(Type == "noncovid") %>% select(PubmedMinusReceived) %>% .[[1]] %>% median() # 130 days for noncovid articles if agnostic of journals

export_f %>% group_by(Type) %>% summarize(Type, n()) %>% unique() # tells you how many articles per query
tmp$DatePubmed = as.Date(tmp$DatePubmed)

# FIGURE 1B. Ridge plots
test = export %>% left_join(export_medians) %>% arrange(as.numeric(Rank))
ridge_list = export_medians[1:40,] %>% select(Journal) %>% .[,1]
test_filtered = test %>% filter(Journal %in% ridge_list) %>% arrange(median)

test_filtered2 = test_filtered %>% arrange(median) %>% mutate(Journal = fct_rev(as_factor(Journal)))
test_filtered2$Journal_Rank = paste0(test_filtered2$Rank, ". ", test_filtered2$Journal)
test_filtered2 = test_filtered2 %>% mutate(Journal_Rank = fct_rev(as_factor(Journal_Rank)))
ggplot(test_filtered2, aes(x = PubmedMinusReceived, y = Journal_Rank)) + 
  geom_density_ridges(aes(fill = Type, alpha = 0.1)) + 
  scale_x_continuous(limits=c(1,365)) + theme_ridges(font_size = 13) + scale_fill_manual(values=c("#440154FF", "#29AF7FFF", "#DCE319FF"),
                                                                                          labels=c("COVID19 (2020)", "Non-COVID19 (2020)", "Pre-COVID19 (2019)") ) +
  theme(legend.position = "none")

# FIG1B_Table and FIG 1S: make the chart about each journal's median and calculate the multiplier effect from covid <> precovid
median_compare = test_filtered2 %>% select(Journal, median, median_noncovid, median_precovid) %>% unique()
median_compare$multiplier = median_compare$median_precovid / median_compare$median
fwrite(median_compare, file = "median_compare_figure.txt", append=F, quote=F, sep="\t")

#####

# FIG1A_boxplot
# Let's compare the medians
export_medians
data_long = gather(export_medians, condition, measurement, median, median_noncovid, median_precovid, factor_key=TRUE)

my_comparisons = list( c("median", "median_noncovid"), c("median", "median_precovid"), c("median_noncovid", "median_precovid") )
ggviolin(data_long, x = "condition", y = "measurement", 
          color = "condition", add.params = list(alpha = 0.4, fill = "black"), palette = c("#440154FF", "#29AF7FFF", "#DCE319FF"),
          ylab = "PubmedMinusReceived", xlab = "Type",  shape = "condition", add = c("jitter", "boxplot"),
         draw_quantiles = 0.5) + stat_compare_means(comparisons = my_comparisons)

# FIGURE 1A chart - the medians for the top 200 journals
data_long %>% group_by(condition) %>% summarize(median(measurement))
export_f %>% group_by(Type) %>% summarize(length(Title))

############ XML record completeness analysis 
# comparing the list and the filtered list

scimago = read.csv("./scimagojr 2019.csv", sep = ";", stringsAsFactors = F)#, quote = '')
prefilter = fread("all_COVID.txt", data.table = F, quote="", fill = T) %>% na_if("")

postfilter = fread("export_data.txt", data.table = F, quote="", fill = T) %>% na_if("")
postfilter_medians = fread("export_medians.txt", data.table = F, quote="", fill = T) %>% na_if("") %>% arrange(Rank)

scimago = read.csv("./scimagojr 2019.csv", sep = ";", stringsAsFactors = F)#, quote = '')


covid_completeness = issn_analysis(prefilter, scimago)
notcovid = fread("not_COVID.txt", data.table = F, quote="", fill = T) %>% na_if("")

noncovid = notcovid %>% filter(DatePubmed >= as.Date("2020-01-01"), DatePubmed <= as.Date("2020-07-21"))
precovid = notcovid %>% filter(DatePubmed >= as.Date("2019-01-01"), DatePubmed <= as.Date("2019-07-21"))
covid = prefilter %>% filter(DatePubmed >= as.Date("2020-01-01"))

# how many total records are there without regard to completeness?
nrow(noncovid) # 170256
nrow(precovid) # 151435
nrow(covid) # 33108

covid$Journal %>% unique() %>% length()
precovid$Journal %>% unique() %>% length()
noncovid$Journal %>% unique() %>% length()





noncovid_completeness = issn_analysis(noncovid, scimago)
precovid_completeness = issn_analysis(precovid, scimago)
noncovid_completeness
precovid_completeness

complete_list = Reduce(intersect, list(precovid_completeness$Journal, noncovid_completeness$Journal, covid_completeness$Journal))

#
c_plot = covid_completeness %>% select(Journal, ratio)
p_plot = precovid_completeness %>% select(Journal, ratio)
n_plot = noncovid_completeness %>% select(Journal, ratio)

all_plot = c_plot %>% left_join(n_plot, by = "Journal") %>% left_join(p_plot, by = "Journal") %>% rename(covid = ratio.x, noncovid = ratio.y, precovid = ratio)

export_medians = fread(file="export_medians.txt", data.table=F)
meta = export_medians %>% left_join(all_plot) %>% rename(covid_completeness = covid, noncovid_completeness = noncovid, precovid_completeness = precovid)

filter_list = as.character(test_filtered2$Journal %>% unique())

all_plot_filtered = all_plot %>% filter(Journal %in% filter_list) %>% mutate(Journal = factor(Journal, levels = rev(unique(test_filtered$Journal))))
all_plot_filtered_long <- gather(all_plot_filtered, type, complete, covid:precovid) %>% mutate(Journal = as_factor(Journal))%>% mutate(Journal = fct_relevel(Journal))#Create long format

# FIGURE 2S. Completeness of XML record in the top 40 journals
plot = ggplot(all_plot_filtered_long, aes(Journal, complete, fill=type)) + scale_fill_manual(values=c("#440154FF", "#29AF7FFF", "#DCE319FF"))
plot = plot + geom_bar(stat = "identity", position = 'dodge') + coord_flip()
plot

# import the figure
export_unfiltered = bind_rows(list(covid = covid %>% mutate(PMID = as.integer(PMID), DatePubmed = as.Date(DatePubmed)), noncovid = noncovid %>% mutate(DatePubmed = as.Date(DatePubmed)), 
                                   precovid = precovid %>% mutate(DatePubmed = as.Date(DatePubmed))), .id = "Type")
covid_unfiltered = export_unfiltered %>% filter(Type=="covid")
precovid_unfiltered = export_unfiltered %>% filter(Type=="precovid")
noncovid_unfiltered = export_unfiltered %>% filter(Type=="noncovid")


hi_covid = issn_analysis_date(covid_unfiltered, scimago, journals=postfilter_medians$Journal)
hi_precovid = issn_analysis_date(precovid_unfiltered, scimago, journals=postfilter_medians$Journal)
hi_noncovid = issn_analysis_date(noncovid_unfiltered, scimago, journals=postfilter_medians$Journal)
hi = bind_rows(rev(list(hi_covid, hi_noncovid, hi_precovid)), .id = "Type") # reorder so the covid is on top

tmp <- as.POSIXlt(hi$DatePubmed)
tmp$year <- 120
dates2 <- as.Date(tmp)
hi$month =  dates2

# Figure 2B. Completeness of XML records over the duration of the COVID-19 pandemic thus far
ggplot(hi, aes(x = month, y = (ratio), color = Type, size = log2(Total))) + geom_point(alpha=.65) + #+ geom_hex(bins = 70) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") + scale_fill_continuous(type = "viridis") +   #geom_point() + 
  stat_smooth(aes(colour = Type)) + 
  scale_color_manual(values=rev(c("#440154FF", "#29AF7FFF", "#DCE319FF")),
                     labels=rev(c("COVID19 (2020)", "Non-COVID19 (2020)", "Pre-COVID19 (2019)")))+
  theme(#legend.direction="vertical",
    legend.key = element_rect(fill = "white")) #+
#guides(colour = guide_legend(override.aes = list(size=6)))#,



