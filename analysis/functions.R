# Functions
library(ggpubr); library(lubridate); library(ggrepel); library(ggridges); library(XML); library(data.table); library(tidyverse)

select = dplyr::select

issn2name = function(issn, scimago=scimago) {
  # returns a data frame with two columns: the issn and the journal title
  dat = issn
  scimago_issn_list = scimago %>% pull(Issn) %>% str_split(", ", simplify=T)
  scimago_issn = as.matrix(cbind(scimago$Title,scimago_issn_list)) %>% as.tibble %>% filter(V4=="") %>% select(1:3) %>% as.data.frame() %>% as.matrix()
  
  tmp_filtered = issn %>% str_replace("-","")  #export %>% select(ISSN) #%>% na.omit()
  # tmp_filtered$ISSN = tmp_filtered$ISSN %>% str_replace("-","")
  
  #sum(tmp_filtered$ISSN %in% scimago_issn_list) # 13989/15484 in Scimago
  
  tmp_filtered = tmp_filtered[tmp_filtered%in% scimago_issn_list]
  
  
  #scimago_issn_list
  #unique(tmp_filtered$Journal)
  
  #str_split("; ", simplify = T)
  
  #match(tmp_filtered$ISSN)
  
  colnames(scimago_issn) = c("Title", "ISSN1", "ISSN2")
  
  idx_match = match(tmp_filtered,scimago_issn) %% nrow(scimago_issn)
  idx_match[idx_match==0] = nrow(scimago_issn)
  titles = scimago_issn[idx_match,"Title"]
  
  length(titles)
  length(export$Journal)
  
  
  length(tmp_filtered)
  length(titles)
  
  map = data.frame(ISSN = tmp_filtered, Journal = titles) %>% na.omit() %>% unique()
  tmp_issn = as.data.frame(map$ISSN %>% str_split("", simplify=T))
  issn_dash = paste0(tmp_issn$V1, tmp_issn$V2, tmp_issn$V3, tmp_issn$V4, "-", tmp_issn$V5, tmp_issn$V6, tmp_issn$V7, tmp_issn$V8)
  map$ISSN = issn_dash
  map = map %>% distinct(ISSN, .keep_all = T)
  
  
  scimago_export = scimago %>% select(Title, Publisher, Rank, H.index) %>% rename(Journal=Title)
  
  map_final = map %>% left_join(scimago_export)
  map_final = map_final %>% distinct(ISSN, .keep_all = T)
  
  return(map_final)
}

issn_analysis = function(prefilter, scimago=scimago) {
  issn = prefilter$ISSN
  #prefilter = prefilter %>% filter(is.na(PubmedMinusReceived)) %>% select(Journal, ISSN) %>% unique()
  map = issn2name(prefilter$ISSN, scimago=scimago)
  test = prefilter %>% select(!Journal) %>% left_join(map, by="ISSN")
  complete_test = test %>% group_by(Journal) %>% filter(!is.na(PubmedMinusReceived)) %>% tally()
  total_test = test %>% group_by(Journal) %>% tally()
  combined_test = total_test %>% left_join(complete_test, by="Journal") %>% rename(Total = n.x, Complete = n.y) %>% 
    mutate(Complete = replace_na(Complete, 0)) %>% mutate(ratio = Complete/Total)
  detailed = combined_test %>% left_join(map)
  detailed_filtered = detailed %>% filter(as.numeric(Total) >=10, as.numeric(Rank) <= 5000, as.numeric(H.index) >=50) %>% 
    arrange(as.numeric(Rank))
  return(detailed_filtered)
}

issn_analysis_date = function(prefilter, scimago=scimago, journals=NA) {
  detailed = prefilter
  issn = prefilter$ISSN
  #prefilter = prefilter %>% filter(is.na(PubmedMinusReceived)) %>% select(Journal, ISSN) %>% unique()
  map = issn2name(prefilter$ISSN, scimago=scimago)
  detailed2 = detailed %>% select(!Journal) %>% left_join(map, by = "ISSN")
  
  if (!is.na(journals)) {
    detailed2 = detailed2 %>% filter(Journal %in% journals)
  }
  
  detailed_filtered = detailed2 %>% 
    #filter(as.numeric(Rank) <= 5000, as.numeric(H.index) >=50) %>% 
    arrange(as.numeric(Rank))
  
  ## 
  test = detailed_filtered #%>% select(!Journal) %>% left_join(map, by="ISSN")
  complete_test = test %>% group_by(DatePubmed) %>% filter(!is.na(PubmedMinusReceived)) %>% tally() #%>% filter(Total >= 10)
  total_test = test %>% group_by(DatePubmed) %>% tally() #%>% filter(n >=10)
  combined_test = total_test %>% left_join(complete_test, by="DatePubmed") %>% rename(Total = n.x, Complete = n.y) %>% 
    mutate(Complete = replace_na(Complete, 0)) %>% mutate(ratio = Complete/Total)
  
  return(combined_test)
}

####### PUBMED QUERY #########
# helper function for the wrapper pubmed_query
pubmed_search = function(final_query, final_out_prefix) {
  library(easyPubMed)
  out.a = batch_pubmed_download(pubmed_query_string = final_query, 
                                format = "xml", 
                                batch_size = 5000,
                                dest_file_prefix = paste0("journal_", final_out_prefix, "_"),
                                encoding = "ASCII", api_key = "b545006c3915644baac911c75722300b2007")
}
# main wrapper function
pubmed_query = function(query, out_prefix, journals="all") {
  library(crayon)
  
  # generate query
  final_query = query
  counter = 0
  if (journals != "all") {
    chunk_size = 100 # change to something smaller if necessary, like 100
    npub = length(journals)
    mat = matrix(nrow=chunk_size, ncol=ceiling(npub/chunk_size))
    mat[1:npub] = journals
    
    nchunk = ncol(mat)
    
    for (i in 3:nchunk) {
      journal = as.character(na.omit(mat[,i]))
      text = paste0(journal, collapse = "[Journal] OR ")
      fin_text = paste0(text, "[Journal]")
      
      final_query = paste0("(", fin_text, ") AND (", query, "))")
      final_out_prefix = paste0(i, "_", out_prefix)
      pubmed_search(final_query, final_out_prefix)
      status = paste0(i, "/", nchunk)
      cat(red(status))
      print(final_query)
      #for (journal in journals) {
      #final_query = paste0("(", journal, "[Journal]) AND (", query, "))")
      #final_out_prefix = paste0(journal, "_", out_prefix)
      #pubmed_search(final_query, final_out_prefix)
      #counter = counter+1
      #status = paste0(counter, "/", length(journals))
      #cat(red(status))
    }
  } else {
    journal = journals
    # do only one query
    final_out_prefix = paste0(journal, "_", out_prefix)
    pubmed_search(final_query, final_out_prefix)
  }
}
### summarize to CSV
# base function for pubmed_summarize
publish = function(xml_path) {
  doc = xmlParse(xml_path)
  d = xmlRoot(doc)
  size = xmlSize(d)
  
  # based on https://stackoverflow.com/questions/32953096/xpathapply-how-to-pass-multiple-paths-or-nodes
  
  out = xpathApply(d, "//PubmedArticle", function(x) {
    
    # extracting the Dates itself
    year = xpathSApply(x, "PubmedData/History/PubMedPubDate/Year", xmlValue)
    month = xpathSApply(x, "PubmedData/History/PubMedPubDate/Month", xmlValue)
    day = xpathSApply(x, "PubmedData/History/PubMedPubDate/Day", xmlValue)
    val = paste(year,month,day,sep="-") %>% as.Date() %>% as.character()
    if (length(val)==0) val = NA_character_
    dates = val
    
    # extracting the Dates type (or attribute i.e. pubmed vs received)
    val = xpathSApply(x, "PubmedData/History/PubMedPubDate",  xmlGetAttr, "PubStatus")
    if (length(val)==0) val = NA_character_
    dates_attr = val
    
    # extracting PMID
    val = xpathSApply(x, "MedlineCitation/PMID", xmlValue)
    if (length(val)==0) val = NA_character_
    pmid = val
    
    # extracting Journals
    val = xpathSApply(x, "MedlineCitation/Article/Journal/ISOAbbreviation", xmlValue)
    if (length(val)==0) val = NA_character_
    journal = val
    
    # extracting ISSN
    val = xpathSApply(x, "MedlineCitation/Article/Journal/ISSN", xmlValue)
    if (length(val)==0) val = NA_character_
    issn = val
    
    # extracting Article Title
    val = xpathSApply(x, "MedlineCitation/Article/ArticleTitle", xmlValue)
    if (length(val)==0) val = NA_character_
    title = val
    
    out = list(Dates = dates, Dates_attr = dates_attr, PMID = pmid, 
               Journal = journal, ISSN = issn, Title = title)
    
    return(out)
  })
  
  out_unlisted = rbindlist(out)
  out_unlisted_relevant = out_unlisted %>% filter(Dates_attr %in% c("pubmed","received","accepted","revised"))
  
  #out_wide = dcast(out_unlisted_relevant, ... ~ Dates_attr, value.var="Dates", fun.aggregate = function(x) NA) %>% as.data.frame()
  out_wide = dcast(out_unlisted_relevant, ... ~ Dates_attr, value.var="Dates") %>% as.data.frame()
  
  columns = c("PMID", "ISSN", "Journal", "Title", "received", "revised", "accepted","pubmed")
  out_wide[,columns[!columns %in% colnames(out_wide)]] = NA
  
  
  if (class(out_wide[1,"pubmed"]) != "character") {
    #out_wide = dcast(out_unlisted_relevant, ... ~ Dates_attr, value.var="Dates") %>% as.data.frame()
    # insert NAs in columns that don't exist but ought to
    blacklist_pmid = out_wide %>% replace(is.na(.), 0) %>% mutate(max = apply(.[5:8], 1, function(x) max(x))) %>% filter(max > 1) %>% select(PMID) %>% as.data.frame() %>% .[,1]
    out_unlisted_relevant = out_unlisted_relevant %>% filter(!(PMID %in% blacklist_pmid))
    out_wide = dcast(out_unlisted_relevant, ... ~ Dates_attr, value.var="Dates") %>% as.data.frame()
    out_wide[,columns[!columns %in% colnames(out_wide)]] = NA
  }
  # reformat
  
  final = out_wide[,c("PMID", "ISSN", "Journal", "Title", "received", "revised", "accepted","pubmed")]
  colnames(final) = c("PMID", "ISSN", "Journal", "Title", "DateReceived", "DateRevised", 
                      "DateAccepted", "DatePubmed")
  # convert to Date for applicable
  
  final = final %>% mutate_at(vars(contains("Date")), as.Date)
  # final$DateAccepted = final$DateAccepted %>% as.Date()
  # final$DateRevised = final$DateRevised %>% as.Date()
  # final$DateReceived = final$DateReceived %>% as.Date()
  # final$DatePubmed = final$DatePubmed %>% as.Date()
  
  # pubmed - received
  final$PubmedMinusReceived = as.numeric(final$DatePubmed - final$DateReceived)
  # pubmed - accepted
  final$PubmedMinusAccepted = as.numeric(final$DatePubmed - final$DateAccepted)
  # accepted - received
  final$AcceptedMinusReceived = as.numeric(final$DateAccepted - final$DateReceived)
  # optional: accepted - revised
  final$AcceptedMinusRevised = as.numeric(final$DateAccepted - final$DateRevised)
  # optional: revised - received
  final$RevisedMinusRevised = as.numeric(final$DateRevised - final$DateReceived)
  
  final = final[,c("PMID", "ISSN", "Journal", "Title", "DateReceived", "DateRevised", 
                   "DateAccepted", "DatePubmed", "PubmedMinusReceived", "PubmedMinusAccepted",	"AcceptedMinusAccepted")]
  
  return(final)
}
# wrapper function
pubmed_summarize = function(files_pattern, outfile_name) {
  files = list.files(pattern=files_pattern)
  counter = 1
  for (xml_index in counter:length(files)) {
    final = publish(files[xml_index])
    if (counter == 1) {
      fwrite(final,file=outfile_name, append=F, quote=F, sep="\t")
    } else {
      fwrite(final,file=outfile_name, append=T, quote=F, sep="\t")
    }
    counter = counter + 1
    print(counter)
  }
}
median_pub = function(final) {
  # takes in a properly formatted data frame for literature, outputs median calculations per journal
  tmp = final
  tmp$DatePubmed = as.Date(tmp$DatePubmed)
  tmp %>% filter(!is.na(PubmedMinusReceived) & PubmedMinusReceived > 0) #%>% 
  tmp$DatePubmed = as.Date(tmp$DatePubmed)
  
  tmp_filtered = tmp %>% select(Journal, ISSN, Title, DatePubmed, PubmedMinusReceived) %>% na.omit()
  #tmp_filtered$ISSN = tmp_filtered$ISSN %>% str_replace("-","")
  
  #sum(tmp_filtered$ISSN %in% scimago_issn_list) # 13989/15484 in Scimago
  
  #tmp_filtered = tmp_filtered %>% filter(ISSN %in% scimago_issn_list)
  #tmp_filtered = tmp_filtered %>% filter(DatePubmed > "2019-09-30")
  
  
  #scimago_issn_list
  #unique(tmp_filtered$Journal)
  
  #idx_match = match(tmp_filtered$ISSN,scimago_issn) %% nrow(scimago_issn)
  #idx_match[idx_match==0] = nrow(scimago_issn)
  #titles = scimago_issn[idx_match,"Title"]
  #tmp_filtered$Journal = titles
  
  med = tmp_filtered %>% arrange(ISSN) %>% group_by(ISSN) %>% #filter(n() >= 10) %>%
    summarise(median = median(PubmedMinusReceived), count = n()) %>%
    arrange(median) %>% as.data.frame()
  
  #tmp_filtered_noncovid = tmp_filtered %>% #filter(Journal %in% med$Journal[1:100]) %>% 
  #  left_join(med_noncovid) %>%
  #  group_by(Journal) %>% arrange(median)
  
  return((med))
}
scimago_filter = function(final, scimago, npub=10, rank=5000, hindex=50) {
  dat = scimago
  tmp = final
  final$ISSN = final$ISSN %>% str_replace("-","")
  
  scimago_issn = dat %>% filter(SJR.Best.Quartile == "Q1") %>% filter(as.numeric(Rank) < rank) %>% filter(as.numeric(H.index) > hindex)
  scimago_issn_list = scimago_issn %>% pull(Issn) %>% str_split(", ", simplify=T)
  scimago_issn = as.matrix(cbind(scimago_issn,scimago_issn_list))
  
  tmp_filtered = tmp %>% select(Journal, ISSN, Title, DateReceived, DatePubmed, PubmedMinusReceived) %>% na.omit()
  tmp_filtered$ISSN = tmp_filtered$ISSN %>% str_replace("-","")
  
  sum(tmp_filtered$ISSN %in% scimago_issn_list) # 13989/15484 in Scimago
  
  tmp_filtered = tmp_filtered %>% filter(ISSN %in% scimago_issn_list)
  
  idx_match = match(tmp_filtered$ISSN,scimago_issn) %% nrow(scimago_issn)
  idx_match[idx_match==0] = nrow(scimago_issn)
  titles = scimago_issn[idx_match,"Title"]
  tmp_filtered$Journal = titles
  
  med_covid = tmp_filtered %>% arrange(Journal) %>% group_by(Journal) %>% filter(n() >= npub) %>%
    summarise(median = median(PubmedMinusReceived), count = n()) %>%
    arrange(median)
  
  tmp_filtered1 = tmp_filtered %>% #filter(Journal %in% med$Journal[1:100]) %>% 
    left_join(med_covid) %>%
    group_by(Journal) %>% arrange(median)
  
  tmp_filtered1$Journal = with(tmp_filtered1, reorder(Journal, -median))
  
  
  ############ CATEGORIES OF JOURNALS #######
  tempdata = dat %>% select(Issn, Title, Categories)
  test = tempdata$Categories %>% str_split("; ", simplify = T)
  test2 = cbind(tempdata, test) 
  melted <- melt(test2[,c(-1,-3)],id.vars=1) %>% filter(value!="") %>% filter(grepl("(Q1)", value)) %>% 
    filter(!grepl("\n", value)) %>% select(!variable)
  unique(melted$value)
  #View(melted)
  
  # filter both the scimago category and the the actaul data as intersection
  tmp_filtered1 = tmp_filtered1 %>% filter(Journal %in% melted$Title)
  melted = melted %>% filter(Title %in% tmp_filtered1$Journal)
  unique(melted$value)
  #View(melted)
  tmp_filtered_covid = tmp_filtered1
  
  
  #fwrite(tmp_filtered1,file="export_Josh.txt", append=F, quote=F, sep="\t")
  #fwrite(melted,file="export_categories_Josh.txt", append=F, quote=F, sep="\t")
  
  #melted %>% filter(Title %in% med_intersect$Journal) %>% select(2) %>% unique() %>% count()
  
  # Compile list of journals by ISSN that made it onto the list and reformat the ISSN to Pubmed 4-4 dash.
  tmp_issn = as.data.frame(tmp_filtered1$ISSN %>% str_split("", simplify=T))
  journals = paste0(tmp_issn$V1, tmp_issn$V2, tmp_issn$V3, tmp_issn$V4, "-", tmp_issn$V5, tmp_issn$V6, tmp_issn$V7, tmp_issn$V8)
  journals = unique(journals)
  return(journals)
}

