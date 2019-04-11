library(data.table)
library(tidyverse)

setwd(here::here("build_data"))

laulud2 <- fread("data/top40_metadata.csv")

filelist <- list.files("data/songs/",full.names=T)

#for (file in filelist){
#contents <- read_lines(file)
#write_lines(contents,file)
#}

texts <- map_df(filelist, ~ data_frame(txt = read_lines(.x)) %>%
                  mutate(filename = .x)) %>%
  mutate(filename= gsub("data/songs//","",filename)) %>%
  mutate(filename= gsub("\\.txt$","",filename))

laulud <- texts%>%
  mutate(txt=trimws(txt))%>%
  filter(txt!="")%>%
  filter(txt!="\"")%>%
  group_by(filename) %>% 
  filter(row_number()!=1) %>% #remove artist name
  filter(row_number()!=1) %>% #remove song title
  mutate(txt=trimws(gsub("\"$","",txt))) %>% #
  summarize(txt=paste(txt, collapse=" "))

lyrics <- merge(laulud2,laulud,by="filename",all=T)

names(lyrics) <- c("filename","artist_title","year","keep","rank","votes","artist","song","source","lyrics")

lyrics_clean <- lyrics[order(year,rank)][,.(year,rank,votes,artist,song,filename,source,lyrics)]


lyrics_clean[,language:=cld2::detect_language(lyrics)]

#tuvastamata on eesti
#manual_langs <- lyrics_clean[is.na(language)&!is.na(lyrics)]
lyrics_clean[is.na(language)&!is.na(lyrics),language:="et"]


fwrite(lyrics_clean,"../eesti_skyplus_top40_1994-2018.tsv",sep="\t")

#basic stats
nrow(lyrics_clean[!is.na(lyrics)])
nrow(unique(lyrics_clean[,.(filename)]))

lyrics_clean[,.N,language]
