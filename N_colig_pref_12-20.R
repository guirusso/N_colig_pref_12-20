### Gui Russo
## October 15, 2020
# Quick analysis for CepespData

# Packages ----
rm(list=ls()); cat("\014")

library(cepespR)
library(dplyr)
library(stringr)
library(writexl)

# 2012 data ----
df12<-get_candidates(2012, "Mayor")
head(df12)

coal12<-df12 %>% filter(NUM_TURNO==1) %>% select(COMPOSICAO_LEGENDA)

coal12$COMPOSICAO_LEGENDA<-str_replace_all(coal12$COMPOSICAO_LEGENDA, " ", "")
parties12<-str_split(coal12$COMPOSICAO_LEGENDA, "/")

for(i in 1:length(parties12)){
  parties12[[i]]<-
    expand.grid(unique(parties12[[i]]), unique(parties12[[i]]))
  parties12[[i]]<-parties12[[i]] %>% filter(Var1!=Var2) 
  parties12[[i]]$pair<-paste(as.character(parties12[[i]]$Var1), 
                             as.character(parties12[[i]]$Var2), sep="-")
  parties12[[i]]<-parties12[[i]] %>% select(pair)
}

pairs12<-do.call(rbind, parties12)
head(pairs12)

# 2016 data ----
df16<-get_candidates(2016, "Mayor")
head(df16)

coal16<-df16 %>% filter(NUM_TURNO==1) %>% select(COMPOSICAO_LEGENDA)

coal16$COMPOSICAO_LEGENDA<-str_replace_all(coal16$COMPOSICAO_LEGENDA, " ", "")
parties16<-str_split(coal16$COMPOSICAO_LEGENDA, "/")

for(i in 1:length(parties16)){
  parties16[[i]]<-
    expand.grid(unique(parties16[[i]]), unique(parties16[[i]]))
  parties16[[i]]<-parties16[[i]] %>% filter(Var1!=Var2) 
  parties16[[i]]$pair<-paste(as.character(parties16[[i]]$Var1), 
                             as.character(parties16[[i]]$Var2), sep="-")
  parties16[[i]]<-parties16[[i]] %>% select(pair)
}

pairs16<-do.call(rbind, parties16)
head(pairs16)

# 2020 data ----
setwd("/Users/guilhermerusso/Downloads")

#download.file("http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2020.zip",
#              destfile = "consulta_cand_2020.zip")
#unzip("consulta_cand_2020.zip", 
#      exdir = "consulta_cand_2020")

list.files(path="./consulta_cand_2020/")
df20<-read.csv("consulta_cand_2020/consulta_cand_2020_BRASIL.csv", sep=";",
               encoding="latin1")

coal20<-df20 %>% filter(DS_CARGO=="PREFEITO") %>% select(DS_COMPOSICAO_COLIGACAO)

coal20$DS_COMPOSICAO_COLIGACAO<-str_replace_all(coal20$DS_COMPOSICAO_COLIGACAO, " ", "")
parties20<-str_split(coal20$DS_COMPOSICAO_COLIGACAO, "/")

for(i in 1:length(parties20)){
  parties20[[i]]<-
    expand.grid(unique(parties20[[i]]), unique(parties20[[i]]))
  parties20[[i]]<-parties20[[i]] %>% filter(Var1!=Var2) 
  parties20[[i]]$pair<-paste(as.character(parties20[[i]]$Var1), 
                             as.character(parties20[[i]]$Var2), sep="-")
  parties20[[i]]<-parties20[[i]] %>% select(pair)
}

pairs20<-do.call(rbind, parties20)
head(pairs20)

# Aggregating ----
nrow(pairs12) # Number of dyads in '12
nrow(pairs16) # Number of dyads in '16
nrow(pairs20) # Number of dyads in '20

dat12<-pairs12 %>% group_by(pair) %>% summarize(n12=n())
head(dat12)

dat16<-pairs16 %>% group_by(pair) %>% summarize(n16=n())

pairs20$pair<-str_replace(pairs20$pair, "MDB", "PMDB") 
dat20<-pairs20 %>% group_by(pair) %>% summarize(n20=n())

# Merging ----
dat<-merge(dat12, dat16, all=T)
dat<-merge(dat, dat20, all=T)

nrow(dat); head(dat)
dat<-dat %>% arrange(desc(n20))
head(dat)

# Dropping the same dyad
df_fin<-dat[(1:num) %% 2 !=0, ]
head(df_fin)

# Exporting into a spreadsheet ----
write_xlsx(df_fin, "N_colig_pref_12a20.xlsx")
