# Correções para FB 13/09

library(janitor)
library(dplyr)
library(xlsx)
library(stringi)
library(data.table)
library(readxl)

tira_tudo <- function(x){
  stopifnot(require(stringi))
  
  x <- tolower(x)
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- gsub("s", "", x)
  x <- gsub("z", "", x)
  x <- gsub("ll", "l", x)
  x <- gsub("nn", "n", x)
  x <- gsub("[[:space:]]", "", x)
  
}

remove_dup <- function(tse) {
  tse1 <- tse %>%
    clean_names() %>%
    mutate(dup1 = duplicated(nr_cpf_candidato),
           dup2 = duplicated(nr_cpf_candidato, fromLast=T),
           dup_end = ifelse(dup1, T, 
                            ifelse(dup2, T, F))) %>%
    filter(!dup_end)
  print(paste("foram removidas", nrow(tse) - nrow(tse1)))
  return(tse1)
}

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\fb_final\\1309")

#nomes dos partidos:
load("C:/Users/jvoig/OneDrive/Documentos/facebook_election_files/partidos.Rdata")
names(partidos) <- c("party" , "party_id")

#perfis que fizemos mas que ainda não conseguimos dar match com o TSE
load("perfis_nao_entregues.Rdata")

#tse
load("C:/Users/jvoig/OneDrive/Documentos/facebook_election_files/Repositório de candidatos TSE/1309/tse_filtrado1309.Rdata")
tse_unico <- remove_dup(tse_filtrado)
tse_unico %>% nrow() #26556

#vendo o ultimo full pra saber quantos candidatos faltam:
load("C:/Users/jvoig/OneDrive/Documentos/fb_final/full_12_09_final_approved.Rdata")
#25769 candidatos

full_12_09_final_approved %>%
  inner_join(tse_unico, by=c("nr_cpf_candidato")) %>%
  nrow() #25513

# Foram considerados inaptos 256 candidatos do nosso arquivo full de ontem. 
# Faltam encontrar 1043 candidatos

#vendo quem desses sâo inaptos
load("C:/Users/jvoig/OneDrive/Documentos/facebook_election_files/Repositório de candidatos TSE/1309/tse_inaptos1309.Rdata")

tse_inaptos <- remove_dup(tse_inaptos)
tse_inaptos <- tse_inaptos %>%
  mutate(nome2 = tira_tudo(nm_candidato),
         estado = tira_tudo(nm_ue),
         inapto = 1) %>%
  select(nome2, estado, nr_cpf_candidato, nr_candidato, inapto)

#join com os aptos e inaptos com esses não entregues por nome sem acento, sem S e sem Z

tse_aptos <- tse_unico %>%
  mutate(nome2 = tira_tudo(nm_candidato),
         estado = tira_tudo(nm_ue)) %>%
  select(nome2, estado, nr_cpf_candidato, nr_candidato)

perfis <- perfis_nao_entregues %>%
  mutate(nome2 = tira_tudo(figure_name),
         estado = tira_tudo(district_id)) %>%
  left_join(tse_aptos, by=c("nome2" = "nome2",
                            "figure_id" = "nr_candidato",
                            "estado" = "estado")) %>%
  left_join(tse_inaptos, by=c("nome2" = "nome2",
                              "figure_id" = "nr_candidato",
                              "estado" = "estado")) %>%    #756 inaptos
  mutate(nr_cpf_candidato = ifelse(is.na(nr_cpf_candidato.x), nr_cpf_candidato.y, 
                                         nr_cpf_candidato.x)) %>%
  select(-c(nr_cpf_candidato.x, nr_cpf_candidato.y)) %>% 
  filter(is.na(inapto)) #806 carinhas

#candidatos que tiveram correspondência no banco do TSE:

correspond_tse <- perfis %>%
  filter(!is.na(nr_cpf_candidato)) %>% #preciso validar
  distinct(nr_cpf_candidato, figure_id, url, .keep_all = TRUE) %>% #437
  group_by(nr_cpf_candidato) %>%
  mutate(dup = n())

#os que eu já tenho prontos
entrega_1309_v1 <- correspond_tse %>%
  filter(dup == 1) #297

#os que eu tenho correspondência mas estão duplicados 
duplicados <- correspond_tse %>%
  filter(dup != 1) #vão ser 135

#write.xlsx(as.data.frame(duplicados), file="duplicados.xlsx", row.names = FALSE)

#subindo os duplicados corrigidos:

dup_corrigidos <- read_excel("C:/Users/jvoig/OneDrive/Documentos/fb_final/1309/duplicados_corrigidos.xlsx")

entrega_1309_v2 <- entrega_1309_v1 %>%
  bind_rows(dup_corrigidos)  #432

####
#Parte 2
#os que eu não encontrei correspondência e podem ou não estar aptos
#reparei um problema, os never takers sáo do DF. Vou tentar novamente o match e ver o problema

never_takers <- perfis %>%
  filter(is.na(nr_cpf_candidato)) %>%
  mutate(estado = ifelse(estado == "federalditrict", "ditritofederal", estado)) %>%
  select(-c(inapto)) %>%
  left_join(tse_aptos, by=c("nome2" = "nome2",
                            "figure_id" = "nr_candidato",
                            "estado" = "estado")) %>%
  left_join(tse_inaptos, by=c("nome2" = "nome2",
                              "figure_id" = "nr_candidato",
                              "estado" = "estado")) %>%
   filter(is.na(inapto))
  
#alguns viraram takers:

leva3 <- never_takers %>%
  select(-c(nr_cpf_candidato.x, nr_cpf_candidato)) %>%
  filter(!is.na(nr_cpf_candidato.y)) %>%
  group_by(nr_cpf_candidato.y) %>%
  mutate(dup = n()) %>%
  ungroup()

leva_3_dup <- leva3 %>%
  filter(dup > 1)
  
write.xlsx(as.data.frame(leva_3_dup), file="leva_3_dup.xlsx", row.names = FALSE)

#buscando de volta:
leva_3_dup_corrigido <- read_excel("C:/Users/jvoig/OneDrive/Documentos/fb_final/1309/leva_3_dup_corrigido.xlsx")

entrega_1309_v3 <- leva3  %>%
  filter(dup == 1) %>%
  bind_rows(entrega_1309_v2) %>% #447
  bind_rows(leva_3_dup_corrigido) %>% #504
  select(-c(nr_cpf_candidato, estado, dup, remover, nome2, inapto, correcao)) %>%
  rename(nr_cpf_candidato = nr_cpf_candidato.y) %>%
  left_join(partidos)

full_13_09_final_approved <- full_12_09_final_approved %>%
  inner_join(tse_unico, by=c("nr_cpf_candidato")) %>%
  bind_rows(entrega_1309_v3)

save(full_13_09_final_approved, file="full_13_09_final_approved.Rdata")
write.csv(full_13_09_final_approved, file="full_13_09_final_approved.csv", row.names=FALSE)
# never_takers

never_takers_true <- never_takers %>%
  filter(is.na(nr_cpf_candidato.y)) %>%
  distinct(nome, estado, figure_id, url, .keep.all = TRUE)

save(never_takers_true, file="never_takers_true.Rdata")

tse_unico <- tse_unico %>%
  mutate(estado = tira_tudo(nm_ue),
         nome2 = tira_tudo(nm_candidato)) 

full_13_09_final_approved <- full_13_09_final_approved %>%
  mutate(estado = tira_tudo(district_id),
         nome2 = tira_tudo(figure_name)) 

tse_restantes_1309 <- tse_unico %>%
  anti_join(full_13_09_final_approved, by=c("nr_cpf_candidato")) %>%
  anti_join(full_13_09_final_approved, by=c("nome2", "nome2",
                                            "estado", "estado"))

save(tse_restantes_1309, file="tse_restantes_1309.Rdata")
