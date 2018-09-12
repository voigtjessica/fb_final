#Select sobras 12/09

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



#Vendo as sobras do dia 12/09
library(data.table)
library(dplyr)
library(janitor)
library(stringr)
library(xlsx)
library(readxl)

#Arquivo com o TSE de hoje:
load("C:/Users/jvoig/OneDrive/Documentos/facebook_election_files/Repositório de candidatos TSE/1209/tse_filtrado1209.Rdata")

tse_unico <- remove_dup(tse_filtrado)

#o que já foi entregue
load("C:/Users/jvoig/OneDrive/Documentos/fb_final/face_0905.RData")

#o que falta do TSE:
load("tse_restantes.Rdata")

#o arquivo com tudo o que fizemos únicos:
load("face_unico_validado.Rdata" )

#os fb que foram duplicados:
load("todos_duplicados.Rdata")

tse_restantes <- tse_restantes %>%
  select(nr_cpf_candidato, nm_ue,  nm_candidato, nm_urna_candidato, ds_cargo, nr_candidato )

rest_fb <- face_unico_validado1 %>%
  anti_join(face_entraga_0509, by=c("district_id", "figure_id")) %>%
  bind_rows(todos_duplicados)

# write.xlsx(as.data.frame(rest_fb), file="rest_fb_teste.xlsx", row.names=FALSE)
#descobri que em uma boa parte dos duplicados está tudo certo, só preciso desduplicar e juntar
# por nome e figure_id

#primeiro, vou pegar de volta aqueles que eu corrigi na mão:
correcoes_manuais <- read_excel("C:/Users/jvoig/OneDrive/Documentos/fb_final/correcoes_manuais.xlsx")

correcoes_manuais <- correcoes_manuais %>%
  mutate_all(as.character)

#como eles sairam do rest_fb , eu vou tirar eles de lá e aplicar o critério no restante

rest_fb1 <- rest_fb %>%
  anti_join(correcoes_manuais, by=c("figure_name")) %>% #menos 22, está certo
  left_join(tse_restantes, by=c("figure_name" = "nm_candidato",
                                "figure_id" = "nr_candidato",
                                "nome_urna" = "nm_urna_candidato")) %>% #não duplicou
  distinct(figure_name, figure_id, url, district_id, .keep_all=TRUE)

entrega_12_09 <- rest_fb1 %>%
  filter(!is.na(nr_cpf_candidato.y)) %>%
  group_by(nr_cpf_candidato.y) %>%
  mutate(dup = n(),
         remover = ifelse(dup == 2 & url == "ne", 1, 0)) %>%
  filter(remover == 0)

preciso_verificar <- rest_fb1 %>%
  filter(is.na(nr_cpf_candidato.y))

# esses que tÊm 2, vou ter que fazer na mão.

# write.xlsx(as.data.frame(entrega_12_09), file="entrega_12_09_para_correcao.xlsx",
#            row.names = FALSE)

entrega_12_09_correcao_rev1 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/fb_final/entrega_12_09_correcao_rev1.xlsx")

#alguns eu já fiz na mão, outros eu vou precisar voltar a conferir amanhã:

falta <- entrega_12_09_correcao_rev1  %>%
  filter(dup != 1)

entrega_12_09_final <- entrega_12_09_correcao_rev1 %>% 
  filter(dup == 1) %>%
  select(-c(remover, dup, ds_cargo, nm_ue, nome_urn_igua_end , nome_urn_igua2, nome_urn_igua1,
            dup_end, dup2, dup1, nr_cpf_candidato.x )) %>%
  rename(nr_cpf_candidato = nr_cpf_candidato.y) %>%
  bind_rows(correcoes_manuais)

#Vou juntar com os caras que já entregamos e dar inner join com o TSE de hoje para retirar os
# que foram postos como inaptos:

tse_controle <- tse_unico %>%
  select(nr_cpf_candidato) %>%
  mutate(tse_deferido = "sim")

full_12_09_final <- entrega_12_09_final %>%
  bind_rows(face_entraga_0509) %>% #26105
  left_join(tse_controle) %>% #25769
  mutate(tse_deferido = ifelse(!is.na(figure_id)& is.na(tse_deferido), "não", tse_deferido))

full_12_09_final %>%
  group_by(tse_deferido) %>%
  summarise(n())

full_12_09_final_approved <- full_12_09_final %>%
  filter(tse_deferido == "sim") %>%
  select(-c(tse_deferido))

save(full_12_09_final_approved, file="full_12_09_final_approved.Rdata")
write.csv(full_12_09_final_approved, file="full_12_09_final_approved.csv" , row.names = FALSE,
          sep=";")

# Preparando os arquivos para amanha:

load("C:/Users/jvoig/OneDrive/Documentos/fb_final/1209/face_todos.Rdata")

perfis_nao_entregues <- face_todos %>%
  anti_join(full_12_09_final_approved, by=c("figure_id", "figure_name", "district_id")) %>%
  bind_rows(falta) %>%
  distinct(district_id, ocd_id, district_type, office_title, figure_name,
           nome_urna, figure_id, party_id, url) %>%
  group_by(district_id, ocd_id, district_type, office_title, figure_name,
           nome_urna, figure_id, party_id) %>%
  mutate(dup = n(),
         remover = ifelse(dup != 1 & url == "ne", 1, 0)) %>%
  ungroup() %>%
  filter(remover == 0) %>%
  group_by(district_id, ocd_id, district_type, office_title, figure_name,
           nome_urna, figure_id, party_id) %>%
  mutate(dup = n())

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\fb_final\\1309")
  
save(perfis_nao_entregues, file="perfis_nao_entregues.Rdata")
