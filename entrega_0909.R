## N?meros
library(data.table)
library(dplyr)
library(janitor)
library(stringr)

#setwd("C:\\Users\\mgaldino\\2018\\Facebook")

# carrega dados que pedi pra fazer (fichas extras)
ultima_leva <- fread("candidatos_finais_040918.csv") %>%
  filter(!is.na(ano_eleicao))


## carrega base do TSE (pode atualizar por uma nova, mas carrega essa aqui para replicar primeiro)
load("C:/Users/jvoig/OneDrive/Documentos/facebook_election_files/Repositório de candidatos TSE/0309/tse_filtrado0309.Rdata")

# filtra inapto e muda nome do estado pra dar join correto
tse_filtrado <- processa_cand_tse(tse) %>%
  clean_names() %>%
  mutate(nome_ue = stringr::str_to_title(nm_ue)) %>%
  mutate(nome_ue = gsub(" Do", " do", nome_ue)) %>%
  mutate(nome_ue = gsub(" De", " de", nome_ue),
         nome_ue = gsub("Distrito Federal", "Federal District", nome_ue),
         nome_ue = str_trim(nome_ue)) %>%
  filter(ds_cargo != "VICE-GOVERNADOR")

# remove dup do TSE

remove_dup <- function(tse) {
  tse1 <- tse %>%
    mutate(dup1 = duplicated(nr_cpf_candidato),
           dup2 = duplicated(nr_cpf_candidato, fromLast=T),
           dup_end = ifelse(dup1, T, 
                            ifelse(dup2, T, F))) %>%
    filter(!dup_end)
  print(paste("foram removidas", nrow(tse) - nrow(tse1)))
  return(tse1)
}

tse_unico <- remove_dup(tse_filtrado)

## carrega ocd_id

ocdid_brazil <- fread("ocdid_brazil.csv", encoding="UTF-8") %>%
  mutate(State1 = iconv(State, from="UTF-8", to="ASCII//TRANSLIT"))

## carrega ?ltimo arquivo enviado pro FACE
face_ultimo <- fread("entrega_0309.csv", encoding="UTF-8")

# carrega vice presidentes (nao esta no ultimo enviado)
face_vice_pres <- fread("entrega_0309_vice_president.csv", encoding="UTF-8")

# carrega presidente, senador e gov
face_pres_sen_gov <- fread("face_full_so_far.csv", encoding="UTF-8") %>%
  filter(office_title %in% c("Senador", "PRESIDENTE", "Governador do estado"))

# senadores que faltavam
sen_faltante <- fread("SENADORES_FALTANTES.csv",  encoding="UTF-8") %>%
  mutate(district_name = district_id) %>%
  mutate_all(as.character)

# muda nomes das colunas (de state pra distric_id etc.)
# filtra colunas que preciso pra enviar pro FACE
face_ultimo <- face_ultimo %>%
  muda_nome_coluna() %>%
  select(names(face_pres_sen_gov))

face_vice_pres <- face_vice_pres %>%
  muda_nome_coluna() %>%
  select(names(face_pres_sen_gov))

ultima_leva1 <- ultima_leva %>%
  mutate(State = nome_ue,
         numero = nr_candidato,
         sigla_partido = sg_partido,
         nome_partido = nm_partido,
         office_title = stringr::str_to_title(ds_cargo),
         nome_completo = nm_candidato,
         nome_urna = nm_urna_candidato,
         url = url1) %>%
  muda_nome_coluna() %>%
  mutate_all(as.character) %>%
  mutate(office_title = gsub("Estadual", "do estado", office_title),
         office_title = gsub("Distrital", "do estado", office_title),
         office_title = gsub(" Federal", "", office_title)) %>%
  left_join(ocdid_brazil, by=c("district_id" = "State1", "office_title" = "title")) %>%
    select(names(face_pres_sen_gov))

###
#gerando arquivo ?nico pro FACE
face_todos <- bind_rows(face_ultimo, face_vice_pres,
                        face_pres_sen_gov, ultima_leva1, sen_faltante) %>%
  mutate(district_id = iconv(district_id, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
  mutate(district_id = ifelse(grepl("BRASIL", district_id), "Brasil", district_id))

remove_dup_face <- function(face) {
  face <- face %>%
    mutate(id_nome_num_estado_cargo = paste(figure_name, figure_id, office_title, district_id, sep="_"),
           dup1 = duplicated(id_nome_num_estado_cargo),
           dup2 = duplicated(id_nome_num_estado_cargo, fromLast=T),
           dup_end = ifelse(dup1, T, 
                            ifelse(dup2, T, F)))
  
  face1 <- face %>%
    filter(!dup_end)
  
  face2 <- face %>%
    filter(dup_end)
  
  print(paste("foram removidas", nrow(face) - nrow(face1)))
  
  return(list(face1, face2))
}

face_remove <- remove_dup_face(face_todos)

face_unico <- face_remove[[1]] %>%
  select(names(face_pres_sen_gov)) %>%
  mutate(office_title = gsub("VICE-PRESIDENTE", "Vice-Presidente", office_title),
         office_title = gsub("PRESIDENTE", "Presidente", office_title)) %>%
  mutate_all(as.character)

face_dup <- face_remove[[2]] %>%
  select(names(face_pres_sen_gov)) %>%
  mutate(office_title = gsub("VICE-PRESIDENTE", "Vice-Presidente", office_title),
         office_title = gsub("PRESIDENTE", "Presidente", office_title)) %>%
  mutate_all(as.character)

## gerando face validado
tse_unico <- tse_unico %>%
  mutate(nome_cargo = stringr::str_to_title(ds_cargo),
         nome_cargo = ifelse(grepl('Vice', nome_cargo), "Vice-Presidente", nome_cargo),
         nome_cargo = gsub("Estadual", "do estado", nome_cargo),
         nome_cargo = gsub("Distrital", "do estado", nome_cargo),
         nome_cargo = gsub(" Federal", "", nome_cargo),
         nome_cargo = gsub("Governador", "Governador do estado", nome_cargo)) %>%
  mutate_all(as.character)

face_unico_validado <- face_unico %>%
  inner_join(tse_unico, by = c("figure_id" = "nr_candidato", 
                               "district_id" = "nome_ue", 
                               "office_title" = "nome_cargo")) %>%
  mutate(party = ifelse(is.na(party), nm_partido, party)) %>%
  select(names(face_pres_sen_gov), nr_cpf_candidato) %>%
  mutate(dup1 = duplicated(nr_cpf_candidato),
         dup2 = duplicated(nr_cpf_candidato, fromLast=T),
         dup_end = ifelse(dup1, T, 
                          ifelse(dup2, T, F))) 

## arquivo correto, sem duplicados nem nada
face_unico_validado1 <- face_unico_validado %>%
  filter(!dup_end)

# arquivo com cpf duplicados, Corrigindo
face_unico_validado2 <- face_unico_validado %>%
  filter(dup_end)


face_unico_validado3 <- face_unico_validado2 %>%
  mutate(nr_cpf_candidato = ifelse(grepl("ALVARO FERREIRA", figure_name), "01389382885", nr_cpf_candidato ),
         nr_cpf_candidato = ifelse(grepl("JOSE CARLOS DOS SANTOS", figure_name), "58942300804", nr_cpf_candidato )) %>%
  mutate(nome_urn_igua1 = duplicated(paste(nr_cpf_candidato, nome_urna)),
         nome_urn_igua2 = duplicated(paste(nr_cpf_candidato, nome_urna), fromLast = T),
         nome_urn_igua_end = ifelse(nome_urn_igua1, T, 
                                    ifelse(nome_urn_igua2, T, F)))

face_unico_volta <- face_unico_validado3 %>%
  filter((nome_urn_igua2 & nome_urn_igua_end) |
           grepl("D'ARC", nome_urna)) %>%
  select(names(face_unico_validado1))

face_unico_validado1 <- bind_rows(face_unico_validado1, 
                                  face_unico_volta)

# face duplicado
 
face_dup_validado <- face_dup %>%
  inner_join(tse_unico, by = c("figure_id" = "nr_candidato", 
                               "district_id" = "nome_ue", 
                               "office_title" = "nome_cargo")) %>%
  select(names(face_pres_sen_gov), nr_cpf_candidato) %>%
  mutate(dup1 = duplicated(nr_cpf_candidato),
         dup2 = duplicated(nr_cpf_candidato, fromLast=T),
         dup_end = ifelse(dup1, T, 
                          ifelse(dup2, T, F))) 

face_dup_validado %>%
  summarise(num_cpf_unico = n_distinct(nr_cpf_candidato))
# 1008

checagem <- tse_unico  %>%
  anti_join(bind_rows(face_dup_validado, face_unico_validado1, face_unico_validado2), by = "nr_cpf_candidato")

dim(checagem)
# 68
## Entrega
face_entraga_0509 <- face_unico_validado1 %>%
  remove_dup() %>%
  select(names(face_unico_validado1)[-c(13, 14, 15)])

tse_unico %>%
  summarise(n_distinct(nr_cpf_candidato))


face_entraga_0509 %>%
  summarise(n_distinct(nr_cpf_candidato),
            n())

tse_unico %>%
  group_by(ds_cargo) %>%
  summarise(n_distinct(nr_cpf_candidato))

tse_unico %>%
  group_by(nome_cargo) %>%
  summarise(n_distinct(nr_cpf_candidato))

face_entraga_0509 %>%
  group_by(office_title) %>%
  summarise(n_distinct(nr_cpf_candidato), n()) %>%
  adorn_totals("row")

## 
write.table(face_entraga_0509, file="face_0905.csv",
            sep=",", fileEncoding="UTF-8", row.names=F)

save(face_entraga_0509, file="face_0905.RData")

write.table(face_entraga_0509, file="face_0905.csv",
            sep=",", fileEncoding="UTF-8", row.names=F)

library(readr)
library(jsonlite)
library(rjson)

face_entraga_0509 %>% 
  toJSON() %>%
  write_lines(path="face_0905.json")


# valida que salvoucerto em json
json_data <- rjson::fromJSON(file="face_0905.json")
head(json_data)

View(face_entraga_0509)

# valida que n?o tem NA
face_entraga_0509 %>%
  summarise(nome_na = sum(is.na(figure_name)),
            nome_urna_na = sum(is.na(nome_urna)),
            id_na = sum(is.na(figure_id)),
            district_id = sum(is.na(district_id)),
            district_name_na = sum(is.na(district_name)),
            office_na = sum(is.na(office_title)),
            party_id_na = sum(is.na(party_id)),
            party_na = sum(is.na(party)),
            ocd_id_na = sum(is.na(ocd_id)),
            url_na = sum(is.na(url)))


