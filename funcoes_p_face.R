#3 Fun??es Facebook

# corrige estados errados

# fun??o auxiliar
gera_troca_uf <- function() {
  
  uf <- "Acre, Alagoas, Amapá, Amazonas, Bahia, Ceará, Federal District, Espírito Santo, Goiás, Maranhão, Mato Grosso, Mato Grosso do Sul, Minas Gerais, Pará, Paraíba, Paraná, Pernambuco, Piauí, Rio de Janeiro, Rio Grande do Norte, Rio Grande do Sul, Rondônia, Roraima, Santa Catarina, São Paulo, Sergipe, Tocantins"
  uf <- unlist(str_split(uf, ", "))
  sigla <- c("AC", "AL", "AP", "AM", "BA", "CE","DF", "ES",
             "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR",
             "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC",
             "SP", "SE", "TO")
  df_uf <- data.frame(uf=uf, sigla=sigla, sigla_lower = tolower(sigla), stringsAsFactors = F)
  
}

# fun??o principal
# retorna mensagem de erro
# e data.frame com banco com os casos errados
corrige_uf_errada <- function(df_face) {
  stopifnot(require(stringr))
  stopifnot(require(dplyr))
  
  df_uf <- gera_troca_uf() 
  
  # olhar sigla NA
  face_valid <- df_face %>%
    mutate(sigla = gsub("ed:", "", str_extract(df_face$ocd_id, "ed:.*"))) %>%
    left_join(df_uf, by = c("sigla" = "sigla_lower")) %>%
    mutate(uf_base_ascii = iconv(uf, from="UTF-8", to="ASCII//TRANSLIT")) %>%
    mutate(uf_ascii = iconv(district_id, from="UTF-8", to="ASCII//TRANSLIT"),
           erro = if_else(uf_ascii != uf_base_ascii, T, F)) %>%
    mutate(district_id = ifelse(erro, uf, district_id)) %>%
    select(-c(uf_ascii, uf, sigla.y ))
  
  x <- face_valid %>%
    filter(erro == 1)
  x <- nrow(x)
  
  print(paste("foram corrigidas", x, "linhas com UF erradas"))
  
  face_valid1 <- face_valid %>%
    select(-erro)
  
}

# muda nomes das colunas (de state para district_id etc.)
muda_nome_coluna <- function(df_face) {
  
  if (sum(grepl("district_name", names(df_face))) == 0) {
    df_face <- df_face %>%
      rename(district_name = State) %>%
      mutate(district_id = district_name)
  }
  
  if (sum(grepl("figure_id", names(df_face))) == 0) {
    df_face <- df_face %>%
      rename(figure_id = numero)
  }
  
  if (sum(grepl("party_id", names(df_face))) == 0) {
    df_face <- df_face %>%
      rename(party_id = sigla_partido,
             party = nome_partido)
  }
  
  if (sum(grepl("office_title", names(df_face))) == 0) {
    df_face <- df_face %>%
      rename(office_title = title)
  }
  
  if (sum(grepl("figure_name", names(df_face))) == 0) {
    df_face <- df_face %>%
      rename(figure_name = nome_completo)
  }
  
  return(df_face)
}


# # remove inpato e suplentes do TSE
# processa_cand_tse <- function(df_tse) {
#   df_tse1 <- df_tse %>%
#     filter(DS_SITUACAO_CANDIDATURA != "INAPTO") %>%
#     filter(!grepl("SUPLENTE", DS_CARGO))
#   
#   print(paste("o banco original tinha", nrow(df_tse),
#               "linhas e ficou com", nrow(df_tse1), "linhas"))
#   return(df_tse1)
#   
# }


# imp?rtar base do TSE
# importa_lista_csv <- function(padrao_import = "csv",
#                               padrao_filtro = "ZZ",
#                               colClasses_tipo = NULL,
#                               pasta = getwd()) {
#   setwd(pasta)
#   # precisa j? est? na pasta correta
#   
#   # carrega biblios necess?rias
#   stopifnot(require(dplyr))
#   stopifnot(require(purrr))
#   stopifnot(require(data.table))
#   
#   # lista arquivos a importar
#   arq <- list.files(pattern = padrao_import)
#   arq <- arq[!grepl(padrao_filtro, arq)]
#   
#   # importa arquivos
#   df <- arq %>%
#     purrr::map(function(x) {
#       data.table::fread(x, colClasses = colClasses_tipo)
#     }) %>%
#     purrr::reduce(rbind)
# }





