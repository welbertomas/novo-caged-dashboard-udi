# ============================================================
# DB_extracao.R
# Constrói base completa do município alvo (microdados)
# Deve ser executado apenas uma vez
# ============================================================

source("config.R")

library(data.table)
library(curl)
library(archive)
library(dplyr)

BASE_URL <- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED"

ANOS  <- 2020:2026
MESES <- sprintf("%02d", 1:12)
TIPOS <- c("CAGEDEXC", "CAGEDFOR", "CAGEDMOV")

dir.create(DIR_RAW, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_DATA, showWarnings = FALSE, recursive = TRUE)

temp_dir <- tempfile()
dir.create(temp_dir)

dados_lista <- list()
contador <- 0
erros <- 0

montar_url <- function(tipo, ano, mes){
  
  anomes <- paste0(ano, mes)
  arquivo <- paste0(tipo, anomes, ".7z")
  
  url <- paste0(BASE_URL,"/",ano,"/",anomes,"/",arquivo)
  
  list(url=url, arquivo=arquivo)
}

for(ano in ANOS){
  for(mes in MESES){
    for(tipo in TIPOS){
      
      cat("Processando:", tipo, ano, mes,"\n")
      
      info <- montar_url(tipo,ano,mes)
      destino <- file.path(temp_dir, info$arquivo)
      
      try({
        
        curl_download(info$url, destino, quiet=TRUE)
        
        conteudo <- archive(destino)
        
        txt <- conteudo$path[grepl("\\.txt$",conteudo$path)]
        
        archive_extract(destino, dir=temp_dir)
        
        caminho_txt <- file.path(temp_dir, txt[1])
        
        dt <- fread(
          caminho_txt,
          sep=";",
          encoding="UTF-8",
          showProgress=FALSE
        )
        
        dt <- dt[município == MUNICIPIO_ALVO]
        
        if(tipo=="CAGEDEXC"){
          dt$saldomovimentação <- dt$saldomovimentação * -1
        }
        
        vars <- c("salário","horascontratuais","valorsaláriofixo")
        
        for(v in vars){
          
          if(v %in% names(dt)){
            dt[[v]] <- as.numeric(gsub(",",".",dt[[v]]))
          }
          
        }
        
        dados_lista[[length(dados_lista)+1]] <- dt
        
        contador <- contador + 1
        
        file.remove(destino)
        file.remove(caminho_txt)
        
      }, silent=TRUE)
      
    }}
  
}

dados <- rbindlist(dados_lista, fill=TRUE)

cat("====================================\n")
cat("Arquivos importados:",contador,"\n")
cat("Total observações:",nrow(dados),"\n")
cat("====================================\n")

saveRDS(
  dados,
  file=file.path(DIR_DATA,"CAGED_completo.rds"),
  compress="xz"
)

cat("✓ Arquivo salvo em data_processed\n")
