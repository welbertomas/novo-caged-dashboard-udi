#=================================================
# Atualiza IPC-CEPES
# Baixar e organizar o arquivo do IPC CEPES
# É executado indiretamente no DB_principal
#=================================================
library(httr)
library(readxl)
library(dplyr)
library(openxlsx)

# Converte "202602" -> "2026_02"
sufixo <- paste0(substr(MES_ATUAL, 1, 4), "_", substr(MES_ATUAL, 5, 6))

# Cria nomes -----
base_url <- "https://www.ieri.ufu.br/system/files/conteudo/"
prefixo  <- "cepes_op_ipc_cepes_serie_historica_agregada_n_indice_1994_"
destino  <- file.path(DIR_RAW, paste0(prefixo, sufixo, ".xlsx"))

# Exclui anterior -----
ipc_existente <- Sys.glob(file.path(DIR_RAW, paste0(prefixo, "*.xlsx")))
unlink(ipc_existente)

# Baixar arquivo -----
url <- paste0(base_url, prefixo, sufixo, ".xlsx")
message("Baixando: ", url)
resp <- GET(url, timeout(30))
if (status_code(resp) == 200) {
  writeBin(content(resp, "raw"), destino)
  message("Arquivo salvo em: ", destino)
} else {
  stop("Erro HTTP ", status_code(resp), " — verifique se o arquivo já está disponível:\n", url)
}

# Ler arquivo -----
ipc <- read_excel(
  destino,
  sheet     = "N_Índice_IPC-CEPES_b",
  col_names = FALSE,
  skip      = 5
)

# Transformar mês em número -----
meses <- c(
  JAN = "01", FEV = "02", MAR = "03", ABR = "04",
  MAI = "05", JUN = "06", JUL = "07", AGO = "08",
  SET = "09", OUT = "10", NOV = "11", DEZ = "12"
)

# Organizar arquivo -----
numeroindice <- ipc |>
  select(ano = 1, mes = 2, numeroindice = 3) |>
  filter(
    !is.na(numeroindice),
    mes %in% names(meses)
  ) |>
  mutate(
    anomes       = paste0(as.integer(ano), meses[mes]),
    numeroindice = as.numeric(numeroindice)
  ) |>
  select(anomes, numeroindice)

# Exportar -----
write.xlsx(list(numeroindice = numeroindice), destino, overwrite = TRUE)