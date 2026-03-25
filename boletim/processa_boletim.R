# ---------------------------------------------------------------------------- #
#                           Configuração Inicial                               #
# ---------------------------------------------------------------------------- #

# Instalar e carregar pacotes necessários
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("haven", quietly = TRUE)) install.packages("haven")
if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
if (!requireNamespace("rmarkdown", quietly = TRUE)) install.packages("rmarkdown")
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales") # Para formatação de números e moedas
if (!requireNamespace("kableExtra", quietly = TRUE)) install.packages("kableExtra") # Para tabelas mais bonitas no PDF
if (!requireNamespace("forcats", quietly = TRUE)) install.packages("forcats") # Para manipulação de fatores em gráficos

library(tidyverse) 
library(readxl)
library(haven) # Para ler arquivos .dta do Stata
library(knitr)
library(rmarkdown)
library(here) # Para gerenciar caminhos de arquivo de forma robusta
library(lubridate) # Para manipulação de datas
library(scales) # Para formatação de números
library(kableExtra) # Para tabelas avançadas
library(forcats) # Para manipulação de fatores em gráficos

# ---------------------------------------------------------------------------- #
#                             Definir Parâmetros Globais                       #
# ---------------------------------------------------------------------------- #
# Estes parâmetros replicam as definições de 'global' no Stata.
global_periodo_referencia <- "202601" # Mês de referência no formato AAAAMM
global_arquivo_tabelas <- "Boletim Mensal Jan.2026.xlsx" # Nome do arquivo de saída (aqui usado para referência de nome)

sm_2025 <- 1518 # Salário mínimo 2025
sm_2026 <- 1621 # Salário mínimo 2026

# Calcular 'periodo_12meses' (lógica do Stata)
periodo_referencia_num <- as.numeric(global_periodo_referencia)
if (periodo_referencia_num %% 100 == 12) { # Se o mês for dezembro (resto da divisão por 100 é 12)
  periodo_12meses <- periodo_referencia_num - 100 # Ano anterior, mesmo mês (ex: 202512 -> 202412)
} else { # Se for janeiro a novembro
  periodo_12meses <- periodo_referencia_num - 99 # Mês do ano anterior (ex: 202501 -> 202402, 202502 -> 202403)
}

# Definir o diretório de trabalho onde os arquivos estão localizados.
setwd("C:/Users/55349/Desktop/Boletim 2026") 

# Configuração robusta de encoding no início do script
Sys.setenv(LANG = "pt_BR.UTF-8")
options(encoding = "UTF-8")
options(scipen = 999)  # Evitar notação científica

# Configuração de locale mais robusta
if (Sys.info()['sysname'] == "Windows") {
  locale_options <- c("Portuguese_Brazil.UTF-8", "pt_BR.UTF-8", "Portuguese_Brazil.1252")
} else {
  locale_options <- c("pt_BR.UTF-8", "pt-BR.UTF-8", "pt_BR")
}

for (locale in locale_options) {
  if (try(Sys.setlocale("LC_ALL", locale), silent = TRUE) != "C") {
    break
  }
}
# ---------------------------------------------------------------------------- #
#                      Extrair e Carregar Dados                                #
# ---------------------------------------------------------------------------- #

# Função para carregar e pré-processar arquivos CAGED do mês de referência (.txt)
# Todas as variáveis serão numéricas, exceto 'seção' que será caractere,
# e vírgulas serão interpretadas como separadores decimais.
load_caged_txt <- function(file_path, type) {
  df <- read_delim(file_path, 
                   delim = ";", 
                   locale = locale(decimal_mark = ",", encoding = "UTF-8"), # !! NOVIDADE: Interpreta vírgulas como decimais !!
                   col_types = cols( # !! NOVIDADE: Define tipos de colunas explicitamente !!
                     .default = col_double(), # A maioria será numérica por padrão
                     seção = col_character()  # 'seção' será caractere
                     # Se houver outras colunas que DEVEM ser caractere, adicione-as aqui.
                     # Ex: município = col_character(), se o ID do município não for sempre numérico puro.
                     # Mas, segundo sua instrução, a maioria deve ser numérica.
                   )) %>%
    filter(município == 317020) # Filtrar para Uberlândia/MG (município como numérico)
  
  # Ajuste específico para CAGEDEXC (excluídos)
  if (type == "EXC") {
    df <- df %>%
      mutate(saldomovimentação = saldomovimentação * (-1))
  }
  return(df)
}

# Carregar dados do mês de referência (assumindo que os TXT estão no mesmo diretório)
df_mov_current <- load_caged_txt(paste0("CAGEDMOV", global_periodo_referencia, ".txt"), "MOV")
df_for_current <- load_caged_txt(paste0("CAGEDFOR", global_periodo_referencia, ".txt"), "FOR")
df_exc_current <- load_caged_txt(paste0("CAGEDEXC", global_periodo_referencia, ".txt"), "EXC")

# Carregar o arquivo CAGEDUberlandia.dta (base histórica)
df_caged_hist <- read_dta("CAGEDUberlandia.dta") %>%
  # Converter explicitamente apenas 'seção' para caractere, para garantir que bata
  # com o tipo que 'load_caged_txt' define para ela.
  # As outras colunas do .dta (que são numéricas no Stata) já devem ser importadas
  # corretamente como numéricas pelo 'read_dta'.
  mutate(seção = as.character(seção))

# Remover dados do período de referência de df_caged_hist para evitar duplicatas, 
# caso o .dta já contenha o mês atual por alguma execução anterior do Stata.
df_caged_hist <- df_caged_hist %>%
  filter(competênciamov != periodo_referencia_num)

# Combinar todos os data frames - AGORA COM TIPOS CONSISTENTES E LIMPOS!
df_caged <- bind_rows(df_caged_hist, df_mov_current, df_for_current, df_exc_current)

# Importar IPC CEPES (assumindo que ipc.xlsx está no mesmo diretório)
df_ipc <- read_excel("ipc.xlsx", sheet = "Série Histórica INPC", 
                     col_types = c("numeric", "numeric", "numeric")) 

# Garantir que 'competênciamov' no IPC também é numérico
df_ipc <- df_ipc %>% mutate(competênciamov = as.numeric(competênciamov))

# Merge com IPC e filtro de período
df_caged_merged <- df_caged %>%
  inner_join(df_ipc, by = "competênciamov") %>%
  filter(competênciamov >= periodo_12meses)

# ---------------------------------------------------------------------------- #
#                             Criar Variáveis Derivadas                        #
# ---------------------------------------------------------------------------- #

df_processed <- df_caged_merged %>%
  # 1. Admissões e Demissões
  mutate(
    admissoes = if_else(saldomovimentação == 1, saldomovimentação, 0),
    demissoes = if_else(saldomovimentação == -1, saldomovimentação, 0)
  ) %>%
  # 2. Mês, Ano e Data Completa
  mutate(
    ano = floor(competênciamov / 100),
    mês_num = competênciamov %% 100,
    mês = case_when(
      mês_num == 1 ~ "Janeiro", mês_num == 2 ~ "Fevereiro", mês_num == 3 ~ "Março",
      mês_num == 4 ~ "Abril", mês_num == 5 ~ "Maio", mês_num == 6 ~ "Junho",
      mês_num == 7 ~ "Julho", mês_num == 8 ~ "Agosto", mês_num == 9 ~ "Setembro",
      mês_num == 10 ~ "Outubro", mês_num == 11 ~ "Novembro", mês_num == 12 ~ "Dezembro",
      TRUE ~ NA_character_
    ),
    data = paste0(mês, "/", ano)
  ) %>%
  # Converter 'data' para fator ordenado para gráficos de série temporal
  arrange(competênciamov) %>%
  mutate(data = factor(data, levels = unique(data))) %>%
  # 3. Setor
  mutate(
    setores = case_when(
      seção == "A" ~ "Agropecuária",
      seção %in% c("B", "C", "D", "E") ~ "Indústria",
      seção == "F" ~ "Construção",
      seção == "G" ~ "Comércio",
      seção %in% c("H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U") ~ "Serviços",
      seção == "Z" ~ "Não Identificado",
      TRUE ~ NA_character_
    )
  ) %>%
  # 4. Porte
  mutate(
    class_porte = case_when(
      seção %in% c("B", "C", "D", "E", "F") ~ 1,
      seção == "O" ~ 2,
      TRUE ~ 0 # Default para 0 conforme a primeira linha do Stata
    )
  ) %>%
  mutate(
    tamanho = case_when(
      tamestabjan <= 4 & class_porte == 1 ~ "MEI e Micro",
      tamestabjan <= 3 & class_porte == 0 ~ "MEI e Micro",
      tamestabjan >= 5 & tamestabjan <= 6 & class_porte == 1 ~ "Pequena",
      tamestabjan >= 4 & tamestabjan <= 5 & class_porte == 0 ~ "Pequena",
      tamestabjan >= 7 & tamestabjan <= 8 & class_porte == 1 ~ "Média",
      tamestabjan == 6 & class_porte == 0 ~ "Média",
      tamestabjan >= 9 & tamestabjan <= 10 & class_porte == 1 ~ "Grande",
      tamestabjan >= 7 & tamestabjan <= 10 & class_porte == 0 ~ "Grande",
      class_porte == 2 ~ "Administração Pública",
      tamestabjan > 10 & class_porte != 2 ~ "Ignorado",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    indiceporte = case_when(
      tamanho == "MEI e Micro" ~ 1,
      tamanho == "Pequena" ~ 2,
      tamanho == "Média" ~ 3,
      tamanho == "Grande" ~ 4,
      tamanho == "Administração Pública" ~ 5,
      tamanho == "Ignorado" ~ 6,
      TRUE ~ NA_real_ # Use NA_real_ para numérico
    )
  ) %>%
  # 5. Faixa Etária
  mutate(
    faixa_etaria = case_when(
      idade <= 17 ~ "17 ou menos",
      idade >= 18 & idade <= 24 ~ "18-24",
      idade >= 25 & idade <= 29 ~ "25-29",
      idade >= 30 & idade <= 39 ~ "30-39",
      idade >= 40 & idade <= 49 ~ "40-49",
      idade >= 50 & idade <= 59 ~ "50-59",
      idade >= 60 ~ "60 ou mais",
      TRUE ~ NA_character_
    )
  ) %>%
  # 6. Gênero
  mutate(
    genero = case_when(
      sexo == 1 ~ "Homem",
      sexo == 3 ~ "Mulher",
      sexo == 9 ~ "Não identificado",
      TRUE ~ NA_character_
    )
  ) %>%
  # 7. Escolaridade
  mutate(
    gdi_leg = case_when(
      graudeinstrução == 1 ~ "Analfabeto",
      graudeinstrução %in% c(2, 3, 4) ~ "Fundamental Incompleto",
      graudeinstrução %in% c(5, 6) ~ "Fundamental Completo",
      graudeinstrução %in% c(7, 8) ~ "Médio Completo",
      graudeinstrução == 9 ~ "Superior Completo",
      graudeinstrução %in% c(10, 11, 80) ~ "Pós-Graduação",
      graudeinstrução == 99 ~ "Não Identificado",
      TRUE ~ NA_character_
    )
  ) %>%
  # 8. Divisão de Atividade Econômica
  mutate(
    divisao = floor(subclasse / 100000),
    div_leg = case_when( # Lista exaustiva conforme o Stata do-file
      divisao == 1 ~ "Agricultura, Pecuária E Serviços Relacionados",
      divisao == 2 ~ "Produção Florestal",
      divisao == 3 ~ "Pesca E Aqüicultura",
      divisao == 5 ~ "Extração de Carvão Mineral",
      divisao == 6 ~ "Extração De Petróleo E Gás Natural",
      divisao == 7 ~ "Extração De Minerais Metálicos",
      divisao == 8 ~ "Extração De Minerais Não-Metálicos",
      divisao == 9 ~ "Atividades De Apoio À Extração De Minerais",
      divisao == 10 ~ "Fabricação De Produtos Alimentícios",
      divisao == 11 ~ "Fabricação De Bebidas",
      divisao == 12 ~ "Fabricação De Produtos Do Fumo",
      divisao == 13 ~ "Fabricação De Produtos Têxteis",
      divisao == 14 ~ "Confecção De Artigos Do Vestuário E Acessórios",
      divisao == 15 ~ "Preparação De Couros E Fabricação De Artefatos De Couro, Artigos Para Viagem E Calçados",
      divisao == 16 ~ "Fabricação De Produtos De Madeira",
      divisao == 17 ~ "Fabricação De Celulose, Papel E Produtos De Papel",
      divisao == 18 ~ "Impressão E Reprodução De Gravações",
      divisao == 19 ~ "Fabricação De Coque, De Produtos Derivados Do Petróleo E De Biocombustíveis",
      divisao == 20 ~ "Fabricação De Produtos Químicos",
      divisao == 21 ~ "Fabricação De Produtos Farmoquímicos E Farmacêuticos",
      divisao == 22 ~ "Fabricação De Produtos De Borracha E De Material Plástico",
      divisao == 23 ~ "Fabricação De Produtos De Minerais Não-Metálicos",
      divisao == 24 ~ "Metalurgia",
      divisao == 25 ~ "Fabricação De Produtos De Metal, Exceto Máquinas E Equipamentos",
      divisao == 26 ~ "Fabricação De Equipamentos De Informática, Produtos Eletrônicos E Ópticos",
      divisao == 27 ~ "Fabricação De Máquinas, Aparelhos E Materiais Elétricos",
      divisao == 28 ~ "Fabricação De Máquinas E Equipamentos",
      divisao == 29 ~ "Fabricação De Veículos Automotores, Reboques E Carrocerias",
      divisao == 30 ~ "Fabricação De Outros Equipamentos De Transporte, Exceto Veículos Automotores",
      divisao == 31 ~ "Fabricação De Móveis",
      divisao == 32 ~ "Fabricação De Produtos Diversos",
      divisao == 33 ~ "Manutenção, Reparação E Instalação De Máquinas E Equipamentos",
      divisao == 35 ~ "Eletricidade, Gás E Outras Utilidades",
      divisao == 36 ~ "Captação, Tratamento E Distribuição De Água",
      divisao == 37 ~ "Esgoto E Atividades Relacionadas",
      divisao == 38 ~ "Coleta, Tratamento E Disposição De Resíduos; Recuperação De Materiais",
      divisao == 39 ~ "Descontaminação E Outros Serviços De Gestão De Resíduos",
      divisao == 41 ~ "Construção De Edifícios",
      divisao == 42 ~ "Obras De Infra-Estrutura",
      divisao == 43 ~ "Serviços Especializados Para Construção",
      divisao == 45 ~ "Comércio E Reparação De Veículos Automotores E Motocicletas",
      divisao == 46 ~ "Comércio Por Atacado, Exceto Veículos Automotores E Motocicletas",
      divisao == 47 ~ "Comércio Varejista",
      divisao == 49 ~ "Transporte Terrestre",
      divisao == 50 ~ "Transporte Aquaviário",
      divisao == 51 ~ "Transporte Aéreo",
      divisao == 52 ~ "Armazenamento E Atividades Auxiliares Dos Transportes",
      divisao == 53 ~ "Correio E Outras Atividades De Entrega",
      divisao == 55 ~ "Alojamento",
      divisao == 56 ~ "Alimentação",
      divisao == 58 ~ "Edição E Edição Integrada À Impressão",
      divisao == 59 ~ "Atividades Cinematográficas, Produção De Vídeos E De Programas De Televisão; Gravação De Som E Edição De Música",
      divisao == 60 ~ "Atividades De Rádio E De Televisão",
      divisao == 61 ~ "Telecomunicações",
      divisao == 62 ~ "Atividades Dos Serviços De Tecnologia Da Informação",
      divisao == 63 ~ "Atividades De Prestação De Serviços De Informação",
      divisao == 64 ~ "Atividades De Serviços Financeiros",
      divisao == 65 ~ "Seguros, Resseguros, Previdência Complementar E Planos De Saúde",
      divisao == 66 ~ "Atividades Auxiliares Dos Serviços Financeiros, Seguros, Previdência Complementar E Planos De Saúde",
      divisao == 68 ~ "Atividades Imobiliárias",
      divisao == 69 ~ "Atividades Jurídicas, De Contabilidade E De Auditoria",
      divisao == 70 ~ "Atividades De Sedes De Empresas E De Consultoria Em Gestão Empresarial",
      divisao == 71 ~ "Serviços De Arquitetura E Engenharia; Testes E Análises Técnicas",
      divisao == 72 ~ "Pesquisa E Desenvolvimento Científico",
      divisao == 73 ~ "Publicidade E Pesquisa De Mercado",
      divisao == 74 ~ "Outras Atividades Profissionais, Científicas E Técnicas",
      divisao == 75 ~ "Atividades Veterinárias",
      divisao == 77 ~ "Aluguéis Não-Imobiliários E Gestão De Ativos Intangíveis Não-Financeiros",
      divisao == 78 ~ "Seleção, Agenciamento E Locação De Mão-De-Obra",
      divisao == 79 ~ "Agências De Viagens, Operadores Turísticos E Serviços De Reservas",
      divisao == 80 ~ "Atividades De Vigilância, Segurança E Investigação",
      divisao == 81 ~ "Serviços Para Edifícios E Atividades Paisagísticas",
      divisao == 82 ~ "Serviços De Escritório, De Apoio Administrativo E Outros Serviços Prestados Principalmente Às Empresas",
      divisao == 84 ~ "Administração Pública, Defesa E Seguridade Social",
      divisao == 85 ~ "Educação",
      divisao == 86 ~ "Atividades De Atenção À Saúde Humana",
      divisao == 87 ~ "Atividades De Atenção À Saúde Humana Integradas Com Assistência Social, Prestadas Em Residências Coletivas E Particulares",
      divisao == 88 ~ "Serviços De Assistência Social Sem Alojamento",
      divisao == 90 ~ "Atividades Artísticas, Criativas E De Espetáculos",
      divisao == 91 ~ "Atividades Ligadas Ao Patrimônio Cultural E Ambiental",
      divisao == 92 ~ "Atividades De Exploração De Jogos De Azar E Apostas",
      divisao == 93 ~ "Atividades Esportivas E De Recreação E Lazer",
      divisao == 94 ~ "Atividades De Organizações Associativas",
      divisao == 95 ~ "Reparação E Manutenção De Equipamentos De Informática E Comunicação E De Objetos Pessoais E Domésticos",
      divisao == 96 ~ "Outras Atividades De Serviços Pessoais",
      divisao == 97 ~ "Serviços Domésticos",
      divisao == 99 ~ "Organismos Internacionais E Outras Instituições Extraterritoriais",
      TRUE ~ NA_character_
    )
  ) %>%
  # 9. Salário Deflacionado e Remuneração UDI
  mutate(
    salariodef = if_else(salário > 0 & deflator > 0, salário * deflator, NA_real_)
  ) %>%
  mutate(
    remuneracao_udi_adm = case_when(
      salário > sm_2025 * 0.3 & salário < sm_2025 * 150 & saldomovimentação == 1 & indtrabintermitente != 1 & ano == 2025 ~ salariodef,
      salário > sm_2026 * 0.3 & salário < sm_2026 * 150 & saldomovimentação == 1 & indtrabintermitente != 1 & ano == 2026 ~ salariodef,
      TRUE ~ NA_real_
    ),
    remuneracao_udi_dem = case_when(
      salário > sm_2025 * 0.3 & salário < sm_2025 * 150 & saldomovimentação == -1 & indtrabintermitente != 1 & ano == 2025 ~ salariodef,
      salário > sm_2026 * 0.3 & salário < sm_2026 * 150 & saldomovimentação == -1 & indtrabintermitente != 1 & ano == 2026 ~ salariodef,
      TRUE ~ NA_real_
    )
  ) %>%
  # 10. Saldo total por mês (saldo_mes)
  group_by(competênciamov, data) %>%
  mutate(saldo_mes = sum(saldomovimentação, na.rm = TRUE)) %>%
  ungroup()


# ---------------------------------------------------------------------------- #
#                   Geração de Dados Agregados para Gráficos e Tabelas         #
# ---------------------------------------------------------------------------- #

# ----- Dados para Tabela 1 e Gráfico 1 (Saldo Geral) -----
df_geral_saldo <- df_processed %>%
  group_by(competênciamov, data) %>%
  summarise(
    admissoes = sum(admissoes, na.rm = TRUE),
    demissoes = sum(demissoes, na.rm = TRUE),
    saldomovimentação = sum(saldomovimentação, na.rm = TRUE),
    saldo_mes = first(saldo_mes),
    .groups = 'drop'
  ) %>%
  arrange(competênciamov)


# ----- Dados para Tabela 2 (Saldo por Setor) -----
df_saldo_setor <- df_processed %>%
  filter(setores != "Não Identificado") %>%
  group_by(competênciamov, data, setores) %>%
  summarise(saldomovimentação = sum(saldomovimentação, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = setores, values_from = saldomovimentação, values_fill = 0) %>%
  arrange(competênciamov) %>%
  # Renomeia colunas para ficarem em minúsculas e sem caracteres especiais para facilitar o uso
  # Adicione o nome das colunas esperadas para o pivot_wider para garantir a ordem
  rename(
    Agropecuária = `Agropecuária`, # Exemplo de como garantir que nomes com acentos sejam tratados.
    Indústria = `Indústria`,
    Construção = `Construção`,
    Comércio = `Comércio`,
    Serviços = `Serviços`
  )


# ----- Dados para Tabela 3 (Saldo por Porte) - CORRIGIDO -----

# 1. Calcular o saldo mensal por porte de empresa
# Os nomes das colunas de porte serão os nomes completos ("MEI e Micro", "Pequena", etc.)
df_saldo_porte_mensal <- df_processed %>%
  filter(indiceporte != 6) %>% # Excluir "Ignorado"
  group_by(competênciamov, data, tamanho) %>%
  summarise(saldomovimentação = sum(saldomovimentação, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = tamanho, values_from = saldomovimentação, values_fill = 0) %>%
  arrange(competênciamov)

# 2. Calcular o saldo acumulado para o ano de 2025 (o ano de referência)
df_saldo_porte_acumulado <- df_processed %>%
  filter(ano == floor(periodo_referencia_num / 100), indiceporte != 6) %>%
  group_by(tamanho) %>%
  summarise(saldomovimentação = sum(saldomovimentação, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = tamanho, values_from = saldomovimentação, values_fill = 0) %>%
  # Adicionar colunas de placeholder para compatibilidade com o dataframe mensal
mutate(
  competênciamov = NA_real_, # NA_real_ para garantir tipo numérico
  data = "Acum. 2025" # Rótulo para a linha de acumulado
)



# 3. Definir a ordem e garantir a presença de todas as colunas de porte
# Isso é importante caso algum mês ou o acumulado não tenha dados para um porte específico
standard_porte_cols <- c("MEI e Micro", "Pequena", "Média", "Grande", "Administração Pública")

ensure_cols <- function(df) {
  missing_cols <- setdiff(standard_porte_cols, names(df))
  for (col in missing_cols) {
    df[[col]] <- 0 # Adiciona colunas ausentes com valor 0
  }
  df %>% select(competênciamov, data, all_of(standard_porte_cols))
}

df_saldo_porte_mensal_prepared <- df_saldo_porte_mensal %>% ensure_cols()
df_saldo_porte_acumulado_prepared <- df_saldo_porte_acumulado %>% ensure_cols()

# 4. Combinar os dados mensais com a linha de acumulado
df_saldo_porte <- bind_rows(df_saldo_porte_mensal_prepared, df_saldo_porte_acumulado_prepared) %>%
  # Remover a coluna competênciamov, pois não é exibida na tabela final do documento
  select(-competênciamov) %>%
  # Renomear a coluna 'data' para 'Mês / Ano' para corresponder à Tabela 3 do documento
  rename(`Mês / Ano` = data)


# ----- Dados para Tabela 4 (Tabela sintética de saldos) -----
# Esta tabela é mais complexa e agregada apenas para o 'periodo_referencia'
df_tabela4_base <- df_processed %>%
  filter(competênciamov == periodo_referencia_num)

# Calcula os totais gerais de admissões e desligamentos para o MÊS DE DEZEMBRO INTEIRO
# Estes serão os denominadores para o cálculo das participações relativas
grand_total_admissoes_dez <- sum(df_tabela4_base$admissoes, na.rm = TRUE)
grand_total_desligamentos_dez_abs <- sum(abs(df_tabela4_base$demissoes), na.rm = TRUE)

# Função auxiliar para calcular totais e porcentagens para Tabela 4
# A função irá calcular todas as métricas e não fará o "select" final das colunas,

summarize_tabela4_metrics <- function(df, group_var_name, overall_total_adm, overall_total_dem_abs) {
  df %>%
    group_by(across(all_of(group_var_name))) %>%
    summarise(
      Admissões = sum(admissoes, na.rm = TRUE),
      Desligamentos = sum(demissoes, na.rm = TRUE), # Mantém o sinal negativo como no doc
      Saldo = sum(saldomovimentação, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    ungroup() %>%
    mutate(
      `Part. Relativa no total de admissões do mês (%)` = (Admissões / overall_total_adm) * 100,
      `Part. Relativa no total de desligamentos do mês (%)` = (abs(Desligamentos) / overall_total_dem_abs) * 100
    )
}

# Parte 1: Saldo por Grupamento de atividade econômica
df_tabela4_setores_metrics <- df_tabela4_base %>%
  filter(setores != "Não Identificado") %>%
  summarize_tabela4_metrics("setores", grand_total_admissoes_dez, grand_total_desligamentos_dez_abs)

# Para Tabela 4 (Números absolutos)
df_tabela4_setores <- df_tabela4_setores_metrics %>%
  select(Variáveis = setores, Admissões, Desligamentos, Saldo)

# Parte 2: Saldo por Porte da empresa
df_tabela4_porte_metrics <- df_tabela4_base %>%
  filter(indiceporte != 6) %>% # Excluir "Ignorado"
  summarize_tabela4_metrics("tamanho", grand_total_admissoes_dez, grand_total_desligamentos_dez_abs) %>%
  mutate(tamanho_ordem = case_when( # Ordenar para que apareça na ordem correta
    tamanho == "MEI e Micro" ~ 1,
    tamanho == "Pequena" ~ 2,
    tamanho == "Média" ~ 3,
    tamanho == "Grande" ~ 4,
    tamanho == "Administração Pública" ~ 5,
    TRUE ~ 6
  )) %>%
  arrange(tamanho_ordem) %>%
  select(-tamanho_ordem)

# Para Tabela 4 (Números absolutos)
df_tabela4_porte <- df_tabela4_porte_metrics %>%
  select(Variáveis = tamanho, Admissões, Desligamentos, Saldo)

# Parte 3: Saldo por Faixa Etária
df_tabela4_faixa_etaria_metrics <- df_tabela4_base %>%
  summarize_tabela4_metrics("faixa_etaria", grand_total_admissoes_dez, grand_total_desligamentos_dez_abs) %>%
  mutate(faixa_etaria_ordem = case_when( # Ordenar para que apareça na ordem correta
    faixa_etaria == "17 ou menos" ~ 1,
    faixa_etaria == "18-24" ~ 2,
    faixa_etaria == "25-29" ~ 3,
    faixa_etaria == "30-39" ~ 4,
    faixa_etaria == "40-49" ~ 5,
    faixa_etaria == "50-59" ~ 6,
    faixa_etaria == "60 ou mais" ~ 7,
    TRUE ~ 8
  )) %>%
  arrange(faixa_etaria_ordem) %>%
  select(-faixa_etaria_ordem)

# Para Tabela 4 (Números absolutos)
df_tabela4_faixa_etaria <- df_tabela4_faixa_etaria_metrics %>%
  select(Variáveis = faixa_etaria, Admissões, Desligamentos, Saldo)

# Parte 4: Saldo por Gênero
df_tabela4_genero_metrics <- df_tabela4_base %>%
  filter(genero != "Não identificado") %>%
  summarize_tabela4_metrics("genero", grand_total_admissoes_dez, grand_total_desligamentos_dez_abs)

# Para Tabela 4
df_tabela4_genero <- df_tabela4_genero_metrics %>%
  select(Variáveis = genero, Admissões, Desligamentos, Saldo)


# Parte 5: Saldo por Escolaridade
df_tabela4_escolaridade_metrics <- df_tabela4_base %>%
  filter(gdi_leg != "Não Identificado") %>%
  summarize_tabela4_metrics("gdi_leg", grand_total_admissoes_dez, grand_total_desligamentos_dez_abs) %>%
  mutate(gdi_leg_ordem = case_when( # Ordenar para que apareça na ordem correta
    gdi_leg == "Analfabeto" ~ 1,
    gdi_leg == "Fundamental Incompleto" ~ 2,
    gdi_leg == "Fundamental Completo" ~ 3,
    gdi_leg == "Médio Completo" ~ 4,
    gdi_leg == "Superior Completo" ~ 5,
    gdi_leg == "Pós-Graduação" ~ 6,
    TRUE ~ 7
  )) %>%
  arrange(gdi_leg_ordem) %>%
  select(-gdi_leg_ordem)

# Para Tabela 4- (Nùmeros Absolutos)
df_tabela4_escolaridade <- df_tabela4_escolaridade_metrics %>%
  select(Variáveis = gdi_leg, Admissões, Desligamentos, Saldo)


# ----- Dados para Tabela 5 e Gráfico 5 (Remuneração Geral) -----
df_remuneracao_geral <- df_processed %>%
  group_by(competênciamov, data) %>%
  summarise(
    remuneracao_udi_adm = mean(remuneracao_udi_adm, na.rm = TRUE),
    remuneracao_udi_dem = mean(remuneracao_udi_dem, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(competênciamov)

# Calcular variações mensais e acumuladas para Tabela 5
df_tabela5_data <- df_remuneracao_geral %>%
  mutate(
    `Variação mensal_adm` = (remuneracao_udi_adm / lag(remuneracao_udi_adm) - 1) * 100,
    `Variação mensal_dem` = (remuneracao_udi_dem / lag(remuneracao_udi_dem) - 1) * 100
  )

# Acumulados para Tabela 5 (final da tabela, conforme Stata)
# Variação acumulada de Janeiro a Dezembro do ano de referência (2025)
# Filtrar apenas o ano de referencia para os valores de inicio e fim
first_adm_val <- df_remuneracao_geral %>% 
  filter(competênciamov == (floor(periodo_referencia_num/100) * 100 + 1)) %>% pull(remuneracao_udi_adm)
last_adm_val <- df_remuneracao_geral %>% 
  filter(competênciamov == periodo_referencia_num) %>% pull(remuneracao_udi_adm)
acum_adm_var <- (last_adm_val / first_adm_val - 1) * 100

first_dem_val <- df_remuneracao_geral %>% 
  filter(competênciamov == (floor(periodo_referencia_num/100) * 100 + 1)) %>% pull(remuneracao_udi_dem)
last_dem_val <- df_remuneracao_geral %>% 
  filter(competênciamov == periodo_referencia_num) %>% pull(remuneracao_udi_dem)
acum_dem_var <- (last_dem_val / first_dem_val - 1) * 100


# ----- Dados para Tabela 6 (Remuneração por Setores) -----
df_remuneracao_setor <- df_processed %>%
  filter(setores != "Não Identificado") %>%
  group_by(competênciamov, data, setores) %>%
  summarise(remuneracao_udi_adm = mean(remuneracao_udi_adm, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = setores, values_from = remuneracao_udi_adm, values_fill = NA) %>%
  arrange(competênciamov) %>%
  # Garantir nomes de colunas sem espaços ou caracteres especiais para facilitar o uso no gather/spread
  rename(
    Agropecuária = `Agropecuária`,
    Indústria = `Indústria`,
    Construção = `Construção`,
    Comércio = `Comércio`,
    Serviços = `Serviços`
  )


# Acumulados para Tabela 6
# Variação (dez/jan - 1) * 100 para cada setor do ano de referencia
df_tabela6_acum <- df_remuneracao_setor %>%
  filter(competênciamov == (floor(periodo_referencia_num/100) * 100 + 1) | competênciamov == periodo_referencia_num) %>%
  select(-data) %>%
  # Converter para formato longo para calcular a variação por setor
  pivot_longer(cols = -competênciamov, names_to = "setor", values_to = "value") %>%
  group_by(setor) %>%
  summarise(Acum. = (last(value) / first(value) - 1) * 100, .groups = 'drop') %>%
  pivot_wider(names_from = setor, values_from = Acum.)


# ----- Dados para Tabela 7 (Remuneração por Porte) -----
df_remuneracao_porte <- df_processed %>%
  filter(indiceporte != 6, tamanho != "Administração Pública") %>% # Stata exclui adm publica para grafico 6, mas nao para tabela 7
  group_by(competênciamov, data, tamanho) %>%
  summarise(remuneracao_udi_adm = mean(remuneracao_udi_adm, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = tamanho, values_from = remuneracao_udi_adm, values_fill = NA) %>%
  arrange(competênciamov) %>%
  rename(
    meimicro = `MEI e Micro`,
    pequena = `Pequena`,
    media = `Média`,
    grande = `Grande`
  )

# Acumulados para Tabela 7
df_tabela7_acum <- df_remuneracao_porte %>%
  filter(competênciamov == (floor(periodo_referencia_num/100) * 100 + 1) | competênciamov == periodo_referencia_num) %>%
  select(-data) %>%
  pivot_longer(cols = -competênciamov, names_to = "porte", values_to = "value") %>%
  group_by(porte) %>%
  summarise(Acum. = (last(value) / first(value) - 1) * 100, .groups = 'drop') %>%
  pivot_wider(names_from = porte, values_from = Acum.)


# ----- Dados para Gráfico 2 (Saldo por Setor e Porte) -----
df_grafico2_data <- df_tabela4_base %>%
  filter(tamanho != "Administração Pública", setores != "Não Identificado") %>%
  group_by(setores, tamanho) %>%
  summarise(saldomovimentação = sum(saldomovimentação, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    # Ordenar 'tamanho' para o gráfico (como 'sort(tamestabjan)' no Stata)
    tamanho_ordem = case_when(
      tamanho == "MEI e Micro" ~ 1,
      tamanho == "Pequena" ~ 2,
      tamanho == "Média" ~ 3,
      tamanho == "Grande" ~ 4,
      TRUE ~ 5 # Para qualquer outro, caso exista
    )
  ) %>%
  arrange(setores, tamanho_ordem) %>%
  mutate(tamanho = factor(tamanho, levels = unique(tamanho))) # Converter para fator para manter a ordem

# ----- Dados para Gráfico 3 (Saldo por Faixa Etária) -----
df_grafico3_data <- df_tabela4_base %>%
  group_by(faixa_etaria) %>%
  summarise(saldomovimentação = sum(saldomovimentação, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    faixa_etaria_ordem = case_when(
      faixa_etaria == "17 ou menos" ~ 1,
      faixa_etaria == "18-24" ~ 2,
      faixa_etaria == "25-29" ~ 3,
      faixa_etaria == "30-39" ~ 4,
      faixa_etaria == "40-49" ~ 5,
      faixa_etaria == "50-59" ~ 6,
      faixa_etaria == "60 ou mais" ~ 7,
      TRUE ~ 8
    )
  ) %>%
  arrange(faixa_etaria_ordem) %>%
  mutate(faixa_etaria = factor(faixa_etaria, levels = unique(faixa_etaria)))

# ----- Dados para Gráfico 4 (Saldo por Gênero e Grau de Instrução) -----
df_grafico4_data <- df_tabela4_base %>%
  group_by(genero, gdi_leg) %>%
  summarise(saldomovimentação = sum(saldomovimentação, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    gdi_leg_ordem = case_when( # Ordem para o grau de instrução
      gdi_leg == "Analfabeto" ~ 1,
      gdi_leg == "Fundamental Incompleto" ~ 2,
      gdi_leg == "Fundamental Completo" ~ 3,
      gdi_leg == "Médio Completo" ~ 4,
      gdi_leg == "Superior Completo" ~ 5,
      gdi_leg == "Pós-Graduação" ~ 6,
      TRUE ~ 7 # Não Identificado
    )
  ) %>%
  arrange(genero, gdi_leg_ordem) %>%
  mutate(gdi_leg = factor(gdi_leg, levels = unique(gdi_leg)))

# ----- Dados para Gráfico 5 (Salário médio real de admissão ao longo do tempo) -----
df_grafico5_data <- df_remuneracao_geral %>%
  filter(competênciamov >= periodo_12meses)

# ----- Dados para Gráfico 6 (Salário de Admissão por Setor e Porte) -----
df_grafico6_data <- df_tabela4_base %>%
  filter(tamanho != "Administração Pública", setores != "Não Identificado", tamanho != "Ignorado") %>%
  group_by(setores, tamanho) %>%
  summarise(remuneracao_udi_adm = mean(remuneracao_udi_adm, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    tamanho_ordem = case_when(
      tamanho == "MEI e Micro" ~ 1,
      tamanho == "Pequena" ~ 2,
      tamanho == "Média" ~ 3,
      tamanho == "Grande" ~ 4,
      TRUE ~ 5
    )
  ) %>%
  arrange(setores, tamanho_ordem) %>%
  mutate(tamanho = factor(tamanho, levels = unique(tamanho)))

# ----- Dados para Gráfico 7 (Salário médio de admissão por faixa etária) -----
df_grafico7_data <- df_tabela4_base %>%
  group_by(faixa_etaria) %>%
  summarise(remuneracao_udi_adm = mean(remuneracao_udi_adm, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    faixa_etaria_ordem = case_when(
      faixa_etaria == "17 ou menos" ~ 1,
      faixa_etaria == "18-24" ~ 2,
      faixa_etaria == "25-29" ~ 3,
      faixa_etaria == "30-39" ~ 4,
      faixa_etaria == "40-49" ~ 5,
      faixa_etaria == "50-59" ~ 6,
      faixa_etaria == "60 ou mais" ~ 7,
      TRUE ~ 8
    )
  ) %>%
  arrange(faixa_etaria_ordem) %>%
  mutate(faixa_etaria = factor(faixa_etaria, levels = unique(faixa_etaria)))

# ----- Dados para Gráfico 8 (Salário médio de admissão por gênero e grau de instrução) -----
df_grafico8_data <- df_tabela4_base %>%
  group_by(genero, gdi_leg) %>%
  summarise(remuneracao_udi_adm = mean(remuneracao_udi_adm, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    gdi_leg_ordem = case_when(
      gdi_leg == "Analfabeto" ~ 1,
      gdi_leg == "Fundamental Incompleto" ~ 2,
      gdi_leg == "Fundamental Completo" ~ 3,
      gdi_leg == "Médio Completo" ~ 4,
      gdi_leg == "Superior Completo" ~ 5,
      gdi_leg == "Pós-Graduação" ~ 6,
      TRUE ~ 7
    )
  ) %>%
  arrange(genero, gdi_leg_ordem) %>%
  mutate(gdi_leg = factor(gdi_leg, levels = unique(gdi_leg)))


# ---------------------------------------------------------------------------- #
#                             Definição de Cores para Gráficos                 #
# ---------------------------------------------------------------------------- #
# Para gráficos de barras que separam positivo/negativo no saldo
bar_colors_saldo <- c("TRUE" = "steelblue", "FALSE" = "darkred")

# ---------------------------------------------------------------------------- #
#                             Geração dos Gráficos (Objetos ggplot)            #
# ---------------------------------------------------------------------------- #

# Fonte padrão para os gráficos
fonte_grafico <- "Fonte: Novo Caged/MTE. Elaboração: CEPES/IERI/UFU. *Dados com ajustes declarados até janeiro de 2026."

# Gráfico 1 – Saldo do emprego formal, com ajustes*, de fevereiro/2025 a janeiro/20265
grafico1 <- ggplot(df_geral_saldo, aes(x = data, y = saldo_mes, fill = saldo_mes > 0)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = bar_colors_saldo, guide = FALSE) +
  geom_text(aes(label = scales::comma(saldo_mes, big.mark = ".", decimal.mark = ",")),
            vjust = ifelse(df_geral_saldo$saldo_mes > 0, -0.5, 1.2), # Ajusta posição do label
            size = 3.5, color = "black") +
  labs(
    title = "Gráfico 1 – Uberlândia/MG: Saldo do emprego formal, com ajustes*,\nde fevereiro/2025  a janeiro/2026",
    x = NULL,
    y = "Saldo",
    caption = fonte_grafico
  ) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","), # Ajusta formatacao do eixo Y
                     limits = c(-3500, 2000),
                     breaks = seq(-3500, 2000, by = 1000))+
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 25, unit = "pt")), # Adicionado margin(t = 15)
    panel.grid.major.x = element_blank() # Remove linhas de grade verticais
  )

# Gráfico 2 – Saldo por setor de atividade e porte da empresa em janeiro de 2026
# Reordena fatores para o gráfico, garantindo que 'setores' e 'tamanho' sigam a ordem desejada.
df_grafico2_data$setores <- factor(df_grafico2_data$setores, levels = c("Agropecuária", "Indústria", "Construção", "Comércio", "Serviços"))
df_grafico2_data$tamanho <- factor(df_grafico2_data$tamanho, levels = c("MEI e Micro", "Pequena", "Média", "Grande"))

grafico2 <- ggplot(df_grafico2_data, aes(x = forcats::fct_rev(tamanho), y = saldomovimentação, fill = saldomovimentação > 0)) +
  geom_col() +
  scale_fill_manual(values = bar_colors_saldo, guide = FALSE) +
  geom_text(aes(label = scales::comma(saldomovimentação, big.mark = ".", decimal.mark = ",")),
            hjust = ifelse(df_grafico2_data$saldomovimentação > 0, -0.2, 1.2), size = 3.5) +
  facet_wrap(~ setores, scales = "free_y", ncol = 1, strip.position = "left") + # Facets à esquerda
  labs(
    title = "Gráfico 2 – Uberlândia/MG: Saldo por setor de atividade e porte da empresa\nem janeiro de 2026",
    x = NULL,
    y = "Saldo",
    caption = fonte_grafico
  ) +
  coord_flip() + # Barras horizontais
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
                     breaks = seq(min(df_grafico2_data$saldomovimentação, na.rm = TRUE) - 100, max(df_grafico2_data$saldomovimentação, na.rm = TRUE) + 100, by = 100)) + # Ajusta breaks dinamicamente
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 25, unit = "pt")),
    strip.text = element_text(size = 10, face = "bold"),
    strip.text.y = element_text(margin = margin(l = 5, r = 5, t = 2, b = 2, unit = "mm")), # Adiciona margem interna ao texto da faceta vertical
    strip.background = element_rect(fill = "gray95"),
    panel.spacing = unit(0.1, "lines"),
    panel.grid.major.y = element_blank()
  )

# Gráfico 3 – Saldo por faixa etária do empregado, com ajustes*, em janeiro de 2026
# Reordena fatores para o gráfico, garantindo que 'faixa_etaria' siga a ordem desejada.
df_grafico3_data$faixa_etaria <- factor(df_grafico3_data$faixa_etaria, levels = c("17 ou menos", "18-24", "25-29", "30-39", "40-49", "50-59", "60 ou mais"))

grafico3 <- ggplot(df_grafico3_data, aes(x = forcats::fct_rev(faixa_etaria), y = saldomovimentação, fill = saldomovimentação > 0)) +
  geom_col() +
  scale_fill_manual(values = bar_colors_saldo, guide = FALSE) +
  geom_text(aes(label = scales::comma(saldomovimentação, big.mark = ".", decimal.mark = ",")),
            hjust = ifelse(df_grafico3_data$saldomovimentação > 0, -0.2, 1.2), size = 3.5) +
  labs(
    title = "Gráfico 3 – Uberlândia/MG: Saldo por faixa etária do empregado, com ajustes*,\n em janeiro de 2026",
    x = NULL,
    y = "Saldo",
    caption = fonte_grafico
  ) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
                     breaks = seq(min(df_grafico3_data$saldomovimentação, na.rm = TRUE) - 100, max(df_grafico3_data$saldomovimentação, na.rm = TRUE) + 100, by = 100)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9), # Tamanho da fonte do eixo numérico
    axis.text.y = element_text(size = 9), # Tamanho da fonte do eixo categórico (faixa etária)
    axis.title.x = element_text(size = 10), # Título do eixo numérico (Saldo)
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 25, unit = "pt")),
    panel.grid.major.y = element_blank() # Remove as linhas de grade verticais (no sistema de coordenadas invertido)
  )

# Gráfico 4 – Saldo por gênero e grau de instrução do empregado, com ajustes*, em janeiro de 2026
# Reordena fatores para o gráfico
df_grafico4_data$genero <- factor(df_grafico4_data$genero,
                                  levels = c("Homem", "Mulher"))

df_grafico4_data$gdi_leg <- factor(df_grafico4_data$gdi_leg,
                                   levels = c("Analfabeto",
                                              "Fundamental Incompleto",
                                              "Fundamental Completo",
                                              "Médio Completo",
                                              "Superior Completo",
                                              "Pós-Graduação"))

grafico4 <- ggplot(df_grafico4_data,
                   aes(x = forcats::fct_rev(gdi_leg),
                       y = saldomovimentação,
                       fill = saldomovimentação > 0)) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = bar_colors_saldo, guide = FALSE) +
  geom_text(aes(label = scales::comma(saldomovimentação,
                                      big.mark = ".",
                                      decimal.mark = ",")),
            hjust = ifelse(df_grafico4_data$saldomovimentação > 0, -0.2, 1.2),
            size = 3.5) +
    facet_wrap(~ genero, scales = "free_y") +
    labs(
    title   = "Gráfico 4 – Uberlândia/MG: Saldo por gênero e grau de instrução do empregado,\ncom ajustes*, em janeiro de 2026",
    x       = NULL,
    y       = "Saldo",
    caption = fonte_grafico
  ) +
    coord_flip() +
    scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
    limits = c(-550, 600),
    breaks = seq(-550, 600, by = 200)
  ) +
    theme_minimal() +
    theme(
    axis.text.x  = element_text(size = 8),
    axis.text.y  = element_text(size = 9),
    axis.title.x = element_text(size = 10),
    plot.title.position = "plot",
    plot.title = element_text(
      size       = 12,
      face       = "bold",
      hjust      = 0.5,
      lineheight = 1.2,                        # espaçamento entre as 2 linhas
      margin     = margin(t = 15, b = 10, unit = "pt")  # respiro acima e abaixo
    ),
    plot.caption = element_text(size = 9, hjust = 0,
                                margin = margin(t = 25, unit = "pt")),
    strip.text       = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "gray95"),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(t = 0.3, r = 1, b = 0.5, l = 0.5, unit = "cm")
  )

# Gráfico 5 – Salário médio real de admissão em Uberlândia, com ajustes*, de fevereiro/2025 a janeiro/2026
grafico5 <- ggplot(df_grafico5_data, aes(x = data, y = remuneracao_udi_adm)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(aes(label = scales::comma(remuneracao_udi_adm, big.mark = ".", decimal.mark = ",", accuracy = 1)),
            vjust = -0.5, size = 3, color = "black") +
  labs(
    title = "Gráfico 5 – Uberlândia/MG: Salário médio real de admissão em Uberlândia, com ajustes*,\n de fevereiro/2025 a janeiro/2026 (em R$)",
    x = NULL,
    y = "Salário de admissão",
    caption = fonte_grafico
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
    axis.text.y = element_blank(),
    axis.title.y = element_text(size = 9),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 25, unit = "pt")), # Adicionado margin(t = 15)
    panel.grid.major.x = element_blank()
  )

# Gráfico 6 – Salário médio real de admissão em Uberlândia por grupamento de atividade econômica e por porte da empresa, com ajustes*, janeiro de 2026
# Reordena fatores para o gráfico
grafico6 <- ggplot(df_grafico6_data, aes(x = forcats::fct_rev(tamanho), y = remuneracao_udi_adm)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::comma(remuneracao_udi_adm, big.mark = ".", decimal.mark = ",", accuracy = 1),
                y = remuneracao_udi_adm + 100),
            hjust = 0,
            size = 3.5, color = "black") +
  facet_wrap(~ setores,
             scales       = "free_y",
             ncol         = 1,
             strip.position = "left",
             labeller     = label_wrap_gen(width = 12)) +
    labs(
    title   = "Gráfico 6 – Uberlândia/MG: Salário médio real de admissão por grupamento\nde atividade econômica e porte da empresa,com ajustes*, janeiro de 2026 (em R$)",
    x       = NULL,
    y       = "Salário de Admissão",
    caption = fonte_grafico
  ) +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 3500, by = 1000),
    limits = c(0, 3500)
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(size = 9),
    axis.title.x = element_text(size = 10),
    plot.title.position = "plot",
    plot.title = element_text(
      size       = 12,
      face       = "bold",
      hjust      = 0.5,
      lineheight = 1.2,
      margin     = margin(t = 15, b = 10, unit = "pt")
    ),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 25, unit = "pt")),
    strip.text.y = element_text(
      size   = 10,
      face   = "bold",
      margin = margin(l = 5, r = 5, t = 4, b = 4, unit = "mm")
    ),
    strip.text       = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "gray95"),
    panel.spacing    = unit(0.1, "lines"),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(t = 0.2, r = 1, b = 0.5, l = 0.5, unit = "cm")
  )

# Gráfico 7 – Salário médio real de admissão por faixa etária, com ajustes*, janeiro de 2026
grafico7 <- ggplot(df_grafico7_data,
                   aes(x = forcats::fct_rev(faixa_etaria), y = remuneracao_udi_adm)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::comma(remuneracao_udi_adm,
                                      big.mark    = ".",
                                      decimal.mark = ",",
                                      accuracy    = 1)),
            hjust = -0.2, size = 3.5) +
  labs(
    title = "Gráfico 7 – Uberlândia/MG: Salário médio real de admissão\npor faixa etária, com ajustes*, janeiro de 2026 (em R$)",
    x       = "Faixa Etária",
    y       = NULL,
        caption = fonte_grafico
  ) +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 3000, by = 500),
    labels = NULL,
    limits = c(0, 3200)
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 9),
    axis.title.y = element_text(size = 10),
    plot.title.position = "plot",
    plot.title = element_text(
      size       = 12,
      face       = "bold",
      hjust      = 0.5,
      lineheight = 1.2,
      margin     = margin(t = 15, b = 10, unit = "pt")
    ),
    plot.caption = element_text(size = 9, hjust = 0,
                                margin = margin(t = 25, unit = "pt")),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(t = 0.2, r = 1.0, b = 0.5, l = 0.5, unit = "cm")
  )

# Gráfico 8 – Salário médio real de admissão por gênero e grau de instrução, com ajustes*, janeiro de 2026

# Reordena fatores para o gráfico
grafico8 <- ggplot(df_grafico8_data,
                   aes(x = forcats::fct_rev(gdi_leg), y = remuneracao_udi_adm)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::comma(remuneracao_udi_adm,
                                      big.mark     = ".",
                                      decimal.mark = ",",
                                      accuracy     = 1)),
            hjust = -0.2, size = 3.5) +
  facet_wrap(~ genero, scales = "free_y") +
  labs(
    title = "Gráfico 8 – Uberlândia/MG: Salário médio real de admissão\npor gênero e grau de instrução do empregado,\ncom ajustes*, janeiro de 2026 (em R$)",
    x       = NULL,
    y       = NULL,
    caption = fonte_grafico
  ) +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 10000, by = 1000),
    labels = NULL,
    limits = c(0, 10000)
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 9),
    axis.title.y = element_text(size = 10),
    plot.title.position = "plot",
    plot.title = element_text(
      size       = 12,
      face       = "bold",
      hjust      = 0.5,
      lineheight = 1.2,
      margin     = margin(t = 15, b = 10, unit = "pt")
    ),
    plot.caption = element_text(size = 9, hjust = 0,
                                margin = margin(t = 25, unit = "pt")),
    strip.text       = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "gray95"),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(t = 0.3, r = 1, b = 0.5, l = 0.5, unit = "cm")
  )

# ---------------------------------------------------------------------------- #
#                             Geração das Tabelas (Objetos para kable)         #
# ---------------------------------------------------------------------------- #

# Fonte padrão para as tabelas
fonte_tabela <- "Fonte: Novo Caged/MTE. Elaboração: CEPES/IERI/UFU. *Dados com ajustes declarados até janeiro de 2026."

# Tabela 1
tabela1 <- df_geral_saldo %>%
  select(`Mês/Ano` = data, Admissões = admissoes, Desligamentos = demissoes, Saldo = saldomovimentação) %>%
  mutate(across(where(is.numeric), ~scales::comma(., big.mark = ".", decimal.mark = ","))) %>%
  add_row(`Mês/Ano` = "Saldo acumulado no ano 2025",
          Admissões = scales::comma(sum(df_geral_saldo$admissoes, na.rm = TRUE), big.mark = ".", decimal.mark = ","),
          Desligamentos = scales::comma(sum(df_geral_saldo$demissoes, na.rm = TRUE), big.mark = ".", decimal.mark = ","),
          Saldo = scales::comma(sum(df_geral_saldo$saldomovimentação, na.rm = TRUE), big.mark = ".", decimal.mark = ","))

# Tabela 2
tabela2 <- df_saldo_setor %>%
  select(
    `Mês / Ano` = data,
    Agropecuária = `Agropecuária`, Indústria = `Indústria`, Construção = `Construção`, Comércio = `Comércio`, Serviços = `Serviços`
  ) %>%
  mutate(across(where(is.numeric), ~scales::comma(., big.mark = ".", decimal.mark = ","))) %>%
  # Adicionar linha de Acumulado 2025
  add_row(`Mês / Ano` = "Acum. 2025",
          Agropecuária = scales::comma(sum(df_saldo_setor$Agropecuária, na.rm = TRUE), big.mark = ".", decimal.mark = ","),
          Indústria = scales::comma(sum(df_saldo_setor$Indústria, na.rm = TRUE), big.mark = ".", decimal.mark = ","),
          Construção = scales::comma(sum(df_saldo_setor$Construção, na.rm = TRUE), big.mark = ".", decimal.mark = ","),
          Comércio = scales::comma(sum(df_saldo_setor$Comércio, na.rm = TRUE), big.mark = ".", decimal.mark = ","),
          Serviços = scales::comma(sum(df_saldo_setor$Serviços, na.rm = TRUE), big.mark = ".", decimal.mark = ","))

# Tabela 3
tabela3 <- df_saldo_porte %>%
  # Selecionar e ordenar as colunas explicitamente, se necessário.
  # Os nomes já vêm corretos de df_saldo_porte.
  select(
    `Mês / Ano`,
    `MEI e Micro`,
    Pequena,
    Média,
    Grande,
    `Administração Pública`
  ) %>%
  # Aplicar a formatação de números para as colunas de saldo
  # 'across' com 'where(is.numeric)' funcionaria se 'df_saldo_porte' ainda tivesse números.
  # Como 'df_saldo_porte' já tem a coluna 'Mês / Ano' que não é numérica,
  # é mais seguro especificar as colunas numéricas ou garantir que elas sejam numéricas antes da formatação.
  # Se df_saldo_porte contém apenas números e o 'Mês / Ano' como texto/fator na sua ultima versao,
  # a linha abaixo está ok.
  mutate(across(c(`MEI e Micro`, Pequena, Média, Grande, `Administração Pública`),
                ~scales::comma(., big.mark = ".", decimal.mark = ",")))

# Tabela 4

# Função auxiliar para criar uma linha de cabeçalho para Tabela 4
create_tabela4_header_row <- function(header_text) {
  row <- data.frame(
    Variáveis = header_text,
    Admissões = NA_real_,
    Desligamentos = NA_real_,
    Saldo = NA_real_
  )
  return(row)
}


# Cria uma linha vazia para separação visual, como no documento. Ela será flexível.
empty_row_4 <- create_tabela4_header_row("")
empty_row_4$Variáveis <- "" # Garante que não apareça "NA"


# --- Tabela 4 - Números Absolutos

# Cria as linhas de cabeçalho específicas para Tabela 4
header_t4_grupamento <- create_tabela4_header_row("Grupamento de atividade econômica")
header_t4_porte <- create_tabela4_header_row("Porte da empresa")
header_t4_faixa_etaria <- create_tabela4_header_row("Faixa Etária")
header_t4_genero <- create_tabela4_header_row("Gênero")
header_t4_escolaridade <- create_tabela4_header_row("Grau de instrução")


# Combina todas as partes da Tabela 4
tabela4_combinada_numerica <- bind_rows(
  header_t4_grupamento,
  df_tabela4_setores,
  empty_row_4, # Linha vazia
  header_t4_porte,
  df_tabela4_porte,
  empty_row_4, # Linha vazia
  header_t4_faixa_etaria,
  df_tabela4_faixa_etaria,
  empty_row_4, # Linha vazia
  header_t4_genero,
  df_tabela4_genero,
  empty_row_4, # Linha vazia
  header_t4_escolaridade,
  df_tabela4_escolaridade
)

# Calcula a linha "Total mês de dezembro" para Tabela 4
total_mes_dezembro_4 <- df_tabela4_base %>%
  summarise(
    Admissões = sum(admissoes, na.rm = TRUE),
    # Desligamentos no documento são representados como números negativos, então usaremos o total absoluto negativo
    Desligamentos = -grand_total_desligamentos_dez_abs,
    Saldo = sum(saldomovimentação, na.rm = TRUE)
  ) %>%
  mutate(Variáveis = "Total mês de dezembro") %>%
  # Garante a ordem e nomes das colunas
  select(Variáveis, Admissões, Desligamentos, Saldo)

# Adiciona a linha de total
tabela4_final_numerica <- bind_rows(tabela4_combinada_numerica, total_mes_dezembro_4)

# Aplica a formatação de números para as colunas numéricas
# E substitui NAs nas colunas formatadas (que viraram texto) por vazios
tabela4 <- tabela4_final_numerica %>%
  mutate(
    across(c(Admissões, Desligamentos, Saldo),
           ~ifelse(is.na(.), "", scales::comma(., big.mark = ".", decimal.mark = ",")))
  )

# Tabela 5
tabela5 <- df_tabela5_data %>%
  select(
    `Ano / Mês` = data,
    `Salário de Admissão` = remuneracao_udi_adm,
    `Variação mensal_adm`,
    `Salário de Demissão` = remuneracao_udi_dem,
    `Variação mensal_dem`
  ) %>%
  mutate(
    `Salário de Admissão` = scales::comma(`Salário de Admissão`, big.mark = ".", decimal.mark = ",", accuracy =1),
    `Salário de Demissão` = scales::dollar(`Salário de Demissão`, big.mark = ".", decimal.mark = ",", accuracy = 1),
    `Variação mensal_adm` = sprintf("%.2f%%", `Variação mensal_adm`),
    `Variação mensal_dem` = sprintf("%.2f%%", `Variação mensal_dem`)
  ) %>%
  add_row(
    `Ano / Mês` = "Acum. 2025 (%)",
    `Salário de Admissão` = "",
    `Variação mensal_adm` = sprintf("%.2f%%", acum_adm_var),
    `Salário de Demissão` = "",
    `Variação mensal_dem` = sprintf("%.2f%%", acum_dem_var)
  )

# Tabela 6
tabela6 <- df_remuneracao_setor %>%
  select(
    `Mês / Ano` = data,
    Agropecuária = `Agropecuária`, Indústria = `Indústria`, Construção = `Construção`, Comércio = `Comércio`, Serviços = `Serviços`
  ) %>%
  mutate(across(where(is.numeric), ~scales::comma(., big.mark = ".", decimal.mark = ",", accuracy=1))) %>%
  # Adicionar linha de Acumulado 2025
  add_row(`Mês / Ano` = "Acum. 2025 (%)",
          Agropecuária = sprintf("%.2f%%", df_tabela6_acum %>% pull(`Agropecuária`)),
          Indústria = sprintf("%.2f%%", df_tabela6_acum %>% pull(`Indústria`)),
          Construção = sprintf("%.2f%%", df_tabela6_acum %>% pull(`Construção`)),
          Comércio = sprintf("%.2f%%", df_tabela6_acum %>% pull(`Comércio`)),
          Serviços = sprintf("%.2f%%", df_tabela6_acum %>% pull(`Serviços`)))

# Tabela 7
tabela7 <- df_remuneracao_porte %>%
  select(
    `Mês / Ano` = data,
    `MEI e Micro` = meimicro, Pequena = pequena, Média = media, Grande = grande
  ) %>%
  mutate(across(where(is.numeric), ~scales::comma(., big.mark = ".", decimal.mark = ",", accuracy=1))) %>%
  # Adicionar linha de Acumulado 2025
  add_row(`Mês / Ano` = "Acum. 2025 (%)",
          `MEI e Micro` = sprintf("%.2f%%", df_tabela7_acum %>% pull(`meimicro`)),
          Pequena = sprintf("%.2f%%", df_tabela7_acum %>% pull(`pequena`)),
          Média = sprintf("%.2f%%", df_tabela7_acum %>% pull(`media`)),
          Grande = sprintf("%.2f%%", df_tabela7_acum %>% pull(`grande`)))
