# novo-caged-dashboard-udi
Processa dados do Novo CAGED e exporta informações para abastecer dashboard do emprego de Uberlândia (MG).

# Fonte dos dados
Ministério do Trabalho e do Emprego
Programa de Disseminação das Estatísticas do Trabalho (PDET)
ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED

# Download dos dados
Em /data_raw estão: IPC-CEPES; estoque de trabalhos em janeiro de 2023; e lista dos nomes e códigos (6 dígitos) dos municípios brasileiros.
Para obter os arqquivos "crus" basta executar "DB_extracao" e "DB_saldoextracao"

# Replicação
DB_principal produz o output DB_trabalho.xlsx que contém todas as tabelas que abastecem os dashboard do emprego de Uberlândia-MG.
