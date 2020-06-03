#' Extrai dados de SRAG para o Brasil da página do Ministério da Saúde
#'
#' Esta função extrai os valores referentes à Síndrome Respiratória Aguda Grave
#' compilados pelo Ministério da Saúde do Brasil (disponíveis em: 'https://covid.saude.gov.br/')
#' e salva o resultado no disco.
#'
#' @param dir Diretório onde salvar o arquivo, valor predeterminado "outputs"
#' @param filename Nome do arquivo, valor predeterminado "minsaude_srag"
#' @param uf Caractere indicando a abreviação do(s) estado(s) brasileiro(s)
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom utils read.csv2 write.csv
#' @importFrom magrittr %>%
#' @importFrom plyr .
#'
#' @export
#'
get_srag_minsaude <- function(dir = "outputs",
                              filename = "minsaude_srag",
                              uf = NULL) {
  rlang::.data #para usar vars no dplyr
  #get original data and format it
  url <-
    httr::GET("https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalGeral",
              httr::add_headers("X-Parse-Application-Id" =
                                  "unAFkcaNDeXajurGB7LChj8SgQYS2ptm")) %>%
    httr::content() %>%
    '[['("results") %>%
    '[['(1) %>%
    '[['("arquivo_srag") %>%
    '[['("url")

  destination <- file.path(tempdir(), paste0(filename, '.csv'))

  utils::download.file(url, destination)

  res <- read.csv2(file = destination) %>%
    tibble::as_tibble() %>%
    mutate(data = as.Date(data))

  # gravando metadados da requisicao
  metadado <- data.frame(intervalo = paste(range(res$data), collapse = ";"),
                         fonte = url,
                         acesso_em = Sys.Date())
  if (!is.null(uf)) {
    res <- res %>% dplyr::filter(.data$estado %in% toupper(uf))
  }

  message(paste0("salvando ", filename, ".csv em ", dir))
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  utils::write.csv(res,
                   paste0(dir, "/", filename,
                          paste(uf, collapse = "-"), ".csv"),
                   row.names = FALSE)
  utils::write.csv(metadado,
                   paste0(dir, "/", "metadado_minsaude",
                          paste(uf, collapse = "-"), ".csv"),
                   row.names = FALSE)
  return(res)
}
