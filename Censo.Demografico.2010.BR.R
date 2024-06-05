### Acessando os dados do censo demogr??fico 2010 - domic??lio
install.packages(lodown)
library(lodown)
# examinar todos os arquivos dispon??veis do Censo
censo_cat <-
  get_catalog("censo" ,
              output_dir = "data")
# informar os microdados dispon??veis do censo 2010
censo_cat2 <- subset(censo_cat , year == 2010)
# informar os microdados dispon??veis do censo 2010 para Pernambuco
censo_cat3 <- subset(censo_cat2 , state == "pe10")
# baixar os microdados do censo 2010 para Pernambuco
censo_cat3 <- lodown("censo" , censo_cat3)
# excluir do environment, liberar espa??o
rm( censo_cat ) ; gc()
rm( censo_cat2 ) ; gc()
# pacote survey para trabalhar com r??plicas bootstrap
library(survey)
## importar as seguintes colunas
#v0002 = c??digo do munic??pio; v1006 = rural ou urbano; v6461 = c??digo da ocupa????o; v6471 = c??digo da atividade; v6527 = renda mensal; v6900 = economicamente ativa ou n??o economicamente ativa; v6910 = ocupado ou desocupado
columns_to_import <-
  c('v0002' , 'v1006' , 'v6461' , 'v6471', 'v6527', 'v6900',
    'v6910')
censo_df <- data.frame(NULL)
stopifnot( length( unique( censo_cat3[ , 'year' ] ) ) == 1 )
for( this_state in seq( nrow( censo_cat3 ) ) ){
  these_columns_to_import <-
    unique(
      c(
        columns_to_import ,
        as.character(
          censo_cat3[ this_state , c( 'weight' , paste0( 'fpc' , 1:5 ) ) ])))}
these_columns_to_import <- these_columns_to_import[ !is.na( these_columns_to_import ) ]
dom_stru <- SAScii::parse.SAScii( censo_cat3[ this_state , 'dom_sas' ] )
19
dom_stru$varname <- tolower( dom_stru$varname )
this_censo_dom_df <-
  data.frame( readr::read_fwf(
    censo_cat3[ this_state , 'dom_file' ] ,
    readr::fwf_widths(
      abs( dom_stru$width ) , col_names = dom_stru[ , 'varname' ]
    ) ,
    col_types =
      paste0(
        ifelse( !( dom_stru$varname %in% these_columns_to_import ) ,
                "_" ,
                ifelse( dom_stru$char , "c" , "d" )
        ) ,
        collapse = "")))
for( this_variable in these_columns_to_import ) {
  if(
    ( this_variable %in% names( this_censo_dom_df ) ) &
    !isTRUE( all.equal( 1 , dom_stru[ dom_stru$varname == this_variable , 'divisor' ] ) )
  ){
    this_censo_dom_df[ , this_variable ] <-
      dom_stru[ dom_stru$varname == this_variable , 'divisor' ] *
      this_censo_dom_df[ , this_variable ]}}
censo_df <- rbind( censo_df , this_censo_dom_df )
rm( this_censo_dom_df ) ; gc()
# adicionar uma coluna de ???1???
censo_df[ , 'one' ] <- 1
## v0010 = peso amostral; v0011 = ??rea de pondera????o
fpc_sums <- aggregate( v0010 ~ v0011 , data = censo_df , sum )
names( fpc_sums )[ 2 ] <- 'fpc'
censo_df <- merge( censo_df , fpc_sums ) ; gc()
## gerar r??plicas via reamostragem bootstrap, B = 80
censo_wgts <-
  survey::bootweights(
    strata = censo_df[ , censo_cat3[ 1 , 'fpc1' ] ] ,
    psu = censo_df[ , censo_cat3[ 1 , 'fpc4' ] ] ,
    replicates = 80 ,
    fpc = censo_df[ , 'fpc' ])
censo_design <-
  survey::svrepdesign(
    weight = ~ v0010 ,
    20
    repweights = censo_wgts$repweights ,
    type = "bootstrap",
    combined.weights = FALSE ,
    scale = censo_wgts$scale ,
    rscales = censo_wgts$rscales ,
    data = censo_df)
rm( censo_df , censo_wgts , fpc_sums ) ; gc()
# Estima total de domic??lios no estado de Pernambuco - urbano ou rural
N_dom.rural.urb <- svyby( ~ one , ~v1006, censo_design, svytotal )
N_dom.rural.urb
# Estima total de domic??lios no estado de Pernambuco por munic??pio
N_dom.mun <- svyby( ~ one , ~v0002, censo_design, svytotal)
N_dom.mun
# Cria subgrupo apenas com pop. rural
rural_renda <- subset(censo_design, v1006==2)
# Estima total de moradores em domic??lio rural por munic??pio
Rural_domicilios <- svyby( ~ one , ~v0002, rural_renda, svytotal)
Rural_domicilios
## comando que permite exportar dataframe para excel
write.table(N_dom.mun, file='arquivo.csv', sep=';', dec=',', row.names=FALSE)
write.table(Rural_domicilios, file='rural_dom.csv', sep=';', dec=',', row.names=FALSE)