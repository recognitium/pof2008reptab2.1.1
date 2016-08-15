# analyze survey data for free (http://asdfree.com) with the r language
# This work is inspired and extends previous work from Anthony Damico and Djalma Pessoa
# pesquisa orcamentos familiares

# previous ---------------------------------------------------------------------
# # # # # # # # # # # # # # # # #
# # block of code to run this # #
# # # # # # # # # # # # # # # # #
options( encoding = "latin1" )		# # only macintosh and *nix users need this line
# library(downloader)
#setwd( "YOURPROJECTDIR" )
# surce_url( "https://raw.githubusercontent.com/ajdamico/asdfree/master/Pesquisa%20de%20Orcamentos%20Familiares/replicate%20tabela%201.1.12.R" , prompt = FALSE , echo = TRUE )
# # # # # # # # # # # # # # #
# # end of auto-run block # #
# # # # # # # # # # # # # # #
# description ---------------------------------------------------------------------
#####################################################################################################################################################################
# this script matches the IBGE tabela 2.1 statistics and coefficients of variation (coeficientes de variacao).  IBGE is the brazilian census bureau/stats agency.   #
# http://www.ibge.gov.br/home/estatistica/populacao/condicaodevida/pof/2008_2009/tabelas_pdf/tabela2_1.pdf                                                       #
# ftp://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_2008_2009/Despesas_rendimentos_e_condicoes_de_vida/tab_rendimentos.zip (tab2101.xls) #
#####################################################################################################################################################################


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#####################################################################################################################
# prior to running this analysis script, the 2008-2009 pof files must be loaded on the local machine.               #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# https://raw.githubusercontent.com/ajdamico/asdfree/master/Pesquisa%20de%20Orcamentos%20Familiares/download%20all%20microdata.R #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# running the download all microdata script for 2009 will place the files you need into a 2009-specific directory   #
#####################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#=======================================================

# uncomment this line by removing the `#` at the front..
# setwd( "C:/My Directory/POF/2009/" )
# ..in order to set your current working directory


# # # are you on a non-windows system? # # #
if ( .Platform$OS.type != 'windows' ) print( 'non-windows users: read this block' )
# ibge's ftp site has a few SAS importation
# scripts in a non-standard format
# if so, before running this whole download program,
# you might need to run this line..
# options( encoding="latin1" )
# ..to turn on latin-style encoding.
# # # end of non-windows system edits.


# remove the `#` in order to run this install.packages line only once
#install.packages( c( "reshape2" , "survey" ) )
# code begins ---------------------------------------------------------------------
library(survey)		# load survey package (analyzes complex design surveys)
library(reshape2)	# load reshape2 package (transposes data frames quickly)
library(stringr)  # pad 0s to uniformize code padding

# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN

## All of the code below assumes your POF microdata has been downloaded into "2009" folder
# load the person-level data file
load("2009/t_morador_s.rda")

# load the post-stratification table
load("2009/poststr.rda")

# load the household-level data
load( "2009/t_domicilio_s.rda" )

# load the detailed income data
load( "2009/t_rendimentos_s.rda" )

# load products registry data -- generated from "import-for-other-replicates.R"
load("2009cadastro-produtos.rda")

# load secondary incomes reception database
load("2009/t_outros_reci_s.rda")

# load income recodes file - used for recoding and for generating rows of final table ### you probably need to download it first from github repo -- not added to the code yet
incomeRecodes <- read.csv("codigos-recodificacao-rendimentos.csv",sep = ";" , as.is = 1:3)
# recodes to imported data---------------------------------------------------------------------
# # # # # # # # # # # # #
# perform a few recodes #

## Prepare incomeRecodes to merge with t_rendimentos_s
# Replace non range codings with a dummy value 50000 - in order to be able to generate proper expanded matrix
incomeRecodes$cod.inc <-gsub(".*\\+.*","50000",incomeRecodes$cod.inc)
# generate data frame with one number per row and recoded number according to hierarchy in 2.1.1 table and csv file
incomeRecodesX <- data.frame()
for (i in 1:nrow(incomeRecodes)) {incomeRecodesX <- rbind(incomeRecodesX,cbind(incomeRecodes[i,1],eval(parse(text = paste("c(" , incomeRecodes[i,3],")")))))}
names(incomeRecodesX) <- c("cod.novo","cod.rec")


t_rendimentos_s <-
  transform(
    t_rendimentos_s ,
 
    # monthly income - should be equal to rendimento corrigido
    recmes = ( valor_anual_expandido2 / fator_expansao2 ) / 12 ,
    
    # unique family code
    cod.uc = paste0( cod_uf , num_seq , num_dv , cod_domc , num_uc ),
    
    # unique income type code
    cod.rec = paste0( num_quadro, substr(cod_item,1,3))
    
    # input recodes of income type according to incomeRecodes table -- done separately
  )


t_rendimentos_recoded <- merge (t_rendimentos_s, incomeRecodesX)
# slim t_rendimentos_recoded to only relevant data
t_rendimentos_recoded <- t_rendimentos_recoded[,c('cod.rec', 'cod.uc', 'rec.mes' , 'fator_expansao1' , 'fator_expansao2')]

## POTENTIAL PROBLEM IN ORIGINAL DATA (OR CODE) : number of rows is reduced
# in 8073 rows indicated as 53007, 53008 or 53009 - trainee / rural / familiar with 0 income;
# Assess later on if there are problems with estimates to manually include it in incomeRecodesX

# Some recodes and slim of table with secondary incomes 
t_outros_reci_s <-
  transform(
    t_outros_reci_s ,
    
    # monthly income
    recmes = ( valor_anual_expandido2 / fator_expansao2 ) / 12 ,
    
    # unique family code
    cod.uc = paste0( cod_uf , num_seq , num_dv , cod_domc , num_uc ),
    
    # unique income type code
    cod.rec = paste0( num_quadro, substr(cod_item,1,3))
  )

# merge on incomeRecodesX to aggregate afterwards
t_outros_reci_recoded <- merge (t_outros_reci_s,incomeRecodesX)

## WARNING - POTENTIAL SIMILAR PROBLEM THAN ABOVE, but probably because
# we are still not considering non monetary incomes, expenses and equity variation items

# slim t_outros_reci_recoded to only relevant data
t_outros_reci_recoded <- t_outros_reci_recoded[,c('cod.rec', 'cod.uc', 'rec.mes' , 'fator_expansao1' , 'fator_expansao2')]



# construct a unique family code in the person-level data file
# that will be used to isolate the family-level income variable
t_morador_s <-
  transform(
    t_morador_s ,
    
    # unique family code
    cod.uc = paste0( cod_uf , num_seq , num_dv , cod_domc , num_uc ) 
  )

# slim the `t_morador_s` table down to just
# family unique identifier and total family income variables
family.level.income <- t_morador_s[ , c( 'cod.uc' , 'renda_total' ) ]

# `family.level.income` currently has one record per person from each sampled household
nrow( family.level.income )

# but each family has the same income value,
# so just keep one record per family
family.level.income <- unique( family.level.income )

# and now you've got one record per family
nrow( family.level.income )


family.level.income <- 
  transform(
    family.level.income , 
    
    # create income categories
    renda.cat = 
      cut(
        renda_total , 
        c( 0 , 830 , 1245 , 2490 , 4150 , 6225 , 10375 , Inf ) ,
        include.lowest = TRUE , 
        labels = c( "[0,830]" , "(830,1245]" , "(1245,2490]" , "(2490,4150]" , "(4150,6225]" , "(6225,10375]" , ">10375" )
      ) ,
    
    # create a control variable that matches 
    # the one in the `poststr` table
    control = substr( cod.uc , 1 , 6 )
  )


# at this point, you've finished with the `t_morador_s` table
# so remove that from working memory
rm( t_morador_s )

# and clear up RAM
gc()

# to be adapted for 2.1.1 table ---------------------------------------------------------------------
# big function that does most of the hard work ---------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# custom function to calculate income by type within the household #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# from here on in, the function `tabela_2.1.1` will do most of the work
# note that (unlike other pof scripts) the survey design object creation
# occurs within the function, so as long as the user provides the appropriate parameters,
# we're done!

tabela_2.1.1 <-
  function(
    # choose a food code
    incomeCode ,
    # specify the family-level data.frame with the income variable
    family.level.income = family.level.income ,
    # specify the income booklet data,
    # which must contain the variables
    # created above
    t_rendimentos_s = t_rendimentos_s ,
    # identify the components table to use
    componentes = componentes ,
    # identify the table to use for post-stratification
    poststr = poststr
  ){
    
    # isolate all records containing the current code *anywhere*
    incomeCode.plus.subcodes <-
      componentes[ apply( componentes == incomeCode , 1 , any ) , 'codigo' ]
    
    # isolate family-wide expenditures to only matching codes
    family.expenditures.by.code <- 
      t_caderneta_despesa_s[ t_caderneta_despesa_s$codigo %in% curCode.plus.subcodes , c( 'codigo' , 'despmes' , 'cod.uc' ) ]
    
    # aggregate spending to the one-record-per-family-level
    family.level.spending <-
      aggregate( 
        despmes ~ cod.uc , 
        family.expenditures.by.code , 
        sum 
      )
    
    # merge the income and expenditure tables,
    # assuming that the income table has no missings
    y <- merge( family.level.income , family.level.spending , all.x = TRUE )
    
    # all missing values from the left-join above
    # should be converted to zeroes
    y[ is.na( y$despmes ) , 'despmes' ] <- 0
    
    
    # merge on necessary post-stratification variables..
    z <- 
      merge( 
        y , 
        poststr[ , c( 'control' , 'estrato_unico' , 'fator_des' , 'pos_estrato' , 'tot_unidade_c' ) ] 
      )
    
    # ..and confirm no record-loss
    stopifnot( nrow( z ) == nrow( y ) )
    
    # construct the preliminary survey object
    # (not yet post-stratified)
    sample.pof <-
      svydesign(
        id = ~control , 
        strata = ~estrato_unico , 
        weights = ~fator_des ,
        data = z , 
        nest = TRUE
      )
    
    # construct the target population table
    uc.totals <- 
      data.frame(
        pos_estrato = unique( z$pos_estrato ) , 
        Freq = unique( z$tot_unidade_c )
      )
    
    # construct the final post-stratified survey object
    pof.design <- 
      postStratify(
        sample.pof , 
        ~pos_estrato , 
        uc.totals
      )
    
    # take the overall mean..
    st <- svymean( ~despmes , pof.design )
    
    # ..and the mean, broken down by income categories
    sb <- 
      svyby(
        ~despmes , 
        ~renda.cat , 
        pof.design , 
        svymean
      )
    
    # make a single-row data.frame for the total..
    ot <-
      data.frame( 
        renda.cat = 'Total' , 
        mean = coef( st ) , 
        se = as.numeric( SE( st ) ) , 
        cv = as.numeric( cv( st ) )
      )
    
    # ..and a multi-row data.frame for the breakouts
    ob <-
      data.frame( 
        renda.cat = sb$renda.cat , 
        mean = coef( sb ) , 
        se = as.numeric( SE( sb ) ) , 
        cv = as.numeric( cv( sb ) )
      )
    
    # stack them
    w <- rbind( ot , ob )
    
    # throw on the current food expenditure code
    w$top.codigo <- curCode
    
    # finish up with a single row of data,
    # stretched out into `wide` format
    reshape( 
      w , 
      idvar = 'top.codigo' ,
      timevar = 'renda.cat' ,
      direction = 'wide'
    )
    # since the result of this `reshape` is the last line of this function
    # the function will return that result.
  }

# examples with new function ---------------------------------------------------------------------
# run a single line with our fancy new
# `tabela_2.1.1` for vegetables --

# legumes e verduras
tabela_2.1.1( 
  "1.5" , 
  family.level.income ,
  t_caderneta_despesa_s , 
  componentes , 
  poststr 
)

# hey why not run one more
# `tabela_2.1.1` for baked goods, yum --

# panificados	
tabela_2.1.1( 
  "1.6" , 
  family.level.income ,
  t_caderneta_despesa_s , 
  componentes , 
  poststr 
)

# proper table replication ---------------------------------------------------------------------
# # # # # # # # # # # # # # # #
# create a table to populate  #

# make an empty, single-column table
tabela <- data.frame( tipo.de.despesa = NULL )

# for every record in the `componentes` table..
for ( i in seq( nrow( componentes ) ) ){
  
  # for 1, 2, and 3..
  for ( j in 1:3 ){
    
    # if the `desc.#` does not yet exist in the `tabela`..
    if ( !( componentes[ i , paste0( 'desc.' , j ) ] %in% tabela$tipo.de.despesa ) ){
      
      # add a new row, and add that `desc.#` to the `tabela` object..
      tabela[ nrow( tabela ) + 1 , 'tipo.de.despesa' ] <- 
        componentes[ i , paste0( 'desc.' , j ) ]
      
      # ..and also copy over the current code.
      tabela[ nrow( tabela ) , 'top.codigo' ] <- 
        componentes[ i , paste0( 'nivel.' , j ) ]
      
    }
  }
}

# remove blank records from the final `tabela`
tabela <- tabela[ tabela$tipo.de.despesa != "" , ]

# want to look at the `tabela` object?
# here are the first six..
head( tabela )

# ..and the last six records.
tail( tabela )


# alright.  now scan through each record in the `tabela` data.frame
for ( i in seq( nrow( tabela ) ) ){
  
  # run the `tabela_2.1.1` function on the current code
  print( tabela[ i , 'top.codigo' ] )
  
  # save the result into a new object `curRow`
  curRow <- 
    tabela_2.1.1( 
      tabela[ i , 'top.codigo' ] , 
      family.level.income ,
      t_caderneta_despesa_s , 
      componentes , 
      poststr 
    )
  
  # if it's the first run, make a new `allRows` object.  otherwise, stack it.
  if ( i == 1 ) allRows <- curRow else allRows <- rbind( allRows , curRow )
  
}

# merge on the descriptions
result_2.1.1 <- merge( tabela , allRows )

# take a look at the final table..
result_2.1.1
# ..or export them using one of the techniques discussed on http://twotorials.com

