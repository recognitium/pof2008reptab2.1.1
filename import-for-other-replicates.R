# analyze survey data for free (http://asdfree.com) with the r language
# pesquisa orcamentos familiares
# 2002-2003 and 2008-2009

# # # # # # # # # # # # # # # # #
# # block of code to run this # #
# # # # # # # # # # # # # # # # #
options( encoding = "latin1" )		# # only macintosh and *nix users need this line
library(downloader)
#setwd( "POF/" )

years.to.download <- c(2009)
# path.to.7z <- normalizePath( "C:/Program Files (x86)/7-zip/7z.exe" )		# # this is probably the correct line for windows
path.to.7z <- "7za"													# # this is probably the correct line for macintosh and *nix
#source_url( "https://raw.githubusercontent.com/ajdamico/asdfree/master/Pesquisa%20de%20Orcamentos%20Familiares/download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )
# # # # # # # # # # # # # # #
# # end of auto-run block # #
# # # # # # # # # # # # # # #
# DESCRIPTION PRE ---------------------------------------------------------------------
#######################################################################
# Analyze the 2008-2009 Pesquisa de Orcamentos Familiares file with R #
#######################################################################


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#####################################################################################################################################################
# prior to running this script, windows users must have 7-zip installed on your computer. it's a free. go to http://www.7-zip.org/download.html     #
# macintosh and *nix users need 7za installed:  http://superuser.com/questions/548349/how-can-i-install-7zip-so-i-can-run-it-from-terminal-on-os-x  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# path.to.7z <- normalizePath( "C:/Program Files (x86)/7-zip/7z.exe" )		# # this is probably the correct line for windows
# path.to.7z <- "7za"														# # this is probably the correct line for macintosh and *nix
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# the line above sets the location of the 7-zip program on your local computer. uncomment it by removing the `#` and change the directory if ya did #
#####################################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# set your working directory.
# the POF microdata files will be stored here
# after downloading and importing them.
# use forward slashes instead of back slashes

# uncomment this line by removing the `#` at the front..
# setwd( "C:/My Directory/POF/" )
# ..in order to set your current working directory


# # # are you on a non-windows system? # # #
if ( .Platform$OS.type != 'windows' ) print( 'non-windows users: read this block' )
# ibge's ftp site has a few SAS importation
# scripts in a non-standard format
# if so, before running this whole download program,
# you might need to run this line..
#options( encoding="latin1" )
# ..to turn on latin-style encoding.
# # # end of non-windows system edits.


# # # # # # # # # # # # # #
# warning: perl required! #
# # # # # # # # # # # # # #

# if you do not have perl installed, this two-minute video
# walks through how to get it (for free): http://www.screenr.com/QiN8


# remove the # in order to run this install.packages line only once
# install.packages( c( 'gdata' , "SAScii" , "downloader" , "digest" ) )


# remove the `#` in order to specify which years to download
# years.to.download <- c( 2009 , 2003 )
# 2003 will download the 2002-2003 survey,
# 2009 will download the 2008-2009 survey, you get the idea.



############################################
# no need to edit anything below this line #
# code begins ---------------------------------------------------------------------
# # # # # # # # #
# program start #
# # # # # # # # #
# checks ---------------------------------------------------------------------
# check if 7z is working
if( system( paste0('"', path.to.7z , '" -h' ) ) != 0 ) stop("you need to install 7-zip")

library(gdata) 				# load the gdata package (imports excel [.xls] files into R)
library(downloader)			# downloads and then runs the source() function on scripts from github


# load the download_cached and related functions
# to prevent re-downloading of files once they've been downloaded.
source_url( 
  "https://raw.githubusercontent.com/ajdamico/asdfree/master/Download%20Cache/download%20cache.R" , 
  prompt = FALSE , 
  echo = FALSE 
)
# code flow ---------------------------------------------------------------------
# create two temporary files and a temporary directory..
tf <- tempfile() ; tf2 <- tempfile() ; td <- tempdir()



# data file location inside the FTP directory
# sas importation instructions location inside the FTP directory
for ( year in years.to.download ){
  # microdata filepath on the IBGE FTP site
  ftp.path <-
    paste0(
      "ftp://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_" ,
      year - 1 ,
      "_" ,
      year , 
      "/Microdados/"
    )
if ( year < 2009 ){
  cadprodutos <- paste0( ftp.path , "Documentacao.zip" )
} else {
  cadprodutos <- paste0( ftp.path , "documentacao.zip" )
}

# download the sas importation instructions inside the same FTP directory..
download_cached( cadprodutos , tf , mode = "wb" )

# ..then unzip them into the temporary directory
files <- unzip( tf , exdir = td )

# some lines need to be manually encoded	
Encoding( files ) <- 'latin1'


# figure out which is the Products Registry File
cda <- files[ grep( 'cadastro de produtos pof' , tolower( files ) ) ]
componentes_g <- read.xls( cda , sheet = 1 , 0 , "QUADRO", colClasses = 'character' )[,c(1:4)]

save( 
  componentes_g, 
  file = tolower( paste0( './' , year , 'cadastro-produtos' , ".rda" ) )
)
}