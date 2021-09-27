pacman::p_load(devtools, fs, tidyverse)


#. only once
# create_package("../Documents/Uni/projects/jjmisc")
# use_git()

use_r("update_taxonomy.R")
use_r("ct_prepare_data_1.R")
use_r("add_typology.R")
use_r("ct_prepare_data_2.R")
use_r("fill_taxon_table.R")
use_r("setNAcols.R")

load_all()
check()
use_mit_license("Jonathan Jupke")
document()

use_package("data.table")
use_package("taxize")
use_package("sf")
use_package("dplyr")
use_package("tidyr")
use_package("parallelDist")


#- create a github token. This function loads the github website which provides you with a token.
create_github_token()
#- Store this token
gitcreds_set(url = "https://github.com")
#- initialize repository
use_github()
