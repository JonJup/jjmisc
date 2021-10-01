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
use_r("compute_indvalstat.R")
use_r("indvalstat.R")
use_r("compute_typical_comm.R")
use_r("typical_comm.R")
use_r("compute_cs.R")
use_r("classification_strength.R")
use_r("get_most_recent_date.R")
use_r("temporal_aggregation.R")

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
use_package("fs")
use_package("lubridate")
use_package("stringr")


#- create a github token. This function loads the github website which provides you with a token.
create_github_token()
#- Store this token
gitcreds_set(url = "https://github.com")
#- initialize repository
use_github()
