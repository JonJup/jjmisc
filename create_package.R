pacman::p_load(devtools, fs, tidyverse)
create_package("../Documents/01_Uni/projects/jjmisc")
use_git()
use_github()
use_r("update_taxonomy.R")
use_git
load_all()
check()
use_mit_license("Jonathan Jupke")
document()

use_package("data.table")
use_package("taxize")
