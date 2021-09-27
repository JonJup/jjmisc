pacman::p_load(devtools, fs, tidyverse)
create_package("../Documents/01_Uni/projects/jjmisc")
use_git()
use_r("update_taxonomy.R")

load_all()
