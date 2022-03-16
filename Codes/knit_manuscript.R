#* manuscript
rmarkdown::render(here::here("GitControlled/Writing/manuscript_cea.rmd"))

#* reply to reviewer 1
rmarkdown::render(here::here("GitControlled/Writing/CEA_2nd_round/replies_rev_1.rmd"))

#* reply to reviewer 2
rmarkdown::render(here::here("GitControlled/Writing/CEA_2nd_round/replies_rev_2.rmd"))

#* reply to editor
rmarkdown::render(here::here("GitControlled/Writing/CEA_3rd_round/replies_editor.rmd"))