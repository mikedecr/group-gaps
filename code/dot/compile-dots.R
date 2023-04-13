# ---- compile dot files into PDF -----------------------

# read dot
dots <- list.files(here::here("code", "dot"), pattern = "[.]dot")
stems <- stringr::str_replace(dots, "[.]dot", "")

system('mkdir writing-rmd/assets/dot-img')

# compile to PDF
# set font if needed
lapply(stems, function(stem) 
       stringr::str_glue("dot -Tpdf -Nfontname='Linux Libertine' code/dot/{stem}.dot > writing-rmd/assets/dot-img/{stem}.pdf") |>
         as.character() |>
         system())
