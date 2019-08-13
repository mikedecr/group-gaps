# ---- compile dot files into PDF -----------------------

# read dot
dots <- list.files(here("code", "dot"), pattern = "[.]dot")
stems <- str_replace(dots, "[.]dot", "")

# compile to PDF
# set font if needed
lapply(stems, function(stem) 
       str_glue("dot -Tpdf -Nfontname='Times New Roman' code/dot/{stem}.dot > writing-rmd/paper-figs/dot-{stem}.pdf") %>%
         as.character() %>%
         system())
