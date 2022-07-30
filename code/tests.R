box::use(tt = testthat)

tt$expect(
    getOption("box.path") == (exp_path <- "code"), 
    sprintf("box path should be %s, found %s", exp_path, getOption('box.path'))
)

