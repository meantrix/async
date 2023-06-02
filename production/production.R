# package version
devtools::load_all()
devtools::document()

folder.prod = paste0( 'production/async-',packageVersion("async"))

dir.create(folder.prod)

#build tar.gza
devtools::build(path = folder.prod)

#copy renv

file.copy('renv.lock', folder.prod,overwrite = T)

devtools::build_manual(path = folder.prod)
