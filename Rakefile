desc "Build files for packaging"
task :default do
    sh 'Rscript -e "knitr::knit(\"README.Rmd\")"'
    sh 'Rscript -e "devtools::document()"'
    sh 'Rscript -e "pkgdown::build_site()"'
end
