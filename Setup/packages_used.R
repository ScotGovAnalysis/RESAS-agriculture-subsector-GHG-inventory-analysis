install.packages("renv")

library(renv)

dependencies<-dependencies(
  path = getwd(),
  root = NULL,
  quiet = NULL,
  progress = TRUE,
  errors = c("reported", "fatal", "ignored"),
  dev = FALSE
)


requiredPackages <- unique(dependencies$Package)

for (package in requiredPackages) { #Installs packages if not yet installed
  if (!requireNamespace(package, quietly = TRUE))
    install.packages(package)
}


