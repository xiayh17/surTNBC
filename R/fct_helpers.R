# createLink for asia.ensembl.org ------------------------------------------------------
aensLink <- function(val,ens) {
  sprintf('<a href="https://asia.ensembl.org/Homo_sapiens/Gene/Summary?g=%s" target="_blank" class="btn btn-primary">%s</a>',val,ens)
}