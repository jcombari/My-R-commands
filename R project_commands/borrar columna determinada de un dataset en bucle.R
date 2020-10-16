if("VLR_CUOTA" %in% names(rfile))
{
  rfile <- rfile[, ! names(rfile) %in% c("VLR_CUOTA"), drop = F]
}