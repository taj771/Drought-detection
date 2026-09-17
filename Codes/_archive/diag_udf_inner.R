log("CHECKPOINT 2: inner.R sourced OK")

tryCatch({
  library(sparklyr)
  library(dplyr)
  sc <- spark_connect(method = "databricks")
  log("CHECKPOINT 3: connected")

  # Test 1: scalar spark_apply
  sdf <- copy_to(sc, data.frame(val = 1L, grp = "a"), overwrite = TRUE)
  r1 <- spark_apply(sdf, function(df) { df$out <- df$val * 2L; df },
                    columns = list(val = "integer", grp = "character", out = "integer"))
  r1c <- collect(r1)
  log(paste0("T1 scalar: PASS — out=", r1c$out))

  # Test 2: SPEI on worker
  sdf2 <- copy_to(sc, data.frame(val = 1L, grp = "b"), overwrite = TRUE)
  r2 <- spark_apply(sdf2,
    function(df) {
      library(SPEI)
      df$spei_loaded <- 1L
      df
    },
    columns = list(val = "integer", grp = "character", spei_loaded = "integer")
  )
  r2c <- collect(r2)
  log(paste0("T2 SPEI on worker: PASS — spei_loaded=", r2c$spei_loaded))

  spark_disconnect(sc)
  log("CHECKPOINT 4: disconnected")

}, error = function(e) {
  log(paste0("FAIL: ", conditionMessage(e)))
})

log("DONE")
