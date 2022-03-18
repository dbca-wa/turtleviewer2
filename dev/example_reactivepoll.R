# Retired from server: reactivePoll to trigger an automated data download.
# Automatic data download ---------------------------------------------------#
# This currently deactivated as it feels it gets in the user's way
#
# Issue 1: it runs on session start (wastd data takes ca 40 mins to dl)
# Issue 2: it resets the UI when done which interrupts the user's workflow
#
# A better approach is to create a separate Docker image to run the
# long-running data ETL and download tasks on a scheduled cron job,
# and mount the named volume read-write from both running containers.
#
# Data is downloaded through a scheduled cronjob by etlTurtleNesting.
#
# WAStD sites ---------------------------------------------------------------#
# expire_wastd_sites <- reactiveTimer(1000 * 60 * 1) # Expire every 1 min

# observe({
#   # expire_wastd_sites()
#   invalidateLater(1000 * 60 * 60 * 1)
#
#   waitress_wastd_sites$start()
#   "[{Sys.time()}] Downloading WAStD Sites to {fn_wastd_sites}" %>%
#     glue::glue() %>%
#     wastdr::wastdr_msg_info()
#
#   sites <- wastdr::download_wastd_sites()
#   saveRDS(sites, file = fn_wastd_sites)
#   waitress_wastd_sites$close()
# })

# WAStD data ----------------------------------------------------------------#
# expire_wastd_data <- reactiveTimer(1000 * 60 * 60 * 2) # Expire every 2h
#
# observe({
#   expire_wastd_data()
#
#   "Downloading WAStD Data to {fn_wastd_data}" %>%
#     glue::glue() %>%
#     wastdr::wastdr_msg_info()
#
#   # wastd_data <- wastdr::download_wastd_turtledata(
#   #   max_records = 1000 # TODO: drop limit for prod
#   # )
#   # saveRDS(wastd_data, file = fn_wastd_data)
#
#
#   "WAStD Data saved locally to folder {fn_wastd_data}." %>%
#     glue::glue() %>%
#     wastdr::wastdr_msg_success()
# })

# WAMTRAM data --------------------------------------------------------------#
# expire_wamtram_data <- reactiveTimer(1000 * 60 * 60) # Expire every 1h
#
# observe({
#   expire_wamtram_data()
#
#   if (wastdr::w2_online() == FALSE) {
#     "WAMTRAM not accessible. Need to run in DBCA intranet with credentials in env vars." %>%
#       glue::glue() %>%
#       wastdr::wastdr_msg_info()
#   } else {
#     "[{Sys.time()}] Downloading WAMTRAM Data to {fn_w2_data}" %>%
#       glue::glue() %>%
#       wastdr::wastdr_msg_info()
#
#     w2_data <- wastdr::download_w2_data(save = fn_w2_data)
#
#     "WAMTRAM Data saved locally to folder {fn_w2_data}." %>%
#       glue::glue() %>%
#       wastdr::wastdr_msg_success()
#   }
# })
