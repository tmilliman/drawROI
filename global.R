## setup default archive dir
ARCHIVE_DIR <- '/data/archive'

## setup default app directory for debian based systems
APP_NAME <- 'drawROI'
APP_HOME <- paste0('/srv/shiny-server/', APP_NAME)

## setup KEYPASS file
PASSWD_FILE = paste0(APP_HOME,'/.key.psw')

## set list path to directory containing midday image lists
MIDDAY_LIST_PATH <- paste0(APP_HOME,'/middayList/')

## flag for test mode
TEST_MODE <- TRUE
