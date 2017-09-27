TEST_MODE <- FALSE
if(getwd()=='/home/bijan/Projects/drawROI') TEST_MODE <- TRUE
if(getwd()=='/home/shiny/apps/drawROI') TEST_MODE <- TRUE

mountPath <- ''

if(TEST_MODE) mountPath <- '/mnt/klima'

midddayListPath <- paste0(mountPath, '/home/shiny/middayList/')

