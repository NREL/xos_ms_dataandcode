# see how much time the analysis takes
ptm <- proc.time()

# set working directory to source file location
setwd(getSrcDirectory(function(){})[1])

#run analysis scripts: 

# validation selection

source("./trainingTestSplitting.R")

# part 1 models

source("./part1_fullDataSet_modelBuild.R")
source("./part1_lateDataSet_modelBuild.R")
source("./part1_earlyWasteDataSet_modelBuild.R")

# part 2 models

source("./part2_lateDataSet_modelBuild.R")
source("./part2_earlyWasteDataSet_modelBuild.R")

# produce all figures, except for 1 which is a powerpoint diagram: 

source("./fig2.R")
source("./fig3.R")
source("./fig4.R")
source("./fig5.R")
source("./fig6.R")
source("./fig7.R")
source("./fig8.R")
source("./fig9.R")

# produce tables: 

source("./table1.R")
source("./table2.R")
source("./table3.R")

# produce supplementary material

source("./S1.R")
source("./S2.R")
source("./S3.R")
source("./S4.R")
source("./S5.R")
source("./S6.R")
source("./S7.R")
source("./S8.R")
source("./S9.R")
source("./S10.R")
source("./S11.R")
source("./S12.R")
source("./S13.R")
source("./S14.R")
source("./S15.R")
source("./S16.R")
source("./S17.R")

# Stop the clock
return(proc.time() - ptm)