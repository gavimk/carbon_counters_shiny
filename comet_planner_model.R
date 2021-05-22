library(tidyverse)
library(tidyr)
library(here)
library(janitor)
library(fable)

acres <- read.csv("acres.csv") %>% 
  clean_names

baseline_c <- read.csv("baseline_c.csv") %>% 
  clean_names %>% 
  mutate(total=rowcrop+orchards+vineyards+pastureland+rangeland+fodder+fallow+greenhouse)

baseline_no2 <- read.csv("baseline_no2.csv") %>% 
  clean_names%>% 
  mutate(total=rowcrop+orchards+vineyards+pastureland+rangeland+fodder+fallow+greenhouse)

#-----------------------------------
#INPUTS - implementation percentages

compost_pct_rowcrop <- .1
compost_pct_orchards <- .1
compost_pct_vineyards <- .1
compost_pct_pastureland <- .1
compost_pct_rangeland <- .025

covercrop_pct_rowcrop <- .1
covercrop_pct_orchards <- .1
covercrop_pct_vineyards <- .1
covercrop_pct_pastureland <- 0
covercrop_pct_rangeland <- 0

hedgerow_pct_rowcrop <- .03
hedgerow_pct_orchards <- .03
hedgerow_pct_vineyards <- .03
hedgerow_pct_pastureland <- .03
hedgerow_pct_rangeland <- .0075

mulch_pct_rowcrop <- .1
mulch_pct_orchards <- .1
mulch_pct_vineyards <- .1
mulch_pct_pastureland <- 0
mulch_pct_rangeland <- 0

reducedtill_pct_rowcrop <- .1
reducedtill_pct_orchards <- .1
reducedtill_pct_vineyards <- .1
reducedtill_pct_pastureland <- 0
reducedtill_pct_rangeland <- 0

restoration_pct_rowcrop <- .03
restoration_pct_orchards <- .03
restoration_pct_vineyards <- .03
restoration_pct_pastureland <- .03
restoration_pct_rangeland <- .0075

#-----------------------------------
#implementation percentages converted to vectors representing slow ramp up %'s

compost_pct_rowcrop_ramp <- c(compost_pct_rowcrop/10,compost_pct_rowcrop/10*2,compost_pct_rowcrop/10*3,compost_pct_rowcrop/10*4,compost_pct_rowcrop/10*5,compost_pct_rowcrop/10*6,compost_pct_rowcrop/10*7,compost_pct_rowcrop/10*8,compost_pct_rowcrop/10*9,compost_pct_rowcrop)
compost_pct_orchards_ramp <- c(compost_pct_orchards/10,compost_pct_orchards/10*2,compost_pct_orchards/10*3,compost_pct_orchards/10*4,compost_pct_orchards/10*5,compost_pct_orchards/10*6,compost_pct_orchards/10*7,compost_pct_orchards/10*8,compost_pct_orchards/10*9,compost_pct_orchards)
compost_pct_vineyards_ramp <- c(compost_pct_vineyards/10,compost_pct_vineyards/10*2,compost_pct_vineyards/10*3,compost_pct_vineyards/10*4,compost_pct_vineyards/10*5,compost_pct_vineyards/10*6,compost_pct_vineyards/10*7,compost_pct_vineyards/10*8,compost_pct_vineyards/10*9,compost_pct_vineyards)
compost_pct_pastureland_ramp <- c(compost_pct_pastureland/10,compost_pct_pastureland/10*2,compost_pct_pastureland/10*3,compost_pct_pastureland/10*4,compost_pct_pastureland/10*5,compost_pct_pastureland/10*6,compost_pct_pastureland/10*7,compost_pct_pastureland/10*8,compost_pct_pastureland/10*9,compost_pct_pastureland)
compost_pct_rangeland_ramp <- c(compost_pct_rangeland/10,compost_pct_rangeland/10*2,compost_pct_rangeland/10*3,compost_pct_rangeland/10*4,compost_pct_rangeland/10*5,compost_pct_rangeland/10*6,compost_pct_rangeland/10*7,compost_pct_rangeland/10*8,compost_pct_rangeland/10*9,compost_pct_rangeland)

covercrop_pct_rowcrop_ramp <- c(covercrop_pct_rowcrop/10,covercrop_pct_rowcrop/10*2,covercrop_pct_rowcrop/10*3,covercrop_pct_rowcrop/10*4,covercrop_pct_rowcrop/10*5,covercrop_pct_rowcrop/10*6,covercrop_pct_rowcrop/10*7,covercrop_pct_rowcrop/10*8,covercrop_pct_rowcrop/10*9,covercrop_pct_rowcrop)
covercrop_pct_orchards_ramp <- c(covercrop_pct_orchards/10,covercrop_pct_orchards/10*2,covercrop_pct_orchards/10*3,covercrop_pct_orchards/10*4,covercrop_pct_orchards/10*5,covercrop_pct_orchards/10*6,covercrop_pct_orchards/10*7,covercrop_pct_orchards/10*8,covercrop_pct_orchards/10*9,covercrop_pct_orchards)
covercrop_pct_vineyards_ramp <- c(covercrop_pct_vineyards/10,covercrop_pct_vineyards/10*2,covercrop_pct_vineyards/10*3,covercrop_pct_vineyards/10*4,covercrop_pct_vineyards/10*5,covercrop_pct_vineyards/10*6,covercrop_pct_vineyards/10*7,covercrop_pct_vineyards/10*8,covercrop_pct_vineyards/10*9,covercrop_pct_vineyards)
covercrop_pct_pastureland_ramp <- c(covercrop_pct_pastureland/10,covercrop_pct_pastureland/10*2,covercrop_pct_pastureland/10*3,covercrop_pct_pastureland/10*4,covercrop_pct_pastureland/10*5,covercrop_pct_pastureland/10*6,covercrop_pct_pastureland/10*7,covercrop_pct_pastureland/10*8,covercrop_pct_pastureland/10*9,covercrop_pct_pastureland)
covercrop_pct_rangeland_ramp <- c(covercrop_pct_rangeland/10,covercrop_pct_rangeland/10*2,covercrop_pct_rangeland/10*3,covercrop_pct_rangeland/10*4,covercrop_pct_rangeland/10*5,covercrop_pct_rangeland/10*6,covercrop_pct_rangeland/10*7,covercrop_pct_rangeland/10*8,covercrop_pct_rangeland/10*9,covercrop_pct_rangeland)

hedgerow_pct_rowcrop_ramp <- c(hedgerow_pct_rowcrop/10,hedgerow_pct_rowcrop/10*2,hedgerow_pct_rowcrop/10*3,hedgerow_pct_rowcrop/10*4,hedgerow_pct_rowcrop/10*5,hedgerow_pct_rowcrop/10*6,hedgerow_pct_rowcrop/10*7,hedgerow_pct_rowcrop/10*8,hedgerow_pct_rowcrop/10*9,hedgerow_pct_rowcrop)
hedgerow_pct_orchards_ramp <- c(hedgerow_pct_orchards/10,hedgerow_pct_orchards/10*2,hedgerow_pct_orchards/10*3,hedgerow_pct_orchards/10*4,hedgerow_pct_orchards/10*5,hedgerow_pct_orchards/10*6,hedgerow_pct_orchards/10*7,hedgerow_pct_orchards/10*8,hedgerow_pct_orchards/10*9,hedgerow_pct_orchards)
hedgerow_pct_vineyards_ramp <- c(hedgerow_pct_vineyards/10,hedgerow_pct_vineyards/10*2,hedgerow_pct_vineyards/10*3,hedgerow_pct_vineyards/10*4,hedgerow_pct_vineyards/10*5,hedgerow_pct_vineyards/10*6,hedgerow_pct_vineyards/10*7,hedgerow_pct_vineyards/10*8,hedgerow_pct_vineyards/10*9,hedgerow_pct_vineyards)
hedgerow_pct_pastureland_ramp <- c(hedgerow_pct_pastureland/10,hedgerow_pct_pastureland/10*2,hedgerow_pct_pastureland/10*3,hedgerow_pct_pastureland/10*4,hedgerow_pct_pastureland/10*5,hedgerow_pct_pastureland/10*6,hedgerow_pct_pastureland/10*7,hedgerow_pct_pastureland/10*8,hedgerow_pct_pastureland/10*9,hedgerow_pct_pastureland)
hedgerow_pct_rangeland_ramp <- c(hedgerow_pct_rangeland/10,hedgerow_pct_rangeland/10*2,hedgerow_pct_rangeland/10*3,hedgerow_pct_rangeland/10*4,hedgerow_pct_rangeland/10*5,hedgerow_pct_rangeland/10*6,hedgerow_pct_rangeland/10*7,hedgerow_pct_rangeland/10*8,hedgerow_pct_rangeland/10*9,hedgerow_pct_rangeland)

mulch_pct_rowcrop_ramp <- c(mulch_pct_rowcrop/10,mulch_pct_rowcrop/10*2,mulch_pct_rowcrop/10*3,mulch_pct_rowcrop/10*4,mulch_pct_rowcrop/10*5,mulch_pct_rowcrop/10*6,mulch_pct_rowcrop/10*7,mulch_pct_rowcrop/10*8,mulch_pct_rowcrop/10*9,mulch_pct_rowcrop)
mulch_pct_orchards_ramp <- c(mulch_pct_orchards/10,mulch_pct_orchards/10*2,mulch_pct_orchards/10*3,mulch_pct_orchards/10*4,mulch_pct_orchards/10*5,mulch_pct_orchards/10*6,mulch_pct_orchards/10*7,mulch_pct_orchards/10*8,mulch_pct_orchards/10*9,mulch_pct_orchards)
mulch_pct_vineyards_ramp <- c(mulch_pct_vineyards/10,mulch_pct_vineyards/10*2,mulch_pct_vineyards/10*3,mulch_pct_vineyards/10*4,mulch_pct_vineyards/10*5,mulch_pct_vineyards/10*6,mulch_pct_vineyards/10*7,mulch_pct_vineyards/10*8,mulch_pct_vineyards/10*9,mulch_pct_vineyards)
mulch_pct_pastureland_ramp <- c(mulch_pct_pastureland/10,mulch_pct_pastureland/10*2,mulch_pct_pastureland/10*3,mulch_pct_pastureland/10*4,mulch_pct_pastureland/10*5,mulch_pct_pastureland/10*6,mulch_pct_pastureland/10*7,mulch_pct_pastureland/10*8,mulch_pct_pastureland/10*9,mulch_pct_pastureland)
mulch_pct_rangeland_ramp <- c(mulch_pct_rangeland/10,mulch_pct_rangeland/10*2,mulch_pct_rangeland/10*3,mulch_pct_rangeland/10*4,mulch_pct_rangeland/10*5,mulch_pct_rangeland/10*6,mulch_pct_rangeland/10*7,mulch_pct_rangeland/10*8,mulch_pct_rangeland/10*9,mulch_pct_rangeland)

reducedtill_pct_rowcrop_ramp <- c(reducedtill_pct_rowcrop/10,reducedtill_pct_rowcrop/10*2,reducedtill_pct_rowcrop/10*3,reducedtill_pct_rowcrop/10*4,reducedtill_pct_rowcrop/10*5,reducedtill_pct_rowcrop/10*6,reducedtill_pct_rowcrop/10*7,reducedtill_pct_rowcrop/10*8,reducedtill_pct_rowcrop/10*9,reducedtill_pct_rowcrop)
reducedtill_pct_orchards_ramp <- c(reducedtill_pct_orchards/10,reducedtill_pct_orchards/10*2,reducedtill_pct_orchards/10*3,reducedtill_pct_orchards/10*4,reducedtill_pct_orchards/10*5,reducedtill_pct_orchards/10*6,reducedtill_pct_orchards/10*7,reducedtill_pct_orchards/10*8,reducedtill_pct_orchards/10*9,reducedtill_pct_orchards)
reducedtill_pct_vineyards_ramp <- c(reducedtill_pct_vineyards/10,reducedtill_pct_vineyards/10*2,reducedtill_pct_vineyards/10*3,reducedtill_pct_vineyards/10*4,reducedtill_pct_vineyards/10*5,reducedtill_pct_vineyards/10*6,reducedtill_pct_vineyards/10*7,reducedtill_pct_vineyards/10*8,reducedtill_pct_vineyards/10*9,reducedtill_pct_vineyards)
reducedtill_pct_pastureland_ramp <- c(reducedtill_pct_pastureland/10,reducedtill_pct_pastureland/10*2,reducedtill_pct_pastureland/10*3,reducedtill_pct_pastureland/10*4,reducedtill_pct_pastureland/10*5,reducedtill_pct_pastureland/10*6,reducedtill_pct_pastureland/10*7,reducedtill_pct_pastureland/10*8,reducedtill_pct_pastureland/10*9,reducedtill_pct_pastureland)
reducedtill_pct_rangeland_ramp <- c(reducedtill_pct_rangeland/10,reducedtill_pct_rangeland/10*2,reducedtill_pct_rangeland/10*3,reducedtill_pct_rangeland/10*4,reducedtill_pct_rangeland/10*5,reducedtill_pct_rangeland/10*6,reducedtill_pct_rangeland/10*7,reducedtill_pct_rangeland/10*8,reducedtill_pct_rangeland/10*9,reducedtill_pct_rangeland)

restoration_pct_rowcrop_ramp <- c(restoration_pct_rowcrop/10,restoration_pct_rowcrop/10*2,restoration_pct_rowcrop/10*3,restoration_pct_rowcrop/10*4,restoration_pct_rowcrop/10*5,restoration_pct_rowcrop/10*6,restoration_pct_rowcrop/10*7,restoration_pct_rowcrop/10*8,restoration_pct_rowcrop/10*9,restoration_pct_rowcrop)
restoration_pct_orchards_ramp <- c(restoration_pct_orchards/10,restoration_pct_orchards/10*2,restoration_pct_orchards/10*3,restoration_pct_orchards/10*4,restoration_pct_orchards/10*5,restoration_pct_orchards/10*6,restoration_pct_orchards/10*7,restoration_pct_orchards/10*8,restoration_pct_orchards/10*9,restoration_pct_orchards)
restoration_pct_vineyards_ramp <- c(restoration_pct_vineyards/10,restoration_pct_vineyards/10*2,restoration_pct_vineyards/10*3,restoration_pct_vineyards/10*4,restoration_pct_vineyards/10*5,restoration_pct_vineyards/10*6,restoration_pct_vineyards/10*7,restoration_pct_vineyards/10*8,restoration_pct_vineyards/10*9,restoration_pct_vineyards)
restoration_pct_pastureland_ramp <- c(restoration_pct_pastureland/10,restoration_pct_pastureland/10*2,restoration_pct_pastureland/10*3,restoration_pct_pastureland/10*4,restoration_pct_pastureland/10*5,restoration_pct_pastureland/10*6,restoration_pct_pastureland/10*7,restoration_pct_pastureland/10*8,restoration_pct_pastureland/10*9,restoration_pct_pastureland)
restoration_pct_rangeland_ramp <- c(restoration_pct_rangeland/10,restoration_pct_rangeland/10*2,restoration_pct_rangeland/10*3,restoration_pct_rangeland/10*4,restoration_pct_rangeland/10*5,restoration_pct_rangeland/10*6,restoration_pct_rangeland/10*7,restoration_pct_rangeland/10*8,restoration_pct_rangeland/10*9,restoration_pct_rangeland)

#----------------------------------------------------------------------------------
#COMPOST (currently assuming high N ratio)

#loading carbon & emissions reductions coefficients

compost_rates <- read.csv("compost.csv")

compost_c_lu <- compost_rates$Carbon
names(compost_c_lu) <- compost_rates$crop_type
compost_c_rowcrop <- compost_c_lu['row_crop']
compost_c_orchards <- compost_c_lu['orchards']
compost_c_vineyards <- compost_c_lu['vineyards']
compost_c_pastureland <- compost_c_lu['pastureland']
compost_c_rangeland <- compost_c_lu['rangeland']

compost_total_lu <- compost_rates$Total
names(compost_total_lu) <- compost_rates$crop_type
compost_total_rowcrop <- compost_total_lu['row_crop']
compost_total_orchards <- compost_total_lu['orchards']
compost_total_vineyards <- compost_total_lu['vineyards']
compost_total_pastureland <- compost_total_lu['pastureland']
compost_total_rangeland <- compost_total_lu['rangeland']

#calculating carbon sequestration

compost_sequestration <- acres %>%
  filter(year > 2020) %>% 
  select(year,rowcrop,orchards,vineyards,pastureland,rangeland) %>% 
  mutate(rowcrop = rowcrop*compost_pct_rowcrop_ramp*compost_c_rowcrop,
         orchards = orchards*compost_pct_orchards_ramp*compost_c_orchards,
         vineyards = vineyards*compost_pct_vineyards_ramp*compost_c_vineyards,
         pastureland = pastureland*compost_pct_pastureland_ramp*compost_c_pastureland,
         rangeland = rangeland*compost_pct_rangeland_ramp*compost_c_rangeland,
         total=rowcrop+orchards+vineyards+pastureland+rangeland)

#Calculating total sequestration and emissions reductions

compost_total_reductions <- acres %>%
  filter(year > 2020) %>% 
  select(year,rowcrop,orchards,vineyards,pastureland,rangeland) %>% 
  mutate(rowcrop = rowcrop*compost_pct_rowcrop_ramp*compost_total_rowcrop,
         orchards = orchards*compost_pct_orchards_ramp*compost_total_orchards,
         vineyards = vineyards*compost_pct_vineyards_ramp*compost_total_vineyards,
         pastureland = pastureland*compost_pct_pastureland_ramp*compost_total_pastureland,
         rangeland = rangeland*compost_pct_rangeland_ramp*compost_total_rangeland,
         total=rowcrop+orchards+vineyards+pastureland+rangeland)

#Calculating total carbon stock

compost_c_stock <- baseline_c %>%
  filter(year > 2020) %>% 
  mutate(rowcrop=rowcrop+cumsum(compost_sequestration$rowcrop)/3.67,
         orchards=orchards+cumsum(compost_sequestration$orchards)/3.67,
         vineyards=vineyards+cumsum(compost_sequestration$vineyards/3.67),
         pastureland=pastureland+cumsum(compost_sequestration$pastureland)/3.67,
         rangeland=rangeland+cumsum(compost_sequestration$rangeland)/3.67,
         total=rowcrop+orchards+vineyards+pastureland+rangeland+fodder+fallow+greenhouse)

#----------------------------------------------------------------------
#COVER CROPS

#loading carbon & emissions reductions coefficients

covercrop_rates <- read.csv("covercrop.csv")

covercrop_c_lu <- covercrop_rates$Carbon
names(covercrop_c_lu) <- covercrop_rates$crop_type
covercrop_c_rowcrop <- covercrop_c_lu['row_crop']
covercrop_c_orchards <- covercrop_c_lu['orchards']
covercrop_c_vineyards <- covercrop_c_lu['vineyards']
covercrop_c_pastureland <- covercrop_c_lu['pastureland']
covercrop_c_rangeland <- covercrop_c_lu['rangeland']

covercrop_total_lu <- covercrop_rates$Total
names(covercrop_total_lu) <- covercrop_rates$crop_type
covercrop_total_rowcrop <- covercrop_total_lu['row_crop']
covercrop_total_orchards <- covercrop_total_lu['orchards']
covercrop_total_vineyards <- covercrop_total_lu['vineyards']
covercrop_total_pastureland <- covercrop_total_lu['pastureland']
covercrop_total_rangeland <- covercrop_total_lu['rangeland']

#calculating carbon sequestration

covercrop_sequestration <- acres %>%
  filter(year > 2020) %>% 
  select(year,rowcrop,orchards,vineyards,pastureland,rangeland) %>% 
  mutate(rowcrop = rowcrop*covercrop_pct_rowcrop_ramp*covercrop_c_rowcrop,
         orchards = orchards*covercrop_pct_orchards_ramp*covercrop_c_orchards,
         vineyards = vineyards*covercrop_pct_vineyards_ramp*covercrop_c_vineyards,
         pastureland = pastureland*covercrop_pct_pastureland_ramp*covercrop_c_pastureland,
         rangeland = rangeland*covercrop_pct_rangeland_ramp*covercrop_c_rangeland,
         total=rowcrop+orchards+vineyards+pastureland+rangeland)

#Calculating total sequestration and emissions reductions

covercrop_total_reductions <- acres %>%
  filter(year > 2020) %>% 
  select(year,rowcrop,orchards,vineyards,pastureland,rangeland) %>% 
  mutate(rowcrop = rowcrop*covercrop_pct_rowcrop_ramp*covercrop_total_rowcrop,
         orchards = orchards*covercrop_pct_orchards_ramp*covercrop_total_orchards,
         vineyards = vineyards*covercrop_pct_vineyards_ramp*covercrop_total_vineyards,
         pastureland = pastureland*covercrop_pct_pastureland_ramp*covercrop_total_pastureland,
         rangeland = rangeland*covercrop_pct_rangeland_ramp*covercrop_total_rangeland,
         total=rowcrop+orchards+vineyards+pastureland+rangeland)

#Calculating total carbon stock

covercrop_c_stock <- baseline_c %>%
  filter(year > 2020) %>% 
  mutate(rowcrop=rowcrop+cumsum(covercrop_sequestration$rowcrop)/3.67,
         orchards=orchards+cumsum(covercrop_sequestration$orchards)/3.67,
         vineyards=vineyards+cumsum(covercrop_sequestration$vineyards/3.67),
         pastureland=pastureland+cumsum(covercrop_sequestration$pastureland)/3.67,
         rangeland=rangeland+cumsum(covercrop_sequestration$rangeland)/3.67,
         total=rowcrop+orchards+vineyards+pastureland+rangeland+fodder+fallow+greenhouse)

#----------------------------------------------------------------------
#HEDGE ROW

#loading carbon & emissions reductions coefficients

hedgerow_rates <- read.csv("hedgerow.csv")

hedgerow_c_lu <- hedgerow_rates$Carbon
names(hedgerow_c_lu) <- hedgerow_rates$crop_type
hedgerow_c_rowcrop <- hedgerow_c_lu['row_crop']
hedgerow_c_orchards <- hedgerow_c_lu['orchards']
hedgerow_c_vineyards <- hedgerow_c_lu['vineyards']
hedgerow_c_pastureland <- hedgerow_c_lu['pastureland']
hedgerow_c_rangeland <- hedgerow_c_lu['rangeland']

hedgerow_total_lu <- hedgerow_rates$Total
names(hedgerow_total_lu) <- hedgerow_rates$crop_type
hedgerow_total_rowcrop <- hedgerow_total_lu['row_crop']
hedgerow_total_orchards <- hedgerow_total_lu['orchards']
hedgerow_total_vineyards <- hedgerow_total_lu['vineyards']
hedgerow_total_pastureland <- hedgerow_total_lu['pastureland']
hedgerow_total_rangeland <- hedgerow_total_lu['rangeland']

#calculating carbon sequestration

hedgerow_sequestration <- acres %>%
  filter(year > 2020) %>% 
  select(year,rowcrop,orchards,vineyards,pastureland,rangeland) %>% 
  mutate(rowcrop = rowcrop*hedgerow_pct_rowcrop_ramp*hedgerow_c_rowcrop,
         orchards = orchards*hedgerow_pct_orchards_ramp*hedgerow_c_orchards,
         vineyards = vineyards*hedgerow_pct_vineyards_ramp*hedgerow_c_vineyards,
         pastureland = pastureland*hedgerow_pct_pastureland_ramp*hedgerow_c_pastureland,
         rangeland = rangeland*hedgerow_pct_rangeland_ramp*hedgerow_c_rangeland,
         total=rowcrop+orchards+vineyards+pastureland+rangeland)

#Calculating total sequestration and emissions reductions

hedgerow_total_reductions <- acres %>%
  filter(year > 2020) %>% 
  select(year,rowcrop,orchards,vineyards,pastureland,rangeland) %>% 
  mutate(rowcrop = rowcrop*hedgerow_pct_rowcrop_ramp*hedgerow_total_rowcrop,
         orchards = orchards*hedgerow_pct_orchards_ramp*hedgerow_total_orchards,
         vineyards = vineyards*hedgerow_pct_vineyards_ramp*hedgerow_total_vineyards,
         pastureland = pastureland*hedgerow_pct_pastureland_ramp*hedgerow_total_pastureland,
         rangeland = rangeland*hedgerow_pct_rangeland_ramp*hedgerow_total_rangeland,
         total=rowcrop+orchards+vineyards+pastureland+rangeland)

#Calculating total carbon stock

hedgerow_c_stock <- baseline_c %>%
  filter(year > 2020) %>% 
  mutate(rowcrop=rowcrop+cumsum(hedgerow_sequestration$rowcrop)/3.67,
         orchards=orchards+cumsum(hedgerow_sequestration$orchards)/3.67,
         vineyards=vineyards+cumsum(hedgerow_sequestration$vineyards/3.67),
         pastureland=pastureland+cumsum(hedgerow_sequestration$pastureland)/3.67,
         rangeland=rangeland+cumsum(hedgerow_sequestration$rangeland)/3.67,
         total=rowcrop+orchards+vineyards+pastureland+rangeland+fodder+fallow+greenhouse)

#----------------------------------------------------------------------
#MULCH

#loading carbon & emissions reductions coefficients

mulch_rates <- read.csv("mulch.csv")

mulch_c_lu <- mulch_rates$Carbon
names(mulch_c_lu) <- mulch_rates$crop_type
mulch_c_rowcrop <- mulch_c_lu['row_crop']
mulch_c_orchards <- mulch_c_lu['orchards']
mulch_c_vineyards <- mulch_c_lu['vineyards']
mulch_c_pastureland <- mulch_c_lu['pastureland']
mulch_c_rangeland <- mulch_c_lu['rangeland']

mulch_total_lu <- mulch_rates$Total
names(mulch_total_lu) <- mulch_rates$crop_type
mulch_total_rowcrop <- mulch_total_lu['row_crop']
mulch_total_orchards <- mulch_total_lu['orchards']
mulch_total_vineyards <- mulch_total_lu['vineyards']
mulch_total_pastureland <- mulch_total_lu['pastureland']
mulch_total_rangeland <- mulch_total_lu['rangeland']

#calculating carbon sequestration

mulch_sequestration <- acres %>%
  filter(year > 2020) %>% 
  select(year,rowcrop,orchards,vineyards,pastureland,rangeland) %>% 
  mutate(rowcrop = rowcrop*mulch_pct_rowcrop_ramp*mulch_c_rowcrop,
         orchards = orchards*mulch_pct_orchards_ramp*mulch_c_orchards,
         vineyards = vineyards*mulch_pct_vineyards_ramp*mulch_c_vineyards,
         pastureland = pastureland*mulch_pct_pastureland_ramp*mulch_c_pastureland,
         rangeland = rangeland*mulch_pct_rangeland_ramp*mulch_c_rangeland,
         total=rowcrop+orchards+vineyards+pastureland+rangeland)

#Calculating total sequestration and emissions reductions

mulch_total_reductions <- acres %>%
  filter(year > 2020) %>% 
  select(year,rowcrop,orchards,vineyards,pastureland,rangeland) %>% 
  mutate(rowcrop = rowcrop*mulch_pct_rowcrop_ramp*mulch_total_rowcrop,
         orchards = orchards*mulch_pct_orchards_ramp*mulch_total_orchards,
         vineyards = vineyards*mulch_pct_vineyards_ramp*mulch_total_vineyards,
         pastureland = pastureland*mulch_pct_pastureland_ramp*mulch_total_pastureland,
         rangeland = rangeland*mulch_pct_rangeland_ramp*mulch_total_rangeland,
         total=rowcrop+orchards+vineyards+pastureland+rangeland)

#Calculating total carbon stock

mulch_c_stock <- baseline_c %>%
  filter(year > 2020) %>% 
  mutate(rowcrop=rowcrop+cumsum(mulch_sequestration$rowcrop)/3.67,
         orchards=orchards+cumsum(mulch_sequestration$orchards)/3.67,
         vineyards=vineyards+cumsum(mulch_sequestration$vineyards/3.67),
         pastureland=pastureland+cumsum(mulch_sequestration$pastureland)/3.67,
         rangeland=rangeland+cumsum(mulch_sequestration$rangeland)/3.67,
         total=rowcrop+orchards+vineyards+pastureland+rangeland+fodder+fallow+greenhouse)

#----------------------------------------------------------------------
#REDUCED TILL

#loading carbon & emissions reductions coefficients

reducedtill_rates <- read.csv("reducedtill.csv")

reducedtill_c_lu <- reducedtill_rates$Carbon
names(reducedtill_c_lu) <- reducedtill_rates$crop_type
reducedtill_c_rowcrop <- reducedtill_c_lu['row_crop']
reducedtill_c_orchards <- reducedtill_c_lu['orchards']
reducedtill_c_vineyards <- reducedtill_c_lu['vineyards']
reducedtill_c_pastureland <- reducedtill_c_lu['pastureland']
reducedtill_c_rangeland <- reducedtill_c_lu['rangeland']

reducedtill_total_lu <- reducedtill_rates$Total
names(reducedtill_total_lu) <- reducedtill_rates$crop_type
reducedtill_total_rowcrop <- reducedtill_total_lu['row_crop']
reducedtill_total_orchards <- reducedtill_total_lu['orchards']
reducedtill_total_vineyards <- reducedtill_total_lu['vineyards']
reducedtill_total_pastureland <- reducedtill_total_lu['pastureland']
reducedtill_total_rangeland <- reducedtill_total_lu['rangeland']

#calculating carbon sequestration

reducedtill_sequestration <- acres %>%
  filter(year > 2020) %>% 
  select(year,rowcrop,orchards,vineyards,pastureland,rangeland) %>% 
  mutate(rowcrop = rowcrop*reducedtill_pct_rowcrop_ramp*reducedtill_c_rowcrop,
         orchards = orchards*reducedtill_pct_orchards_ramp*reducedtill_c_orchards,
         vineyards = vineyards*reducedtill_pct_vineyards_ramp*reducedtill_c_vineyards,
         pastureland = pastureland*reducedtill_pct_pastureland_ramp*reducedtill_c_pastureland,
         rangeland = rangeland*reducedtill_pct_rangeland_ramp*reducedtill_c_rangeland,
         total=rowcrop+orchards+vineyards+pastureland+rangeland)

#Calculating total sequestration and emissions reductions

reducedtill_total_reductions <- acres %>%
  filter(year > 2020) %>% 
  select(year,rowcrop,orchards,vineyards,pastureland,rangeland) %>% 
  mutate(rowcrop = rowcrop*reducedtill_pct_rowcrop_ramp*reducedtill_total_rowcrop,
         orchards = orchards*reducedtill_pct_orchards_ramp*reducedtill_total_orchards,
         vineyards = vineyards*reducedtill_pct_vineyards_ramp*reducedtill_total_vineyards,
         pastureland = pastureland*reducedtill_pct_pastureland_ramp*reducedtill_total_pastureland,
         rangeland = rangeland*reducedtill_pct_rangeland_ramp*reducedtill_total_rangeland,
         total=rowcrop+orchards+vineyards+pastureland+rangeland)

#Calculating total carbon stock

reducedtill_c_stock <- baseline_c %>%
  filter(year > 2020) %>% 
  mutate(rowcrop=rowcrop+cumsum(reducedtill_sequestration$rowcrop)/3.67,
         orchards=orchards+cumsum(reducedtill_sequestration$orchards)/3.67,
         vineyards=vineyards+cumsum(reducedtill_sequestration$vineyards/3.67),
         pastureland=pastureland+cumsum(reducedtill_sequestration$pastureland)/3.67,
         rangeland=rangeland+cumsum(reducedtill_sequestration$rangeland)/3.67,
         total=rowcrop+orchards+vineyards+pastureland+rangeland+fodder+fallow+greenhouse)

#----------------------------------------------------------------------
#RESTORATION

#loading carbon & emissions reductions coefficients

restoration_rates <- read.csv("restoration.csv")

restoration_c_lu <- restoration_rates$Carbon
names(restoration_c_lu) <- restoration_rates$crop_type
restoration_c_rowcrop <- restoration_c_lu['row_crop']
restoration_c_orchards <- restoration_c_lu['orchards']
restoration_c_vineyards <- restoration_c_lu['vineyards']
restoration_c_pastureland <- restoration_c_lu['pastureland']
restoration_c_rangeland <- restoration_c_lu['rangeland']

restoration_total_lu <- restoration_rates$Total
names(restoration_total_lu) <- restoration_rates$crop_type
restoration_total_rowcrop <- restoration_total_lu['row_crop']
restoration_total_orchards <- restoration_total_lu['orchards']
restoration_total_vineyards <- restoration_total_lu['vineyards']
restoration_total_pastureland <- restoration_total_lu['pastureland']
restoration_total_rangeland <- restoration_total_lu['rangeland']

#calculating carbon sequestration

restoration_sequestration <- acres %>%
  filter(year > 2020) %>% 
  select(year,rowcrop,orchards,vineyards,pastureland,rangeland) %>% 
  mutate(rowcrop = rowcrop*restoration_pct_rowcrop_ramp*restoration_c_rowcrop,
         orchards = orchards*restoration_pct_orchards_ramp*restoration_c_orchards,
         vineyards = vineyards*restoration_pct_vineyards_ramp*restoration_c_vineyards,
         pastureland = pastureland*restoration_pct_pastureland_ramp*restoration_c_pastureland,
         rangeland = rangeland*restoration_pct_rangeland_ramp*restoration_c_rangeland,
         total=rowcrop+orchards+vineyards+pastureland+rangeland)

#Calculating total sequestration and emissions reductions

restoration_total_reductions <- acres %>%
  filter(year > 2020) %>% 
  select(year,rowcrop,orchards,vineyards,pastureland,rangeland) %>% 
  mutate(rowcrop = rowcrop*restoration_pct_rowcrop_ramp*restoration_total_rowcrop,
         orchards = orchards*restoration_pct_orchards_ramp*restoration_total_orchards,
         vineyards = vineyards*restoration_pct_vineyards_ramp*restoration_total_vineyards,
         pastureland = pastureland*restoration_pct_pastureland_ramp*restoration_total_pastureland,
         rangeland = rangeland*restoration_pct_rangeland_ramp*restoration_total_rangeland,
         total=rowcrop+orchards+vineyards+pastureland+rangeland)

#Calculating total carbon stock

restoration_c_stock <- baseline_c %>%
  filter(year > 2020) %>% 
  mutate(rowcrop=rowcrop+cumsum(restoration_sequestration$rowcrop)/3.67,
         orchards=orchards+cumsum(restoration_sequestration$orchards)/3.67,
         vineyards=vineyards+cumsum(restoration_sequestration$vineyards/3.67),
         pastureland=pastureland+cumsum(restoration_sequestration$pastureland)/3.67,
         rangeland=rangeland+cumsum(restoration_sequestration$rangeland)/3.67,
         total=rowcrop+orchards+vineyards+pastureland+rangeland+fodder+fallow+greenhouse)


#----------------------------------------------------------------------
#SUMMARY TABLES

summary_c_stock <- baseline_c %>% 
  select(year,total) %>%
  filter(year > 2020) %>%
  rename(baseline=total) %>% 
  mutate(compost=compost_c_stock$total,
         covercrop=covercrop_c_stock$total,
         hedgerow=hedgerow_c_stock$total,
         mulch=mulch_c_stock$total,
         reducedtill=reducedtill_c_stock$total,
         restoration=restoration_c_stock$total)

summary_total_reductions <- data.frame(year=compost_total_reductions$year,
                                       compost=compost_total_reductions$total,
                                       covercrop=covercrop_total_reductions$total,
                                       hedgerow=hedgerow_total_reductions$total,
                                       mulch=mulch_total_reductions$total,
                                       reducedtill=reducedtill_total_reductions$total,
                                       restoration=restoration_total_reductions$total) %>% 
                            adorn_totals("row")



