### LCA Model Setup for Need for Dominance ###

## Load libraries for project
library(tidyverse)
library(tidyplots)
library(patchwork)
library(poLCA)
#library(rjags)

## Load files
setwd("~/Desktop/Projects/Github-Repos/Need-for-Dominance/scripts/r")
Study1 <- read_csv("~/Desktop/Projects/Github-Repos/Need-for-Dominance/data_raw/NFC Study Data.csv")
Study2 <- read_csv("~/Desktop/Projects/Github-Repos/Need-for-Dominance/data_raw/NFC Study 2 Data.csv")

# Initial dataframe prep
Study1$Study <- 1
Study1$REALtgm <- NA
Study1$REALtgw <- NA
Study1$REALpan <- NA
Study1$REALace <- NA
Study1$EVALtgm <- NA
Study1$EVALtgw <- NA
Study1$EVALpan <- NA
Study1$EVALace <- NA
Study1$STAB1tgm <- NA
Study1$STAB1tgw <- NA
Study1$STAB1pan <- NA
Study1$STAB1ace <- NA
Study1$STAB2tgm <- NA
Study1$STAB2tgw <- NA
Study1$STAB2pan <- NA
Study1$STAB2ace <- NA
selection <- c(
  Study = "Study",
  NFC1 = "NFC Items_1",
  NFC2 = "NFC Items_2",
  NFC3 = "NFC Items_3",
  NFC4 = "NFC Items_4",
  NFC5 = "NFC Items_5",
  NFC6 = "NFC Items_6",
  NFC7 = "NFC Items_7",
  NFC8 = "NFC Items_8",
  NFC9 = "NFC Items_9",
  NFC10 = "NFC Items_10",
  NFC11 = "NFC Items_11",
  NFC12 = "NFC Items_12",
  NFC13 = "NFC Items_13",
  NFC14 = "NFC Items_14",
  NFC15 = "NFC Items_15",
  SDO1 = "SDO Items_1",
  SDO2 = "SDO Items_2",
  SDO3 = "SDO Items_3",
  SDO4 = "SDO Items_4",
  SDO5 = "SDO Items_5",
  SDO6 = "SDO Items_6",
  SDO7 = "SDO Items_7",
  SDO8 = "SDO Items_8",
  SDO9 = "SDO Items_9",
  SDO10 = "SDO Items_10",
  SDO11 = "SDO Items_11",
  SDO12 = "SDO Items_12",
  SDO13 = "SDO Items_13",
  SDO14 = "SDO Items_14",
  SDO15 = "SDO Items_15",
  SDO16 = "SDO Items_16",
  REALcgm = "Q8_1",
  REALcgw = "Q8_2",
  REALtgm = "REALtgm",
  REALtgw = "REALtgw",
  REALnb = "Q8_3",
  REALhet = "Q8_6",
  REALgl = "Q8_5",
  REALbi = "Q8_4",
  REALpan = "REALpan",
  REALace = "REALace",
  EVALcgm = "Group Evaluations_1",
  EVALcgw = "Group Evaluations_2",
  EVALtgm = "EVALtgm",
  EVALtgw = "EVALtgw",
  EVALnb = "Group Evaluations_3",
  EVALhet = "Group Evaluations_6",
  EVALgl = "Group Evaluations_5",
  EVALbi = "Group Evaluations_4",
  EVALpan = "EVALpan",
  EVALace = "EVALace",
  STAB1cgm = "Stability Items 1_1",
  STAB1cgw = "Stability Items 1_2",
  STAB1tgm = "STAB1tgm",
  STAB1tgw = "STAB1tgw",
  STAB1nb = "Stability Items 1_3",
  STAB1het = "Stability Items 1_6",
  STAB1gl = "Stability Items 1_5",
  STAB1bi = "Stability Items 1_4",
  STAB1pan = "STAB1pan",
  STAB1ace = "STAB1ace",
  STAB2cgm = "Stability Items 2_1",
  STAB2cgw = "Stability Items 2_2",
  STAB2tgm = "STAB2tgm",
  STAB2tgw = "STAB2tgw",
  STAB2nb = "Stability Items 2_3",
  STAB2het = "Stability Items 2_6",
  STAB2gl = "Stability Items 2_5",
  STAB2bi = "Stability Items 2_4",
  STAB2pan = "STAB2pan",
  STAB2ace = "STAB2ace",
  Lib_Con = "Liberal_Conservative",
  Religiousness = "Religiousness",
  Education = "Education",
  Age = "Age")
# dplyr:: is necessary to avoid namespace conflict as poLCA requires the MASS package
Study1 <- Study1 |> dplyr::select(all_of(selection))

Study2$Study <- 2
selection <- c(
  Study = "Study",
  NFC1 = "NFC_1",
  NFC2 = "NFC_2",
  NFC3 = "NFC_3",
  NFC4 = "NFC_4",
  NFC5 = "NFC_5",
  NFC6 = "NFC_6",
  NFC7 = "NFC_7",
  NFC8 = "NFC_8",
  NFC9 = "NFC_9",
  NFC10 = "NFC_10",
  NFC11 = "NFC_11",
  NFC12 = "NFC_12",
  NFC13 = "NFC_13",
  NFC14 = "NFC_14",
  NFC15 = "NFC_15",
  SDO1 = "SDO_1",
  SDO2 = "SDO_2",
  SDO3 = "SDO_3",
  SDO4 = "SDO_4",
  SDO5 = "SDO_5",
  SDO6 = "SDO_6",
  SDO7 = "SDO_7",
  SDO8 = "SDO_8",
  SDO9 = "SDO_9",
  SDO10 = "SDO_10",
  SDO11 = "SDO_11",
  SDO12 = "SDO_12",
  SDO13 = "SDO_13",
  SDO14 = "SDO_14",
  SDO15 = "SDO_15",
  SDO16 = "SDO_16",
  REALcgm = "Real_Men",
  REALcgw = "Real_Women",
  REALtgm = "Real_TM",
  REALtgw = "Real_TW",
  REALnb = "Real_NB",
  REALhet = "Real_Straight",
  REALgl = "Real_GL",
  REALbi = "Real_Bi",
  REALpan = "Real_Pan",
  REALace = "Real_Ace",
  EVALcgm = "Eval_1",
  EVALcgw = "Eval_2",
  EVALtgm = "Eval_3",
  EVALtgw = "Eval_4",
  EVALnb = "Eval_5",
  EVALhet = "Eval_6",
  EVALgl = "Eval_7",
  EVALbi = "Eval_8",
  EVALpan = "Eval_9",
  EVALace = "Eval_10",
  STAB1cgm = "SB_1_Men",
  STAB1cgw = "SB_1_Women",
  STAB1tgm = "SB_1_TM",
  STAB1tgw = "SB_1_TW",
  STAB1nb = "SB_1_NB",
  STAB1het = "SB_1_Straight",
  STAB1gl = "SB_1_GL",
  STAB1bi = "SB_1_Bi",
  STAB1pan = "SB_1_Pan",
  STAB1ace = "SB_1_Ace",
  STAB2cgm = "SB_2_Men",
  STAB2cgw = "SB_2_Women",
  STAB2tgm = "SB_2_TM",
  STAB2tgw = "SB_2_TW",
  STAB2nb = "SB_2_NB",
  STAB2het = "SB_2_Straight",
  STAB2gl = "SB_2_GL",
  STAB2bi = "SB_2_Bi",
  STAB2pan = "SB_2_Pan",
  STAB2ace = "SB_2_Ace",
  Lib_Con = "Self_Lib_Conserv",
  Religiousness = "Religiosity_1",
  Education = "Education",
  Age = "Age")
Study2 <- Study2 |> dplyr::select(all_of(selection))
rm(selection)
data <- bind_rows(Study1, Study2, .id = "Study")
rm(Study1)
rm(Study2)
data$Study <- as_factor(data$Study)
data$Lib_Con <- as_factor(data$Lib_Con)
data$Education <- as_factor(data$Education)
# Remove obvious mistakes
data <- data[-1, ]
data <- data[-133, ]
# Set eval to minimum 1 instead of 0
data <- data %>%
  mutate(across(43:52, ~ if_else(is.na(.), NA_integer_, . + 1)))
# Integer conversion
data <- data %>%
  mutate(across(33:72, ~ as.integer(as.character(.))))
# Scoring
data$NFC_Total <- rowMeans(data[ , 2:16])
data$SDO_Total <- with(data, rowMeans(cbind(SDO1, SDO2, SDO3, SDO4, 8 - SDO5, 8 - SDO6, 8 - SDO7, 8 - SDO8, SDO9, SDO10, SDO11, SDO12, 8 - SDO13, 8 - SDO14, 8 - SDO15, 8 - SDO16)))
data$CG_Real_Total <- rowSums(cbind(data$REALcgm, data$REALcgw))
data$TG_Real_Total <- rowSums(cbind(data$REALtgm, data$REALtgw))
data$TGNB_Real_Total <- rowSums(cbind(data$REALtgm, data$REALtgw, data$REALnb))
data$SHOM_Real_Total <- rowSums(cbind(data$REALgl, data$REALbi))
data$SOTH_Real_Total <- rowSums(cbind(data$REALpan, data$REALace))
data$CG_EVAL_Total <- rowSums(cbind(data$EVALcgm, data$EVALcgw))
data$TG_EVAL_Total <- rowSums(cbind(data$EVALtgm, data$EVALtgw))
data$TGNB_EVAL_Total <- rowSums(cbind(data$EVALtgm, data$EVALtgw, data$EVALnb))
data$SHOM_EVAL_Total <- rowSums(cbind(data$EVALgl, data$EVALbi))
data$SOTH_EVAL_Total <- rowSums(cbind(data$EVALpan, data$EVALace))
data$CG_STAB_Total <- with(data, rowSums(cbind(8 - STAB1cgm, 8 - STAB1cgw, STAB2cgm, STAB2cgw))) # For stability items reverse code the first set 
data$TG_STAB_Total <- with(data, rowSums(cbind(8 - STAB1tgm, 8 - STAB1tgw, STAB2tgm, STAB2tgw)))
data$TGNB_STAB_Total <- with(data, rowSums(cbind(8 - STAB1tgm, 8 - STAB1tgw, 8- STAB1nb, STAB2tgm, STAB2tgw, STAB2nb)))
data$SHOM_STAB_Total <- with(data, rowSums(cbind(8 - STAB1gl, 8- STAB1bi,STAB2gl,STAB2bi)))
data$SOTH_STAB_Total <- with(data, rowSums(cbind(8 - STAB1pan, 8- STAB1ace,STAB2pan,STAB2ace)))

# Create data1 variable
data1 <- data |>
  filter(Study == 1)

# Models
sink(file = "models_1-4_diagnostic.txt")
ModelNum <- 1
while(ModelNum < 5){
  cat("CURRENT MODEL IS:", ModelNum, "\n") 
  cat("CURRENT RUN TIME IS",Sys.time(), "\n")
  max_II <- -100000
  min_bic <- 100000
  if (ModelNum == 1) {
    f <- cbind(REALcgm,REALcgw,REALnb,REALhet,REALgl,REALbi,EVALcgm,EVALcgw,EVALnb,EVALhet,EVALgl,EVALbi,STAB1cgm,STAB1cgw,STAB1nb,STAB1het,STAB1gl,STAB1bi,STAB2cgm,STAB2cgw,STAB2nb,STAB2het,STAB2gl,STAB2bi)~1
    cat("INDIVIDUAL MODEL WITHOUT COVARIATES:", ModelNum, "\n") 
    for(i in 2:6){
      lc <- poLCA(f, data1, nclass=i, maxiter=3000, 
                  tol=1e-5, na.rm=FALSE,  
                  nrep=5, verbose=TRUE, calc.se=TRUE)
      if(lc$bic < min_bic){
        min_bic <- lc$bic
        LCA_best_model<-lc
      }
    } 
    Model1Best <- LCA_best_model
  }
  if (ModelNum == 2) {
    f <- cbind(REALcgm,REALcgw,REALnb,REALhet,REALgl,REALbi,
               EVALcgm,EVALcgw,EVALnb,EVALhet,EVALgl,EVALbi,
               STAB1cgm,STAB1cgw,STAB1nb,STAB1het,STAB1gl,STAB1bi,
               STAB2cgm,STAB2cgw,STAB2nb,STAB2het,STAB2gl,STAB2bi)~NFC_Total + SDO_Total + Age + Religiousness + Education + Lib_Con
    cat("INDIVIDUAL MODEL WITH COVARIATES:", ModelNum, "\n") 
    for(i in 2:6){
      lc <- poLCA(f, data1, nclass=i, maxiter=3000, 
                  tol=1e-5, na.rm=FALSE,  
                  nrep=5, verbose=TRUE, calc.se=TRUE)
      if(lc$bic < min_bic){
        min_bic <- lc$bic
        LCA_best_model<-lc
      }
    } 
    Model2Best <- LCA_best_model
  }
  if (ModelNum == 3) {
    f <- cbind(CG_Real_Total,REALnb,REALhet,SHOM_Real_Total,
               CG_EVAL_Total,EVALnb,EVALhet,SHOM_EVAL_Total,
               CG_STAB_Total,STAB1nb,STAB2nb,STAB1het,STAB2het,SHOM_STAB_Total)~1    
    cat("PARCELED MODEL WITHOUT COVARIATES:", ModelNum, "\n") 
    for(i in 2:6){
      lc <- poLCA(f, data1, nclass=i, maxiter=3000, 
                  tol=1e-5, na.rm=FALSE,  
                  nrep=5, verbose=TRUE, calc.se=TRUE)
      if(lc$bic < min_bic){
        min_bic <- lc$bic
        LCA_best_model<-lc
      }
    } 
    Model3Best <- LCA_best_model
  }
  if (ModelNum == 4) {
    f <- cbind(CG_Real_Total,REALnb,REALhet,SHOM_Real_Total,
               CG_EVAL_Total,EVALnb,EVALhet,SHOM_EVAL_Total,
               CG_STAB_Total,STAB1nb,STAB2nb,STAB1het,STAB2het,SHOM_STAB_Total)~NFC_Total + SDO_Total + Age + Religiousness + Education + Lib_Con
    cat("PARCELED MODEL WITH COVARIATES:", ModelNum, "\n") 
    for(i in 2:6){
      lc <- poLCA(f, data1, nclass=i, maxiter=3000, 
                  tol=1e-5, na.rm=FALSE,  
                  nrep=5, verbose=TRUE, calc.se=TRUE)
      if(lc$bic < min_bic){
        min_bic <- lc$bic
        LCA_best_model<-lc
      }
    } 
    Model4Best <- LCA_best_model
  }
  ModelNum <- ModelNum + 1
}
sink(file = NULL)

sink(file = "models_1-4_best_performance.txt")
Model1Best
Model2Best
Model3Best
Model4Best
sink(file = NULL)
# Visualize data 
NFC_Density <- data |>
  tidyplot(x = NFC_Total, color = Study) |>
  add_histogram(bins = 15) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "NFC Score") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Need for Closure Density")
SDO_Density <- data |>
  tidyplot(x = SDO_Total, color = Study) |>
  add_histogram(bins = 15) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "SDO Score") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "left") |>
  adjust_title(title = "Social Dominance Orientation Density")
NFC_Density + SDO_Density
Real_CG_Density <- data |>
  tidyplot(x = CG_Real_Total, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Realness Score") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Cisgender Men & Women Density")
Real_TG_Density <- data |>
  tidyplot(x = TGNB_Real_Total, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Realness Score") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Transgender Men, Women & Non-binary Density")
Real_SHET_Density <- data |>
  tidyplot(x = REALhet, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Realness Score") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "top") |>
  adjust_title(title = "Heterosexuality Density")
Real_SHOM_Density <- data |>
  tidyplot(x = SHOM_Real_Total, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Realness Score") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Gay, Lesbian, and Bisexual Idenity Density")
Real_SHOT_Density <- data |>
  tidyplot(x = SOTH_Real_Total, color = Study) |>
  add_histogram(bins = 7) |>
  theme_minimal_xy() |>
  adjust_size(height = NA, width = NA) |> 
  adjust_colors(colors_discrete_apple) |>
  adjust_font(fontsize = 16, family = "Helvetica Neue") |>
  adjust_x_axis(title = "Realness Score") |>
  adjust_y_axis_title("Count") |>
  adjust_legend_position(position = "none") |>
  adjust_title(title = "Pansexuality and Asexuality Idenity Density")

Real_CG_Density + Real_TG_Density + Real_SHET_Density + Real_SHOM_Density + Real_SHOT_Density + plot_layout(ncol = 5, nrow = 1)

# Remove data
f <- cbind(REALcgm,REALcgw,REALtgm,REALtgw,REALnb,REALhet,REALgl,REALbi,REALpan,REALace,EVALcgm,EVALcgw,EVALtgm,EVALtgw,EVALnb,EVALhet,EVALgl,EVALbi,EVALpan,EVALace,STAB1cgm,STAB1cgw,STAB1tgm,STAB1tgw,STAB1nb,STAB1het,STAB1gl,STAB1bi,STAB1pan,STAB1ace,STAB2cgm,STAB2cgw,STAB2tgm,STAB2tgw,STAB2nb,STAB2het,STAB2gl,STAB2bi,STAB2pan,STAB2ace)
rm(f)
rm(i)
rm(max_II)
rm(ModelNum)
rm(min_bic)
rm(data)
rm(data1)
rm(data2)
rm(NFC_Density)
rm(SDO_Density)
rm(Real_CG_Density)
rm(Real_TG_Density)
rm(Real_SHET_Density)
rm(Real_SHOM_Density)
rm(Real_SHOT_Density)