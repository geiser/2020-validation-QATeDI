wants <- c('readr','dplyr','psych','lavaan','ggraph','semPlot','robustHD','GPArotation')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
options(stringsAsFactors = FALSE)

library(readr)
library(dplyr)
library(psych)
library(lavaan)
library(ggraph)
library(semPlot)

library(xlsx)
library(r2excel)

library(MVN)
library(daff)
library(robustHD)

fdat <- read_csv('data/data-fa.csv')
ugroups <- read_csv('data/user-groups.csv')
dat <- merge(ugroups, fdat)

theoretical_mdl <- '
ML4 =~ Item8+Item9+Item10
ML1 =~ Item11+Item12+Item13
ML5 =~ Item14+Item15+Item16
ML2 =~ Item17+Item18+Item19

ML6 =~ Item20+Item21
ML3 =~ Item22+Item23

INF =~ ML4+ML1+ML5+ML2
EXP =~ ML6+ML3
EXP ~~ INF
'

mdls <- list('multi-mdl'=list(name='multi-mdl', mdl='
ML2 =~ Item17+Item18+Item19
ML4 =~ Item8+Item9+Item10
ML1 =~ Item11+Item12+Item13
ML3 =~ Item22+Item23
ML6 =~ Item20+Item21
ML5 =~ Item14+Item15+Item16

ML1 ~~ ML2
ML1 ~~ ML3
ML1 ~~ ML4
ML1 ~~ ML5
ML1 ~~ ML6

ML2 ~~ ML3
ML2 ~~ ML4
ML2 ~~ ML5
ML2 ~~ ML6

ML3 ~~ ML4
ML3 ~~ ML5
ML3 ~~ ML6

ML4 ~~ ML5
ML4 ~~ ML6

ML5 ~~ ML6
'), 'efa-mdl'=list(name='efa-mdl', mdl='
ML2 =~ Item17+Item18+Item19
ML4 =~ Item8+Item9+Item10
ML1 =~ Item11+Item12+Item13
ML3 =~ Item22+Item23
ML6 =~ Item20+Item21
ML5 =~ Item14+Item15+Item16

ML1 ~~ 0*ML2
ML1 ~~ 0*ML5
ML1 ~~ 0*ML6

ML2 ~~ 0*ML3
ML2 ~~ 0*ML4
ML2 ~~ 0*ML5
ML2 ~~ 0*ML6

ML3 ~~ 0*ML4
ML3 ~~ 0*ML5

ML4 ~~ 0*ML5
'), 'theoretical_mdl'=list(name='theoretical_mdl', mdl=theoretical_mdl))

lgroups <- lapply(colnames(ugroups)[2:5], FUN = function(ngroup) {
  gdat <- dat[dat[[ngroup]] %in% names(table(dat[[ngroup]])[table(dat[[ngroup]]) > 100]),] 
  gdat[[ngroup]] <- factor(gdat[[ngroup]])
  return(list(group_by=ngroup, data = gdat))
})
names(lgroups) <- colnames(ugroups)[2:5]

#################################################################################
## 2.	Identifing the base model for groups - testing the final models          ##
#################################################################################
cfa_by_groups_mods <- lapply(lgroups, FUN = function(lgroup) {
  lgroup_values <- as.list(unique(lgroup$data[[lgroup$group_by]]))
  names(lgroup_values) <- as.character(unique(lgroup$data[[lgroup$group_by]]))
  lapply(lgroup_values, FUN = function(vgroup) {
    lapply(mdls, FUN = function(x) {
      vdat <- lgroup$data[lgroup$data[[lgroup$group_by]] == vgroup,]
      cfa_mod <- tryCatch(cfa(model = x$mdl, data = vdat, std.lv=T, estimator="MLR", meanstructure = T, check.gradient = F), error = function(e) e$message)
      fit <- tryCatch(fitmeasures(cfa_mod, c("chisq", "df", "cfi", "tli", "rmsea", 'rmsea.ci.lower','rmsea.ci.upper'
                                             ,'cfi.robust','tli.robust','rmsea.robust','rmsea.ci.lower.robust','rmsea.ci.upper.robust'))
                      , error = function(e) e$message)
      return(list(group=lgroup$group_by, value.of.group = as.character(vgroup), mdl=x$name, n=nrow(vdat), dat=vdat, cfa=cfa_mod, fit=fit))
    })
  })
})

cfa_by_groups_df <- do.call(rbind, lapply(cfa_by_groups_mods, FUN = function(cfa_by_group_mod) {
  do.call(rbind, lapply(cfa_by_group_mod, FUN = function(cfa_by_value_mod) {
    do.call(rbind, lapply(cfa_by_value_mod, FUN = function(x) {
      print(paste(x$group,x$value.of.group,x$mdl))
      
      aov_df <- do.call(cbind, lapply(names(mdls), FUN = function(name_mdl) {
        aov <- tryCatch(anova(x$cfa, cfa_by_groups_mods[[x$group]][[x$value.of.group]][[name_mdl]]$cfa), error = function(e) NULL)
        if (is.null(aov) || typeof(x$cfa) == 'character' || typeof(x$fit) == 'character' || x$mdl == name_mdl) {
          aov_comp_df <- data.frame(NA, NA, NA)
        } else {
          aov_comp_df <- cbind(aov$`Chisq diff`[[2]], aov$`Df diff`[[2]], aov$`Pr(>Chisq)`[[2]])
        }
        colnames(aov_comp_df) <- paste(c('diff Chisq','diff df','Pr(>Chisq)'), name_mdl)
        return(aov_comp_df)
      }))
      
      if (typeof(x$cfa) == 'character' || typeof(x$fit) == 'character') { 
        cbind(group=x$group, value.of.group=x$value.of.group, mdl=x$mdl, n=x$n
              , data.frame('chisq'=NA, 'df'=NA, 'cfi'=NA, 'tli'=NA, 'rmsea'=NA, 'rmsea.ci.lower'=NA, 'rmsea.ci.upper'=NA
                           ,'cfi.robust'=NA, 'tli.robust'=NA, 'rmsea.robust'=NA, 'rmsea.ci.lower.robust'=NA, 'rmsea.ci.upper.robust'=NA)
              , aov_df
              , obs = NA, err = ifelse(typeof(x$cfa) == 'character', x$cfa, x$fit))
      } else {
        cbind(group=x$group, value.of.group=x$value.of.group, mdl=x$mdl, n=x$n
              , round(t(as.data.frame(x$fit)), 4)
              , aov_df
              , 'obs'=ifelse(x$fit[['cfi']] < 0.85 || x$fit[['tli']] < 0.85 || x$fit[['rmsea']] > 0.10, '*', NA), err = NA)
      }
    }))
  }))
}))
write_csv(cfa_by_groups_df, "report/mgcfa/cfa-baseline-for-groups.csv")

#####################################################
## 3. Multigroup confirmatory factorial analysis   ##
#####################################################

source('common/get_mgcfa.R')
source('common/mgcfa2df.R')
source('common/generate_all_mgcfa.R')
source('common/find_free_parameters.R')

skips <- list(
  "Unidade" = c("Faculdade de Odontologia - FOUFAL" # Item11=1 without variance
                , "Faculdade de Medicina - FAMED" # Item8=1 without variance
                , "Faculdade de Nutrição - FANUT" # Item8=1 without variance
                , "Faculdade de Direito - FDA" # Item11=1 without variance
                , "Instituto de Matemática - IM" # Item8=1 without variance
                , "Escola de Enfermagem e Farmácia - ESENFAR" # Item11=1 without variance
                , "Instituto de Computação - IC" # Item8=1 without variance
                , "Escola Técnica de Artes - ETA" # Item17=1 without variance
              )
  ,"Nivel" = c("Especialização" # Item8=1 e Item11=1 without variance
               , "Técnico" # Item11=1 without variance
              ) 
)

all_mgcfa <- generate_all_mgcfa(theoretical_mdl, dat, c("Unidade","Nivel","Modalidade", "DispositivoEmCasa"), skips = skips)
write_csv(all_mgcfa$df, "report/mgcfa/test-of-invariants-without-frees.csv")

##### Identifing cfa modules in which to define free itens ####
uni_df <- unique(all_mgcfa$df[!is.na(all_mgcfa$df$obs),c('group','value')])
rownames(uni_df) <- seq(1:nrow(uni_df))
(uni_df)

fix.configs <- list()

## Unidade: Instituto de Geografia, Desenvolvimento e Meio Ambiente - IGDEMA
i <- 1; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, fit_mod = mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("ML1 =~ Item13", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item21 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item11 ~~ Item11","Item12 ~~ Item12","Item14 ~~ Item14","Item15 ~~ Item15","Item16 ~~ Item16", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial))

group.partial <- c("", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

## Unidade: Campus Arapiraca (Arapiraca, Palmeira dos Índios e Penedo)
i <- 2; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("ML2 =~ Item18","ML2 =~ Item19","ML4 =~ Item10","ML4 =~ Item9","ML5 =~ Item15","ML5 =~ Item16", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item11 ~ 1","Item17 ~ 1","Item19 ~ 1","Item20 ~ 1","Item8 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item14 ~~ Item14","Item16 ~~ Item16","Item8 ~~ Item8", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial))

group.partial <- c("INF ~~ INF","ML1 ~~ ML1","ML2 ~~ ML2","ML4 ~~ ML4","ML5 ~~ ML5", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))


## Unidade: Instituto de Química e Biotecnologia - IQB
i <- 3; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals
  , to_fit_mdl = "Factor model", group.partial = group.partial, include.combn = T, limit.combn = 2))

group.partial <- c("ML2 ~~ ML2","ML3 ~~ ML3","ML4 ~~ ML4", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

## Unidade: Faculdade de Economia, Administração e Contabilidade - FEAC
i <- 4; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, fit_mod = mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("ML5 =~ Item15", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item14 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial))

group.partial <- c("", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

## Unidade: Instituto de Ciências Humanas, Comunicação e Artes - ICHCA
i <- 5; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, fit_mod = mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("ML1 =~ Item12","ML4 =~ Item9", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item21 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item11 ~~ Item11","Item15 ~~ Item15", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial))

group.partial <- c("ML1 ~~ ML1", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))


## Unidade: Faculdade de Arquitetura e Urbanismo - FAU
i <- 6; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item17 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial))

group.partial <- c("", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))


## Unidade Centro de Tecnologia - CTEC
i <- 7; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item14 ~ 1","Item8 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item11 ~~ Item11","Item14 ~~ Item14","Item15 ~~ Item15"
                   ,"Item16 ~~ Item16","Item17 ~~ Item17","Item18 ~~ Item18", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial))

group.partial <- c("", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

## Unidade: Faculdade de Letras - FALE
i <- 8; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("ML1 =~ Item12","ML4 =~ Item10","ML4 =~ Item9", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item14 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial))

group.partial <- c("", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))


## Unidade: Centro de Educação - CEDU
i <- 9; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("ML2 =~ Item18","ML2 =~ Item19", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item14 ~ 1","Item17 ~ 1","Item21 ~ 1","Item23 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item14 ~~ Item14","Item15 ~~ Item15","Item16 ~~ Item16", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial, include.combn = T, limit.combn = 2))

group.partial <- c("ML2 ~~ ML2","ML4 ~~ ML4", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))


## Unidade: Campus Sertão (Delmiro Gouveia e Santana do Ipanema)
i <- 10; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("ML5 =~ Item15", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item19 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial))

group.partial <- c("", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

## Unidade: Instituto de Ciências Atmosféricas - ICAT
i <- 11; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item14 ~~ Item14","Item15 ~~ Item15", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial))

group.partial <- c("", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

## Unidade: Faculdade de Serviço Social - FSSO
i <- 12; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("ML5 =~ Item15", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item15 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item11 ~~ Item11","Item14 ~~ Item14","Item15 ~~ Item15", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial, include.combn = T, limit.combn = 4))

group.partial <- c("ML1 ~~ ML1", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

## Unidade: Instituto de Ciências Sociais - ICS
i <- 13; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("ML5 =~ Item16", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial, include.combn = T, limit.combn = 4))

group.partial <- c("ML4 ~~ ML4", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))


## Nivel: Graduação (bacharelado)
i <- 14; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("INF =~ ML5","ML1 =~ Item12","ML1 =~ Item13","ML2 =~ Item18"
                   ,"ML2 =~ Item19","ML5 =~ Item15","ML5 =~ Item16", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item13 ~ 1","Item14 ~ 1","Item17 ~ 1","Item21 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item11 ~~ Item11","Item14 ~~ Item14","Item15 ~~ Item15","Item16 ~~ Item16"
                   ,"Item17 ~~ Item17","Item8 ~~ Item8", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial, include.combn = T, limit.combn = 4))

group.partial <- c("ML1 ~~ ML1","ML2 ~~ ML2","ML5 ~~ ML5", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))


## Nivel: Graduação (licenciatura)
i <- 15; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("ML1 =~ Item12","ML1 =~ Item13","ML2 =~ Item18","ML2 =~ Item19","ML4 =~ Item10"
                   ,"ML4 =~ Item9","ML5 =~ Item15","ML5 =~ Item16", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item14 ~ 1","Item17 ~ 1","Item8 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item11 ~~ Item11","Item8 ~~ Item8", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial, include.combn = T, limit.combn = 4))

group.partial <- c("INF ~~ EXP","INF ~~ INF","ML1 ~~ ML1","ML2 ~~ ML2","ML4 ~~ ML4", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

## Nivel: Mestrado
i <- 16; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("INF =~ ML1","ML1 =~ Item13","ML2 =~ Item18","ML2 =~ Item19","ML4 =~ Item10","ML4 =~ Item9", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item21 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item14 ~~ Item14","Item15 ~~ Item15","Item19 ~~ Item19","Item8 ~~ Item8", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial, include.combn = T, limit.combn = 4))

group.partial <- c("ML2 ~~ ML2","ML4 ~~ ML4", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))


## Nivel: Doutorado
i <- 17; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item14 ~ 1","Item21 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item14 ~~ Item14","Item15 ~~ Item15", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , mdl_type = "Factor model", group.partial = group.partial, include.combn = T, limit.combn = 4))

group.partial <- c("INF ~~ INF", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

## Modalidade: Presencial
i <- 18; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("EXP =~ ML3","ML2 =~ Item18","ML2 =~ Item19","ML4 =~ Item10","ML4 =~ Item9","ML5 =~ Item16", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item17 ~ 1","Item20 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item14 ~~ Item14","Item15 ~~ Item15", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial, include.combn = T, limit.combn = 4))

group.partial <- c("INF ~~ EXP","INF ~~ INF","ML1 ~~ ML1","ML2 ~~ ML2","ML4 ~~ ML4","ML5 ~~ ML5", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

## Modalidade: Educação a Distância
i <- 19; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial, include.combn = T, limit.combn = 2))

group.partial <- c("EXP =~ ML3","ML2 =~ Item18","ML2 =~ Item19","ML4 =~ Item10","ML4 =~ Item9","ML5 =~ Item16", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("Item17 ~ 1","Item20 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item14 ~~ Item14","Item15 ~~ Item15", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial, include.combn = T, limit.combn = 4))

group.partial <- c("INF ~~ EXP","INF ~~ INF","ML1 ~~ ML1","ML2 ~~ ML2","ML4 ~~ ML4","ML5 ~~ ML5", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

## DispositivoEmCasa: Não Tem Nemhum Dispositivo
i <- 20; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("INF =~ ML1","INF =~ ML2","INF =~ ML5","ML2 =~ Item18","ML2 =~ Item19","ML4 =~ Item10"
                   ,"ML4 =~ Item9","ML5 =~ Item15","ML5 =~ Item16","ML6 =~ Item21", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("Item11 ~ 1","Item8 ~ 1","Item9 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item11 ~~ Item11","Item12 ~~ Item12","Item14 ~~ Item14","Item8 ~~ Item8", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial, include.combn = T, limit.combn = 4))

## DispositivoEmCasa: Dispositivo Compartilhado
i <- 21; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("INF =~ ML1","INF =~ ML5","ML1 =~ Item12","ML1 =~ Item13"
                   ,"ML4 =~ Item10","ML4 =~ Item9","ML5 =~ Item15","ML5 =~ Item16", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("Item12 ~ 1","Item14 ~ 1","Item17 ~ 1","Item8 ~ 1", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item11 ~~ Item11","Item8 ~~ Item8", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.strict.residuals.alt
  , to_fit_mdl = "Factor model", group.partial = group.partial))

group.partial <- c("EXP ~~ EXP","INF ~~ EXP","INF ~~ INF","ML1 ~~ ML1","ML4 ~~ ML4", group.partial)
fix.configs[[cod]][["fit.varfactor.alt"]] <- list(
  name='Factor model alt', aov="fit.strict.residuals.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

## DispositivoEmCasa: Dispositivo Próprio
i <- 22; print(uni_df[i,])
cod <- paste0(uni_df$group[i],':',uni_df$value[i])

fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
group.partial = c()
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.combine.groups
  , to_fit_mdl = "Metric model", group.partial = group.partial))

group.partial <- c("INF =~ ML1","INF =~ ML2","INF =~ ML5","ML1 =~ Item12","ML1 =~ Item13","ML2 =~ Item18","ML2 =~ Item19"
                   ,"ML4 =~ Item10","ML4 =~ Item9","ML5 =~ Item15","ML5 =~ Item16","ML6 =~ Item21", group.partial)
fix.configs[[cod]][["fit.metric.alt"]] <- list(
  name='Metric model alt', aov="fit.combine.groups"
  , group.partial = group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.metric.alt
  , to_fit_mdl = "Scalar model", group.partial = group.partial))

group.partial <- c("", group.partial)
fix.configs[[cod]][["fit.scalar.alt"]] <- list(
  name='Scalar model alt', aov="fit.metric.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))

#
(ffp_df <- find_free_parameters(
  theoretical_mdl, mgcfa_mod$fit.mdls$fit.scalar.alt
  , to_fit_mdl = "Strict model", group.partial = group.partial))

group.partial <- c("Item11 ~~ Item11","Item13 ~~ Item13","Item17 ~~ Item17","Item8 ~~ Item8","Item9 ~~ Item9", group.partial)
fix.configs[[cod]][["fit.strict.residuals.alt"]] <- list(
  name='Strict model alt', aov="fit.scalar.alt"
  , group.partial=group.partial)
fix.config <- fix.configs[[cod]]
mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
(mgcfa2df(mgcfa_mod))


## ## Write reports ## ##

test_of_invariants_df <- do.call(rbind, lapply(seq(1,nrow(uni_df)), FUN = function(i) {
  fix.config <- fix.configs[[paste0(uni_df$group[i],':',uni_df$value[i])]]
  mgcfa_mod <- get_mgcfa(theoretical_mdl, dat, group_by=uni_df$group[i], value=uni_df$value[i], fix.config=fix.config)
  lapply(mgcfa_mod$fit.mdls, FUN = function(fit_mod) {
    filename <- paste0('report/mgcfa/', mgcfa_mod$group) 
    dir.create(filename, showWarnings = F)
    
    filename <- paste0('report/mgcfa/', mgcfa_mod$group,'/',mgcfa_mod$value)
    dir.create(filename, showWarnings = F)
    
    filename <- paste0(filename,'/',fit_mod$name,'.txt')
    capture.output(summary(fit_mod$cfa, fit.measures=T), file = filename)
  })
  
  return(mgcfa2df(mgcfa_mod))
}))
write_csv(test_of_invariants_df, "report/mgcfa/test-of-invariants-fixed.csv")

