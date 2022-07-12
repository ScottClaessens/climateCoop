# custom functions

# set ggplot theme
theme_set(theme_classic())

# load data
loadData <- function() {
  # load data
  out <-
    read_sav("data/NZAVS Longitudinal SPSS Base Dataset.sav",
           col_select = c("Questionnaire.Num", "Env.SacMade.T09",
                          "Env.ClimateChgCause.T09", "Env.ClimateChgConcern.T09",
                          "Env.ClimateChgReal.T09", "Age.T09", "Gender.T09",
                          "EthnicCats.T09", "Pol.ReportedPartySupport.T09",
                          "T9.EXTRAVERSION", "T9.AGREEABLENESS", "T9.CONSCIENTIOUSNESS", 
                          "T9.NEUROTICISM", "T9.OPENNESS", "T9.HONESTY_HUMILITY", "T9.NARCISSISM",
                          "NZREG.T09", "egame.DG.T10", "egame.cmpDG.T10",
                          "egame.PGG.T10", "egame.cmpPGG.T10", "egame.TG1.T10",
                          "egame.TG2.T10", "egame.cmpTG.T10", "egame.SH.T10",
                          "egame.cmpSH.T10", "egame.Chk1.T10", "egame.Chk2.T10",
                          starts_with("egame.Secs") & ends_with(".T10"),
                          -"egame.SecsW.T10", -"egame.SecsG.T10",
                          "egame.Paid.T10"
                          )) %>%
    # create and modify vars
    mutate(
      # calculate new "time on games" variable
      secs = rowSums(select(., egame.Secs01.T10:egame.Secs48.T10), na.rm = TRUE),
      # EthnicCats is a factor
      EthnicCats.T09 = factor(ifelse(EthnicCats.T09 == 1, "Pakeha",
                              ifelse(EthnicCats.T09 == 2, "Maori",
                              ifelse(EthnicCats.T09 == 3, "Pacific",
                              ifelse(EthnicCats.T09 == 4, "Asian", EthnicCats.T09))))),
      # Pol.ReportedPartySupport is a factor
      Pol.ReportedPartySupport.T09 = factor(ifelse(Pol.ReportedPartySupport.T09 == 1 , "National",
                                            ifelse(Pol.ReportedPartySupport.T09 == 2 , "Labour",
                                            ifelse(Pol.ReportedPartySupport.T09 == 3 , "Greens",
                                            ifelse(Pol.ReportedPartySupport.T09 == 4 , "NZFirst", NA))))),
      # game data scaled between 0 and 1
      egame.DG.T10     = egame.DG.T10  / 100,
      egame.TG2.T10    = egame.TG2.T10 / 150,
      egame.PGG.T10    = egame.PGG.T10 / 100,
      # update comprehension questions (1 = correct, 0 = incorrect)
      egame.cmpDG.T10  = ifelse(egame.cmpDG.T10  == 1, 1, 0),
      egame.cmpTG.T10  = ifelse(egame.cmpTG.T10  == 1, 1, 0),
      egame.cmpPGG.T10 = ifelse(egame.cmpPGG.T10 == 3, 1, 0),
      egame.cmpSH.T10  = ifelse(egame.cmpSH.T10  == 3, 1, 0)
      ) %>%
    # filter rows
    filter(
      # keep only participants who were paid and did not time out
      egame.Chk1.T10 == 1 & egame.Chk2.T10 == 1 &
      # remove if less than 5 mins or longer than 50 mins to complete games
      secs > 300 & secs < 3000 &
      # must have complete cases for climate change vars
      !is.na(Env.SacMade.T09) & !is.na(Env.ClimateChgReal.T09) & 
      !is.na(Env.ClimateChgCause.T09) & !is.na(Env.ClimateChgConcern.T09)
      # n = 895
    ) %>%
    # scaled vars
    mutate(
      Age.T09.unstd = Age.T09,
      Age.T09   = as.numeric(scale(Age.T09)),
      NZREG.T09 = as.numeric(scale(NZREG.T09)),
      T9.EXTRAVERSION      = as.numeric(scale(T9.EXTRAVERSION     )),
      T9.AGREEABLENESS     = as.numeric(scale(T9.AGREEABLENESS    )),
      T9.CONSCIENTIOUSNESS = as.numeric(scale(T9.CONSCIENTIOUSNESS)),
      T9.NEUROTICISM       = as.numeric(scale(T9.NEUROTICISM      )),
      T9.OPENNESS          = as.numeric(scale(T9.OPENNESS         )),
      T9.HONESTY_HUMILITY  = as.numeric(scale(T9.HONESTY_HUMILITY )),
      T9.NARCISSISM        = as.numeric(scale(T9.NARCISSISM       ))
      )
  # remove labels
  out <- remove_all_labels(out)
  # dummy cols
  out <- dummy_cols(out, c("EthnicCats.T09", "Pol.ReportedPartySupport.T09"))
  return(out)
}

# fit cooperative phenotype cfa
cfaModel1 <- function(d) {
  model <- 'coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
            # comprehension
            egame.PGG.T10 ~ egame.cmpPGG.T10
            egame.TG1.T10 + egame.TG2.T10 ~ egame.cmpTG.T10
            egame.SH.T10 ~ egame.cmpSH.T10
            egame.DG.T10 ~ egame.cmpDG.T10'
  out <- cfa(model, data = d, ordered = c('egame.TG1.T10','egame.SH.T10'))
  return(out)
}

# fit climate change belief cfa
cfaModel2 <- function(d) {
  model <- 'ccb =~ Env.ClimateChgCause.T09 + Env.ClimateChgConcern.T09 + Env.ClimateChgReal.T09'
  out <- cfa(model, data = d, ordered = c('Env.ClimateChgCause.T09','Env.ClimateChgConcern.T09','Env.ClimateChgReal.T09'))
  return(out)
}

# fit sem Env.SacMade.T09 ~ coop
semModel1 <- function(d, controls = "") {
  model <- 'coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
            # comprehension
            egame.PGG.T10 ~ egame.cmpPGG.T10
            egame.TG1.T10 + egame.TG2.T10 ~ egame.cmpTG.T10
            egame.SH.T10 ~ egame.cmpSH.T10
            egame.DG.T10 ~ egame.cmpDG.T10
            # regression
            Env.SacMade.T09 ~ coop'
  if (controls != "") model <- paste0(model, controls)
  out <- sem(model, data = d, ordered = c('egame.TG1.T10','egame.SH.T10','Env.SacMade.T09'))
  return(out)
}

# fit sem ccb ~ coop
semModel2 <- function(d, controls = "") {
  model <- 'coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
            ccb =~ Env.ClimateChgCause.T09 + Env.ClimateChgConcern.T09 + Env.ClimateChgReal.T09
            # comprehension
            egame.PGG.T10 ~ egame.cmpPGG.T10
            egame.TG1.T10 + egame.TG2.T10 ~ egame.cmpTG.T10
            egame.SH.T10 ~ egame.cmpSH.T10
            egame.DG.T10 ~ egame.cmpDG.T10
            # regression
            ccb ~ coop'
  if (controls != "") model <- paste0(model, controls)
  out <- sem(model, data = d, ordered = c('egame.TG1.T10','egame.SH.T10','Env.ClimateChgCause.T09',
                                          'Env.ClimateChgConcern.T09','Env.ClimateChgReal.T09'))
  return(out)
}

# fit sem ccb ~ coop
semModel3 <- function(d, var, controls = "") {
  model <- paste0(
            'coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
            # comprehension
            egame.PGG.T10 ~ egame.cmpPGG.T10
            egame.TG1.T10 + egame.TG2.T10 ~ egame.cmpTG.T10
            egame.SH.T10 ~ egame.cmpSH.T10
            egame.DG.T10 ~ egame.cmpDG.T10
            # regression
            ', var, ' ~ coop')
  if (controls != "") model <- paste0(model, controls)
  out <- sem(model, data = d, ordered = c('egame.TG1.T10','egame.SH.T10',var))
  return(out)
}

# fit coop ~ pol parties
semModel4 <- function(d) {
  d <- drop_na(d, "Pol.ReportedPartySupport.T09")
  model <- 'coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
            # comprehension
            egame.PGG.T10 ~ egame.cmpPGG.T10
            egame.TG1.T10 + egame.TG2.T10 ~ egame.cmpTG.T10
            egame.SH.T10 ~ egame.cmpSH.T10
            egame.DG.T10 ~ egame.cmpDG.T10
            # regression
            coop ~ Pol.ReportedPartySupport.T09_Labour + Pol.ReportedPartySupport.T09_National + Pol.ReportedPartySupport.T09_NZFirst'
  out <- sem(model, data = d, ordered = c('egame.TG1.T10','egame.SH.T10'))
  return(out)
}

# mediation model 1
medModel1 <- function(d, controls = "") {
  model <- 'coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
            ccb =~ Env.ClimateChgCause.T09 + Env.ClimateChgConcern.T09 + Env.ClimateChgReal.T09
            # comprehension
            egame.PGG.T10 ~ egame.cmpPGG.T10
            egame.TG1.T10 + egame.TG2.T10 ~ egame.cmpTG.T10
            egame.SH.T10 ~ egame.cmpSH.T10
            egame.DG.T10 ~ egame.cmpDG.T10
            # regression
            ccb ~ coop + b*Env.SacMade.T09'
  if (controls != "") model <- paste0(model, controls)
  model <- paste0(model, "\n            Env.SacMade.T09 ~ a*coop")
  if (controls != "") model <- paste0(model, controls)
  model <- paste0(model, "\n            ab := a * b")
  out <- sem(model, data = d, ordered = c('egame.TG1.T10','egame.SH.T10','Env.SacMade.T09',
                                          'Env.ClimateChgCause.T09','Env.ClimateChgConcern.T09','Env.ClimateChgReal.T09'))
  return(out)
}

# mediation model 2
medModel2 <- function(d, controls = "") {
  model <- 'coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
            ccb =~ Env.ClimateChgCause.T09 + Env.ClimateChgConcern.T09 + Env.ClimateChgReal.T09
            # comprehension
            egame.PGG.T10 ~ egame.cmpPGG.T10
            egame.TG1.T10 + egame.TG2.T10 ~ egame.cmpTG.T10
            egame.SH.T10 ~ egame.cmpSH.T10
            egame.DG.T10 ~ egame.cmpDG.T10
            # regression
            ccb ~ a*coop'
  if (controls != "") model <- paste0(model, controls)
  model <- paste0(model, "\n            Env.SacMade.T09 ~ b*ccb + coop")
  if (controls != "") model <- paste0(model, controls)
  model <- paste0(model, "\n            ab := a * b")
  out <- sem(model, data = d, ordered = c('egame.TG1.T10','egame.SH.T10','Env.SacMade.T09',
                                          'Env.ClimateChgCause.T09','Env.ClimateChgConcern.T09','Env.ClimateChgReal.T09'))
  return(out)
}

plotMain <- function(d, cfa1, cfa2) {
  predCoop <- lavPredict(cfa1)
  predCCB  <- lavPredict(cfa2)
  d$coop   <- predCoop[,1]
  d$ccb    <- predCCB[,1]
  # plot A
  pA <-
    ggplot(d, aes(x = coop, y = Env.SacMade.T09)) +
    geom_jitter(height = 0.3, width = 0.015, size = 0.8, alpha = 0.6, colour = "#0072B2") +
    stat_smooth(method = "lm", colour = "black") +
    scale_y_continuous(name = "Have you made sacrifices to your\nstandard of living in order to\nprotect the environment? (1-7)",
                       breaks = 1:7) +
    scale_x_continuous(name = "Cooperative Phenotype (factor score)")
  # plot B
  pB <-
    ggplot(d, aes(x = coop, y = ccb)) +
    geom_jitter(height = 0.15, width = 0.015, size = 0.8, alpha = 0.6, colour = "#0072B2") +
    stat_smooth(method = "lm", colour = "black") +
    scale_y_continuous(name = "Climate Change Belief (factor score)") +
    scale_x_continuous(name = "Cooperative Phenotype (factor score)")
  # put together
  out <- plot_grid(pA, pB, labels = c("a","b"))
  # save
  ggsave(out, file = "figures/plotMain.pdf", width = 7, height = 3.5)
  return(out)
}

plotCoef <- function(sem1.01, sem1.02, sem1.03, sem1.04, sem1.05, sem1.06, sem1.07, 
                     sem1.08, sem1.09, sem1.10, sem1.11, sem1.12, sem1.13, sem1.14, 
                     sem2.01, sem2.02, sem2.03, sem2.04, sem2.05, sem2.06, sem2.07,
                     sem2.08, sem2.09, sem2.10, sem2.11, sem2.12, sem2.13, sem2.14) {
  makePlot <- function(modelPrefix, LHS, ylab) {
    estimate <- se <- c()
    for (i in 1:14) {
      out <- parameterEstimates(get(paste0(modelPrefix, formatC(i, width = 2, flag = "0"))))
      estimate <- c(estimate, out$est[out$lhs==LHS & out$rhs=="coop" & out$op=="~"])
      se <- c(se, out$se[out$lhs==LHS & out$rhs=="coop" & out$op=="~"])
    }
    labels <- c("No controls", "Age", "Gender", "Ethnicity", "Party support", "Education", "Extraversion", "Agreeableness", 
                "Conscientiousness", "Neuroticism", "Openness", "Honesty-Humility", "Narcissism", "Full model")
    tibble(
      Model = factor(labels, levels = labels),
      Estimate = estimate,
      ci.lower = estimate + (1.96*se),
      ci.upper = estimate - (1.96*se)
    ) %>%
      ggplot(aes(x = fct_rev(Model), 
                 y = Estimate, 
                 ymin = ci.lower, 
                 ymax = ci.upper)) +
      geom_pointrange(size = 0.35) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      coord_flip() +
      xlab(NULL) +
      scale_y_continuous(name = ylab, breaks = seq(-1, 2, by = 0.5), limits = c(-1.1, 2.1))
  }
  # make plots
  pA <- makePlot("sem1.", "Env.SacMade.T09", "Unstandardised Estimate\n(Cooperative Phenotype predicting\nPro-Environmental Behaviour)")
  pB <- makePlot("sem2.", "ccb", "Unstandardised Estimate\n(Cooperative Phenotype predicting\nClimate Change Belief)")
  # put together
  out <- plot_grid(NULL, pA, NULL, pB + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
                   labels = c("a","","b",""), nrow = 1, rel_widths = c(0.05, 1, 0.06, 0.73))
  # save
  ggsave(out, file = "figures/plotCoef.pdf", width = 7, height = 4)
  return(out)
}


dataR2 <- function(d, sem1.01, sem1.14, sem2.01, sem2.14) {
  pM <- 'Env.SacMade.T09 ~ '
  cM <- 'ccb =~ Env.ClimateChgCause.T09 + Env.ClimateChgConcern.T09 + Env.ClimateChgReal.T09\nccb ~ '
  or <- c('Env.SacMade.T09','Env.ClimateChgCause.T09','Env.ClimateChgConcern.T09','Env.ClimateChgReal.T09')
  fitFun <- function(prefix, var, suffix) sem(paste0(prefix, suffix), data = drop_na(d, all_of(var)), ordered = or) 
  out <-
    tibble(
      Outcome = c(rep("Pro-environmental behaviour", 14), rep("Climate change belief", 14)),
      Predictor = rep(c("Cooperation", "Age", "Gender", "Ethnicity", "Party support", "Education", "Extraversion", "Agreeableness", 
                        "Conscientiousness", "Neuroticism", "Openness", "Honesty-Humility", "Narcissism", "Full model"), times = 2),
      Model = c(
        sem1.01, 
        fitFun(pM, "Age.T09"                     , "Age.T09"), 
        fitFun(pM, "Gender.T09"                  , "Gender.T09"),
        fitFun(pM, "EthnicCats.T09"              , "EthnicCats.T09_Asian + EthnicCats.T09_Maori + EthnicCats.T09_Pacific"),
        fitFun(pM, "Pol.ReportedPartySupport.T09", "Pol.ReportedPartySupport.T09_Greens + Pol.ReportedPartySupport.T09_Labour + Pol.ReportedPartySupport.T09_NZFirst"),
        fitFun(pM, "NZREG.T09"                   , "NZREG.T09"),
        fitFun(pM, "T9.EXTRAVERSION"             , "T9.EXTRAVERSION"),
        fitFun(pM, "T9.AGREEABLENESS"            , "T9.AGREEABLENESS"),
        fitFun(pM, "T9.CONSCIENTIOUSNESS"        , "T9.CONSCIENTIOUSNESS"),
        fitFun(pM, "T9.NEUROTICISM"              , "T9.NEUROTICISM"),
        fitFun(pM, "T9.OPENNESS"                 , "T9.OPENNESS"),
        fitFun(pM, "T9.HONESTY_HUMILITY"         , "T9.HONESTY_HUMILITY"),
        fitFun(pM, "T9.NARCISSISM"               , "T9.NARCISSISM"),
        sem1.14,
        sem2.01, 
        fitFun(cM, "Age.T09"                     , "Age.T09"), 
        fitFun(cM, "Gender.T09"                  , "Gender.T09"),
        fitFun(cM, "EthnicCats.T09"              , "EthnicCats.T09_Asian + EthnicCats.T09_Maori + EthnicCats.T09_Pacific"),
        fitFun(cM, "Pol.ReportedPartySupport.T09", "Pol.ReportedPartySupport.T09_Greens + Pol.ReportedPartySupport.T09_Labour + Pol.ReportedPartySupport.T09_NZFirst"),
        fitFun(cM, "NZREG.T09"                   , "NZREG.T09"),
        fitFun(cM, "T9.EXTRAVERSION"             , "T9.EXTRAVERSION"),
        fitFun(cM, "T9.AGREEABLENESS"            , "T9.AGREEABLENESS"),
        fitFun(cM, "T9.CONSCIENTIOUSNESS"        , "T9.CONSCIENTIOUSNESS"),
        fitFun(cM, "T9.NEUROTICISM"              , "T9.NEUROTICISM"),
        fitFun(cM, "T9.OPENNESS"                 , "T9.OPENNESS"),
        fitFun(cM, "T9.HONESTY_HUMILITY"         , "T9.HONESTY_HUMILITY"),
        fitFun(cM, "T9.NARCISSISM"               , "T9.NARCISSISM"),
        sem2.14
        )
      ) %>%
    mutate(r2 = map(Model, function(x) tail(inspect(x, "R2"), 1))) %>%
    select(-Model) %>%
    unnest(c(r2))
  return(out)
}

plotR2 <- function(r2data) {
  out <-
    r2data %>%
    mutate(Predictor = factor(Predictor, levels = Predictor[1:14])) %>%
    ggplot(aes(x = Predictor, y = r2)) +
    geom_col(fill = "skyblue4") +
    facet_wrap(. ~ Outcome) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = NULL, y = bquote("R"^2))
  # save plot
  ggsave(out, filename = "figures/plotR2.pdf", width = 8, height = 5)
  return(out)
}

makeItemTable <- function() {
  tibble(
    Item = 
      c("Climate change belief", rep("", 2), 
        "Pro-environmental behaviour",
        "Age", "Gender", "Ethnicity", "Education level",
        "Political party support", rep("", 3),
        "Extraversion", rep("", 3),
        "Agreeableness", rep("", 3),
        "Conscientiousness", rep("", 3),
        "Neuroticism", rep("", 3),
        "Openess to experience", rep("", 3),
        "Narcissism", "",
        "Honesty/Humility", ""
      ),
    `Description / Text` = 
      c("Climate change is real",
        "Climate change is caused by humans",
        "I am deeply concerned about climate change",
        "Have you made sacrifices to your standard of living (e.g., accepted higher prices, driven less, conserved energy) in order to protect the environment?",
        "What is your date of birth?",
        "What is your gender? (open-ended)",
        "Which ethnic group do you belong to? (NZ census question)",
        "NZ Reg (0-10 education ordinal rank)",
        "Please rate how strongly you oppose or support each of the following political parties... the National Party",
        "Please rate how strongly you oppose or support each of the following political parties... the Labour Party",
        "Please rate how strongly you oppose or support each of the following political parties... the Green Party",
        "Please rate how strongly you oppose or support each of the following political parties... the NZ First Party",
        "Am the life of the party",
        "Don't talk a lot (reversed)",
        "Keep in the background (reversed)",
        "Talk to a lot of different people at parties",
        "Sympathize with others' feelings",
        "Am not interested in other people's problems (reversed)",
        "Feel others' emotions",
        "Am not really interested in others (reversed)",
        "Get chores done right away",
        "Like order",
        "Make a mess of things (reversed)",
        "Often forget to put things back in their proper place (reversed)",
        "Have frequent mood swings",
        "Am relaxed most of the time (reversed)",
        "Get upset easily",
        "Seldom feel blue (reversed)",
        "Have a vivid imagination",
        "Have difficulty understanding abstract ideas (reversed)",
        "Do not have a good imagination (reversed)",
        "Am not interested in abstract ideas (reversed)",
        "Feel entitled to more of everything",
        "Deserve more things in life",
        "Would like to be seen driving around in a very expensive car (reversed)",
        "Would get a lot of pleasure from owning expensive luxury goods (reversed)"
      )
  )
}

# fit sem Env.SacMade.T09 ~ coop (with exclusions)
semModel1Exc <- function(d) {
  model <- 'coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
            # regression
            Env.SacMade.T09 ~ coop'
  out <- sem(model, data = d, ordered = c('egame.TG1.T10','egame.SH.T10','Env.SacMade.T09'))
  return(out)
}

# fit sem ccb ~ coop (with exclusions)
semModel2Exc <- function(d) {
  model <- 'coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
            ccb =~ Env.ClimateChgCause.T09 + Env.ClimateChgConcern.T09 + Env.ClimateChgReal.T09
            # regression
            ccb ~ coop'
  out <- sem(model, data = d, ordered = c('egame.TG1.T10','egame.SH.T10','Env.ClimateChgCause.T09',
                                          'Env.ClimateChgConcern.T09','Env.ClimateChgReal.T09'))
  return(out)
}

# mediation model 1 (with exclusions)
medModel1Exc <- function(d) {
  model <- 'coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
            ccb =~ Env.ClimateChgCause.T09 + Env.ClimateChgConcern.T09 + Env.ClimateChgReal.T09
            # regression
            ccb ~ coop + Env.SacMade.T09
            Env.SacMade.T09 ~ coop'
  out <- sem(model, data = d, ordered = c('egame.TG1.T10','egame.SH.T10','Env.SacMade.T09',
                                          'Env.ClimateChgCause.T09','Env.ClimateChgConcern.T09','Env.ClimateChgReal.T09'))
  return(out)
}

# mediation model 2 (with exclusions)
medModel2Exc <- function(d) {
  model <- 'coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
            ccb =~ Env.ClimateChgCause.T09 + Env.ClimateChgConcern.T09 + Env.ClimateChgReal.T09
            # regression
            ccb ~ coop
            Env.SacMade.T09 ~ ccb + coop'
  out <- sem(model, data = d, ordered = c('egame.TG1.T10','egame.SH.T10','Env.SacMade.T09',
                                          'Env.ClimateChgCause.T09','Env.ClimateChgConcern.T09','Env.ClimateChgReal.T09'))
  return(out)
}

# fit sem Env.SacMade.T09 ~ coop (with public goods game)
semModel1PGG <- function(d) {
  model <- 'Env.SacMade.T09 ~ egame.PGG.T10'
  out <- sem(model, data = d, ordered = c('Env.SacMade.T09'))
  return(out)
}

# fit sem ccb ~ coop (with public goods game)
semModel2PGG <- function(d) {
  model <- 'ccb =~ Env.ClimateChgCause.T09 + Env.ClimateChgConcern.T09 + Env.ClimateChgReal.T09
            # regression
            ccb ~ egame.PGG.T10'
  out <- sem(model, data = d, ordered = c('Env.ClimateChgCause.T09','Env.ClimateChgConcern.T09','Env.ClimateChgReal.T09'))
  return(out)
}

# mediation model 1 (with public goods game)
medModel1PGG <- function(d) {
  model <- 'ccb =~ Env.ClimateChgCause.T09 + Env.ClimateChgConcern.T09 + Env.ClimateChgReal.T09
            # regression
            ccb ~ egame.PGG.T10 + Env.SacMade.T09
            Env.SacMade.T09 ~ egame.PGG.T10'
  out <- sem(model, data = d, ordered = c('Env.SacMade.T09','Env.ClimateChgCause.T09','Env.ClimateChgConcern.T09','Env.ClimateChgReal.T09'))
  return(out)
}

# mediation model 2 (with public goods game)
medModel2PGG <- function(d) {
  model <- 'ccb =~ Env.ClimateChgCause.T09 + Env.ClimateChgConcern.T09 + Env.ClimateChgReal.T09
            # regression
            ccb ~ egame.PGG.T10
            Env.SacMade.T09 ~ ccb + egame.PGG.T10'
  out <- sem(model, data = d, ordered = c('Env.SacMade.T09','Env.ClimateChgCause.T09','Env.ClimateChgConcern.T09','Env.ClimateChgReal.T09'))
  return(out)
}
