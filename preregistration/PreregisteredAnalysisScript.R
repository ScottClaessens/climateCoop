library(laavan)

##############
# Analysis 1 #
##############

# 1.1. Cooperative phenotype
model <- 'coop =~ tg1 + tg2 + pgg + dg + sh'
cfaModel1.1 <- cfa(model, data = d, ordered = c('tg1', 'sh'))

# 1.2. Climate change belief
model <- 'ccb =~ Env.ClimateChgReal.T9 + Env.ClimateChgCause.T9 + Env.ClimateChgConcern.T9'
cfaModel1.2 <- cfa(model, data = d, 
                   ordered = c('Env.ClimateChgReal.T9', 
                               'Env.ClimateChgCause.T9',
                               'Env.ClimateChgConcern.T9'))

##############
# Analysis 2 #
##############

# Cooperative phenotype predicting pro-environmental behaviour
model <- '# measurement model
          coop =~ tg1 + tg2 + pgg + dg + sh
          # regression
          Env.SacMade.T9 ~ coop'
semModel2 <- sem(model, data = d, 
                 ordered = c('tg1','sh','Env.SacMade.T9'))

##############
# Analysis 3 #
##############

# Cooperative phenotype predicting climate change beliefs
model <- '# measurement model
          coop =~ tg1 + tg2 + pgg + dg + sh
          ccb =~ Env.ClimateChgReal.T9 + Env.ClimateChgCause.T9 + Env.ClimateChgConcern.T9
          # regression
          ccb ~ coop'
semModel3 <- sem(model, data = d, 
                 ordered = c('tg1','sh',
                             'Env.ClimateChgReal.T9',
                             'Env.ClimateChgCause.T9',
                             'Env.ClimateChgConcern.T9'))

##############
# Analysis 4 #
##############

# Mediation model
model <- '# measurement model
          coop =~ tg1 + tg2 + pgg + dg + sh
          ccb =~ Env.ClimateChgReal.T9 + Env.ClimateChgCause.T9 + Env.ClimateChgConcern.T9
          # regressions
          ccb ~ c*coop
          Env.SacMade.T9 ~ a*coop
          ccb ~ b*Env.SacMade.T9
          # direct effect
          ab := a*b
          # total effect
          total := c + (a*b)'
semModel4 <- sem(model, data = d, 
                 ordered = c('tg1','sh',
                             'Env.SacMade.T9',
                             'Env.ClimateChgReal.T9',
                             'Env.ClimateChgCause.T9',
                             'Env.ClimateChgConcern.T9'))