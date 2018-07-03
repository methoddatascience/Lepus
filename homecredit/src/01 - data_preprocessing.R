library(tidyverse)
library(magrittr)
library(recipes)
library(stringr)

# Data Loading ----
train           <- read_csv("data-raw/application_train.csv")
test            <- read_csv("data-raw/application_test.csv")
cc_balance      <- read_csv("data-raw/credit_card_balance.csv")
payments        <- read_csv("data-raw/installments_payments.csv") 
pc_balance      <- read_csv("data-raw/POS_CASH_balance.csv")
previous_app    <- read_csv("data-raw/previous_application.csv")
bureau_balance  <- read_csv("data-raw/bureau_balance.csv") 
bureau          <- read_csv("data-raw/bureau.csv")

# Feature Engineering ----
fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))

bureau %<>% 
  left_join(bureau_balance, by = "SK_ID_BUREAU") %>% 
  select(-SK_ID_BUREAU) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
rm(bureau_balance); gc()

cc_balance %<>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
gc()

payments %<>% 
  select(-SK_ID_PREV) %>% 
  mutate(PAYMENT_PERC = AMT_PAYMENT / AMT_INSTALMENT,
         PAYMENT_DIFF = AMT_INSTALMENT - AMT_PAYMENT,
         DPD = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
         DBD = DAYS_INSTALMENT - DAYS_ENTRY_PAYMENT,
         DPD = ifelse(DPD > 0, DPD, 0),
         DBD = ifelse(DBD > 0, DBD, 0)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn) 
gc()

pc_balance %<>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
gc()

previous_app %<>%
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING == 365243, NA, DAYS_FIRST_DRAWING),
         DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE == 365243, NA, DAYS_FIRST_DUE),
         DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION == 365243, NA, DAYS_LAST_DUE_1ST_VERSION),
         DAYS_LAST_DUE = ifelse(DAYS_LAST_DUE == 365243, NA, DAYS_LAST_DUE),
         DAYS_TERMINATION = ifelse(DAYS_TERMINATION == 365243, NA, DAYS_TERMINATION),
         APP_CREDIT_PERC = AMT_APPLICATION / AMT_CREDIT) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn) 
gc()

# Merging ----
train_idx <- 1:nrow(train)
y         <- train$TARGET

full_data <- train %>% 
  select(-TARGET) %>% 
  bind_rows(test) %>%
  left_join(bureau, by = "SK_ID_CURR") %>% 
  left_join(cc_balance, by = "SK_ID_CURR") %>% 
  left_join(payments, by = "SK_ID_CURR") %>% 
  left_join(pc_balance, by = "SK_ID_CURR") %>% 
  left_join(previous_app, by = "SK_ID_CURR") %>% 
  select(-SK_ID_CURR) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  mutate(na = apply(., 1, function(x) sum(is.na(x))),
         DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, NA, DAYS_EMPLOYED),
         DAYS_EMPLOYED_PERC = sqrt(DAYS_EMPLOYED / DAYS_BIRTH),
         INCOME_CREDIT_PERC = AMT_INCOME_TOTAL / AMT_CREDIT,
         INCOME_PER_PERSON = log1p(AMT_INCOME_TOTAL / CNT_FAM_MEMBERS),
         ANNUITY_INCOME_PERC = sqrt(AMT_ANNUITY / (1 + AMT_INCOME_TOTAL)),
         LOAN_INCOME_RATIO = AMT_CREDIT / AMT_INCOME_TOTAL,
         ANNUITY_LENGTH = AMT_CREDIT / AMT_ANNUITY,
         CHILDREN_RATIO = CNT_CHILDREN / CNT_FAM_MEMBERS, 
         CREDIT_TO_GOODS_RATIO = AMT_CREDIT / AMT_GOODS_PRICE,
         INC_PER_CHLD = AMT_INCOME_TOTAL / (1 + CNT_CHILDREN),
         SOURCES_PROD = EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3,
         CAR_TO_BIRTH_RATIO = OWN_CAR_AGE / DAYS_BIRTH,
         CAR_TO_EMPLOY_RATIO = OWN_CAR_AGE / DAYS_EMPLOYED,
         PHONE_TO_BIRTH_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_BIRTH,
         PHONE_TO_BIRTH_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED) 

docs <- str_subset(names(train), "FLAG_DOC")
live <- str_subset(names(train), "(?!NFLAG_)(?!FLAG_DOC)(?!_FLAG_)FLAG_")

inc_by_org <- full_data %>% 
  group_by(ORGANIZATION_TYPE) %>% 
  summarise(m = median(AMT_INCOME_TOTAL)) %$% 
  setNames(as.list(m), ORGANIZATION_TYPE)

rm(train, test, fn, bureau, cc_balance, payments, pc_balance, previous_app); gc()

full_data %<>% 
  mutate(
    DOC_IND_KURT = apply(full_data[, docs], 1, moments::kurtosis),
    LIVE_IND_SUM = apply(full_data[, live], 1, sum),
    NEW_INC_BY_ORG = recode(full_data$ORGANIZATION_TYPE, !!!inc_by_org),
    NEW_EXT_SOURCES_MEAN = apply(full_data[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, mean),
    NEW_SCORES_STD = apply(full_data[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, sd)
  ) %>%
  mutate_all(funs(ifelse(is.nan(.), NA, .))) %>% 
  mutate_all(funs(ifelse(is.infinite(.), NA, .))) %>% 
  data.matrix()

# Center/Scale, Train/Test split ----
y <- data.frame(TARGET = y)

train <- full_data[train_idx, ] 
train <- cbind(train, y)
test <- full_data[-train_idx, ]
rm(full_data, y, train_idx, inc_by_org, live, docs); gc()

# Mutate variables > 50% NA
na_cols <- colnames(train[, map_lgl(train, ~sum(is.na(.x)) > 0.5*NROW(train))])
train %<>%
  mutate_at(vars(na_cols), function(x){if_else(is.na(x), 0, 1)})
test %<>%
  as.tibble() %>% 
  mutate_at(vars(na_cols), function(x){if_else(is.na(x), 0, 1)})

na_cols <- colnames(train[, map_lgl(train, ~sum(is.na(.x)) > 0)])

recipe_obj <- recipe(TARGET ~ ., data = train) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors()) %>% 
  prep()

train <- bake(recipe_obj, newdata = train)
test  <- bake(recipe_obj, newdata = test)

train[is.na(train)] <- 99999
test[is.na(test)] <- 99999

train_x <- train[, -680]
train_y <- train[, 'TARGET']

saveRDS(train_x, 'train_x.rds')
saveRDS(train_y, 'train_y.rds')
saveRDS(test, 'test.rds')