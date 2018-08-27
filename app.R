library(pacman)
p_load(shiny)
p_load(shinyjs)
p_load(DT)             # data table object
p_load(magrittr)       # suppot the piping syntax
p_load(xgboost)        # enhanced version of gbm
p_load(ggplot2)        # draw graph
p_load(RODBC)          # Connection to Database
p_load(mlr)            # comprehensive auto machine learning package
p_load(tidyverse)      # Easy Data manipulation, (loaded last to avoid being overided)


## Global Variables
col_target <- 'CONFIRMED_FRAUD'
col_features_prod <- c('TRAN_AMT', 'NOT_FD_RC', 'AUTH_2FA', 'AUTH_TOKEN', 'SECONDS_SINCE_LOGON', 'HOUR_OF_DAY', 'CUSTOMER_TENURE', 'CUSTOMER_AGE', 
                       'NRI_RISK_LEVEL_POSTCODE', 'DAYS_LAST_MOBILE_PORTING', 'DAYS_SINCE_FIRST_PYMT', 'AMT_PAID_SAME_PAYEE', 'TIMES_PAID_SAME_PAYEE', 
                       'IS_PREPAID_CARD', 'DEAL_ACCTS', 'DEAL_VOLUME', 'DEAL_VALUE', 'IS_MULE_PAYEE_HIGH_RISK', 'MIN_BSB_DISTANCE', 
                       'IS_RELATED_JOINT_HOLDER', 'IS_RELATED_SAME_POSTCODE', 'IS_RELATED_SAME_ADDRESS', 'IS_RELATED_SAME_STATE', 'IS_RELATED_SAME_SURNAME',
                       'IS_SAME_SEX', 'AGE_DIFFERENCE', 'IS_LOGON_MOBILE_OTHER_DEV', 'IS_LOGON_MOBILE_APP', 'IS_LOGON_PC_WINDOWS', 'IS_LOGON_PC_APPLE', 
                       'RECENT_FRAUD_NUM_DEVICE', 'DAYS_SINCE_UA_FIRST_USED', 'TIMES_UA_USED', 'DAYS_SINCE_BROWSER_FIRST_USED', 'TIMES_BROWSER_USED', 
                       'TRUST_BROWSER_RATIO', 'DAYS_SINCE_BROWSER_LAST_USED', 'DAYS_SINCE_BROWSER_LAST_PAID', 'DAYS_SINCE_DEVICE_FIRST_USED', 
                       'TIMES_DEVICE_USED', 'DAYS_SINCE_DEVICE_LAST_USED', 'DAYS_SINCE_DEVICE_LAST_PAID', 'DEVICE_RATIO', 'DAYS_SINCE_IP_FIRST_USED', 
                       'TIMES_IP_USED', 'DAYS_SINCE_IP_FIRST_PAID', 'AMT_IP_PAID', 'TIMES_IP_PAID', 'DAYS_LOGON_CNTRY_FIRST_USED', 'TIMES_LOGON_CNTRY_USED', 
                       'DAYS_SINCE_ISP_FIRST_USED', 'TIMES_ISP_USED', 'IS_LOGON_OVERSEAS', 'IS_NETWORK_LOGON', 'TRUSTED_TXN_INSESSION', 
                       'DAYS_SINCE_LAST_FRAUD_PAYEE', 'FRAUD_AVG_CASE_AMT_PAYEE', 'FRAUD_TOTAL_AMT_PAYEE', 'DAYS_SINCE_LAST_FRAUD_PAYER', 
                       'FRAUD_TRAN_NUM_PAYER', 'FREQUENCY_TPP_6M', 'AVG_AMT_TPP_6M', 'MAX_AMT_TPP_6M', 'RATIO_TPP_AMT_TO_6M_AVG', 'RATIO_TPP_AMT_TO_6M_MAX', 
                       'TRAN_NUM_TPP_ABOVE_AMT_6M', 'RATIO_TPP_NUM_ABOVE_AMT_6M', 'DAYS_SINCE_LAST_TPP_6M', 'PAYEE_NUM_TPP', 'DAYS_SINCE_LAST_NPAYEE', 
                       'RATIO_DAYS_LAST_NPAYEE_TO_AVG', 'TRAN_NUM_TRUST_PAYEE_TPP_1Y', 'TRAN_AMT_TRUST_PAYEE_AVG_1Y', 'TRAN_AMT_INFREQ_PAYEE_AVG_6M', 
                       'RATIO_CURR_AMT_TO_TRUST_AVG', 'RATIO_CURR_AMT_TO_INFREQ_AVG', 'TRAN_NUM_TPP_NEW_PAYEE_3D14', 'AVG_AMT_TPP_NEW_PAYEE_3D14', 
                       'AVG_DEAL_NEW_PAYEE_3D14', 'MIN_UA_NEW_PAYEE_3D14', 'MIN_IP_NEW_PAYEE_3D14', 'MIN_DEV_NEW_PAYEE_3D14', 'TRAN_AMT_TPP_1ST_SPAYEE_14D', 
                       'DAYS_IP_1ST_SPAYEE_14D', 'DAYS_DEV_1ST_SPAYEE_14D', 'ALL_MOB_OTHER_SPAYEE_14D', 'IS_PC_WINDOW_1ST_SPAYEE_14D', 
                       'IS_PC_APPLE_1ST_SPAYEE_14D', 'DAYS_LAST_NEW_BRCH_TRAN', 'DAYS_LAST_TRUST_BRCH_TRAN', 'TOTAL_AMT_TPP_TODAY', 
                       'TRAN_NUM_TRST_TPP_CURR_DEV_3D', 'TRAN_NUM_TRST_TPP_CURR_IP_3D', 'TRAN_NUM_CDLS_3D','CDLS_DAYS_ATM_MIN_3D','CDLS_DISTANCE_HOME_AVG_3D',
                       'NUM_LOGON_3D', 'RATIO_LOGON_3D_TO_3M', 'LOGON_DAYS_BRWS_MIN_3D', 
                       'LOGON_TIMES_DEVICE_MIN_3D', 'LOGON_TRST_DEV_RATIO_MIN_3D', 'LOGON_TIMES_ISP_MIN_3D', 'NUM_LOGON_TRST_ISP_3D', 
                       'NUM_LOGON_TRST_DEV_ISP_3D', 'NUM_LOGON_CURR_ISP_3D', 'RATIO_LOGON_CURR_DEV_3D', 'RATIO_LOGON_CURR_ISP_3D', 
                       'RATIO_LOGON_CURR_DEV_ISP_3D', 'TRAN_NUM_FT_IN_RJCT_6H', 'TOT_AMT_FT_IN_6H', 'TOT_AMT_FT_OUT_6H', 'TOT_AMT_FT_NET_6H', 'NUM_LOGON_6H', 
                       'LOGON_DAYS_UA_MIN_6H', 'LOGON_DAYS_BRWS_MIN_6H', 'LOGON_DAYS_DEVICE_MIN_6H', 'LOGON_TRST_DEV_RATIO_MIN_6H', 'LOGON_DAYS_ISP_MIN_6H', 
                       'LOGON_TIMES_ISP_MIN_6H', 'RATIO_LOGON_TRST_DEV_ISP_6H', 'NUM_LOGON_CURR_UA_6H', 'NUM_LOGON_CURR_DEV_6H', 'RATIO_LOGON_CURR_UA_6H', 
                       'RATIO_LOGON_CURR_ISP_6H', 'RATIO_LOGON_CURR_DEV_ISP_6H', 'DAYS_LAST_TB_REG_WBC', 'DAYS_LAST_TB_RESET_CCC_WBC', 
                       'DAYS_LAST_TB_RESET_BRCH_WBC', 'DAYS_LAST_TB_RESET_CUST_WBC', 'DAYS_LAST_IB_REG_WBC', 'DAYS_LAST_IB_PW_WBC', 'DAYS_LAST_IB_TMP_PW_WBC', 
                       'DAYS_LAST_SMS_LINK_WBC', 'DAYS_LAST_IB_FRGT_PW_WBC', 'DAYS_LAST_ADDR_CHG', 'DAYS_LAST_MOB_PHONE_CHG', 'DAYS_LAST_EMAIL_CHG', 
                       'DAYS_LAST_CONTACT_CHG', 'DAYS_LAST_IB_PW_CCC_SGB', 'DAYS_LAST_IB_PW_BRCH_SGB', 'DAYS_LAST_IB_PW_CUST_SGB', 'DAYS_LAST_IB_REG_CCC_SGB', 
                       'DAYS_LAST_IB_REG_BRCH_SGB', 'DAYS_LAST_IB_REG_CUST_SGB', 'NUM_IB_PW_CUST_SGB_0M1', 'NUM_TB_RESET_CCC_WBC_3D', 'NUM_TB_RESET_BRCH_WBC_3D',
                       'NUM_TB_RESET_CUST_WBC_3D', 'NUM_IB_PW_WBC_3D', 'NUM_IB_TMP_PW_WBC_3D', 'NUM_IB_FRGT_PW_WBC_3D', 'NUM_SMS_LINK_WBC_3D', 'NUM_MOB_PHONE_CHG_3D',
                       'NUM_EMAIL_CHG_3D', 'NUM_CONTACT_CHG_3D', 'NUM_ADDR_CHG_3D', 'NUM_IB_PW_CCC_SGB_3D', 'NUM_IB_PW_BRCH_SGB_3D', 'NUM_IB_PW_CUST_SGB_3D',
                       'NUM_PYMT_AFT_PW_CHG', 'RATIO_NUM_NEW_PYMT_AFT_PW_CHG', 'NUM_LOGON_AFT_PW_CHG', 'R_TRST_LOGON_IP_AFT_PW_CHG', 'R_TRST_LOG_BRWS_ISP_AFT_PW_CHG', 
                       'MAX_HOME_DIST_CARD_3D', 'ISP_RISK_GROUP', 'IS_WBG_PAYEE', 'DAYS_SINCE_DEVICEID_FIRST_USED', 'TIMES_DEVICEID_USED')


## User functions
get_extra_features <- function(data){
  # Derive more features
  data <- data %>% mutate(IS_DEVICE_ID_AFT_IB_PW_WBC = if_else(is.na(DAYS_SINCE_DEVICEID_FIRST_USED) | is.na(DAYS_LAST_IB_PW_WBC) | DAYS_SINCE_DEVICEID_FIRST_USED <= -1, as.double(NA),
                                                               if_else(DAYS_LAST_IB_PW_WBC>21, 0,
                                                                       if_else(DAYS_SINCE_DEVICEID_FIRST_USED < DAYS_LAST_IB_PW_WBC + 2, 1, 0)))
                          ,IS_DEVICE_AFT_IB_PW_WBC = if_else(is.na(DAYS_SINCE_DEVICE_FIRST_USED) | is.na(DAYS_LAST_IB_PW_WBC) | DAYS_SINCE_DEVICE_FIRST_USED <= -1, as.double(NA),
                                                             if_else(DAYS_LAST_IB_PW_WBC>21, 0,
                                                                     if_else(DAYS_SINCE_DEVICE_FIRST_USED < DAYS_LAST_IB_PW_WBC + 2, 1, 0)))
                          ,IS_BROWSER_AFT_IB_PW_WBC = if_else(is.na(DAYS_SINCE_BROWSER_FIRST_USED) | is.na(DAYS_LAST_IB_PW_WBC) | DAYS_SINCE_BROWSER_FIRST_USED <= -1, as.double(NA),
                                                              if_else(DAYS_LAST_IB_PW_WBC>21, 0,
                                                                      if_else(DAYS_SINCE_BROWSER_FIRST_USED < DAYS_LAST_IB_PW_WBC + 2, 1, 0)))
                          ,IS_ISP_AFT_IB_PW_WBC = if_else(is.na(DAYS_SINCE_ISP_FIRST_USED) | is.na(DAYS_LAST_IB_PW_WBC) | DAYS_SINCE_ISP_FIRST_USED <= -1, as.double(NA),
                                                          if_else(DAYS_LAST_IB_PW_WBC>21, 0,
                                                                  if_else(DAYS_SINCE_ISP_FIRST_USED < DAYS_LAST_IB_PW_WBC + 2, 1, 0)))                        
                          ,IS_IP_AFT_IB_PW_WBC = if_else(is.na(DAYS_SINCE_IP_FIRST_USED) | is.na(DAYS_LAST_IB_PW_WBC) | DAYS_SINCE_IP_FIRST_USED <= -1, as.double(NA),
                                                         if_else(DAYS_LAST_IB_PW_WBC>21, 0,
                                                                 if_else(DAYS_SINCE_IP_FIRST_USED < DAYS_LAST_IB_PW_WBC + 2, 1, 0))) 
                          
                          ,IS_DEVICE_ID_AFT_IB_TMP_WBC = if_else(is.na(DAYS_SINCE_DEVICEID_FIRST_USED) | is.na(DAYS_LAST_IB_TMP_PW_WBC) | DAYS_SINCE_DEVICEID_FIRST_USED <= -1, as.double(NA),
                                                                 if_else(DAYS_LAST_IB_TMP_PW_WBC>21, 0,
                                                                         if_else(DAYS_SINCE_DEVICEID_FIRST_USED < DAYS_LAST_IB_TMP_PW_WBC + 2, 1, 0)))
                          ,IS_DEVICE_AFT_IB_TMP_WBC = if_else(is.na(DAYS_SINCE_DEVICE_FIRST_USED) | is.na(DAYS_LAST_IB_TMP_PW_WBC) | DAYS_SINCE_DEVICE_FIRST_USED <= -1, as.double(NA),
                                                              if_else(DAYS_LAST_IB_TMP_PW_WBC>21, 0,
                                                                      if_else(DAYS_SINCE_DEVICE_FIRST_USED < DAYS_LAST_IB_TMP_PW_WBC + 2, 1, 0)))
                          ,IS_BROWSER_AFT_IB_TMP_WBC = if_else(is.na(DAYS_SINCE_BROWSER_FIRST_USED) | is.na(DAYS_LAST_IB_TMP_PW_WBC) | DAYS_SINCE_BROWSER_FIRST_USED <= -1, as.double(NA),
                                                               if_else(DAYS_LAST_IB_TMP_PW_WBC>21, 0,
                                                                       if_else(DAYS_SINCE_BROWSER_FIRST_USED < DAYS_LAST_IB_TMP_PW_WBC + 2, 1, 0)))
                          ,IS_ISP_AFT_IB_TMP_WBC = if_else(is.na(DAYS_SINCE_ISP_FIRST_USED) | is.na(DAYS_LAST_IB_TMP_PW_WBC) | DAYS_SINCE_ISP_FIRST_USED <= -1, as.double(NA),
                                                           if_else(DAYS_LAST_IB_TMP_PW_WBC>21, 0,
                                                                   if_else(DAYS_SINCE_ISP_FIRST_USED < DAYS_LAST_IB_TMP_PW_WBC + 2, 1, 0)))                        
                          ,IS_IP_AFT_IB_TMP_WBC = if_else(is.na(DAYS_SINCE_IP_FIRST_USED) | is.na(DAYS_LAST_IB_TMP_PW_WBC) | DAYS_SINCE_IP_FIRST_USED <= -1, as.double(NA),
                                                          if_else(DAYS_LAST_IB_TMP_PW_WBC>21, 0,
                                                                  if_else(DAYS_SINCE_IP_FIRST_USED < DAYS_LAST_IB_TMP_PW_WBC + 2, 1, 0)))                      
                          
                          ,IS_DEVICE_ID_AFT_IB_FRG_WBC = if_else(is.na(DAYS_SINCE_DEVICEID_FIRST_USED) | is.na(DAYS_LAST_IB_FRGT_PW_WBC) | DAYS_SINCE_DEVICEID_FIRST_USED <= -1, as.double(NA),
                                                                 if_else(DAYS_LAST_IB_FRGT_PW_WBC>21, 0,
                                                                         if_else(DAYS_SINCE_DEVICEID_FIRST_USED < DAYS_LAST_IB_FRGT_PW_WBC + 2, 1, 0)))
                          ,IS_DEVICE_AFT_IB_FRG_WBC = if_else(is.na(DAYS_SINCE_DEVICE_FIRST_USED) | is.na(DAYS_LAST_IB_FRGT_PW_WBC) | DAYS_SINCE_DEVICE_FIRST_USED <= -1, as.double(NA),
                                                              if_else(DAYS_LAST_IB_FRGT_PW_WBC>21, 0,
                                                                      if_else(DAYS_SINCE_DEVICE_FIRST_USED < DAYS_LAST_IB_FRGT_PW_WBC + 2, 1, 0)))
                          ,IS_BROWSER_AFT_IB_FRG_WBC = if_else(is.na(DAYS_SINCE_BROWSER_FIRST_USED) | is.na(DAYS_LAST_IB_FRGT_PW_WBC) | DAYS_SINCE_BROWSER_FIRST_USED <= -1, as.double(NA),
                                                               if_else(DAYS_LAST_IB_FRGT_PW_WBC>21, 0,
                                                                       if_else(DAYS_SINCE_BROWSER_FIRST_USED < DAYS_LAST_IB_FRGT_PW_WBC + 2, 1, 0)))
                          ,IS_ISP_AFT_IB_FRG_WBC = if_else(is.na(DAYS_SINCE_ISP_FIRST_USED) | is.na(DAYS_LAST_IB_FRGT_PW_WBC) | DAYS_SINCE_ISP_FIRST_USED <= -1, as.double(NA),
                                                           if_else(DAYS_LAST_IB_FRGT_PW_WBC>21, 0,
                                                                   if_else(DAYS_SINCE_ISP_FIRST_USED < DAYS_LAST_IB_FRGT_PW_WBC + 2, 1, 0)))                        
                          ,IS_IP_AFT_IB_FRG_WBC = if_else(is.na(DAYS_SINCE_IP_FIRST_USED) | is.na(DAYS_LAST_IB_FRGT_PW_WBC) | DAYS_SINCE_IP_FIRST_USED <= -1, as.double(NA),
                                                          if_else(DAYS_LAST_IB_FRGT_PW_WBC>21, 0,
                                                                  if_else(DAYS_SINCE_IP_FIRST_USED < DAYS_LAST_IB_FRGT_PW_WBC + 2, 1, 0)))                          
                          
                          ,IS_DEVICE_ID_AFT_SMS_WBC = if_else(is.na(DAYS_SINCE_DEVICEID_FIRST_USED) | is.na(DAYS_LAST_SMS_LINK_WBC) | DAYS_SINCE_DEVICEID_FIRST_USED <= -1, as.double(NA),
                                                              if_else(DAYS_LAST_SMS_LINK_WBC>21, 0,
                                                                      if_else(DAYS_SINCE_DEVICEID_FIRST_USED < DAYS_LAST_SMS_LINK_WBC + 2, 1, 0)))
                          ,IS_DEVICE_AFT_SMS_WBC = if_else(is.na(DAYS_SINCE_DEVICE_FIRST_USED) | is.na(DAYS_LAST_SMS_LINK_WBC) | DAYS_SINCE_DEVICE_FIRST_USED <= -1, as.double(NA),
                                                           if_else(DAYS_LAST_SMS_LINK_WBC>21, 0,
                                                                   if_else(DAYS_SINCE_DEVICE_FIRST_USED < DAYS_LAST_SMS_LINK_WBC + 2, 1, 0)))
                          ,IS_BROWSER_AFT_SMS_WBC = if_else(is.na(DAYS_SINCE_BROWSER_FIRST_USED) | is.na(DAYS_LAST_SMS_LINK_WBC) | DAYS_SINCE_BROWSER_FIRST_USED <= -1, as.double(NA),
                                                            if_else(DAYS_LAST_SMS_LINK_WBC>21, 0,
                                                                    if_else(DAYS_SINCE_BROWSER_FIRST_USED < DAYS_LAST_SMS_LINK_WBC + 2, 1, 0)))
                          ,IS_ISP_AFT_SMS_WBC = if_else(is.na(DAYS_SINCE_ISP_FIRST_USED) | is.na(DAYS_LAST_SMS_LINK_WBC) | DAYS_SINCE_ISP_FIRST_USED <= -1, as.double(NA),
                                                        if_else(DAYS_LAST_SMS_LINK_WBC>21, 0,
                                                                if_else(DAYS_SINCE_ISP_FIRST_USED < DAYS_LAST_SMS_LINK_WBC + 2, 1, 0)))                        
                          ,IS_IP_AFT_SMS_WBC = if_else(is.na(DAYS_SINCE_IP_FIRST_USED) | is.na(DAYS_LAST_SMS_LINK_WBC) | DAYS_SINCE_IP_FIRST_USED <= -1, as.double(NA),
                                                       if_else(DAYS_LAST_SMS_LINK_WBC>21, 0,
                                                               if_else(DAYS_SINCE_IP_FIRST_USED < DAYS_LAST_SMS_LINK_WBC + 2, 1, 0))) 
                          
                          
                          ,IS_DEVICE_ID_AFT_ADDR_CHG = if_else(is.na(DAYS_SINCE_DEVICEID_FIRST_USED) | is.na(DAYS_LAST_ADDR_CHG) | DAYS_SINCE_DEVICEID_FIRST_USED <= -1, as.double(NA),
                                                               if_else(DAYS_LAST_ADDR_CHG>21, 0,
                                                                       if_else(DAYS_SINCE_DEVICEID_FIRST_USED < DAYS_LAST_ADDR_CHG + 2, 1, 0)))
                          ,IS_DEVICE_AFT_ADDR_CHG  = if_else(is.na(DAYS_SINCE_DEVICE_FIRST_USED) | is.na(DAYS_LAST_ADDR_CHG) | DAYS_SINCE_DEVICE_FIRST_USED <= -1, as.double(NA),
                                                             if_else(DAYS_LAST_ADDR_CHG>21, 0,
                                                                     if_else(DAYS_SINCE_DEVICE_FIRST_USED < DAYS_LAST_ADDR_CHG + 2, 1, 0)))
                          ,IS_BROWSER_AFT_ADDR_CHG  = if_else(is.na(DAYS_SINCE_BROWSER_FIRST_USED) | is.na(DAYS_LAST_ADDR_CHG) | DAYS_SINCE_BROWSER_FIRST_USED <= -1, as.double(NA),
                                                              if_else(DAYS_LAST_ADDR_CHG>21, 0,
                                                                      if_else(DAYS_SINCE_BROWSER_FIRST_USED < DAYS_LAST_ADDR_CHG + 2, 1, 0)))
                          ,IS_ISP_AFT_ADDR_CHG  = if_else(is.na(DAYS_SINCE_ISP_FIRST_USED) | is.na(DAYS_LAST_ADDR_CHG) | DAYS_SINCE_ISP_FIRST_USED <= -1, as.double(NA),
                                                          if_else(DAYS_LAST_ADDR_CHG>21, 0,
                                                                  if_else(DAYS_SINCE_ISP_FIRST_USED < DAYS_LAST_ADDR_CHG + 2, 1, 0)))                        
                          ,IS_IP_AFT_ADDR_CHG  = if_else(is.na(DAYS_SINCE_IP_FIRST_USED) | is.na(DAYS_LAST_ADDR_CHG) | DAYS_SINCE_IP_FIRST_USED <= -1, as.double(NA),
                                                         if_else(DAYS_LAST_ADDR_CHG>21, 0,
                                                                 if_else(DAYS_SINCE_IP_FIRST_USED < DAYS_LAST_ADDR_CHG + 2, 1, 0))) 
  )
}


# partial dependency plot
get_partial_dependence <- function(test_cases, feature_to_change
                                   ,mlr_model= mlr_xgboost, mlr_model_test = mlr_xgboost_test, target = col_target, features = col_features, features_test = col_features_test
                                   ,scale_from = threshold, scale_from_test = threshold_test, scale_to = 0.9
                                   ,value_from = 0, value_to = 200, num_points = 10){
  ## create value list
  Original_value <- test_cases[1,] %>% select(!!feature_to_change) %>% unlist
  
  ## set point interval
  value_list <- seq(value_from, value_to, length.out = num_points) %>% data.frame(value = .) %>%
    rbind(value = Original_value) %>% arrange(value) %>% distinct(.keep_all = TRUE) ## add the original value in the list
  
  ## combine with other features
  test_cases_new <- test_cases %>% crossing(value_list) %>% mutate(!!feature_to_change := value)

  
  ##----   PROD Model  ----##
  ## apply model to get score 
  pred_prod_model  <- predict(mlr_model, newdata = test_cases_new[c(target, features)])
  
  ## scale model score to production scor
  dependency <- pred_prod_model$data %>% 
    mutate(prod_model = 1/(1 + exp(log(1/prob.FRAUD- 1) + log((1/scale_to - 1)/(1/scale_from - 1))))) %>% select(prod_model) 
  

  ##----  TEST Model  ----##  
  if(! all(is.na(mlr_model_test))){
    ## apply model to get score
    pred_test_model  <- predict(mlr_model_test, newdata = test_cases_new[c(target, features_test)])
    
    ## scale model score to production score
    test_dependency <- pred_test_model$data %>%
      mutate(test_model = 1/(1 + exp(log(1/prob.FRAUD - 1) + log((1/scale_to - 1)/(1/scale_from_test - 1))))) %>% select(test_model)    
  }else{
    test_dependency <- dependency %>% mutate(test_model = as.numeric(NA)) %>% select(test_model)
  }
  
  dependency <- dependency %>% cbind(test_dependency) %>%
    cbind(test_cases_new %>% select(SD_UAN, SD_TIEBREAKER, DD_DATE, !!feature_to_change), .) %>%
    mutate_if(is.numeric, funs(round(., 4)))
  
  
  ## plot the partial dependence
  plot <- dependency %>% rename(value = !!feature_to_change) %>%
    ggplot(aes(x = value)) +
    geom_line(aes(y = prod_model, colour = "prod_model")) +
    geom_line(aes(y = test_model, colour = "test_model")) +
    facet_wrap(~DD_DATE) +
    geom_vline(xintercept = Original_value) +
    ggtitle("Partitial Dependence") + ylab("Score") +xlab(feature_to_change)
  
  partial_denpence <- list()
  partial_denpence[[1]] <- dependency
  partial_denpence[[2]] <- plot
  
  return(partial_denpence)
}



# Define UI for app
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(".rightAlign  {float: right;}
                        .grey_out    {color: #a9a9a9;}
                        .bold_weight {font-weight: 700;}
                       ")),
  
  # App title ----
  titlePanel("Skynet Features Inspector", windowTitle = "Skynet Features Inspector"),
  
  br(),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: UAN  ----
      textInput(inputId = "UAN",
                label = "SD_UAN:",
                value = "WBC50154520020",  # default example at startup
                placeholder = "Input an UAN"),
      
      actionLink("refleshData", "Reflesh Data", class = 'bond grey_out rightAlign'),
      
      # Input: DD_PARTITION_DATE  ----
      # dateInput(inputId = "DATE",
      #           label = "DD_PARTITION_DATE:"),
      
      br(),
      br(),
      # Input: Model choice for feature order and model rerun  ----
      selectInput("model", "Scoring Model:",
                  list("Choose a model" = "",
                       `WBC` = c("WBC PROD 2018-06-06"  ="XGBoost_WBC_PROD_2018-06-06"
                                 ,"WBC TEST 2018-07-19" ="XGBoost_WBC_TEST_2018-07-19"),
                       `SGB` = c("SGB PROD 2018-06-06"  ="XGBoost_SGB_PROD_2018-06-06"
                                 ,"SGB PROD 2018-07-17" ="XGBoost_SGB_PROD_2018-07-17"),
                       `Temp` = c("Temp Model" = "temp_model"))
                  
      ),
      
      # # Action Button: Calculate score with the chosen model  ----
      # actionButton("calculateScore", "Calculate Model Score"),
      
      
      br(),
      # Input: Feature Name for partial dependency chart  ----
      textInput(inputId = "pd_featureName",
                label = "Partial Dependency Plot:",
                value = "",  # default example at startup
                placeholder = "Input a feature name" ),
      # # slider to set display interval
      # sliderInput(inputId = 'pd_slider',
      #             label = 'Display Interval',
      #             min(0), max(100), value=c(15,80)),
      # # input for number of estimation points to show
      # numericInput(inputId = 'pd_bin_num',
      #              label = 'Numer of Bins',
      #              value = 10),
      
      uiOutput("pd_control")
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      textOutput("SD_UAN") %>% h3,
      
      # Partial dependency plot output
      plotOutput("pd_1_way"),
      
      
      # Output: HTML table with requested number of observations ----
      dataTableOutput("feature_table")
      
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  ## reactive data objects that persist through actions
  reactValues <- reactiveValues(rawData=NULL, displayData=NULL)
  
  
  ## Get data from PRM database (when clicking the reflesh button)
  observe({
    ## make the data reload to be dependent on the button action
    dummy <- input$refleshData
    
    PRM_pwd <- 'M042906'
    PRM_db <- odbcConnect('PRM_DR', uid = 'M042906', pwd = PRM_pwd, believeNRows = TRUE)
    
    isolate(
      if(input$UAN == ""){
        trans <- sqlQuery(PRM_db, "
                          SELECT * FRSOM GFMPFACE.TPP_SKYNET_FEATURES_DEV
                          WHERE DD_PARTITION_DATE = trunc(sysdate) 
                          AND rownum <= 1
                          ")      
      }else{
        trans <- sqlQuery(PRM_db, paste0("
                                         SELECT * FROM GFMPFACE.TPP_SKYNET_FEATURES_DEV
                                         WHERE SD_UAN = '", input$UAN, "'
                                         ORDER BY DD_DATE DESC
                                         "))
        
        # trans1 <- sqlQuery(PRM_db, paste0("
        #                                  SELECT * FROM GFMPFACE.CT_WBC_NPP_SKYNET_RT_FEATURE
        #                                   WHERE SD_UAN = '", input$UAN, "' 
        #                                   ORDER BY DD_DATE DESC
        #                                   "))  
        # common_col <- trans1 %>% colnames %>% intersect(trans %>% colnames)
        # trans <- trans1[,common_col] %>% rbind(trans[,common_col])
      }
    )
    odbcClose(PRM_db)   
    
    ## Calculate some extra features for some models
    #if(input$model == "XGBoost_WBC_TEST_2018-07-19"){
    trans <- trans %>% get_extra_features()
    #}
    
    ## return
    reactValues$rawData <- trans
  })
  
  
  ## prepare feature value table from the loaded data (re-generate when changing the selected model or the loaded data)
  observe({
    table <- NULL
    
    ## retreived transaction feature data
    transactions <- reactValues$rawData
    IS_WBC_CUSTOMER <- 0
    if(input$UAN %>% substr(1,3) == 'WBC') {
      IS_WBC_CUSTOMER <- 1
    }
    
    ## load the correspond model objects
    if(input$model != "" & input$model != "temp_model"){
        load(file = paste0("C:/Users/M042906/Works/Shiny App - Skynet Transaction Inspector/Skynet Model Objects/", input$model,".rda"))
    }
    
    ## features to show -- by feature importancy if a model was chosen
    if(input$model != "" & exists("mlr_xgboost")){
      col_features_sorted <- getFeatureImportance(mlr_xgboost)$res %>%  gather(key = "feature", value = "importance") %>% arrange(desc(importance)) %>% as.data.frame %>% .[, 'feature']
    }else{
      col_features_sorted <- col_features_prod %>% c(transactions %>% colnames %>% setdiff(col_features_prod))
    }
    col_info <- c("ETL_DT", "SCORE_PROD", "SCORE_RECALCULATED","CONFIRMED_FRAUD", "PRM_ALERTED", "TRAN_AMT")
    col_features_sorted <- col_features_sorted %>% intersect(transactions %>% colnames) %>% #only keep the available columns
      setdiff(col_info) ## avoid duplicate columns
    
    ## Update the recalculated score if a model was chosen
    if(input$model != "" & exists("mlr_xgboost")){
      ## Apply model
      pred <- predict(mlr_xgboost, newdata = transactions[c(col_target, col_features)])
      
      ## Rescale to production score
      new_score <- pred$data <- pred$data %>% mutate(prob.FRAUD = round(1/(1 + exp(log(1/prob.FRAUD - 1) + log((1/0.9 - 1)/(1/threshold - 1)))), 4))
      
      ## replace the field with new calculated score
      transactions <- transactions %>% cbind(new_score %>% select(prob.FRAUD)) %>% mutate(SCORE_RECALCULATED = prob.FRAUD) %>% select(-prob.FRAUD)
    }else{
      ## don't show any recalculated score if no model was chosen
      col_info <- col_info %>% setdiff('SCORE_RECALCULATED')
    }    
    
    ## transform to long table for better readability
    table <- transactions[,c(col_info, col_features_sorted)]
    output_table <- table %>% t %>% as.data.frame(stringsAsFactors=FALSE)
    output_table <- output_table %>% cbind(rownames(.), .)
    colnames(output_table) <- c("Feature Name", transactions[, "DD_DATE"] %>% as.array %>%  as.character)
    
    reactValues$displayData <- output_table
    ## Output as an interactive dataTable
    #datatable(output_table, rownames = FALSE, options = list(pageLength = 100, bLengthChange=FALSE), selection = 'none', editable = TRUE)
  })
  
  ## render as an interactive dataTable
  output$feature_table <- renderDataTable(reactValues$displayData, rownames = FALSE, options = list(pageLength = 100, bLengthChange=FALSE), selection = 'none', editable = TRUE)
  
  
  ## update underlying data with edited value from UI
  proxy = dataTableProxy('feature_table')
  observeEvent(input$feature_table_cell_edit, {
    info = input$feature_table_cell_edit
    #str(info)
    i = info$row
    j = info$col + 1  ## offset needed when rownames = FALSE in rendered data table
    v = info$value
    
    featureName <- reactValues$displayData[i, 'Feature Name'] %>% as.character
    reactValues$rawData[j-1, featureName] <- DT::coerceValue(v, reactValues$displayData[i, j]) ## update the underlying data
    replaceData(proxy, reactValues$displayData, resetPaging = FALSE)  # important
  })  
  
  
  ## Dynamic UI for controls of the partital dependency chart
  output$pd_control <- renderUI({
    if(input$pd_featureName != "") {
      show("pd_control")
    
      original_value <- reactValues$rawData[1,] %>% select(!!input$pd_featureName) %>% unlist %>% as.numeric
      ## set default slider value
      if(original_value != 0) {
        value_to <- original_value*2
      }else{value_to <- 3} 
      
      ## set slider max value
      slider_max <- max(value_to * 2, 50)
      
      ## construct UIs
      output_UI <- tagList()
      
      # input for display interval of the pd chart
      output_UI[[1]] <- 
        sliderInput(inputId = 'pd_slider',
                    label = 'Display Interval',
                    min(-1), max(slider_max), value=c(0,value_to))   
      
      # input for number of estimation points
      output_UI[[2]] <-
        numericInput(inputId = 'pd_bin_num',
                     label = 'Numer of Bins',
                     value = 10)
      
      ## return UIs
      output_UI
      
    }else{
      hide("pd_control")
    }
  })
  
  ## prepare debug text output
  # output$SD_UAN <- renderText({
  #   input$pd_slider
  # })    
  
  ## prepare the partial dependency plot
  output$pd_1_way <- renderPlot({
    ## do it only when a feature is selected
    if(input$pd_featureName != "") {
      req(input$pd_bin_num) ## only try to show chart when the dynamic controls has been loaded (avoid error messages)
      show("pd_1_way")
      
      transactions <- reactValues$rawData
      
      # load the chosen model
      if(input$model != ""){
        if(input$model != "temp_model"){
          load(file = paste0("C:/Users/M042906/Works/Shiny App - Skynet Transaction Inspector/Skynet Model Objects/", input$model,".rda"))
        }
        mlr_xgboost_test <- mlr_xgboost
        col_features_test <- col_features
        threshold_test <- threshold
      }else{
        mlr_xgboost_test <- NA
        col_features_test <- NA
        threshold_test <- NA          
      }
      
      # load the prod model by default
      if(input$UAN %>% str_sub(1,3) == "WBC"){
        load(file = "C:/Users/M042906/Works/Shiny App - Skynet Transaction Inspector/Skynet Model Objects/XGBoost_WBC_PROD_2018-06-06.rda")
      }else{
        load(file = "C:/Users/M042906/Works/Shiny App - Skynet Transaction Inspector/Skynet Model Objects/XGBoost_SGB_PROD_2018-07-17.rda")     
      }
      
      ## calculate the plot
      if(exists("mlr_xgboost")){  
        result <- get_partial_dependence(transactions, input$pd_featureName
                                         ,mlr_model= mlr_xgboost, mlr_model_test = mlr_xgboost_test, target = col_target, features = col_features, features_test = col_features_test
                                         ,scale_from = threshold, scale_from_test = threshold_test, scale_to = 0.9
                                         ,value_from = input$pd_slider[1], value_to = input$pd_slider[2], num_points = input$pd_bin_num)
      }
      
      result[[2]]    #plot
    }else{
      hide("pd_1_way")
    }
  })
  
  outputOptions(output, "pd_1_way", suspendWhenHidden = FALSE) # needed for show/hide, otherwise Shiny won't sent value to hidden output 
  outputOptions(output, "pd_control", suspendWhenHidden = FALSE) # needed for show/hide, otherwise Shiny won't sent value to hidden output 
  
  # ## Output new score of the chosen model
  # output$newScore <- renderText({  
  #   transactions <- datasetInput()
  #   
  #   if(input$model != ""){
  #     ## Load model
  #     load(file = paste0("C:/Users/M042906/Works/Shiny App - Skynet Transaction Inspector/Skynet Model Objects/", input$model,".rda"))
  #     
  #     ## Apply model
  #     pred <- predict(mlr_xgboost, newdata = transactions[c(col_target, col_features)])
  #     
  #     ## Rescale to production score
  #     new_score <- pred$data <- pred$data %>% mutate(prob.FRAUD = round(1/(1 + exp(log(1/prob.FRAUD - 1) + log((1/0.9 - 1)/(1/threshold - 1)))), 4)) %>%
  #       select(prob.FRAUD) %>% unlist %>% as.character
  #     
  #     ## Return
  #     paste("Re-calculated Score:", new_score)
  #   }
  # })
  
  
  # ## Re-calculate score with the chosen model when the button is clicked
  # observeEvent(input$calculateScore, {
  #   transactions <- datasetInput()
  # 
  #   ## load the correspond model object
  #   if(input$model != ""){
  #     load(file = paste0("C:/Users/M042906/Works/Shiny App - Skynet Transaction Inspector/Skynet Model Objects/", input$model,".rda"))
  #   
  #     ## Apply model
  #     pred <- predict(mlr_xgboost, newdata = transactions[c(col_target, col_features)])
  #     
  #     ## Rescale to production score
  #     pred$data <- pred$data %>% mutate(prob.FRAUD = round(1/(1 + exp(log(1/prob.FRAUD - 1) + log((1/0.9 - 1)/(1/threshold - 1)))), 4))
  #     
  #     ## Output new score
  #     output$newScore <- renderText({
  #       pred$data %>% select(prob.FRAUD) %>% unlist %>% as.character
  #     })
  # 
  #   }
  # }, ignoreInit = TRUE, once = FALSE)
  
}



shinyApp(ui = ui, server = server)