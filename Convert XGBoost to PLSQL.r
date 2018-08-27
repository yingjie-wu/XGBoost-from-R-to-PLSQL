#==========================================================================================
#         Function to convert a dumped XGBoost table into PL/SQL
#
# Author: Yingjie (Terence) Wu
# Date: 2018-01-05
# Description:
#   XgBoost provides an API (xgb.model.dt.tree) to dump it's model into a structured table.
#   The functions here aim to convert the table into PL/SQL, which can be consumed by
#   Oracle/PRM.
#
# Example:
#   model_xgb <- getLearnerModel(mlr_xgboost) -- needed if model was trained with the mlr package
#   trees_xgb <- xgb.model.dt.tree(feature_names = col_features, model = model_xgb, n_first_tree = NULL)
#   file_output = "C:/Users/M042906/Documents/R/models/SQL - xgboost test.txt"
#   convert.xgboost.to.plsql(trees_xgb, file_output) 
#===========================================================================================


write.node <- function(tree_table, node_id, prev_node_id, feature_prefix) {
  node <- tree_table %>% filter(ID==node_id)
  
  ## check if the previous line ended with 'END'
  is_prev_end <- FALSE
  if(prev_node_id != ''){
    prev_node <- tree_table %>% filter(ID==prev_node_id)
    if(prev_node$Feature == 'Leaf' & prev_node_id %in% tree_table$No){
      is_prev_end <- TRUE
    }   
  }
  
  ## check if to start a new line 
  new_line <- FALSE
  if(node$Feature != 'Leaf'){
    ## 'CASE WHEN' criteria node
    new_line <- TRUE
  }else{
    ## 'leaf' node following an 'END'
    new_line <- is_prev_end
  }
  
  ## adjust indention space and add 'ELSE' key word 
  if(node$ID %in% tree_table$No){
    if(is_prev_end) {indent <<- substring(indent, 3)}
    if(new_line) {cat('\n', indent)}
    cat('ELSE', '')
  }else{
    if(new_line){
      indent <<- paste(indent, ' ')
      cat('\n', indent)
    }    
  }  
  
  ## write leaf value or the node criteria
  if(node$Feature == 'Leaf') {
    cat(node$Quality, '')
  }else{
    cat('CASE WHEN', paste(feature_prefix, node$Feature, sep = ''), '<', node$Split, '')
    if(node$Yes == node$Missing){
      cat('OR', paste(feature_prefix, node$Feature, sep = ''), 'IS NULL', '')
    }
    cat('THEN', '')
  }
  # cat('(',nchar(indent),')', sep = '') ## debug the indent space
}


get.next.node.id <- function(tree_table, node_id){
  curr_node <- tree_table %>% filter(ID==node_id)
  
  if(curr_node$Feature != 'Leaf'){
    ## when currently at node criteria, go to the 'yes' node
    return(curr_node$Yes)
  }else{
    ## when currently at leaf node, find the parent node with unprocessed 'no' node
    while(!grepl('-0', curr_node$ID)) {
      parent_node <- tree_table %>% filter(Yes == curr_node$ID | No == curr_node$ID)
      if(curr_node$ID == parent_node$Yes){
        return(parent_node$No)
      }else{
        curr_node <- parent_node
        cat('END', '')
      }
    }
  }
}


write.a.tree <- function(tree_table, feature_prefix){
  tree_id <- tree_table$Tree[1]
  node_id <- paste(tree_id, '-0', sep = '')
  prev_node_id <- ''
  indent <<- ''
  
  cat('SELECT', '')
  while(!is.null(node_id)){
    write.node(tree_table, node_id, prev_node_id, feature_prefix)
    prev_node_id <- node_id
    node_id <- get.next.node.id(tree_table, node_id)
    
  }
  cat('INTO TREE_',tree_id, '\n', sep = '')
  cat('FROM DUAL;\n')
}



## convert trees to SQL
convert.xgboost.to.plsql <- function(trees_table, file_output, feature_prefix='FEATURES_VALUE.', base_score=0.5, scale_from=0.5, scale_to=0.5){
  sink(file_output, type="output")
  
  ## Write the declaration
  indent <- ' '
  max_tree_idx <- max(trees_table$Tree)
  cat('DECLARE\n')
  cat(indent, 'BASE_SCORE NUMBER :=', base_score,';\n')
  cat(indent, 'SCALE_FROM_SCORE NUMBER :=', scale_from,';\n')
  cat(indent, 'SCALE_TO_SCORE NUMBER :=', scale_to,';\n')
  cat(indent, 'LOGIT NUMBER;\n')
  cat(indent, 'SKYNET_SCORE NUMBER;\n')
  
  for(i in seq(0, max_tree_idx, 1)){
    if(i%%10==0){
      cat('\n', indent)
    }
    cat('TREE_', i, ' NUMBER; ', sep = '')
  }
  cat('\n\n')
  
  
  ## Write the trees  
  indent <- ' '
  cat('BEGIN\n\n')
  for(i in seq(0, max_tree_idx, 1)){
    tree_table <- trees_table %>% filter(Tree == i)
    write.a.tree(tree_table, feature_prefix)
    cat('\n')
  }
  
  ## Write the score calculation
  indent <- ' '
  cat(indent, 'LOGIT := -ln(1/BASE_SCORE - 1) + ln((1/SCALE_TO_SCORE - 1)/(1/SCALE_FROM_SCORE - 1))\n')
  cat(indent, ' + (TREE_0')
  for(i in seq(1, max_tree_idx, 1)){
    if(i%%10==0){
      cat('\n', indent)
    }
    cat(' + TREE_', i, sep = '')
  }
  cat(');\n', indent)
  cat('SKYNET_SCORE := round(1/(1+exp(LOGIT)), 4);', '\n\n')
  
  cat('END;')
  sink()
}

## How to derive the final rescaled scores:
##    1. intercept = -ln(1/base_score - 1)
##    2. rescal intercept = ln((1/SCALE_TO_SCORE - 1)/(1/SCALE_FROM_SCORE - 1))
##    3. logit = intercept + rescal intercept + sum(leaf values of all trees)
##    4. score = 1/(1+exp(logit))   #Beware the extracted trees gives the probability of GENUINE, Here the formular is the FRAUD Probability 
