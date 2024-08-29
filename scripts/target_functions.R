sim_query <- function(model, restrictions=NULL, n_sim=100, n_iter=4000) {
  model <- make_model(model) 
  if(!is.null(restrictions)){
    model <- model %>% 
      set_restrictions(restrictions)
  }
  
  lista_df <- list()
  data_dif <- data.frame(X = c(0,1), W = c(1,1), Y = c(0,1))
 
  for ( i in 1:n_sim) {
    my_df <- do.call(rbind, replicate(i, data_dif, simplify = FALSE))
      
    df_posterior <- model %>%
      update_model(my_df, iter  = n_iter)
    
    lista_df[[i]] <- df_posterior
  }
  query_result_prior <- numeric()
  query_result_posterior <- numeric()
  
  
  for ( i in 1:n_sim) {
    query_result <- query_model(
      lista_df[[i]], 
      query = "Y[X=1]> Y[X=0]",
      using = c("priors", "posteriors"),
      expand_grid = TRUE)
    
    query_result_prior[i] <- query_result$mean[1]
    query_result_posterior[i] <- query_result$mean[2]
  }
  df_plot <- data.frame(posterior = query_result_posterior,
                        n = seq(from=2, to=200, by=2))
  return(df_plot)
}
