###This function helps identify the lowest lower burst size or
###the maximum upper burst size (a host may have multiple
###parasite species exploiting it).


Identifier_Burst_Size_Order <- function(merged_order_df){

  merged_order_Upper <-  by(merged_order_df, merged_order_df$Species, 
                                 function (x) max(na.omit(x$Upper)), 
                                 simplify = FALSE)

  merged_order_Lower <-  by(merged_order_df, merged_order_df$Species, 
                                  function (x) min(na.omit(x$Lower)), 
                                  simplify = FALSE)

  merged_order_df_FULL_Upper <- data.frame(Upper = do.call(rbind,merged_order_Upper))
  merged_order_df_FULL_Lower <- data.frame(Lower = do.call(rbind,merged_order_Lower))
  
  merged_order_df_FULL_Upper$Species <- rownames(merged_order_df_FULL_Upper)
  merged_order_df_FULL_Upper <- merged_order_df_FULL_Upper[is.finite(merged_order_df_FULL_Upper$Upper),]

  merged_order_df_FULL_Lower$Species <- rownames(merged_order_df_FULL_Lower)
  merged_order_df_FULL_Lower <- merged_order_df_FULL_Lower[is.finite(merged_order_df_FULL_Lower$Lower),]

  return(list( merged_order_df_FULL_Lower, merged_order_df_FULL_Upper))
}
  