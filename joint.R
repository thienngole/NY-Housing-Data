joint <- function(df){
  m = matrix(0, ncol = ncol(df), nrow=ncol(df))
  for(i in 1:ncol(df)){
   for(j in 1:ncol(df)){
     m[i,j] <- sum(df[,i] == "Yes" & df[,j]=="Yes",na.rm = T) 
   } 
  }
  colnames(m) <- colnames(df)
  rownames(m) <- colnames(df)
  return(m)
}
joint(df)
df$a <- 1:3

df %>% select(-one_of("a")) %>% joint()
data %>% select(contains("Condition")) %>%
  select(-one_of("Condition Of Building")) %>%
  joint() %>% write.csv("cond_joint_table")
