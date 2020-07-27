# function for creating item responses using 2PL ####

# paramters: formlength, n (sample size), 
# a (item disc.), b (item difficulty), theta (person ability)
bi.irt<-function(formlength, n, a, b, theta){
  require(dplyr, lib.loc=NULL)
  # create label for items: item1-item n
  item<-rep("item", formlength)
  for (i in 1:formlength){
    item[i]<-paste(item[i], i, sep="")
  }
  # item parameters rounded to 2 demical place
  item_para<-as.data.frame(cbind(item, round(a, 4), round(b, 4)))
  
  # create examinee uniqu IDs
  id<-seq(1, n, by =1)
  
  # sample P
  # Note: only as dataframe works for the probj calculuation
  df_theta<-as.data.frame(cbind(id, theta))
  df<-df_theta
  
  # calculate probability of getting an item correct using 2PL
  # formula: 1/(1+exp(-(a[j]*theta-b[j])))
  # j is the jth item in the test form. 
  
  for (j in 1:formlength){
    # calculate probj (probability of getting an item correct)
    df[[paste0("probj", j)]] = 1/(1+exp(-(a[j]*theta-b[j])))
    # create item responses based on probj
    df[[paste0("item", j)]]<-ifelse (df[[paste0("probj", j)]]> 0.5,1, 0 )
  }
  
  # create a total score
  df2<-df%>% 
    mutate(total=select(df, contains("item")) %>% rowSums())
  # 
  # # # verifying item1
  # probj1_check=1/(1+exp(-(a[1]*df$theta-b[1])))
  # probj60_check=1/(1+exp(-(a[60]*df$theta-b[60])))
  # # printing results of the first 25 rows
  # checkprobj1<-cbind(df[c("probj1")], probj1_check, df[c("probj60")], probj60_check)[1:25,]
  # # correct
  # 
  # # # verifying item responses
  # item30_check<-ifelse(df$probj30>0.5, 1, 0)
  # item59_check<-ifelse(df$probj59>0.5, 1, 0)
  # checkitem1<-cbind(df$item30, item30_check, df$item59, item59_check)[1:25, ]
  # # correct
  
  # get the item probability
  item_prob<-select(df2, id, theta, contains("probj"))
  #
  # # get the item responses with total score
  item_responses<-select(df2, id, theta, contains("item"), total)
  return(list(item_responses=item_responses, theta=df_theta, item_para=item_para, item_prob=item_prob, all=df2))
}
# list of outputs: item_responses, theta, item_para, item_prob 

# end of function ####
