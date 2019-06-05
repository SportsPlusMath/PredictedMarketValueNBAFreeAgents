
#Predicted Market Value For NBA Players#

library(car)
library(tidyverse)
library(plotly)
library(data.table)
library(MASS)
library(caret)
library(glmulti)


#Renaming Data
Salary<-Contract_Season_Stats

##Splitting Dataset to train
set.seed(1)
row.number <- sample(1:nrow(Salary), 0.8*nrow(Salary))
train = Salary[row.number,]
test = Salary[-row.number,]
dim(train)
dim(test)

#Checking for collinearity between variables 
#Creating dataset with just numeric predictors so it doesnt cause errors within in R, also removing everything but predictors
x <- train[,3:57]
x$Tm <- x$Pos <- x$SalaryCapFollowingYear <- x$ContractYearandSigned<- x$SalaryFollowingYear<- x$AllNBATeam <- x$AllNBADefensiveTeam <- NULL

#Looking for collinearity
omcdiag(x,x$CapHitpctFollowingYear)
imcdiag(x,x$CapHitpctFollowingYear)

#Removing collinearity by using VIF
#Github/Fawda123 has created a VIF function to help see which variables to remove for collinearity 
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  library(fmsb)
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
}


#Using vif_function to find varaibles cause collinearity issues
vif_func(in_frame=x,thresh=20,trace=T)

#Removing variables from model to help with collinearity issues
x$CapHitpctFollowingYear <- NULL
keep.dat<-vif_func(in_frame=x,thresh=10,trace=F)
form.in<-paste('CapHitpctFollowingYear ~ AllNBATeam + PER +',paste(keep.dat,collapse='+'))
mod1<-lm(form.in,data=train)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1)


#Using GLMULTI function to find best model
prednames <- c("Age","G","MPG","ThreePAr",
               "FTr","DRBpct","TRBpct","ASTpct","STLpct","TOVpct","USGpct","OWS","DWS","DBPM","TSpct","AllNBATeam","PER","eFGpct","PPG")
g1 <- glmulti("CapHitpctFollowingYear",xr=prednames,data=train,level=1)
print(g1)
summary(g1@objects[[1]])
plot(g1, type="s")


step.model=lm(formula = CapHitpctFollowingYear ~ Age+DWS+PPG+USGpct+OWS+AllNBATeam , 
              data = train)
summary(step.model)
#Predicting on test data set
test$prediction <- predict(step.model,test)

#Seeing how well we did
RMSE(test$CapHitpctFollowingYear,test$prediction)
MAE(test$CapHitpctFollowingYear,test$prediction)

#Predicting Upcoming Season
PMV <-subset(X2018_2019_NBA_Season_Stats, FreeAgent=='Yes')
PMV$PMV <- predict(step.model,PMV)
Salary$PMV <- predict(step.model,Salary)

#Interactive Plot showing PMV

plot_ly(data = PMV, x = ~PMV, y = ~Age+DWS+PPG+USGpct+OWS, color = ~PMV,colors=c("orange","red"),hoverinfo="text",
        text = ~paste("Name: ", Player,
                      "<br> PMV: ",round(PMV*100,digits=2),"%"))%>%layout(paper_bgcolor='#4666d1',
                                                                          plot_bgcolor='#4666d1',title = "Predicted Market Value for All NBA 2019 Free Agents" , xaxis = list(title="Predicted Market Value (PMV)",color="#ffffff",tickangle = -40,tickformat = "%"),yaxis=list(title="",showticklabels = FALSE),titlefont= list(family = "Agency FB", size = 45,  color = '#ffffff'), font = list(
                                                                            family = "Agency FB",
                                                                            size = 25),margin = 10)%>%hide_colorbar()