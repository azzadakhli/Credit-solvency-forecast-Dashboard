library(shiny)
library(ggplot2)
library(lattice)
library(ggplot2)
library(caret)
library(ROCR)
library(pROC)
library(readxl)
library(aod)
library(plotly)
library(Matrix)
library(glmnet)
library(nnet)
library(shinydashboard)
library(shiny)
#library(questionr)
german_credit_dataset_Projet <- read_excel("C:/Users/azza/Downloads/german credit dataset_Projet(1).xlsx")
names(german_credit_dataset_Projet) <- c("balance_CurrentAccount","credit_Duration","credit_history",
                                         "motive","amount","savings","seniority","personal_status_and_gender",
                                         "guarantor_debtor","Residence_since","age","lodging","Total_credits",
                                         "job","nbr_Persons_dependant","status")
german_credit_dataset_Projet$PurposeEssential <-ifelse(german_credit_dataset_Projet$motive %in% c(6,7,8,9 ), 1, 0)
german_credit_dataset_Projet$status <- ifelse(german_credit_dataset_Projet$status==2, 0, 1)
inTrain <- createDataPartition(y = german_credit_dataset_Projet$status, p = .60, list = FALSE)## avec 60% de data dans la base train
training <- german_credit_dataset_Projet[inTrain,] #60%
testing <- german_credit_dataset_Projet[-inTrain,] #40%
###################Logit#########################"
fit <- glm(status~., family=binomial, data=training)
status_prob_train1 <- predict(fit,training,type='response')
status_pred_logit<-prediction(status_prob_train1, training$status)
prob_test=predict(fit,testing,type = "response")
pred_test <- prediction(prob_test, testing$status)
perf <- performance(status_pred_logit, "mat")
##Calcul des indicateurs
perf_roc_train1 <- performance(status_pred_logit, measure = "tpr", x.measure = "fpr")
auc_train <- performance(status_pred_logit, measure = "auc") 
#Calcul des indicateurs pour la base test 
perf_roc_test <- performance(pred_test, measure = "tpr", x.measure = "fpr") 
auc_test <- performance(pred_test, measure = "auc") 
####
perf_lift1_train <- performance(status_pred_logit, measure = "lift", x.measure = "rpp") #courbe de Lift
perf_lift2_train <- performance(status_pred_logit, measure = "tpr", x.measure = "rpp") #courbe de Lift
####
perf_lift1_test <- performance(pred_test, measure = "lift", x.measure = "rpp") #courbe de Lift
perf_lift2_test <- performance(pred_test, measure = "tpr", x.measure = "rpp")
###
tpr<-perf_roc_test@y.values[[1]]
fpr<-perf_roc_train1@x.values[[1]]
##Test
obj<-tpr-fpr
m<-max(obj,na.rm = T) # na.rm=T: missing values should be removed
seuil <- perf_roc_test@alpha.values[[1]][which.max(obj>=m)]
##Train
corr<-perf@y.values[[1]]
m<-max(corr,na.rm = T)
alpha<-perf@x.values[[1]]
seuil2 <- alpha[which.max(corr>=m)]
logit_results <-ifelse(status_prob_train1>seuil ,1,0)
training$logit_results <-logit_results
mat <- confusionMatrix(data=as.factor(training$logit_results), reference=as.factor(training$status))
###############################nnet#########################
nnet <- nnet(status~.,data=training,family=binomial,size=2)
nnet_train=predict(nnet,training,type = "raw")
nnet_results <- sapply(nnet_train,round,digits=0)
training$nnet_pred<-nnet_results
pred_nn_train <- prediction(nnet_train, training$status) 
perf_roc_nn_train <- performance(pred_nn_train, measure = "tpr", x.measure = "fpr")
perf_auc_nn_train <- performance(pred_nn_train, measure = "auc")
perf_lift2_nn_train <- performance(pred_nn_train, measure = "lift", x.measure = "rpp")
perf_lift1_nn_train <- performance(pred_nn_train, measure = "tpr", x.measure = "rpp")
pred_nn_train <- prediction(nnet_train, training$status) 
perf_roc_nn_train <- performance(pred_nn_train, measure = "tpr", x.measure = "fpr")
perf_auc_nn_train <- performance(pred_nn_train, measure = "auc")
perf_lift2_nn_train <- performance(pred_nn_train, measure = "lift", x.measure = "rpp")
perf_lift1_nn_train <- performance(pred_nn_train, measure = "tpr", x.measure = "rpp")
##Results of logit
logit_results <-ifelse(status_prob_train1>seuil ,1,0)
training$logit_results <-logit_results
##Results of nnet
nnet_results <- sapply(nnet_train,round,digits=0)
training$nnet_pred<-nnet_results
# Define side bar panel
sidebar <- dashboardSidebar(
  tags$head(
    tags$style("body {background-color: black; }")
  ),
  conditionalPanel(condition="input.tabselected==1",hr(),hr(),
                   verbatimTextOutput("t1"), verbatimTextOutput("text_comptes"),hr(),
                   verbatimTextOutput("t5"),verbatimTextOutput("occupation"),hr(),
                   verbatimTextOutput("t3"), verbatimTextOutput("text_statusetgenre"),hr(),
                   verbatimTextOutput("t4"),verbatimTextOutput("logement"),hr(),
                   verbatimTextOutput("t2"),verbatimTextOutput("Text_motives")
                   
  ),
  conditionalPanel(condition="input.tabselected==5",hr(),
                   selectInput("balance_CurrentAccount","Balance du compte",
                               c(sort(factor(training$balance_CurrentAccount))),selected=factor(training$balance_CurrentAccount)[1]),
                   selectInput("credit_Duration","la durée:",
                               c(sort(factor(training$credit_Duration))),selected=factor(training$credit_Duration)[1]),
                   selectInput("credit_history","l'historique:",
                               c(sort(factor(training$credit_history))),selected=factor(training$credit_history)[1]),
                   selectInput("personal_status_and_gender","status personnel/sexe:",
                               c(sort(training$personal_status_and_gender)),selected=training$personal_status_and_gender[1]),
                   selectInput("amount","le montant :",
                               c(sort(training$amount)),selected=1169),
                   sliderInput(inputId = "savings", label = "les réserves", 
                               min = 1, max = 5, value = 1, step = 1),
                   sliderInput(inputId = "seniority", label = "l''ancienneté", 
                               min = 1, max = 5, value = 5, step = 1),
                   sliderInput(inputId = "motive", label = "le motif", 
                               min = 0, max = 9, value = 3, step = 1),
                   sliderInput(inputId = " Residence_since", label = "la période de résidence", 
                               min = 1, max = 4, value = 1, step = 4),
                   
                   selectInput("guarantor_debtor","le guarantor_debtor :",
                               c(sort(training$guarantor_debtor)),selected=training$guarantor_debtor[1]),
                   
                   selectInput("age","Selectionner l''age :",
                               c(sort(training$age)),selected=67),
                   
                   selectInput("lodging","le logement :",
                               c(sort(training$lodging)),selected=training$lodging[1]),
                   
                   selectInput("Total_credits","le total des crédits :",
                               c(sort(training$Total_credits)),selected=training$Total_credits[1]),
                   
                   
                   selectInput("job","le travail :",
                               c(sort(training$job)),selected=training$job[1]),
                   
                   selectInput("nbr_Persons_dependant","le nombre depersonne en charge :",
                               c(sort(training$nbr_Persons_dependant)),selected=training$nbr_Persons_dependant[1]),
                   actionButton("logit", "Logit_prediction", class = "btn-success"),
                   actionButton("nnet", "nnet_prediction", class = "btn-success"),
                   ), width=375
)
header <- dashboardHeader(title="Germany credit dashboard",titleWidth = 450)

body <- dashboardBody(
  width=400,
  fluidRow(
  mainPanel(
    tabsetPanel(
      tabPanel("Analyse univariée", value=1,h3("Etude univariée des variables de la base"),
              
               fluidRow(splitLayout(cellWidths = NULL, plotlyOutput("pie_compte"),plotlyOutput("pie_job"))),
               fluidRow(splitLayout(cellWidths = NULL, plotlyOutput("pie_personal_status_and_gender"),plotlyOutput("pie_lodging"))),
               plotlyOutput("pie_motive"),
               plotOutput("hist_age"),plotOutput("plot_amount")),
      tabPanel("Etude bivariée", value=2,h3("Etude bivariée des variables de la base"),plotlyOutput("age_status"),hr(),
               h4("Testons maintenant l'égalité des moyennes des deux populations :\n",
                  "les ages des personnes dont les crédits sont solvable et les ages des personnes dont\n",
                  " les âges sont non solvables"),
               hr(),
               verbatimTextOutput("test"),
               fluidRow(splitLayout(cellWidths = c("50%","50%"),plotlyOutput("solv"),plotlyOutput("notsolv"))),
               verbatimTextOutput("testNorm"),
              h4("Etudions maintenant les montants des crédits et leurs solvabilité"),
               plotlyOutput("status_amount"),h4("Etudions maintenant de la même façon les deux population des crédits nsolvable et non solvable"),
               h4("Testons maintenant l'égalité des moyennes des montants deux populations :\n",
                  "les montant des crédits solvables et non solvables"),
              verbatimTextOutput("test2"),
               fluidRow(splitLayout(cellWidth=c("50%","50%"),plotlyOutput("solv1"),plotlyOutput("notsolv1"))),
               verbatimTextOutput("testNorm1")),
      tabPanel("Regression Logit", value=3,h4("On effectue une prévision pour la solvabilité des crédits et on crée pour cela deux sous bases : train (60%) et test (40%)\n",
                                               "En premier lieu on effectue une regression linéaire pour la variable status et toutes les autres variables\n",
                                               "Les coefficients du modele de regression de la variable statut"),verbatimTextOutput("intro"),verbatimTextOutput("logit1"),plotOutput("Mathews"),verbatimTextOutput("logit2"),verbatimTextOutput("logit3")
               ,fluidRow(splitLayout(cellWidths = c("50%","50%"),plotOutput("roc1"),plotOutput("lift1"))),
               h4("la courbe lift augmente fortement au-dessus de la ligne de référence, 
                  puis s'aplatit. Dans ce cas, environ 60 % des données représentent environ 90% des vrais positifs pour les bases. Ainsi, 
                  si une banque cible les 20 % de la population indiqués par le modèle, le taux de vrais positifs est 
                  d'environ 30 %. Sans le modèle, le pourcentage correspondant est de 20 %(diagonale). Cette différence est le gain 
                  supplémentaire dû à l'utilisation du modèle."),tableOutput("confusionmat1"),
      ),
      tabPanel("Réseaux de neurones", value=4,hr(),h3("Prévision par la méthode des réseaux de neurones"),hr(),
               fluidRow(splitLayout(cellWidth=c("50%","50%"),plotOutput("roc2"),plotOutput("lift2"))),
               h4("la courbe lift augmente fortement au-dessus de la ligne de référence, 
                  puis s'aplatit. Dans ce cas, environ 57 % des données représentent environ 70% des vrais positifs pour les bases. Ainsi, 
                si une banque cible les 40 % de la population indiqués par le modèle, le taux de vrais positifs est 
                  d'environ 50 %. Sans le modèle, le pourcentage correspondant est de 20 %(diagonale). Cette différence est le gain 
                  supplémentaire dû à l'utilisation du modèle."),
               fluidRow(splitLayout(cellWidth=c("50%","50%"),verbatimTextOutput("ann2"),verbatimTextOutput("ann3")))),
      
      tabPanel("Results", value=5,h3("Résultat de la prévision: "),
               fluidRow(splitLayout(cellWidths = c("50%","50%"),h4("Logit Résultat"),verbatimTextOutput("nnet_result"))),
               fluidRow(splitLayout(cellWidths = c("50%","50%"),h4("NNET résultat"),verbatimTextOutput("logit_result")))
               
               
               
      ),
      
      id = "tabselected"
    )
  ))
)

ui <- dashboardPage(
  header, sidebar, body,skin="yellow")

server <- function(input, output) {
  output$t1 <- renderText({
    paste("Les modalités des comptes")
  })
  output$text_comptes <- renderText ({
    paste("1: inférieur à 0\n",
          "2: entre 0 et 200\n",
          "3: supérieur à 200\n",
          "4: Pas de compte courant\n")
  })
  output$t2 <- renderText({
    paste("Les modalités des motifs")
  })
  output$Text_motives <- renderText ({
    paste("1: Voiture nouvelle\n",
          "2: Voiture d'occasion\n",
          "3: fourniture/equipement\n",
          "4: Télévision\n",
          "5: Appareil electroménager\n",
          "6: Réparation\n",
          "7: Education\n",
          "8: Recyclage\n",
          "9: Projet\n",
          "10: Autres\n")
  })
  output$t3 <- renderText({
    paste("Les modalités des status et du genre")
  })
  output$text_statusetgenre <- renderText({
    paste("1:Homme :Divorcé\n",
          "2:Femme :Divorcée/Mariée\n",
          "3:Homme :Célibataire \n",
          "4:Homme : Marié/Veuf\n",
          "5:Femme :Célibataire")
    
  })
  output$t4 <- renderText({
    paste("Les modalités des logements")
  })
  output$logement <- renderText({
    paste("1: Locataire\n",
          "2: Prpriétaire\n",
          "3: Gratuitement")
  })
  output$t5 <- renderText({
    paste("Les modalités de l'occupations")
  })
  output$occupation<- renderText({
    paste("1:En chomage\n",
          "2:Inférieur à 1ans\n",
          "3:Entre 1 ans et 4ans\n",
          "4:Entre 4ans et 7ans\n",
          "5:Inférieur à 7ans")
  })
  output$hist_age <- renderPlot( {
    ggplot(german_credit_dataset_Projet, aes(x = age))+geom_histogram(aes(y = ..density..), 
                                                                      colour="black", fill="white") +
      ggtitle("Les âges")+
      geom_density(alpha = 0.2, fill = "#FF6666")
    })
  output$plot_amount <- renderPlot({
    ggplot(german_credit_dataset_Projet, aes(x = amount))+
      geom_density(alpha = 0.2, fill = "#FF6666")+ylab("Density")+ ggtitle("Les montants")
  })
  output$pie_compte <- renderPlotly({
    p<- plot_ly(german_credit_dataset_Projet , labels = ~factor(balance_CurrentAccount), values = ~balance_CurrentAccount, type = 'pie')%>%
      
      layout(title = 'les comptes courants', plot_bgcolor = "#e5ecf6")
    ggplotly(p)
    
  })
  output$pie_motive <- renderPlotly({
        p<- plot_ly(german_credit_dataset_Projet,x=~motive, y=~motive, type = 'bar')%>%
          
          layout(title = 'Les  motifs', plot_bgcolor = "#e5ecf6")
        ggplotly(p)

    })
  output$pie_personal_status_and_gender <- renderPlotly({
    p<- plot_ly(german_credit_dataset_Projet , labels = ~factor(personal_status_and_gender), values = ~personal_status_and_gender, type = 'pie')%>%
      
      layout(title = 'Les status personnel et sexe' , plot_bgcolor = "#e5ecf6")
    ggplotly(p)
    
  })
  output$pie_lodging  <- renderPlotly({
    p<- plot_ly(german_credit_dataset_Projet , labels = ~factor(lodging), values = ~lodging, type = 'pie')%>%
      
      layout(title = 'les logements', plot_bgcolor = "#e5ecf6")
    ggplotly(p)
    
  })
  output$pie_job <- renderPlotly({
    p<- plot_ly(german_credit_dataset_Projet , labels = ~factor(job), values = ~job, type = 'pie')%>%
      
      layout(title = 'Les occupations', plot_bgcolor = "#e5ecf6")
    ggplotly(p)
    
  })
  ##bivariée
  output$age_status <-renderPlotly({
    p<- plot_ly(data = german_credit_dataset_Projet,y = ~age, x = ~status,type = "box")%>%
      
      layout(title = 'Les status par Age', plot_bgcolor = "#e5ecf6")
    ggplotly(p)
    
  })
  output$test <- renderText({
    paste(
      ##t.test(german_credit_dataset_Projet$age,german_credit_dataset_Projet$status),
          "Welch Two Sample t-test\n",
          
          "data:  german_credit_dataset_Projet$age and german_credit_dataset_Projet$status\n",
          "t = 96.79\t, df = 1002.2\t, p-value < 2.2e-16\t\n",
          "alternative hypothesis: true difference in means is not equal to 0\n",
          "95 percent confidence interval:\n",
            "34.13953 35.55247\n",
          "sample estimates:",
            "mean of x =   35.546 \n",
            "mean of y =  0.700" )
  })
  output$notsolv <- renderPlotly({
    non_solv <- subset(german_credit_dataset_Projet, status == 1)
    p<-plot_ly(non_solv,  y=~age, type = 'bar')%>%
      
      layout(title = 'Les crédits non solvables', plot_bgcolor = "#e5ecf6")
    ggplotly(p)
  })
  output$solv <- renderPlotly({
    solv <- subset(german_credit_dataset_Projet, status == 0)
    p<-plot_ly(solv,y=~age, type = 'bar')%>%
      
      layout(title = 'Les crédits solvables', plot_bgcolor = "#e5ecf6")
    ggplotly(p)
  })
  output$testNorm <- renderText({
    solv <- subset(german_credit_dataset_Projet, status == 0)
    non_solv <- subset(german_credit_dataset_Projet, status == 1)
    paste("Etudions maintenant la normalité des âges\n",
          "Commençons par les individus dont les crédits sont solvables\n",
          "Statistique= ",shapiro.test(solv$amount)[1],"\n",
          "la p-value du shapiro test est ",shapiro.test(solv$age)[2],"\n",
          "Et pour les ages des individus dont les crédits ne sont pas solvable\n",
          "Statistique= ",shapiro.test(solv$amount)[1],"\n",
          "la p-value du shapiro test est ",shapiro.test(non_solv$age)[2], 
          "\n","On remarque que la p-value est inférieure à 0.05 dans les deux cas donc on rejette\n
          l'hypothèse nulle de normalité des distribution")
    
  })

  output$status_amount <- renderPlotly({
    p<- plot_ly( data = german_credit_dataset_Projet,y = ~amount, x = ~status, type = "box" )%>%
      
      layout(title = 'les montants et les status', plot_bgcolor = "#e5ecf6")
    ggplotly(p)
  })
  
  output$test2 <- renderText({
  
    paste(
      #t.test(german_credit_dataset_Projet$amount,german_credit_dataset_Projet$status)

      "Welch Two Sample t-test\n",
          
          "data:  german_credit_dataset_Projet$amount and german_credit_dataset_Projet$status\n",
          "t = 36.64,\t df = 999\t, p-value < 2.2e-16\t\n",
          "alternative hypothesis: true difference in means is not equal to 0\n",
          "95 percent confidence interval:\n",
            "3095.394\t 3445.722",
          "sample estimates:\n",
            "mean of x = 3271.258\n", 
          "mean of y = 0.700")
  })
  output$notsolv1 <- renderPlotly({
    non_solv1 <- subset(german_credit_dataset_Projet, status == 1)
    p<-plot_ly(non_solv1,y=~amount, type = 'bar')%>%
      
      layout(title = 'Les crédits solvables', plot_bgcolor = "#e5ecf6")
    ggplotly(p)
  })
  output$solv1 <- renderPlotly({
    solv1 <- subset(german_credit_dataset_Projet, status == 0)
    p<-plot_ly(solv1,y=~amount, type = 'bar')%>%
      
      layout(title = 'Les crédits solvables', plot_bgcolor = "#e5ecf6")
    ggplotly(p)
  })
  output$testNorm1 <- renderText({
    solv1 <- subset(german_credit_dataset_Projet, status == 0)
    non_solv1 <- subset(german_credit_dataset_Projet, status == 1)
    paste("Etudions maintenant la normalité des âges\n",
          "Commençons par les individus dont les crédits sont solvables\n",
          "Statistique= ",shapiro.test(solv1$amount)[1],"\n",
          "la p-value du shapiro test est ",shapiro.test(solv1$amount)[2],"\n",
          "Et pour les ages des individus dont les crédits ne sont pas solvable\n",
          "Statistique= ",shapiro.test(non_solv1$amount)[1],"\n",
          "la p-value du shapiro test est ",shapiro.test(non_solv1$amount)[2], 
          "\n","On remarque que la p-value est inférieure à 0.05 dans les deux cas donc on rejette \n",
          "l'hypothèse nulle de normalité des distribution")
    
  })
  ##################Logit############
  output$intro <- renderText({
    inTrain <- createDataPartition(y = german_credit_dataset_Projet$status, p = .60, list = FALSE)## avec 60% de data dans la base train
    training <- german_credit_dataset_Projet[inTrain,] #60%
    testing <- german_credit_dataset_Projet[-inTrain,] #40%
    fit <- glm(status~., family=binomial, data=training)
    f <- function(s){
      ch <- ""
      for (i in 2:length(s)) {
       
        ch <- paste(ch,names(s)[i],":",round(s[i],digits=3),"\n")
      }
      return(ch)
    }
   
      paste(f(fit$coefficients))
  })
  
  output$logit1 <- renderText({
    paste("Nous allons maintenant effectuer l'étape de prévision des deux bases test et train\n .Voyons les seuil optimaux\n et commençons par la courbe de Mathews:Une courbe qui nous donne une idée sur le seuil optimal")
  })
  output$Mathews <- renderPlot({
  plot(perf,main="Coeffcicient de correlation de Matthews",xlab="Seuil",ylab="Correlation")
  abline(v=seuil)
  })
  output$logit2 <- renderText({
    ##Test
    obj<-tpr-fpr
    m<-max(obj,na.rm = T) # na.rm=T: missing values should be removed
    seuil <- perf_roc_test@alpha.values[[1]][which.max(obj>=m)]
    ##Train
    corr<-perf@y.values[[1]]
    m<-max(corr,na.rm = T)
    alpha<-perf@x.values[[1]]
    seuil2 <- alpha[which.max(corr>=m)]
    print(paste("Le seuil optimal est = ",round(seuil2,digits = 3)))
    paste("Le seuil optimal est pour la base Test = ",round(seuil,digits = 3),"\nLe seuil optimal est pour la base Train = ",round(seuil2,digits = 3))
  })
  output$confusionmat1 <- renderTable({
    logit_results <-ifelse(status_prob_train1>seuil ,1,0)
    training$logit_results <-logit_results
    mat <- confusionMatrix(data=as.factor(training$logit_results), reference=as.factor(training$status))
    mat$table
  })
  output$logit3 <- renderText({
    paste("Perfomance:")
  })
  output$roc1 <- renderPlot({
    plot(perf_roc_train1, col="blue", main="Courbe ROC", xlab="1-Specificity (false positive rate)", ylab="Sensibility (true positive rate)",
         bg="white",cex.main=2,cex.lab=1,print.cutoffs.at=seq(0,1,by=0.1),lwd=3) 
    
    abline(0, 1,col="green",lty=3) #rajout d'une premi?re bisectrice
    
    #rajout de la courbe ROC pour la base test
    lines(perf_roc_test@x.values[[1]],perf_roc_test@y.values[[1]],col="red",lwd=2) 
    text(1,.05,labels=paste("__ train, AUC = ",round(auc_train@y.values[[1]],digits=3),sep=""),adj=1,col = "blue")
    text(1,.15,labels=paste("__ test,  AUC = ",round(auc_test@y.values[[1]],digits=3),sep=""),adj=1,col = "red")
    
  })
  output$lift1 <- renderPlot({
    plot(perf_lift1_train, col="blue", main="Courbe de Lift", xlab="RPP", ylab="Sensibilit? (tpr)",
         bg="white",cex.main=1,cex.lab=1,lwd=3) 
    
    lines(perf_lift1_test@x.values[[1]],perf_lift1_test@y.values[[1]],col="red",lwd=2) 
    text(1,.25,labels="__ train",adj=1,col = "blue")
    text(1,.15,labels="__ test",adj=1,col = "red")
    #mettre les deux de Lift ensemble
    plot(perf_lift2_train, col="blue", main="Courbe de Lift", xlab="RPP", ylab="Sensibilit? (tpr)",
         bg="white",cex.main=1,cex.lab=1,lwd=3) 
    
    lines(perf_lift2_test@x.values[[1]],perf_lift2_test@y.values[[1]],col="red",lwd=2) 
    text(1,.25,labels="__ train",adj=1,col = "blue")
    text(1,.15,labels="__ test",adj=1,col = "red")
    segments(0,0,1,1)
    #interpret:la courbe lift augmente fortement au-dessus de la ligne de référence, 
    ##puis s'aplatit. Dans ce cas, environ 60 % des données représentent environ 90% des vrais positifs pour les bases. Ainsi, 
    ##si une banque cible les 20 % de la population indiqués par le modèle, le taux de vrais positifs est 
    ##d'environ 30 %. Sans le modèle, le pourcentage correspondant est de 20 %(diagonale). Cette différence est le gain 
    ##supplémentaire dû à l'utilisation du modèle.
    
  })
 
  output$roc2 <- renderPlot({
    pred_nn_train <- prediction(nnet_train, training$status) 
    perf_roc_nn_train <- performance(pred_nn_train, measure = "tpr", x.measure = "fpr")
    perf_auc_nn_train <- performance(pred_nn_train, measure = "auc")
    perf_lift2_nn_train <- performance(pred_nn_train, measure = "lift", x.measure = "rpp")
    perf_lift1_nn_train <- performance(pred_nn_train, measure = "tpr", x.measure = "rpp")
    #Courbe de ROC
    plot(perf_roc_nn_train,main="Courbe ROC Reseau de neurones",col="blue")
    text(0.5,.7,paste("AUC - NN = ",round(perf_auc_nn_train@y.values[[1]],3)),col="blue",cex=0.75)
    segments(0,0,1,1,lty=3,col="green")
  })
  output$ann2 <- renderText({
    paste("La courbe de ROC montre que le seuil est de 0.5 , et que la méthode de réseau de neurone est meilleure")
  })
  output$lift2 <- renderPlot({
    pred_nn_train <- prediction(nnet_train, training$status) 
    perf_roc_nn_train <- performance(pred_nn_train, measure = "tpr", x.measure = "fpr")
    perf_auc_nn_train <- performance(pred_nn_train, measure = "auc")
    perf_lift2_nn_train <- performance(pred_nn_train, measure = "lift", x.measure = "rpp")
    perf_lift1_nn_train <- performance(pred_nn_train, measure = "tpr", x.measure = "rpp")
    
    #Courbe de Lift
    plot(perf_lift2_nn_train,main="Courbe de Lift Reseau de neur?nes")
    plot(perf_lift1_nn_train,main="Courbe de Lift Reseau de neur?nes",col="blue",grid=TRUE)
    segments(0,0,1,1,lty=3)
    abline(h=.69,col="red",lty=3)
    text(0.15,0.75, "TPR = 0.69",col=250)
    abline(v=0.4)
    ##interpret:la courbe lift augmente fortement au-dessus de la ligne de référence, 
    ##puis s'aplatit. Dans ce cas, environ 57 % des données représentent environ 70% des vrais positifs pour les bases. Ainsi, 
    ##si une banque cible les 40 % de la population indiqués par le modèle, le taux de vrais positifs est 
    ##d'environ 50 %. Sans le modèle, le pourcentage correspondant est de 20 %(diagonale). Cette différence est le gain 
    ##supplémentaire dû à l'utilisation du modèle.
  })
  output$ann3 <- renderText({
    paste("Dans ce cas, environ 57 % des données représentent environ 70% des vrais positifs pour les bases")
  })
  output$logit_result <- renderText({
    if(input$goButton){
      if(!is.null(reactiveValues())){
        df <- as.data.frame(training)
        col <- subset(df,age==input$age)
        r<-training$nnet_pred[ col$balance_CurrentAccount==input$balance_CurrentAccount&
                                     col$credit_Duration==input$credit_Duration&
                                     col$credit_history==input$credit_history&
                                     col$motive==input$motive&
                                     col$amount==input$amount&
                                     col$savings==input$savings&
                                     col$seniority==input$seniority&
                                     col$personal_status_and_gender==input$personal_status_and_gender&
                                     col$guarantor_debtor==input$guarantor_debtor&
                                     col$Residence_since==input$Residence_since&
                                     col$age==input$age&
                                     col$lodging==input$logding&
                                     col$Total_credits==input$Total_credits&
                                     col$job==input$job&
                                     col$nbr_Persons_dependant==input$nbr_Persons_dependant]
        paste(r)}
      else paste("Remplissez tous les champs!")}     
  })
  
  

  output$logit_result <- renderText({
    r=" "
    if(input$goButton){
        df <- as.data.frame(training)
        col <- subset(df,age==input$age)
      r<-training$logit_results[ col$balance_CurrentAccount==input$balance_CurrentAccount&
                                  col$credit_Duration==input$credit_Duration&
                                  col$credit_history==input$credit_history&
                                  col$motive==input$motive&
                                  col$amount==input$amount&
                                  col$savings==input$savings&
                                  col$seniority==input$seniority&
                                   col$personal_status_and_gender==input$personal_status_and_gender&
                                   col$guarantor_debtor==input$guarantor_debtor&
                                   col$Residence_since==input$Residence_since&
                                   col$age==input$age&
                                   col$lodging==input$logding&
                                   col$Total_credits==input$Total_credits&
                                   col$job==input$job&
                                   col$nbr_Persons_dependant==input$nbr_Persons_dependant]
    }
    paste(r)
    })
 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
