shinyApp(
    ui = tagList(
        shinythemes::themeSelector(),# <---- this is pop up to display different theme options 
        navbarPage(
            # theme = "cerulean",    
            "CA ONE",
            tabPanel("Linear Regression ",                     #<-------- Title of app
                     sidebarPanel( 
                         
                         
                         selectInput("ds", "Data Source :",    #<---------- ds is the variable use to store data from file or in build data set of R 
                                     
                                     c("File" = "file",  #<------- it will take dataset fro file and store in "File" variable
                                       
                                       "In-Build" = "ib" #<----------- it will use the R studio dataset and store it in "In-Build" variable
                                       
                                     )), 
                         
                         conditionalPanel(     #<------ this defines the functionality of selected Data Source 
                             
                             condition = "input.ds == 'file'",  #<-------- It ill work if the user select the data set from the file
                             
                             fileInput("datafile", "Choose CSV File",  #<--- it will promt the user to insert CV file and store it in datafile
                                       
                                       multiple = FALSE,  #<--------- it will check for multiplicity whih is set false.
                                       
                                       accept = c("text/csv",  #<---------- it will check for the format of file that is uploaded
                                                  
                                                  "text/comma-separated-values,text/plain", 
                                                  
                                                  ".csv")) 
                             
                         ), 
                         
                         
                         conditionalPanel(     
                             
                             condition = "input.ds == 'ib'", 
                             
                             selectInput(inputId = "ib", label = "Select a DataSet", choices = ls("package:datasets"))  #<-it will use the in build dataset by package::datasets and list it out using ls finction 
                             
                         ), 
                         
                         
                         
                         selectInput(inputId = "tarvar", label = "Select a Target Variable", choices = ""), #<--- it will allow the user to set target variable and it will not allow user to have multiple target variable at a given time
                         
                         
                         
                         selectInput(inputId = "indvar", label = "Select Independent Variables", multiple = TRUE, choices = ""), #<--- this funtion will allow the user to enter mutilple variable 
                         
                         #this is a slider input with range 1 to 100 and default value is 45
                         
                         sliderInput("ratio", "Ratio for trainset", min = 1, max = 100, value = 45 ), 
                         
                         uiOutput("Input_Ind") 
                         
                         
                         
                         
                         
                     ),
                     mainPanel(                  #<---- this is the main funtion 
                         tabsetPanel(type = "tabs", #<---- this the defination of the various tabs
                                     
                                     tabPanel("Data Set", DT::dataTableOutput("extdata")), #<-- it will give the table of the dataset of selected file or data
                                     
                                     tabPanel("Selected ", DT::dataTableOutput("selData")), #<--- it will display the table of selected coloum i.e. target and independent variable
                                     
                                     tabPanel("Test/Predicted", plotOutput("glmperf")), #<--- it will give the graphical representation of real vs predectied value 
                                     
                                     tabPanel("RMSE", DT::dataTableOutput("RMSE")), #<--- this wil show the rmse value
                                     
                                     tabPanel("SVM", DT::dataTableOutput("SVM"))
                                     
                         )
                     )
            ),
            
            ##############################################################################
            # CONTINOUS MODEL
            
            tabPanel("Continous", sidebarPanel( 
                
                selectInput("conmodel", "Select Model",  #<----THREE continous model options
                            
                            choices = c("Normal" = "normal", 
                                        
                                        "Exponential" = "exponential", 
                                        
                                        "Uniform" = "uniform"), 
                            
                            selected = "exponential" #<--exponential is selected by default
                            
                ), 
                
                
                # slider input with range from 1 to 100 and 35 as a set value
                sliderInput("s", "number of simulated data" ,min=1, max=1000, value = 35), 
                
                
                # condition check 
                # for exponential model lamda value
                conditionalPanel(     
                    
                    condition = "input.conmodel == 'exponential'", 
                    
                    numericInput("lam", "parameter lambda in exponential" ,min=0, max=10, value = 1)  
                    
                ), 
                
                # condition check 
                # for normal model mu and sigma value with set value 0 and 1 respectively
                
                conditionalPanel( 
                    
                    condition = "input.conmodel == 'normal'", 
                    
                    numericInput("mu", "parameter mu in Normal" , value = 0),  
                    
                    numericInput("sigma", "parameter sigma in Normal" , value = 1) 
                    
                ), 
                
                
                
                numericInput("i", "support" , value = 2), 
                
                
                
                # j1 value set to ZERO for normal model
                
                conditionalPanel(     
                    
                    condition = "input.conmodel == 'normal'", 
                    
                    numericInput("j1", "j in Normal" , value = 0) 
                    
                    
                    
                ), 
                
                # j2 value set to Zero for exponential
                
                conditionalPanel(     
                    
                    condition = "input.conmodel == 'exponential'", 
                    
                    numericInput("j2", "j in exponential" , value = 0) 
                    
                ), 
                
                ## condition check 
                # for uniform model with vaue of a and b as -1 and 0.5 respectively 
                
                conditionalPanel( 
                    
                    condition = "input.conmodel == 'uniform'", 
                    
                    numericInput("a", "parameter a in Normal" , value = -1),  
                    
                    numericInput("b", "parameter b in Normal" , value = 0.5) 
                    
                ) 
                
                
            ),  
            ###### Main function for Continous model
            mainPanel(  
                
                plotOutput("histogram1"),  
                
                tableOutput('tab1'), 
                
                tableOutput('prob')  
                
            )),
            
            
            
            ##########################################################
            
            # K-MEANS
            
            
            tabPanel("K-means",
                     sidebarLayout(
                         sidebarPanel(
                             h3("Filtering data"),
                             # options for selection of data set
                             selectInput("dataset", "Choose a dataset (or a subset) :", 
                                         choices = c("all iris data", "setosa", "versicolor", "virginica")),
                             # option for selecion of X Variable
                             selectInput("Xvar", "X variable", 
                                         choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
                             # option for selecion of y Variable
                             selectInput("Yvar", "Y variable", 
                                         choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), selected = "Sepal.Width"),
                             
                             # Number of observatio to view pannel
                             
                             numericInput("obs", "Number of observations to view on table:", 10),
                             h3("K-Means"),
                             # cluster count pannel from 1 to 9
                             numericInput("clusters", "Cluster count", 3, min = 1, max = 9),
                             h3("DBSCAN"),
                             
                             
                             #Epsilon
                             
                             sliderInput("eps", "Radius of neighborhood of each point", min = 0.0, max = 1.0, value = 0.2),
                             sliderInput("minPoints", "Number of neighbors within the eps radius", min = 0, max = 10, value = 3)
                             
                         ),
                         
                         # MainPanel divided into many tabPanel
                         mainPanel(
                             tabsetPanel( 
                                 ############### DIffrent tabs for diffrent operation 
                                 tabPanel("Plot", h1("Scatterplot"), plotOutput("simplePlot"), h1("Boxplot"), plotOutput("boxPlot")),
                                 tabPanel("Descriptive statistics", h1("Descriptive statistics"),verbatimTextOutput("summary")),
                                 tabPanel("Table", h1("Table"), textOutput("NbRows"), tableOutput("view")),
                                 tabPanel("Clustering", h1("K-Means"), textOutput("NbClust"), plotOutput("kmeansPlot"), 
                                          h1("Density-based cluster (DBSCAN)"), textOutput("dbscan_Param"), plotOutput("dbscanPlot"),
                                          h1("Decision tree"), plotOutput("treePlot"))
                             ) 
                         )
                     )
                     
                     
            ),
            tabPanel("Discreate Analysis", #<-- This will reate another tab for Discreate Analysis
                     sidebarPanel( 
                         
                         selectInput("dismodel", "Select Model", #<--- it wil give user options among various discrete model 
                                     
                                     choices = c("Binomial" = "binomial", #<-- selected option will be store in choice variable
                                                 
                                                 "Poisson" = "poisson",
                                                 
                                                 "Bernoulli" = "bernoulli",
                                                 
                                                 "Hypergeometric" = "hypergeometric",
                                                 
                                                 "Geometric" = "geometric"), 
                                     
                                     selected = "poisson"#<-- by default we have selected to poission model  
                                     
                         ), 
                         
                         conditionalPanel(  #<- function for binomial model
                             
                             condition = "input.dismodel == 'binomial'", 
                             
                             numericInput("n", "parameter n in Binomial" , value = 10), #<- we have set value to 10
                             
                             numericInput("p", "parameter p in Binomial" , value = 1) #<- we have set value to 1
                             
                         ), 
                         
                         conditionalPanel(
                             condition = "input.dismodel == 'bernoulli'",
                             # Slider input for the probability of successful trail
                             sliderInput("p", "Probability of successful trail(p)", min=0, max=1, step = 0.01, value = 0.5)
                         ),
                         
                         conditionalPanel(
                             condition = "input.dismodel == 'hypergeometric'",
                             numericInput("m", "M" , value = 10),
                             numericInput("n", "N" , value = 20),
                             numericInput("k", "K" , value = 5)
                         ),
                         
                         conditionalPanel(     #<- condtion check for POISSION DATASET
                             
                             condition = "input.dismodel == 'poisson'", 
                             
                             numericInput("lam", "parameter lambda in Poisson" , value = 1)  #<- we have set value of lambda to 1 by-defult
                             
                         ), 
                         
                         
                         
                         conditionalPanel(     #<- condition Check for Geomertic model
                             
                             condition = "input.dismodel == 'geometric'", 
                             
                             numericInput("p", "parameter p in Geometric" , value = 0.5) #<- we have set value of p to 0.5 by-defult
                             
                         ), 
                         
                         
                         
                         numericInput("max", "upper limit for x" , value = 5),  #<- upper limit is set 5
                         
                         sliderInput("s", "number of simulated data" ,min=1, max=1000, value = 45),  #<- Slider with range of 1 to 100 with set value as 45
                         
                         # j1 value set to one for binomial 
                         
                         conditionalPanel( 
                             
                             condition = "input.dismodel == 'binomial'", 
                             
                             numericInput("j1", "j for Bin" , value = 1) 
                             
                         ), 
                         
                         # j2 value set to one for poisson
                         
                         conditionalPanel( 
                             
                             condition = "input.dismodel == 'poisson'", 
                             
                             numericInput("j2", "j for Poisson" , value = 1) 
                             
                         ), 
                         
                         # j3 value set to one for geometric
                         
                         conditionalPanel( 
                             
                             condition = "input.dismodel == 'geometric'", 
                             
                             numericInput("j3", "j for geometric" , value = 1) 
                             
                         ) 
                         
                         
                         
                         
                         
                     ),  
                     
                     mainPanel(  
                         
                         plotOutput("histogram"),  #<- it will plot histogram on above input values
                         
                         tableOutput('tab')  
                         
                     ))
        )
    ),
    
    
    #this is a server function 
    
    
    
    server = function(input, output,session) {
        RMSE <- 0 #<- initialising RMSE to zero 
        SVM <- 0
        values <- reactiveValues() 
        
        myData <- reactive({ 
            switch(input$ds, 
                   
                   file = {  #<---- function for the data selection 
                       
                       file1 <- input$datafile  #<-- it will store the value of dataset into file1 variable using input funtion 
                       
                       if (is.null(file1)) {  
                           
                           return()  #<-- if File slected is not present then it will return null
                           
                       }  
                       
                       data = read.csv(file=file1$datapath) #<-- otherwise it will load the data set in the data using the path 
                       
                   }, 
                   
                   
                   ib =   { #<-- it will load the in build data set of R studio into file1 variable
                       
                       data = data.frame(get(input$ib))  
                       
                   }, 
                   
            ) 
            return(data)    
            
        })
        observe({ 
            
            updateSelectInput(session, "indvar", #<-- the entered indipenedent variables will be store here 
                              
                              choices = colnames(myData()))#<-- coloums name from a given data set is stored in choices
            
            
            updateSelectInput(session, "tarvar",  #<-- value of target variable will be store here and accessed through session 
                              
                              choices = colnames(myData())) #<-- target coloum name 
            
            
        }) 
        output$extdata = DT::renderDataTable({  #<-------- this is the data table function in R
            DT::datatable(myData(), options = list(lengthChange = TRUE)) #<--- change the number of records per page with the help of lengthchange 
            
        })
        
        output$glmperf <- renderPlot({ #<---- Output of Glm will be represented in graphical way
            
            df <- na.omit(myData()) #<-- Cleaning of given dataset using na.omit function 
            
            TarIndData <- cbind(df[,input$tarvar],df[,input$indvar]) #<-- binding the target variable and independent variable into 'TarIndData'
            
            colnames(TarIndData) = c(input$tarvar,input$indvar) 
            
            colnames(TarIndData)[1] <- "Y" 
            set.seed(199) #<-- setting a seed value 199
            
            n=nrow(TarIndData)  #<- number of row will be store in 'n'
            
            indexes = sample(n,n*(input$ratio/100)) #<--- it will give the value ration of trainset data on the bases of user inpur value for ratio
            
            trainset = data.frame(TarIndData[indexes,]) 
            
            testset = data.frame(TarIndData[-indexes,]) #<-- subtracting user ratio value from 100 will give the testset ratio
            
            actual <- testset$Y  #<-- Setting the actual variale to 'Y' of testset
            
            pred_test <- data.frame(testset) 
            #############################################################################
            #svr <- svm(Y~ ., data=trainset, method='eps-regression')
            #test set predictions linear 
            #pred_test_svr <-predict(svr,testset)
            #mse2=(sum(pred_test_svr-testset$Y)^2)/(n[1]-n[2])
            #rmse2=sqrt(mse2) 
            #values$svm_lin <- rmse2
            #################################################################################
            
            full.model <- glm(Y ~., data = trainset, family='gaussian')  #<-- Appying full model glm on 'Y'
            
            summary(full.model) 
            
            values$full <- full.model  
            
            pred_full <-  predict( full.model, testset[,input$indvar])  #<--- predicting the value of full model
            
            rmse_full = sqrt(sum((pred_full -actual)^2)/(nrow(testset)))  #<--getting the RMSE of full model
            
            #Applying RMSE on reduce model  
            
            reduced.model=stepAIC(full.model)  #<-- using stepAIC function on full model
            
            values$full <- full.model 
            
            values$reduced <- reduced.model 
            
            pred_red = predict( reduced.model, testset[,input$indvar])  #<-- predicting value on reduce model
            
            rmse_red = sqrt(sum((pred_red -actual)^2)/(nrow(testset))) 
            
            values$rmse <- data.frame('Full'=rmse_full,'Reduced'=rmse_red)  #<- geting the rmse value on reduce model
            
            par(mfrow=c(1,2)) 
            
            ################################################
            # graph on Full model
            
            plot(actual,type = "o",col = "red", xlab = "Observations", ylab = input$tarvar,  
                 
                 main = "FULL")  #<-- it will represent the actual data in red colour and mark x as observation and y axis as target variable name
            
            lines(pred_full, type = "o", col = "blue") #<-- blue colour is used to represent predicted value
            
            legend(  #<-- it is used to create box onthe top left corner of graph to give info about red and blue colour line
                
                "topleft",  
                
                lty=c(1,1),  
                
                col=c("red", "blue"),  
                
                legend = c("Real", "Predicted") 
                
            ) 
            ###############################################################
            # graph on reduce model
            
            plot(actual,type = "o",col = "red", xlab = "Observations", ylab = input$tarvar,  
                 
                 main = "Reduced") 
            
            lines(pred_red, type = "o", col = "blue") 
            
            legend( 
                
                "topleft",  
                
                lty=c(1,1),  
                
                col=c("red", "blue"),  
                
                legend = c("Real", "Predicted") 
                
            ) 
            
        })
        ########################################
        #OUTPUT of SLECTE DATA i.e. Target variable and Independent Variable
        output$selData <- DT::renderDataTable({ 
            
            
            
            df <- myData() 
            
            TarIndData <- cbind(df[,input$tarvar],df[,input$indvar]) 
            
            colnames(TarIndData) = c(input$tarvar,input$indvar) 
            
            
            
            DT::datatable(TarIndData, options = list(lengthChange = TRUE)) 
            
        })
        #######################################################
        output$SVM  <- DT::renderDataTable({ 
            
            
            
            DT::datatable(values$svm_lin, options = list(lengthChange = TRUE)) 
            
        })
        
        #######################################################
        # OUTPUT FOR RMSE tab
        
        output$RMSE  <- DT::renderDataTable({ 
            
            
            
            DT::datatable(values$rmse, options = list(lengthChange = TRUE)) 
            
        })
        
        output$Input_Ind <- renderUI({ 
            
            Var_count <- 0 
            
            Var_count <- length(input$indvar) 
            
            max_val <- 500 # default 5000 
            
            if (Var_count != 0) { 
                
                lapply(1:Var_count, function(i) { 
                    
                    numericInput(inputId = paste0(input$indvar[i]), label = input$indvar[i],value = 0) 
                    
                })
            } 
        })
        
        
        # predective value for new value or run time value
        forecast_out <- reactive({ 
            
            Var_Count <- length(input$indvar) 
            
            
            
            new_data <- as.numeric(paste(lapply(1:Var_Count, function(i) { 
                
                inputName <- paste0(input$indvar[i]) 
                
                input[[inputName]] 
                
            }))) 
            
            
            
            # taking the input in df formate
            
            input_data <- data.frame(t(new_data)) 
            
            
            
            for (i in 1:Var_Count)  
                
            { 
                
                colnames(input_data)[i] <- input$indvar[i] 
                
            } 
            
            
            ####################################################
            # value of new predicted value for reduce and full model
            new_predict_full <- predict(values$full,input_data) 
            
            new_predict_red <- predict(values$reduced,input_data) 
            
            #### using new value of reduce and full model for prediction and storing it in pred_data_new
            
            pred_data_new <- data.frame(new_predict_full,new_predict_red) 
            
            
            
            colnames(pred_data_new)[1] <- paste('Full Mode - ',input$tarvar) 
            
            colnames(pred_data_new)[2] <- paste('Reduced Mode - ',input$tarvar) 
            
            
            return(pred_data_new) 
            
        })
        ###############################################################################
        
        output$histogram <- renderPlot({ #<---- Output of various discrete model in graphical way
            
            
            
            # Algorithm  for binomial model  
            
            if (input$dismodel == 'binomial') { 
                
                par(mfrow=c(1,2))  
                
                d <- density(rbinom(1000,input$n,input$p))  #<-- rbinom is a binomial function 
                
                plot(d, main="Kernel Density of generated data")  
                
                polygon(d, col="red", border="blue") 
                
                x=0:input$n  
                
                plot(x,dbinom(x,input$n,input$p))  
                
            } 
            if (input$dismodel == 'bernoulli') { 
                par(mfrow=c(1,2))
                Density <- density(rbinom(input$s,1,input$p))
                plot(Density, main="Kernel Density of generated data")
                polygon(Density, col="red", border="blue")
                x=0:1
                plot(x,dbinom(x,1,input$p))
            }
            
            # hypergeometric
            if (input$dismodel == 'hypergeometric') { 
                par(mfrow=c(1,2))
                D=rhyper(nn=input$s, m=input$m, n=input$n, k=rep(input$k, input$s))
                tab=table(D)
                barplot(tab,col='blue')
                x2=0:input$s
                y2=dhyper(x2, m=input$m, n=input$n, k=input$k, log=FALSE)
                plot(x2,y2,type='b')
            }
            
            # Algorithm for poisson model
            
            if (input$dismodel == 'poisson') { 
                
                par(mfrow=c(1,2))   
                
                D=rpois(input$s, input$lam) #<-poisson function 
                
                tab=table(D)  #<- putting the value in table function
                
                barplot(tab,col='blue')  
                
                x1=0:input$max  
                
                y1=dpois(x1,input$lam)  
                
                plot(x1,y1,type='b')  
                
            } 
            
            # for geometric model  
            
            if (input$dismodel == 'geometric') { 
                
                par(mfrow=c(1,2)) 
                
                D=rgeom(input$s, input$p)   #<-- rgeom id geometric function
                
                tab=table(D)  
                
                barplot(tab,col='blue')  
                
                x2=0:input$max  
                
                y2=dgeom(x2,input$p)  
                
                plot(x2,y2,type='b')  
                
            } 
            
        }) 
        ###########################################################
        # Otput of various model
        output$histogram1 <- renderPlot({ 
            
            
            
            # normal  model
            
            if (input$conmodel == 'normal') { 
                
                par(mfrow=c(1,2))  
                
                x=seq(-input$i,input$i,0.01)  
                
                plot(x,dnorm(x,input$mu,input$sigma),type='l', col='red')  
                
                
                
            } 
            
            # exponential model
            
            
            
            if (input$conmodel == 'exponential') { 
                
                # exponential  
                
                par(mfrow=c(1,2)) 
                
                x=seq(0,input$i,0.01)  
                
                plot(x,dexp(x,input$lam),type='l',col='green') 
                
                
            } 
            
            # uniform model
            
            if (input$conmodel == 'uniform') { 
                
                a <- input$a 
                
                b <- input$b 
                
                n1 <- input$s 
                
                
                
                rand.unif <- runif(n1, min = a, max = b) #<--- uniform function
                
                
                
                hist(rand.unif,  #<---- histogram function for uniform model
                     
                     freq = FALSE,  
                     
                     xlab = 'x',   
                     
                     ylim = c(0, 0.4), 
                     
                     xlim = c(-3,3), 
                     
                     density = 20, 
                     
                     main = "Uniform distribution") 
                
                
                
                
                
                curve(dunif(x, min = a, max = b),  
                      
                      from = -3, to = 3,  
                      
                      n = n1,  
                      
                      col = "darkblue",  
                      
                      lwd = 2,  
                      
                      add = TRUE,  
                      
                      yaxt = "n", 
                      
                      ylab = 'probability') 
                
                
                
                
                
            } 
            
            
            
        })
        
        #########output of prob for various model
        
        
        output$prob <- renderPrint({  
            
            p1=pnorm(input$j1,input$mu, input$sigma)  
            
            p2=pexp(input$j2,input$lam)  
            
            
            
            if (input$conmodel == 'exponential') { 
                
                c(p2)  
                
            } 
            
            
            
            if (input$conmodel == 'normal') { 
                
                c(p1)  
                
            } 
            
            
            
            
            
        })
        
        
        output$tab1 <- renderTable({  
            
            Normal=rnorm(input$s,input$mu, input$sigma)  
            
            Exp=rexp(input$s,input$lam)  
            
            
            
            if (input$conmodel == 'exponential') { 
                
                d2=data.frame(Exp)  
                
            } 
            
            else 
                
            { 
                
                d1=data.frame(Normal)  
                
            } 
            
            
            
        })
        
        output$tab <- renderTable({  
            
            p1=dbinom(input$j1,input$n, input$p)  #<<<---------Binomal table output
            
            p2=dpois(input$j2,input$lam)  #<<<---------Pisson table output
            
            p3=dgeom(input$j3,input$p)  #<<<---------Geometric table output
            
            c(p1,p2,p3) 
            
            
        })
        
        datasetInput <- reactive({
            
            ########## Switch operator amoung various dataset
            switch(input$dataset,
                   "all iris data" = iris,
                   "setosa" = subset(iris, iris$Species == "setosa"),
                   "versicolor" = subset(iris, iris$Species == "versicolor"),
                   "virginica" = subset(iris, iris$Species == "virginica"))
        })
        
        
        colX <- reactive({
            ########## Switch operator amoung various X Variable input
            switch(input$Xvar,
                   "Sepal.Length" = iris$Sepal.Length,
                   "Sepal.Width" = iris$Sepal.Width,
                   "Petal.Length" = iris$Petal.Length,
                   "Petal.Width" = iris$Petal.Width)
        })
        
        colY <- reactive({
            ########## Switch operator amoung various Y variable Input
            switch(input$Yvar,
                   "Sepal.Length" = iris$Sepal.Length,
                   "Sepal.Width" = iris$Sepal.Width,
                   "Petal.Length" = iris$Petal.Length,
                   "Petal.Width" = iris$Petal.Width)
        })
        #### Clusturing 
        clusters <- reactive({
            kmeans(iris[,1:4], input$clusters) #<--- clustering on 1 to 4 coloum
        })
        
        myColors <- reactive({
            switch(input$dataset,
                   "all iris data" = c(palette()[1],palette()[2],palette()[3]),
                   "setosa" = palette()[1],
                   "versicolor" = palette()[2],
                   "virginica" = palette()[3])
        })
        
        # Generate a summary of the dataset (or subset by Iris.Species)
        output$summary <- renderPrint({
            dataset <- datasetInput()
            summary(dataset)
        })
        
        # Show the first n observations
        output$view <- renderTable({
            head(datasetInput(), n = input$obs)
        })
        output$NbRows <- renderText({ 
            paste("You have selected to show ", input$obs," lines.")
        })
        
        
        # Show a simple x,y plot
        output$simplePlot <- renderPlot({
            
            df_iris <- datasetInput() #<------- storing dataset in df formate
            
            plot(df_iris[,c(input$Xvar,input$Yvar)], xlab = input$Xvar, ylab = input$Yvar,
                 main=toupper(ifelse(input$dataset == "all iris data", "iris", input$dataset)), pch=16, cex = 2,
                 col = ifelse(df_iris$Species == "setosa", palette()[1], 
                              ifelse(df_iris$Species == "versicolor", palette()[2], palette()[3])) )
            
            legend("bottomright", legend = unique(df_iris[,5]), 
                   col = myColors(), title = expression(bold("Iris.Species")),
                   pch = 16, bty = "n", pt.cex = 2, 
                   cex = 0.8, text.col = "black", horiz = FALSE, inset = c(0.05, 0.05))
        })
        
        # Show boxplot
        output$boxPlot <- renderPlot({
            df_iris <- datasetInput()
            
            if (input$dataset == "all iris data") {
                boxplot(df_iris[,c(input$Yvar)] ~ df_iris[,5], xlab = "Species", ylab = input$Yvar, main = "IRIS", 
                        border = "black", col = myColors())
            }
            else {
                boxplot(df_iris[,c(input$Yvar)], xlab = "Species", ylab = input$Yvar, main = toupper(input$dataset),
                        border = "black", col = myColors())
            }
        })
        
        # K-Means Plot
        output$NbClust <- renderText({ 
            paste("K-means clustering performed with ", input$clusters," clusters.")
        })
        output$kmeansPlot <- renderPlot({
            plot(iris[,c(input$Xvar,input$Yvar)],
                 col = clusters()$cluster,
                 pch = 20, cex = 2)
            points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
        })
        
        # Density-based cluster
        output$dbscan_Param <- renderText({ 
            paste("DBSCAN clustering performed with eps = ", input$eps," and minPts = ", input$minPoints,".")
        })
        output$dbscanPlot <- renderPlot({
            cluster <- dbscan(iris[,-5], eps = input$eps, MinPts = input$minPoints)
            plot(cluster, iris[,c(input$Xvar, input$Yvar)])
        })
        
        # Decision Tree
        output$treePlot <- renderPlot({
            ctree <- ctree(Species ~ ., data = iris)
            plot(ctree, type="simple")
        })
        
        # Create a .csv file with dataframe inside
        output$downloadData <- downloadHandler(
            filename = function() {
                paste('data-Iris-', Sys.Date(), '.csv', sep='')
            },
            content = function(con) {
                write.csv(iris, con)
            }
        )
        
        
    }
)
