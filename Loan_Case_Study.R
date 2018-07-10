# Load the data
loan_df <- read.csv('loan/loan.csv',stringsAsFactors = FALSE)
loan_df_copy <- read.csv('loan/loan.csv',stringsAsFactors = FALSE)
View(loan_df)

# Loading the required libraries
install.packages('tidyr')
library('tidyr')

install.packages('dplyr')
library("dplyr")

install.packages('stringr')
library("stringr")

install.packages('lubridate')
library("lubridate")

install.packages('ggplot2')
library("ggplot2")

install.packages('sqldf')
library("sqldf")

install.packages("gridExtra")
library("gridExtra")

install.packages('corrplot')
library('corrplot')

install.packages('PerformanceAnalytics')
library("PerformanceAnalytics")

# Structure of the dataset
glimpse(loan_df)

# No. of counts of data  
row_counts <- nrow(loan_df) # no. of rows = 39717
Percentage_15 <- row_counts*15/100
ncol(loan_df) # no. of cols = 111


# There are many columns with only a single unique value- e.g. NA, 0, F etc. Lets us drop
# all such columns as they will not be useful for EDA or visualisation
col_unique_1 <- as.vector(which(sapply(loan_df, function(x) length(unique(x))==1)))


# As a next step we should also drop columns which only have 0 or NA's as their unique values
col_0orNA <- as.vector(which(sapply(loan_df, function(x) length(unique(x))==2 & sum(unique(x) %in% c(0,NA))==2)))

colnames_gt_15_na <- c((colnames(loan_df)[colSums(is.na(loan_df)) > Percentage_15]))

# Drop these columns

loan_df <- loan_df[,-c(col_unique_1,col_0orNA)]
loan_df <- loan_df[,!(colnames(loan_df) %in% colnames_gt_15_na)]

ncol(loan_df)

View(loan_df)

# col wise data cleaning
# 1-10 cols
glimpse(loan_df[,1:10])

n_distinct(loan_df$id) # 39717 distinct_id is there
n_distinct(loan_df$member_id) # 39717 distinct_member_id is there

# Loan amount range is 500 to 35000
max_loan_amt <- max(loan_df$loan_amnt, na.rm = TRUE)
min_loan_amt <- min(loan_df$loan_amnt, na.rm = TRUE)

# Loan_df$term data cleaning and term_year new derived column
loan_df <- loan_df %>% mutate(term_year = as.numeric(substr(loan_df$term,0,3))/12)

sqldf("select term,term_year,count(*) from loan_df group by term,term_year")

# Loan_df$int_rate data cleaning

loan_df$int_rate <- as.numeric(gsub("%","",loan_df$int_rate))


#11-20 cols
glimpse(loan_df[,11:20])

# emp_title, desc and URL is irrelevent in this analysis so dropping the column
loan_df <- loan_df[ , !(names(loan_df) %in% c('emp_title','url','desc'))]

# emp_length data cleaning
loan_df$emp_length <- gsub("[year|years]","",loan_df$emp_length)

# issue_d data cleaning and fetch the year
loan_df$issue_d_year <- as.numeric(substr(loan_df$issue_d,5,nchar(loan_df$issue_d)))+2000

#21-30 cols
glimpse(loan_df[,21:30])
# zip_code,addr_state can be deleted
# earliest_cr_line data cleaning , set all the date 1st of that month
?parse_date_time()
loan_df <- loan_df %>% 
           mutate(earliest_cr_line = paste("01",earliest_cr_line,sep="-"),
           earliest_cr_line = parse_date_time(earliest_cr_line, orders = c("dmy"), locale = "eng"))

#31-40 cols
glimpse(loan_df[,31:40])

# revol_util data cleaning
loan_df$revol_util <- as.numeric(gsub("%","",loan_df$revol_util))

#41-46 cols
glimpse(loan_df[,41:46])

#There will be some warnings because of the blank values
loan_df <- loan_df %>% mutate(last_pymnt_d = paste("01",last_pymnt_d,sep="-"),
                              last_pymnt_d = parse_date_time(last_pymnt_d, orders = c("dmy"), locale = "eng"),
                              next_pymnt_d = paste("01",next_pymnt_d,sep="-"),
                              next_pymnt_d = parse_date_time(next_pymnt_d, orders = c("dmy"), locale = "eng"),
                              last_credit_pull_d = paste("01",last_credit_pull_d,sep="-"),
                              last_credit_pull_d = parse_date_time(last_credit_pull_d, orders = c("dmy"), locale = "eng"))
  

#Outliers chcking

# For a given continuous variable, outliers are those observations that lie outside 1.5 * IQR,
# where IQR, the 'Inter Quartile Range' is the difference between 75th and 25th quartiles. 
# Look at the points outside the whiskers in below box plot.

outlier_detection <- function(data,var){
  var <- eval(substitute(var),eval(data))
  na_1 <- sum(is.na(var))
  par(mfrow = c(2,2))
  boxplot(var, horizontal = T)
  hist(var, main = "With Outliers")
  outlier <- boxplot.stats(var)$out
  length(outlier)
  
  upper_limit <- as.numeric(quantile(var)[4] + 1.5*IQR(var))
  # var_within_upper_limit <- ifelse(var <= upper_limit,var,NA)
  var <- ifelse(var %in% outlier, NA, var)
  boxplot(var, horizontal = T)
  hist(var, main = "Without Outliers")
  
  title("Outlier Check", outer=TRUE)
  
  na_2 <- sum(is.na(var))
  cat("Outliers identified:", na_2 - na_1, " ")
  cat("Propotion of outliers:", round((na_2 - na_1) / sum(!is.na(var))*100, 1), "%")
  return(upper_limit)
}

upper_limit_loan_amnt <- outlier_detection(loan_df,loan_amnt)
upper_limit_annul_inc <- outlier_detection(loan_df,annual_inc)
upper_limit_funded_amnt_inv <- outlier_detection(loan_df,funded_amnt_inv)

# Replaceing any values over upper_limit with NA
loan_df$loan_amnt <- ifelse(loan_df$loan_amnt >= upper_limit_loan_amnt,NA,loan_df$loan_amnt)
loan_df$annual_inc <- ifelse(loan_df$annual_inc >= upper_limit_annul_inc,NA,loan_df$annual_inc)
loan_df$funded_amnt_inv <- ifelse(loan_df$funded_amnt_inv >= upper_limit_funded_amnt_inv,NA,loan_df$funded_amnt_inv)

# Taking a copy of current loan data fram
loan_df_2 <- loan_df

# As loan_status="Current" will not serving our purpose we are omitting it for further analysis
loan_df <- loan_df %>% filter(loan_status != "Current")


# 1.. Home Ownership distribution,fill = loan_status

sqldf("select count(id),home_ownership,loan_status from loan_df where trim(home_ownership)='NONE' group by home_ownership,loan_status")

# Set back ground theme
theme_set(theme_bw())

options(scipen = 999)

par(mfrow=c(1,1))

 G1 <- ggplot(data = loan_df %>% 
       group_by(home_ownership,loan_status)) +
       geom_bar(aes(x=home_ownership,fill=loan_status),color='black',position="stack") +
       labs(title="G1-Home Ownership & Loan Status",x="Home Ownership",y="Count",fill="Loan Status: ")+
       theme(axis.text.x = element_text(angle = 50, hjust = 1))+
       scale_x_discrete(limits=c("RENT","MORTGAGE","OWN","OTHER","NONE"))+
       theme(legend.position="top")+
       theme_minimal()+
       scale_fill_manual(values=c('#999999','#E69F00'))

 # 2..Verfication status distribution
 
 G2 <- ggplot(data = loan_df %>%
       group_by(verification_status,loan_status)) +
       geom_bar(aes(x=verification_status,fill=loan_status),color='black',position="stack") +
       labs(title="G1-verification_status & Loan Status",x="Verification Status",y="Count",fill="Loan Status: ")+
       theme(axis.text.x = element_text(angle = 50, hjust = 1))+
       theme_minimal()+
       theme(legend.position="top")+
       scale_fill_manual(values=c('#999999','#E69F00'))
 
 # 3..Public Derogatory Record 
 G3 <- ggplot(data = loan_df %>%
       group_by(pub_rec,loan_status))+
       geom_bar(aes(x=pub_rec,fill=loan_status),color='black',position="stack") +
       labs(title="G3-Public dergatory tecord & Loan Status",x="Public Record",y="Count",fill="Loan Status: ")+
       theme(axis.text.x = element_text(angle = 50, hjust = 1))+
       theme_minimal()+
       theme(legend.position="top")+
       scale_fill_manual(values=c('#999999','#E69F00'))

 # 4..Public Derogatory bankruptcies Record
 G4 <- ggplot(data = loan_df %>%
       group_by(pub_rec_bankruptcies,loan_status))+
       geom_bar(aes(x=pub_rec_bankruptcies,fill=loan_status),color='black',position="stack") +
       labs(title="G4-Public dergatory record bankruptcies & Loan Status",x="Public Record",y="Count",fill="Loan Status: ")+
       theme(axis.text.x = element_text(angle = 50, hjust = 1))+
       theme_minimal()+
       theme(legend.position="top")+
       scale_fill_manual(values=c('#999999','#E69F00'))

 # 5..Term of the loan and loan Status
 G5 <- ggplot(data = loan_df %>%
       group_by(term_year,loan_status))+
       geom_bar(aes(x=as.factor(term_year),fill=loan_status),color='black',position="dodge") +
       labs(title="G5-Loan term & Loan Status",x="Loan term in Years",y="Count",fill="Loan Status: ")+
       theme_minimal()+
       theme(legend.position="top")+
       scale_fill_manual(values=c('#999999','#E69F00'))
 
 #6..Annual income and loan Status
 G6 <- ggplot(data = loan_df)+
       geom_histogram(aes(x = annual_inc,fill = loan_status),color = "black") +
       xlim(0, 150000)+
       theme_minimal()+
       theme(legend.position="top")+
       labs(title="G6-Annual Income & Loan Status",x="Annual Income in Rupees",y="Count",fill="Loan Status: ")+
       scale_fill_manual(values=c('#999999','#E69F00'))

grid.arrange(G1,G2,G3,G4,G5,G6,nrow=2,ncol=3)

 #7..Sub Grade and loan Status
 G7 <- ggplot(data = loan_df)+ 
       geom_bar(aes(x = sub_grade, fill = loan_status))+
       labs(title="G7-Sub Grade & Loan Status",x="Sub Grade",y="Count",fill="Loan Status: ")+
       theme_minimal()+
       theme(axis.text.x = element_text(angle = 90, hjust = 1))+
       theme(legend.position="top")+
       scale_fill_brewer()
 
       #scale_fill_manual(values=c('#999999','#E69F00'))
 #8..Grade and loan Status
 G8 <- ggplot(data = loan_df)+
       geom_bar(aes(x = grade,fill = loan_status))+
       labs(title = "G8-Grade & Loan Status",x="Sub Grade",y="Count",fill="Loan Status: ")+
       theme_minimal()+
       theme(legend.position = "top")+
       scale_fill_brewer()
 
 grid.arrange(G7,G8,nrow=1,ncol=2)   
 
 #9.. Purpose and loan status
 G9 <- ggplot(data = loan_df)+
       geom_bar(aes(x = purpose,fill = loan_status))+
       labs(title = "G9-Purpose & Loan Status",x="Purpose",y="Count",fill="Loan Status: ")+
       theme_minimal()+
       theme(axis.text.x = element_text(angle = 90, hjust = 1))+
       theme(legend.position = "top")+
       scale_fill_manual(values=c('#999999','#E69F00'))
 
 #..10 revol_util analysis
 # Check NA values in revol_util: (main data)
 length(which(is.na(loan_df$revol_util))) #50 NAs
 # median & mean of this revol_util
 mean(loan_df$revol_util,na.rm=T)
 # 48.70278
 median(loan_df$revol_util,na.rm=T)
 # 49.1
 
 # based on the mean and median value we can say that data is evenly distributed
 # We are going consider median for replacement.
 
 loan_df[which(is.na(loan_df$revol_util)),'revol_util'] <- median(loan_df$revol_util,na.rm=TRUE)
 
 G10 <- ggplot(data = loan_df,aes(x = revol_util,fill = loan_status))+
        geom_histogram(col='black',bins=21)+
        labs(title = "G10-Purpose & Loan Status",x="Revol util",y="Count",fill="Loan Status: ")+
        theme_minimal()+
        theme(legend.position = "top")+
        scale_fill_manual(values=c('#999999','#E69F00'))
        
 G10_1 <- ggplot(loan_df, aes(x=revol_util)) + 
          geom_density(fill='red',alpha = 0.5)+
          geom_vline(aes(xintercept=median(revol_util)),
          color="blue", linetype="dashed", size=1)+
          theme_minimal()
 
 grid.arrange(G10,G10_1,nrow=2,ncol=1)
 
 G11 <- ggplot(data = loan_df %>%
        group_by(loan_amnt,loan_status))+
        geom_histogram(aes(x = loan_amnt,fill = loan_status),col = 'black')+
        labs(title = "G8-Grade & Loan Status",x="Sub Grade",y="Count",fill="Loan Status: ")+
        theme_minimal()+
        theme(legend.position = "top")+
        scale_fill_brewer()
        
 
 #Loan_amnt_range derived matrics
 loan_df$loan_amnt_range <- (gsub("]",")",cut(loan_df$loan_amnt,60,dig.lab = 10))) 
 
 
 grid.arrange(G10,G10_1,nrow=2,ncol=1)
 
 
 #..12 addr_state
 G12 <- ggplot(data = loan_df %>% 
        group_by(addr_state,loan_status)) + 
        geom_bar(aes(x=addr_state,fill=loan_status), position="dodge") +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        theme(legend.position="top")+
        scale_fill_manual(values=c('#999999','#E69F00'))+
        labs(title="G12 - State - Address State",x="State",y="Count",fill="Loan Status")
 
 # 13..delinq_2yrs
 G13 <- ggplot(loan_df %>% 
        group_by(delinq_2yrs,loan_status))+
        geom_bar(aes(x=delinq_2yrs,fill=loan_status),position="dodge") +
        # scale_y_continuous(breaks=seq(0,1,0.1)) +
        # scale_x_continuous(breaks=seq(0,12,1)) +
        theme_minimal()+
        theme(legend.position="top")+ 
        scale_fill_manual(values=c('#999999','#E69F00'))+
        labs(title="G13 - Delinquent in last 2 years Vs\nLoan Status",x="Number of delinquent",y="Percentage",fill="Loan Status")

  #14..Employee experience
 G14 <- ggplot(data = loan_df %>% 
        filter(emp_length!='n/') %>%
        group_by(emp_length,loan_status)) + 
        geom_bar(aes(x=as.factor(emp_length),fill=loan_status), position="dodge") +
        #scale_x_discrete(c("<1","1","2","3","4","6","7","8","9","10+"))+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        theme(legend.position="top")+
        scale_fill_manual(values=c('#999999','#E69F00'))+
        labs(title="G14 - State - Employee Experience",x="State",y="Count",fill="Loan Status")
 
 
 grid.arrange(G12,G13,G14,nrow=1,ncol=3)
 # Variables that effect the charge off Vs Fully paid loans
 # 1. home_ownership
 # 2. verification_status
 # 3. public bankruptcy records
 # 4. term
 # 5. sub_grade
 # 6. purpose
 # 7. revol_util_range
 # 8. loan_amnt_range
 
 
 effective_cols= c("loan_amnt", "int_rate", "installment", "sub_grade", "annual_inc","dti","revol_util","delinq_2yrs")
 
 correlation <- select(loan_df, one_of(effective_cols))
 correlation <- correlation[complete.cases(correlation), ]
 
 # Convert the character types to numeric types #
 correlation$subgrade_n <- as.numeric(as.factor(correlation$sub_grade))
 
 # Remove the character 
 correlation <- select(correlation, -sub_grade)
 
 C_plot <- cor(correlation)
 par(mfrow=c(1,1))
 col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
 corrplot(C_plot, method = "color", 
          col=col(200),
          title = "Correlation Map of Subgrade & Factors", 
          type = "upper", 
          order = "FPC", 
          number.cex = 1, 
          tl.cex = 0.9,
          addCoef.col = "black",
          bg="white")
 
 
 
 
 
 # 15.. Grades and revol_util relation #
 G15 <- ggplot(loan_df) + 
        geom_boxplot(aes(x=sub_grade,y=revol_util,fill=grade)) +
        geom_line(data = (loan_df %>% group_by(sub_grade) %>% summarize(avg=mean(revol_util,na.rm=TRUE))),aes(x=sub_grade,y=avg,group=1)) +
        scale_y_continuous(breaks=seq(0,100,5)) +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        theme(legend.position="top")+
        scale_fill_brewer()+
        labs(title="G15 - Grade Vs Revolving Utilization",x="Sub Grade",y="Revolving Utilization(%)",fill="Grade")
 
 # There seems to a relationship between the grade of the loan and the revolving credit utilization
 # However, this isnt consistent. So it is better if we consider revol_util as a separate variable
 
 
 # 16.. Grades on loan amount #
 # Note : the line signifies the mean of the dataset
 G16 <- ggplot(loan_df) + 
        geom_boxplot(aes(x=sub_grade,y=loan_amnt,fill=grade)) + 
        geom_line(data = (loan_df %>% 
        group_by(sub_grade) %>% 
        summarize(avg=mean(loan_amnt,na.rm=TRUE))),
        aes(x=sub_grade,y=avg,group=1)) +
        scale_y_continuous(breaks=seq(0,30000,5000)) +
        scale_fill_manual(values=c('#B7C8B6','#838B83','#71C671','#699864','#00CD00','#228B22','#003300'))+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        theme(legend.position="top")+
        labs(title="G16 - Grades Vs Loan Amount",x="Sub Grades",y="Loan Amount",fill="Grade")
 
 
 # Inference : It seems that Grade/sub grade is related to loan_amnt, but there seem
 # to be exceptions to it. It is better to keep loan_amnt separate for analysis.
 
 # Inference:
 # Variables that effect the charge off Vs Fully paid loans
 # 1. home_ownership
 # 2. verification_status
 # 3. public bankruptcy records
 # 4. term
 # 5. sub_grade
 # 6. purpose
 # 7. revol_util_range
 # 8. loan_amnt_range
 
 
 # Build a separate column for sub_grade for the numerical value #
 loan_df$sub_grade_2 <- sapply(loan_df$sub_grade,function(x) str_split(x,"")[[1]][2])
 
 
 # 17.. Relation between DTI and GRADES
 # Note : the line signifies the mean DTI in that sub_grade
   G17 <- ggplot(loan_df) + 
          geom_boxplot(aes(x=sub_grade,y=dti,fill=grade)) + 
          geom_line(data=(loan_df %>% 
          group_by(sub_grade) %>% 
          summarize(avg_dti=mean(dti,na.rm=TRUE))),
          aes(x=sub_grade,y=avg_dti,group=1)) +
          scale_y_continuous(breaks=seq(0,32,1)) +
          labs(title="G17 - Grades Vs DTI",x="Sub Grade",y="DTI",fill="Grade")
 
 
 
 # Since median & median both seems to justify the distribution in a just manner, 
 # lets look it from a different perspective
 
 G18 <- ggplot(data = loan_df %>% 
        group_by(grade,sub_grade_2) %>% 
        summarize(median_dti = median(dti,na.rm=TRUE)),aes(x=grade,y=sub_grade_2,value=median_dti)) + 
        geom_tile(aes(fill=median_dti)) +
        geom_text(aes(label=median_dti),col="white") +
        labs(title="G18 - Grades vs DTI(Median)",x="Grade",y="Sub Grade",fill="Median DTI")
 
 grid.arrange(G16,G17,G18,nrow=1,ncol=3)
 
 # DTI vs Grades vs Loan Status
 G19 <- ggplot(data = loan_df %>% 
        group_by(grade,sub_grade_2,loan_status) %>% 
        summarize(median_dti = median(dti,na.rm=TRUE)),aes(x=grade,y=sub_grade_2,value=median_dti)) + 
        geom_tile(aes(fill=median_dti)) +
        geom_text(aes(label=median_dti),col="white") + 
        labs(title="G19 - DTI Vs Grades Vs Loan Status",x="Grade",y="Sub Grade",fill="Median DTI") +
        facet_wrap(~loan_status)
 
 grid.arrange(G16,G17,G18,G19,nrow=2,ncol=2)
 
 # Inference : There is a direct relationship between the DTI and the Grade, therefore,
 #             between dti and grades, grades can be considered as a valid reflection for DTI data as well.
 
 
 #### IMPORTANT ####
 # Grade = G3 seems is having oulier in the loan allocation. There is a relationship between these
 # Grade level and the number of Charged off loans.
 
 
 # Grade Vs Sub_Grade Vs Median DTI (tile) Vs Percentage Charged Off #
 G20 <- ggplot() + 
        geom_tile(data = loan_df %>% 
        group_by(grade,sub_grade_2) %>% 
        summarize(median_dti = median(dti,na.rm=TRUE)),aes(x=grade,y=sub_grade_2,fill=median_dti)) +
        geom_text(data = (loan_df %>% 
                  group_by(grade,sub_grade_2,loan_status) %>% 
                  summarize(count=length(id)) %>% 
                  mutate(ratio=paste("Charged Off =",round(count/sum(count),4)*100,"%")) %>% 
                  filter(loan_status=="Charged Off")),
                  aes(x=grade,y=sub_grade_2,label=ratio),col="black") +
        geom_text(data = (loan_df %>% 
                  group_by(grade,sub_grade_2,loan_status) %>% 
                  summarize(count=length(id)) %>% 
                  mutate(ratio=paste("Fully Paid =",round(count/sum(count),4)*100,"%")) %>% 
                  filter(loan_status=="Fully Paid")),
                  aes(x=grade,y=sub_grade_2,label=ratio),col="black",vjust=-1.2) +
        geom_text(data = (loan_df %>% 
                  group_by(grade,sub_grade_2) %>% 
                  summarize(count=length(id)) %>%
                  mutate(count_2=paste("Total = ",count))),
                  aes(x=grade,y=sub_grade_2,label=count_2),col="black",vjust=-2.4) +
                  labs(title="G20 - Grade vs Sub Grade vs Median DTI\nWith percentage Charged off for each\nSub Grade",
                  x="Grade",y="Sub Grade",fill="Median DTI",label="Percentage of Charged Off Loans")+
                  scale_fill_gradientn(colours = terrain.colors(10))

 
 # G3 Grade level is a clear risk for LC #
 
 
 # Influence of Grades on Interest Rate
 # Note : the line signifies the mean interest rate in that sub_grade
 G21 <- ggplot(loan_df) + 
        geom_boxplot(aes(x=sub_grade,y=int_rate,fill=grade)) + 
        geom_line(data=(loan_df %>% 
        group_by(sub_grade) %>% 
        summarize(avg_dti=mean(int_rate,na.rm=TRUE))),
        aes(x=sub_grade,y=avg_dti,group=1)) +
        scale_y_continuous(breaks=seq(0,25,1)) +
        scale_fill_manual(values=c('#B7C8B6','#838B83','#71C671','#699864','#00CD00','#228B22','#003300'))+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        theme(legend.position="top")+
        labs(title="G21 - Grades vs Interest Rate",x="Sub Grade",y="Interest Rate",fill="Grade")
     
 
 G22 <- ggplot(data = loan_df %>% 
        group_by(grade,sub_grade_2) %>% 
        summarize(med=median(int_rate)),
        aes(x=grade,y=sub_grade_2,value=med)) + 
        geom_tile(aes(fill=med)) +
        geom_text(aes(label=med),col="Black") +
        theme(legend.position="top")+
        scale_fill_gradientn(colours = terrain.colors(7))+
        labs(title="G22 - Grade vs Sub_Grade vs Interest Rate",x="Grade",y="Sub Grade",fill="Median\nInterest Rate")
 
 G23 <- ggplot(data = loan_df %>% 
        group_by(grade,sub_grade_2,loan_status) %>% 
        summarize(med=median(int_rate)),
        aes(x=grade,y=sub_grade_2,value=med)) + 
        geom_tile(aes(fill=med)) +
        geom_text(aes(label=med),col="black") +
        facet_wrap(~loan_status) +
        theme(legend.position="top")+
        scale_fill_gradientn(colours = terrain.colors(7))+
        labs(title="G23 - Grade vs Sub_Grade vs Interest Rate vs Loan Status",x="Grade",y="Sub Grade",fill="Median\nInterest Rate")
 
 grid.arrange(G21,G22,G23,nrow=1,ncol=3)
 
 #Inference : There seems to be direct relationship between Interest Rate and the Grade, therefore,
 #            between interest rate and grades, grades can be considered as a valid reflection of interest
 #            data as well.
 
 # Influence of grade on verification status
 G24 <- ggplot(loan_df %>% 
        group_by(verification_status,sub_grade) %>% 
        summarize(count=length(id))) + 
        geom_col(aes(x=verification_status,y=count,fill=sub_grade),position="fill") +
        scale_y_continuous(breaks=seq(0,1,0.1)) +
        labs(title="G24 - Verification Status vs Grade",x="Verification Status",y="Percentage",fill="Grade")
 
# Influence of grade on home ownership
 G25 <- ggplot(loan_df %>% 
        group_by(home_ownership,sub_grade) %>% 
        summarize(count=length(id))) + 
        geom_col(aes(x=home_ownership,y=count,fill=sub_grade),position="fill") + 
        scale_y_continuous(breaks=seq(0,1,0.1)) +
        labs(title="G23 - Verification Status Vs Grade",x="Verification Status",y="Percentage",fill="Grade")

 grid.arrange(G24,G25,nrow=1,ncol=2)
 # Inference : There is relationship between home_ownership and verification status, hence
 #             keeping them as separate influential variables
 
 # Inference:
 # Variables that effect the charge off Vs Fully paid loans
 # 1. sub_grade
 # 2. purpose
 # 3. term 
 # 4. home_ownership 
 # 5. verification_status
 # 6. public bankruptcy records
 # 7. revol_util_range
 # 8. loan_amnt_range   
 
 ### Final EDA Conclusion Graph - for Influencing Variables #
 grid.arrange(G7,G8,G6,G2,G3,G5,G9,G10,nrow=2,ncol=4)
 

 
 
 
 
 
       
 
 