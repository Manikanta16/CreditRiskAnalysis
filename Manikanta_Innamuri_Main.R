###########################
######## EDA Case Study
#### Group Members
# Manikandan Kulanghat
# Gaurav Jain
# Guru Manikanta Innamuri
## Date: 3/31/2018
##########################

# Set working directory to the directory where the input file is located. 
# Load the required Libraries. 
library(ggplot2)
library(dplyr)
library(GGally)

########################################
# Checkpoint 1: Read Raw Data
########################################

# Reading the data from the csv file. Setting the String as factors as false as there are huge number of columns with text data.
# There will be a separate operation downstream to convert only specific columns to factors after doing some research on the data using Summary & str. 
loanRaw <- read.csv("loan.csv",stringsAsFactors = FALSE)

########################################
# Checkpoint 2: Data Understanding
########################################

summary(loanRaw)
str(loanRaw)
# Observation 1: The data has only 111 variables rather than the 115 as listed in the Data Dictionary
# Observation 2: Most of the columns have only NA Values (or) 0s. These columns cannot be used for any analysis. 
# Observation 3: Zip code is scrubbed and thus it cannot be used. 
# Observation 4: The link https://www.lendingclub.com/foliofn/rateDetail.action helps in understanding more about the purpose of the grades & their impact to Interest rates. 
# Observation 5: Some of the data like Interest rate & revolving utilization has percentage in the text. We need to remove the percentage character to do analysis on these fields. 


########################################
# Checkpoint 3: Data Cleansing
########################################

# Removing columns that have only NA's as values. 
loanRawNonNA <- loanRaw[,colSums(is.na(loanRaw))<nrow(loanRaw)]
summary(loanRawNonNA)

# After analyzing the data, following columns can be removed from data frame to clean the dataset.
# On looking at the data in the columns, we cannot make any major inferences. Thus removing the fields. 
#           - id,member_id,pymnt_plan,url,zip_code (no purpose since data is scrapped),collections_12_mths_ex_med,
#           - policy_code,application_type,acc_now_delinq,chargeoff_within_12_mths,delinq_amnt,tax_liens
loanClean <- select(loanRawNonNA,-1:-2,-18,-19,-23,-36,-50:-55,-56)
str(loanClean)

# Converting the selected columns int factors for analysis.
loanClean$term <- sapply(loanClean$term,factor)
loanClean$grade <- sapply(loanClean$grade,factor,levels=c("A","B","C","D","E","F","G")) # Setting the order of the values in factor to get a meaningful plot.
loanClean$sub_grade <- sapply(loanClean$sub_grade,factor)
loanClean$emp_length <- sapply(loanClean$emp_length,factor)
loanClean$home_ownership <- sapply(loanClean$home_ownership,factor)
loanClean$verification_status <- sapply(loanClean$verification_status,factor)
loanClean$loan_status <- sapply(loanClean$loan_status,factor,levels=c("Charged Off","Fully Paid","Current")) # Setting the order of the values in factor to get a meaningful plot.
loanClean$purpose <- sapply(loanClean$purpose,factor)
loanClean$addr_state <- sapply(loanClean$addr_state,factor)
loanClean$int_rate<-as.numeric(gsub("%","",loanClean$int_rate)) # removing the % from the value & setting to number for handling in the ggplot as continuous variable. 
loanClean$revol_util<-as.numeric(gsub("%","",loanClean$revol_util)) # removing the % from the value & setting to number for handling in the ggplot as continuous variable. 
loanClean$fac_inq_last_6mths <- sapply(loanClean$inq_last_6mths,factor,levels = c(0,1,2,3,4,5,6,7,8)) # Setting the order of the values in factor to get a meaningful plot.
str(loanClean)

########################################
# Checkpoint 4: Derived Variables
########################################

# Add Derived Variables
loan <- loanClean

# Quartile creation based on funded amount. This will help to analyze various parameters based on quartiles. 
loan <- within(loan, RangeFundedAmt <- as.integer(cut(funded_amnt, quantile(funded_amnt, probs=0:4/4), include.lowest=TRUE)))

# converting teh quartiles of funded amount to factors. 
loan$RangeFundedAmt <- sapply(loan$RangeFundedAmt,factor,levels=c(1,2,3,4))

# Deriving the loss amount for investors in each investment. This will be used in analyzing the charged off loans. 
loan$lostAmount <- round(loan$funded_amnt -loan$total_rec_prncp,digits = 0)

# Deriving the loss % in each investment. This will be used in analyzing the charged off loans. 
loan$lossPerc <- (loan$lostAmount/loan$funded_amnt)*100

# Deriving the range for revolving utilization % for each borrower.
loan$revol_util_range <- factor(round(loan$revol_util/10.00,digits = 0)*10)

# Deriving a numeric grade variable for use in Correlation plot. 
loan$gradeNum <- as.numeric(as.factor(loan$grade))
summary(loan$RangeFundedAmt)
str(loan)

########################################
# Checkpoint 5: Simple Plots & Analysis 
########################################

###### Histogram ######

#Plot Showing the distribution of loans by Interest rates with grade identification.
plotIntRate <- ggplot(loan, aes(x = int_rate)) + geom_histogram(aes(fill = grade),binwidth = 1) 
plotIntRate + facet_wrap(~loan_status, ncol = 1) + labs(x = "Interest Rate",y = "Number of Loans", title = "Issued Loans by Interest Rate with grade classification & faceted by Loan Status. ")
## Analysis: From the plot we can see that there are more loans issues for borrowers in grades in A, B & C. In the charged off loans,we can see a small up-tick from B through E.

#Plot Showing the distribution of loans by Interest rates with grade identification and Loan Term.
plotIntRate + facet_wrap(~term, ncol = 1) + labs(x = "Interest Rate",y = "Number of Loans", title = "Issued Loans by Interest Rate with grade classification & faceted by Loan Term. ")
## Analysis: The Plot shows overall higher percentage of loans issues in 36 month term. However, for the grade level E, F & G, we see comparatively higher % of loans issued in 60 month term. 

#Plot Showing the distribution of loans by grade & Loan status. 
plotGradeNum <- ggplot(loan,aes(x = gradeNum)) + geom_histogram(aes(fill = loan_status),binwidth = 1) 
plotGradeNum + facet_wrap(~term, nrow = 1) + labs(x = "Grade in Numeric (e.g A=1, B=2 etc.,",y = "Number of Loans", title = "Issued Loans by grade classification & faceted by Loan Term. ")
## Analysis: Same as previous plot. higher number of loans in 36 months. However, lower grades are having a higher % of loans in 60 months compared to 36 months. 

#Plot Showing the distribution of charged off loans by Interest Rate & faceted by grade.
plotChrgOffGrade <- ggplot(loan[loan$loan_status == "Charged Off",], aes(x = int_rate)) + geom_histogram(aes(fill = loan_status),binwidth = 1) 
plotChrgOffGrade + facet_wrap(~grade, ncol = 4) + labs(x = "Interest Rate",y = "Number of Loans", title = "Charged Off Loans by Interest Rate & faceted by grade. ")
# Analysis: We see that the number of charged off loans are high even for grades B, C, D & E. This may be due to higher # of loans issues on those grades as we saw in the previous plots. 

#Going to the next level of detail, Plot Showing the distribution of loans by Interest Rate & faceted by grade + Loan Status.
plotLoanStatusGradeFaceted <- ggplot(loan, aes(x = int_rate))+ geom_histogram(aes(fill = loan_status),binwidth = 1) 
plotLoanStatusGradeFaceted + facet_wrap(loan_status~grade, ncol = 3) + labs(x = "Interest Rate",y = "Number of Loans", title = "Issued Loans by Interest Rate & faceted by grade & Loan Status ")
# Analysis: This shows that our understanding from previous plot is partially correct. Because for Grade D, E & F, the ratio of charged off loan compared to fully paid seems to be high. 

#Plot Showing the distribution of charged off loans by funded amount & faceted by quartile of Funded amount.
loanPlotQuartile <- ggplot(loan[loan$loan_status == "Charged Off",],aes(x=funded_amnt,group = factor(RangeFundedAmt)))
loanPlotQuartile + geom_histogram(binwidth = 500) + facet_wrap(~RangeFundedAmt, ncol = 1) + labs(x = "Funded Amount",y = "Number of Loans", title = "Charged Off Loans by funded amount & faceted by quartile of funded amount.")
# Analysis: We can see a higher count of loans in 4th quartile that are charged off. This will increase the net loss for investor as they are funded with a larger investment. 

#As a next step introducing the faceting by grade & funded amount quartile. 
loanPlotQuartile + geom_histogram(binwidth = 500) + facet_wrap(grade~RangeFundedAmt, ncol = 4) + labs(x = "Funded Amount",y = "Number of Loans", title = "Charged Off Loans by funded amount & faceted by quartile of funded amount & Grade.")
# Analysis: There is a higher number of charged off loans in 3rd & 4th quartiles across the grades B, C, D & E. For Grade A, there is less number of charged off loans in 4th quartile. 
#           For Grade F, there is a higher number of loans charged off in 4th quartile of funded amount. 
#           Grade G does not show this pattern. This may also be due to lower probability of Grade G borrower getting a loan funded.

###### Bar Plot ######

#Analysis based on home ownership
mort_df <- loan[loan$home_ownership %in% c("MORTGAGE", "OWN", "RENT"),] 
g_mort <- ggplot(mort_df, aes(grade))
g_mort + geom_bar(aes(fill = grade)) + facet_wrap(~home_ownership,ncol = 1) + labs(x = "Grade",y = "Number of Loans", title = "Issued Loans of Different Home Ownership & Grade") + theme_bw()
# Analysis: We can see that higher number of loans are issued for borrowers who Rent or Mortgage. 
#           Also there is a higher number of borrowers in Grade A in Mortgage compared to rent. 
#           The borrowers who rent a home are more concentrated in Grade B & C when compared to mortgage. 
#           Thus borrowers who mortgage are having a higher chances to be in better grade A.
g_mort + geom_bar(aes(fill = grade)) + facet_wrap(loan_status~home_ownership) + labs(x = "Grade", y = "Number of Loans", title = "Issued Loans of Different Home Ownership & Grade by Loan Status") + theme_bw()
# Analysis: Analysis from previous plot holds good. However, there in the current loans, we see a higher concentration of Borrowers who mortgage vs Rent. 

########################################
# Checkpoint 6: Complex Plots & Analysis 
########################################

###### Box Plot ######
# Interest Rate Distribution by Grade & Ranged Utilization percentage. 
g_tig <- ggplot(loan, aes(grade, int_rate))
g_tig + geom_boxplot(outlier.size = 1, color = "blue") + facet_wrap(~revol_util_range,nrow = 2) + labs(title = "Interest Rate Distribution by Grade faceted by Utilization percentage", x = "Grade", y = "Interest Rate(%)") + theme_bw()
# Analysis: It shows the Interest rate vs grade is constant across utilization percentage. 

g_tig + geom_boxplot(outlier.size = 1, color = "blue") + facet_wrap(~loan_status,nrow = 1) + labs(title = "Interest Rate Distribution by grade faceted by Loan Status", x = "Loan Status", y = "Interest Rate(%)") + theme_bw()
# Analysis: Comparing Charged off  & Fully paid in the plot, the median Interest rate remains the same for grades A, B, C & D. However, for Grades E & F, the median Interest rate is high for charged off loans. 

# Interest Rate Distribution by Grade & Ranged Utilization percentage. 
g_tig2 <- ggplot(loan, aes(loan_status, int_rate)) +geom_boxplot()
g_tig2 + geom_boxplot(outlier.size = 1, color = "blue") + facet_wrap(~inq_last_6mths,nrow = 3) + labs(title = "Interest Rate Distribution by Loan Status faceted by Utilization Percentage range", x = "Loan Status", y = "Interest Rate(%)") + theme_bw()
# Analysis: We can see a higher median Interest rate % in charged off loans.
#           We can also see that Lending Club has reduced investments its current loans for borrowers with higher range in utilization percentage. 

# Interest Rate Distribution by Grade & Ranged Utilization percentage. 
g_tig3 <- ggplot(loan[loan$loan_status == "Charged Off",], aes(grade, lostAmount)) +geom_boxplot() # getting data onluy for charged off loans since the Lost amount is only relevant for this category. 
g_tig3 + geom_boxplot(outlier.size = 1, color = "grey") + facet_wrap(~home_ownership,nrow = 3) + labs(title = "Lost Investment by Grade & Home Ownership", x = "Grade", y = "Lost Investment") + theme_bw()
# Analysis: Overall median loss amount is high for borrowers who Mortgage in Grade G
#           Median loss is very high in Home owners who have G grade. 
g_tig3 + geom_boxplot(outlier.size = 1, color = "grey") + facet_wrap(~RangeFundedAmt,nrow = 2) + labs(title = "Lost Investment by Grade & Range Of Funded Amount", x = "Grade", y = "Lost Investment") + theme_bw()
# Analysis: Average loss is low in 1st & 2 quartile of funding amount . It shows a growth in 3rd quartile of funding amount. 
#           In the 4th quartile, it increased significantly showing higher net loss for investors in this segment. 

# Loss percentage Distribution by Grade & Home ownership. 
g_tig4 <- ggplot(loan[loan$loan_status == "Charged Off",], aes(grade, lossPerc)) +geom_boxplot()
g_tig4 + geom_boxplot(outlier.size = 1, color = "red") + facet_wrap(~home_ownership,nrow = 3) + labs(title = "Loss Percentage by Grade & Home Ownership", x = "Grade", y = "Loss Percent") + theme_bw()
# Analysis: Comparing only Rent, Own & Mortgage, we see that renters perform better compared to others. 
#           Loss percentage in home owners are skewed across grade while comparing with Renters or Mortgaged borrowers. 
g_tig4 + geom_boxplot(outlier.size = 1, color = "red") + facet_wrap(~RangeFundedAmt,nrow = 2) + labs(title = "Loss Percentage by Grade & Funded amount quartiles", x = "Grade", y = "Loss Percent") + theme_bw()
# Analysis: Loss percentage pattern is similar across all funding amount quartiles. 1st quartile shows a minor decrease in loss percent in grade B. 

###### Correlation Plot ######

# Plot to identify correlation between a selected set of variables from the data frame. 

cor_var_name = c( "loan_amnt", "int_rate", "installment", "annual_inc", "loan_status",
                  "dti", "open_acc", "total_acc", "sub_grade","mths_since_last_delinq",
                  "revol_bal","revol_util","total_acc","total_pymnt_inv","total_rec_prncp",
                  "total_rec_int","total_rec_late_fee","lossPerc") # identify the variables for finding correlation. 
cor_var <- select(loan, one_of(cor_var_name))
cor_var <- cor_var[complete.cases(cor_var), ]  # remove incomplete cases
cor_var$num_subgrade <- as.numeric(as.factor(cor_var$sub_grade)) # Converting to numeric to find correlation
cor_var$num_status <- as.numeric(as.factor(cor_var$loan_status)) # Converting to numeric to find correlation
cor_var <- select(cor_var, -sub_grade,-loan_status) # Removing the factor fields since we have created an equivalent numeric field above. 
M <- cor(cor_var)  # transfer to matrix 
summary(cor_var)
ggcorr(M) # Correlation plot creation.

# Analysis: Higher correlation between the following key elements. (Note: This does not include the variables that are expected to have a higher correlation liek loan amount vs installment)
#           dti is not correlated with loan status (or) Interest rate
#           Principle received has a minor correlation with Loan Status
#           Interest Rate is correlated with Grade. 

###### Pie Chart ######

# Plot to show Charge Off percentage across Grades. 

plotPie <- ggplot(data=loan, aes(x=factor(1), stat="bin", fill=loan_status)) + geom_bar(position="fill")
plotPie <- plotPie + ggtitle("Percentage of Loans Charged Off by Grade") + xlab("") + ylab("Grade") 
plotPie <- plotPie + facet_wrap(~grade,ncol = 3) # Side by side bar chart
plotPie + coord_polar(theta="y") # side by side pie chart
# Analysis: The Percentage of loans charged off increases with the grade of the borrower. 

# Plot to show Charge Off percentage by Home Ownership

plotPie2 <- ggplot(data=loan, aes(x=factor(1), stat="bin", fill=loan_status)) + geom_bar(position="fill")
plotPie2 <- plotPie2 + ggtitle("Percentage of Loans Charged Off by home ownership") + xlab("") + ylab("Grade") 
plotPie2 <- plotPie2 + facet_wrap(~home_ownership,ncol = 3) # Side by side bar chart
plotPie2 + coord_polar(theta="y") # side by side pie chart
# Analysis: The Percentage of loans charged off is same across various HOme ownerships. 

# Plot to show Loan Term Preference by Grade

plotPie3 <- ggplot(data=loan, aes(x=factor(1), stat="bin", fill=term)) + geom_bar(position="fill")
plotPie3 <- plotPie3 + ggtitle("Loan Term by Grade") + xlab("") + ylab("Grade") 
plotPie3 <- plotPie3 + facet_wrap(~grade,ncol = 3) # Side by side bar chart
plotPie3 + coord_polar(theta="y") # side by side pie chart
# Analysis: As the grade decreases (A through G), Loan term preference increases to 60 months. 

# Plot to show Charge Off percentage by verification status

plotPie4 <- ggplot(data=loan, aes(x=factor(1), stat="bin", fill=loan_status)) + geom_bar(position="fill")
plotPie4 <- plotPie4 + ggtitle("Loan Status by Verification Status") + xlab("") + ylab("Verification Status") 
plotPie4 <- plotPie4 + facet_wrap(~verification_status,ncol = 3) # Side by side bar chart
plotPie4 + coord_polar(theta="y") # side by side pie chart
# Analysis: No Significance change in charge off percentage by Verification status. 

###### Stacked Bar to show percentage of total ###### 

#Plot to show the charge off percentage by revolving utilization range & grade. 
plotRevutilStack <- ggplot(data=loan, aes(x=revol_util_range, stat="bin", fill=loan_status)) + geom_bar(position="fill")
plotRevutilStack + ggtitle("Charge Off percentage by revolving Utilization Range & Grade") + xlab("Revolving Utilization Range") + facet_wrap(~grade,nrow = 3) + ylab("Percentage") 
# Analysis: We see that in Grade A, as the revolving utilization percentage increases, the charge off percentage increases. 
#           For Grade F & G, the data is skewed. But the charge off percentage is highest for largest Utilization percentage. 

# Plot to show the charge off percentage by revolving utilization range
plotRevutilStack + ggtitle("Charge Off percentage by revolving Utilization Range") + xlab("Revolving Utilization Range") + ylab("Percentage") 
# Analysis: As the revolving utilization percentage increases, charged off loan percentage (or) risk of charge off increases. 

# Plot to show the charge off percentage by revolving utilization range and faceted by Home ownership
plotRevutilStack + ggtitle("Charge Off percentage by revolving Utilization Range & Home Ownership") + xlab("Revolving Utilization Range") + facet_wrap(~home_ownership,nrow = 3) + ylab("Percentage") 
# For Renters, Home owners & Mortgaged home owners, the trend remains the same. As the revolving utilization percentage increases, the charged off percentage increases. 


# Plot to show the charge off percentage by Employee length
plotEmpLenStack <- ggplot(data=loan, aes(x=emp_length, stat="bin", fill=loan_status)) + geom_bar(position="fill")
plotEmpLenStack + ggtitle("Charge Off percentage by Employee Length") + xlab("Employee Length") + ylab("Percentage") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Analysis: Charge off percentage of loans is higher for borrowers for whom this data is unavailable. . 

# Since Zip Code cannot be used, Plot to show the charge off percentage by State.
plotStateStack <- ggplot(data=loan, aes(x=addr_state, stat="bin", fill=loan_status)) + geom_bar(position="fill")
plotStateStack + ggtitle("Charge Off percentage by State") + xlab("State") + ylab("Percentage") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Analysis: Charge off percentage is high in NE. But the number of loans issues is also low. 

#Plot to show the charge off percentage by Grade
plotLoanStatGradeStack <- ggplot(data=loan, aes(x=grade, stat="bin", fill=loan_status)) + geom_bar(position="fill")
plotLoanStatGradeStack + ggtitle("Loan Status Percentage by Grade") + xlab("Grade") + ylab("Percentage") 
# Analysis: As grade decreases (A through G), the charge off percentage increases.

#Plot to show the charge off percentage by number of inquiries in last 6 months
plotInqLast6mths <- ggplot(data=loan, aes(x=fac_inq_last_6mths, stat="bin", fill=loan_status)) + geom_bar(position="fill")
plotInqLast6mths + ggtitle("Charge off percentage by Inquiries in last 6 Months") + xlab("Inquiries in Last 6 Months") + ylab("Percentage") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Analysis: Plot shows that there is no strong relation between the Inquiry count in last 6 months vs the charged off percentage. 
#           However, the charged off percentage gradually increases by number of Inquiry count in last 6 months. 

# Plot to show the charge off percentage by loan purpose and faceted by Grade
plotPurposeStack <- ggplot(data=loan, aes(x=purpose, stat="bin", fill=loan_status)) + geom_bar(position="fill")
plotPurposeStack + ggtitle("Charge Off percentage by Purpose of Loan") + xlab("Purpose of Loan") + ylab("Percentage") + facet_wrap(~grade,ncol = 2) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Analysis: Charge off percentage of loans is higher for Small business purpose across the board. 
#           In Grade G, loans for renewable energy & medical are the highest risk. 
#           In grades A through C, loans for purpose of renewable energy are at a higher risk. 