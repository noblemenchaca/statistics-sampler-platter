##If you remember from previous labs, bootstrapping allows us to make conclusions about the population from a small sample.  We went over the process of doing bootstrapping by hand in detail before the test.  It included these basic steps:

#1) resampling multiple times.
#2) finding a suitable measure of central tendency/ statistic of interest from all the new samples you created

#( in R we do steps 1 and 2 with the 3 step replication process: sample(), brackets and semicolons between lines, replication() )

#3) put all these mct/si from your samples into a distribution.

#The true mct/soi most likely lies inside this distribution if you have a good sample. If your sample is crappy then this is less likely and your bootstrapping is useless which is why sampling procedure is so important!!
  
#  ( in R we would this by putting our replication code into a variable, then using hist() to visualize this distribution)

#4)find a confidence interval

#( in R we do this with the quantile() function. You can visualize the ci on your histogram using the abline() function)

#5) if the value specified in your hypothesis is outside your confidence interval, you can reject your hypothesis

##Bootstrapping in R is very similar, except you will need to do some of the steps slightly out of order since we have to conform to RStudio.  I have 3 examples of bootstrapping for you in this script file.

#######################################################################################################################################################
#Example 1:

#we will be using the mtcars dataset for this example which is built into Rstudio.

View(mtcars)##allows you to visualize the dataset.

#Suppose we want to know if cars with 4 gears have higher mpg on average than do other types of cars.  We can examine this problem by using bootstrapping.
#First things first, you need to figure out your null and alternative hypotheses with which you will be working.

#remember, your alternative hypothesis is just the original hypothesis restated with the average being more specific to the dataset you are working with.
#in other words you need to figure out what measure of central tendency you will be using for your bootstrapping.

##To figure this out you will need to look at the data you will be using.  In this case we know we are looking at the mpg and the gear numbers.
##What columns in the dataset are involved?

#mtcars$gear 
#and
#mtcars$mpg

##since we want to compare the mpg for 4 gears against all other gears you can use subsets to pick out these two groups

four=subset(mtcars, gear==4)##remember that you must use == for T/F statements
other=subset(mtcars, gear!=4)#remember that ! means not?

##once you get the 2 gear groups, you can look at the mpg of the two groups using boxplots to identify if there are any outliers

fourmpg=four$mpg
othermpg=other$mpg

boxplot(fourmpg)
boxplot(othermpg)

## as you can see from these boxplots, there is an outlier, (the lone dot outside the whiskers) present in the other group.  This means we should use the median as our measure of central tendency for our hypotheses.

##Here are the resultant null and alternative hypothesis:


# Alternative Hypothesis: The median mpg of the cars that have 4 gears is more than the mpg of other cars. mpg(4gear)> mpg(othergear)
# Null: The median mpg of cars with 4 gears is the same or less than the other cars.  mpg(4gear)<=mpg(othergear)

##This is a directional hypothesis which determines which side our confidence interval will be placed on.  Since the null hypothesis arrow looks like this <= we know that it points to the side on which the CI is absent.  So our CI will run from the .05 to the 1.00 quantiles



##So now that we have figured our our hypotheses we can start on the bootstrapping procedure.

####Step 1: Finding your statistic of interest with the raw data.
##First you need to figure out how to get a number that will answer your hypothesis, or a Measure of central tendency/ statistic of interest if you will:
#our question is mainly about the difference between mpg of 4 gear cars and other cars.
#so our Statistic of interest would be:

diff=median(fourmpg)-median(othermpg)

##if we get a positive number as a result, we know that four cylinder cars have higher mpgs, if we get a negative number we know that other mpg's have a higher median mpg.


#######Step 2: resampling
##Now we can resample using a sample() and replicate() function as we learned in previous labs.  Remember the 3 step process? 1. sample() 2. semicolons between lines and brackets around all arguments, 3. replicate()

###########################R1.  Come up with the things you will be resampling from and how you will create your statistic of interest.  In this case you'll be resampling from your 2 groups of mpgs, then looking at their differences.

fourmpgs=sample(four$mpg, replace=T); ##This resamples from the mpgs of cars with 4 cylinders
othermpgs=sample(other$mp, replace=T);##This resamples from the mpgs of cars with other numbers of cylinders
diffs= median(fourmpgs)-median(othermpgs)##This gives you the difference between your resamples.  This is what numbers will fill your histogram/new distribution

##Make sure that when you resample, to use replace=T otherwise you will get an empty histogram/ a vector of the same value repeated multiple times!

###########################R2.# put semicolons between lines and brackets around the whole thing.


{fourmpgs=sample(fourmpg, replace=T); 
othermpgs=sample(othermpg, replace=T);
diff= median(fourmpgs)-median(othermpgs)}


####Step 3: Make a new distribution of your statistic of interest:
###########################R3. Use a replicate function to create your new distribution.  10000 replicates is usually a pretty safe bet.
distofdiff=replicate(10000, {fourmpgs=sample(fourmpg, replace=T);  
                               othermpgs=sample(othermpg, replace=T);
                               diffs= median(fourmpgs)-median(othermpgs)})

##Once you have completed this step, make a histogram to make sure you didn't make any mistakes and came up with a valid distribution.

hist(distofdiff, main= "MPG Difference between  4 gears other numbers of gears", freq=F, xlab= "median(4gears)-median(othergears)")
##Now you can see the distribution of possible differences.

####Step 4: Find your confidence interval.

#Do this using the quantile() function

(CI= quantile(distofdiff, c(0.05, 1)))

##This should give you values froma round 3.5 to 17.4.
##You should visualize these lines on your histogram using the abline function.

abline(v=CI, lwd=2, col="blue")

##At this point you can make a statement about the confidence interval that sounds like this:

#There is 95% confidence that the true difference between the median mpg of 4 geared cars and other cars is between 3.5 and 17.4.

####Step 5: Reject or fail to reject your null hypothesis.

#Look back at your old NULL hypothesis.
# Null: The median mpg of cars with 4 gears is the same or less than the other cars.

##since our distribution is median(4gears)- median(othergears)
##Looking at this null hypothesis there are two types of numbers you will look for:
##Numbers indicating that the 4 gear cars have the same mpg as other cars-----  this would be represented by the number zero
##Numbers indicating that the 4 gear cars have lower mpg than other cars----- this would be represenetd by negative numbers.

##Are either of these numbers within our confidence interval?

#No.
#This means we can reject our Null hypothesis!!!

##An important note though, even if you reject your Null hypothesis, you CAN NEVER accept your alternative hypothesis.  You can only state that your rejected your null hypothesis (with 95% confidence if that is the CI you used) and that your alternative hypothesis is supported by this result.

####WE NEVER ACCEPT ANYTHING!####


##################################################################################################################################################
#Example 2:

##A former student in ISTA 116 took a survey of GPAs for honors and non- honors students.  
##Here are the two vectors of data she gathered.
hnrs = c(3.286,3.57,4,4,3.5,3.7,4,3.5,3,3.56,4,3.12,3.4,3.4,3,4,3.4,3)##Honors GPAs
nothnrs = c(3.5,3.8,4,3.8,3.4,3.1,4,3.9,3.9,3.8,3.3)##NonHonors GPAs

##She thinks the honors college is a bunch of baloney and wants to see if honors students have lower GPAs than non-honors students.  To narrow down her hypotheses she needs to visualize the data.

##Visualize the data using boxplots
boxplot(hnrs)
boxplot(nothnrs)

##These boxplots have no outliers.  Which measure of central tendency should we use?

##And our resultant null and alternative hypotheses?

#Ho: The  x GPA of honors students is y than the x GPA of non-honors students    
#Ha:  x GPA of honors students is y than the x GPA of non-honors students

##Now how will we calculate the answer to this question? What will be our statistic of interest?
##The difference between Honors and non-honors right?
# In R that looks like:

diff=mean(hnrs)-mean(nothnrs)

##Finally we can begin bootstrapping.

##Step 1: Resampling

##################G1 and G2: sample(), semicolons& brackets
(gpa_diff = mean(sample(hnrs, 50, replace=T)) - mean(sample(nothnrs,50, replace = T)))##See here that we were able to turn our resampling procedure into a single line of code.  This does the same work as the three lines of code used in the mtcars example.

################G3: replicate()

replicate(10000, {gpa_diff = mean(sample(hnrs, replace=T)) - mean(sample(nothnrs, replace = T))})

##Step 2: create a new distribution of the statistic of interest (store your replicate function in a variable)
gpa_diff_rep = replicate(10000, {gpa_diff = mean(sample(hnrs, replace=T)) - mean(sample(nothnrs, replace = T))})

##Now use hist() to visualize your new distribution of differences
hist(gpa_diff_rep, freq=F, main="Density Histogram of GPA Differences", xlab=  "Differences in GPA")

##Make sure to name your histogram so you know what it shows!!


##Step 3:  Find your confidence interval (95%)
##Is this a one or 2 tailed test?  
#look at your null hypothesis
#Ho: The  mean GPA of honors students is  greater than or equivalent to the mean GPA of non-honors students 
#Convert it to a logical statement:  
#mean(hnrs)>=mean(nothnrs)
#Where does the arrow point?  to the left,  so we know it is one tailed, with the confidence interval running from 0.00 to 0.95 quantiles


(lower <- quantile(gpa_diff_rep, 0)) #5% percentile
(upper <- quantile(gpa_diff_rep, 0.95)) #1 percentile
(conf_int <- c(lower, upper))

#Now put the confidence interval onto the histogram. Notice that the 95% CI is near one end of the distribution because it is 1 tailed. 
abline(v = conf_int, col = "red", lwd=4)

##Our statement about the CI:  There is 95% confidence that the true difference between the mean GPA of honors students and the mean GPA of non-honors students falls between [interval1] and [interval2]

##Step 4: Reject or Fail to reject your null hypothesis

##So can we reject our null hypothesis?
##Look at it again:
##Ho: The  mean GPA of honors students is  greater than or equivalent to the mean GPA of non-honors students
##Look at your Statistic of interest: 
##diff=mean(hnrs)-mean(nothnrs)

##If mean gpa of honors is greater, what would our statistic of interest look like?
#### It would be positive right?
##If mean gpa is equivalent, what would our statisticc of interest look like?
###IT would be zero.

##Do either positive or zero values lie inside our CI?

####YES.  
##so we CANNOT reject our null hypothesis!!!

#####################################################################################################################################################
##Example 3:

##We will be using the chickwts dataset. It shows the weights of chicks after eating each different kind of feed.
View(chickwts)
##We are are statistician farmer who currently feed our chicks linseed.  A traveling salesman comes buy to try and sell us casein feed with the claim that  60% of our chicks will gain at least 100 ounces more with the new feed than with our original linseed.

##Based on this claim what are our null and alternative hypotheses?
#HA: 60% of chicks will gain 100 ounces or more with the new casein feed  
#HO: 60% of chicks will gain less than 100 ounces with the new casein feed 

##Now find your statistic of interest.  Since we are looking for a difference between casein weights and linseed weights:
cas=subset(chickwts, feed== "casein")$weight##Weights on casein
lin= subset(chickwts, feed== "linseed")$weight##Weights on linseed
growth=cas-lin

##Now since we were supposed to be seeing how 60% of our chicks fared, how would we do this in R?
###Answer: Use the quantile function!
##The quantile function gives us the value where data lies at or below a specified  quantile.

quantile(growth, .4)

##So this value can be interpreted as the value at which 60% or more of the data lies above.
##So we could say "60% of the chicks gained more than 134 ounces"

##But since we are a statistician farmer we want to know if the advertisers claim was true to the entire population not just for ourself.  We can do this using bootstrapping.

#Resample:

distofgrowth=replicate(10000, {growths=sample(growth, replace=T); q=quantile(growths, 0.4)}) 

#find confidence interval

ci= quantile(distofgrowth, c(0.975, 0.025))
ci

#Visualize with histogram and plotted Ci

hist(distofgrowth, main="Growth of casein from linseed", freq=F)
abline(v=ci)

##Looking at the Confidence interval, can we prove the advertiser incorrect in his assertion?

##No we cannot because values at and above 100 lie inside the CI.

##Can we prove the advertiser correct?

##No we cannot because values below 100 also lie within the CI.










