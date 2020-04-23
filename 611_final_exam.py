from random import sample
class Question:
    def __init__(self, prompt, answer, topic):
        self.prompt = prompt
        self.answer = answer
        self.topic = topic

descriptive_questions = [
    "What measure of central tendency would be used to describe a nominal variable? "
    "\n (a) Mean \n (b) Median \n (c) Mode \n (d) Range \n",
    "Suppose that you make a psychological scale and the psychological difference between a rating of 4 and 5 is much \n "
    " larger than the difference between a rating of 2 and 3, what kind of scale have you made? "
    "\n (a) invalid \n (b) ordinal \n (c) equal interval \n (d) ratio \n",
    "An example of a ratio variable would include: "
    "\n (a) Weight \n (b) Grade (A-F) \n (c) Color \n (d) All of the above \n (e) None of the above \n",
    "The mode of a distribution can be "
    "\n (a) the average of all the values in the distribution \n (b) the value(s) that are most frequent"
    " in the distribution \n (c) the middle value in the distribution \n (d) all of the above \n",
    "What is not an acceptable way to resolve outliers in one's data? "
    "\n (a) Replace them with a less extreme score \n (b) Perform a nonlinear transformation of the data"
    " \n (c) Covert them to z-scores \n (d) Exclude them from any statistical analyses \n",
    "Which of the following transformations would preserve the original shape of the distribution of scores? "
    "\n (a) Calculating the inverse of each score \n (b) Finding the logarithm of each score \n "
    "(c) Adding 3 to each score \n (d) Taking the square root of each score \n",
    "A constant is when there is: "
    "\n (a) High variance \n (b) Low variance \n (c) No variance \n (d) none of the above \n",
    "_______ statistics involve collecting, organizing, and summarizing data. "
    "\n (a) Experimental \n (b) Descriptive \n (c) Inferential \n (d) Organizational \n",
    "Suppose you are told that a list of the weights of 25 persons (measured in pounds) follows the normal \n "
    "curve and that the range of the numbers in the list is 48 pounds. Which of the following is the most likely \n "
    "to be the standard deviation in the list? "
    "\n (a) 4 pounds \n (b) 8 pounds \n (c) 24 pounds \n (d) 100 pounds \n",
    "If your raw score on the SAT (M = 1000, s = 100) was converted to a z-score of 2.5, then your raw score \n"
    "was ______ and your percentile rank was _____. "
    "\n (a) 1205, 95th \n (b) 1250, 99th \n (c) 1300, 98th \n (d) 1350, 99th \n",
    "When a Z score is zero, "
    "\n (a) its raw score is zero \n (b) its raw score is equal to the mean of its distribution"
    " \n (c) its distribution is normal \n (d) this is a trick question; you can't get a Z score of zero \n",
    "If the area under the normal curve between two corresponding positive and negative z-scores is \n"
    "15.85%, then your z-score is? "
    "\n (a) .40 standard deviations away from the mean \n (b) .20 standard deviations away from the mean "
    "\n (c) .40 raw scores away from the mean \n (d) .20 raw scores away from the mean \n",

]

test_theory_questions = [
    "In Classical Test Theory, x = t + e. What does 'x' represent? "
    "\n (a) the mean of values representing the latent construct \n (b) the observed value"
    " \n (c) the assumed value of the latent construct \n (d) the error associated with measuring the latent"
    "construct \n",
    "True or False: Validity is consistency and reliability is accuracy"
    "\n (a) True \n (b) False \n",
    "When giving a test at two time points, reliability is increased "
    "\n (a) With a shorter amount of time between them \n (b) With a longer amount of time between them"
    " \n (c) When the pre-test and post-test are different \n (d) a & c \n",
    "True or False: More items on an assessment will decrease Cronbach's Alpha "
    "\n (a) True \n (b) False \n",
    "To ask if survey items represent all relative components of a construct would be measuring: "
    "\n (a) Content Validity \n (b) Face validity \n (c) Test-retest Validity \n (d) Criterion Validity \n",
    "Conceptually, reliability is "
    "\n (a) the total obtained variance minus the error variance \n (b) the true variance minus the error variance"
    " \n (c) the proportion of error variance out of the total obtained variance \n (d) the proportion of true"
    "variance out of the total obtained variance \n",
    "True or False: The Coefficient Alpha estimates internal reliability "
    "\n (a) True \n (b) False \n",
    "In order for scores to be ____, they must be _____. "
    "\n (a) interval; ratio \n (b) valid; reliable \n (c) reliable; valid \n (d) reliable; predictable \n",
]

hypothesis_testing_questions = [
    "If a study estimated the population mean reaction time to be 300 milliseconds (ms) with a 95% \n"
    "confidence interval of 10ms, what does this mean? (choose the best answer) "
    "\n (a) there is a 95% chance that each reaction time will be between 290ms and 310 ms"
    "\n (b) there is a 5% chance that each reaction time will be exactly 300ms"
    "\n (c) there is a 5% chance that the population mean is <290ms, and a 5% chance that it is >310ms"
    "\n (d) there is a .95 probability this sample mean generates a confidence interval (290ms and 310ms) \n"
    "where the population mean falls between \n",
    "If we reject the null hypothesis based on the available data when in fact the null hypothesis is really true, \n"
    "we will be committing a "
    "\n (a) Type I error \n (b) Type II error \n (c) Null Hypothesis \n (d) Alternative Hypothesis \n",
    "If the null hypothesis is rejected using a one-tailed test, then by default it would be rejected using a \n"
    "two-tailed test"
    "\n (a) True \n (b) False \n",
    "What is meant by a 'spurious' relationship between two variables? "
    "\n (a) One that is so illogical it cannot possibly be true "
    "\n (b) An apparent relationship that is so curious it demands further attention"
    " \n (c) A relationship that appears to be true because each variable is related to a third one"
    " \n (d) One that produces a perfect negative correlation on a scatter diagram"
    " \n (e) All of the above \n",
    "A researcher is conducting a taste test to see whether there is a difference in people's liking of Coca Cola \n"
    "versus a generic cola. She first has each person sip Coca Cola and rate how much they liked it. \n"
    "She then has people do the same for the generic cola. Which of the following terms best describes \n"
    "the research design she used for this study?"
    "\n (a) Between subjects"
    "\n (b) Within subjects"
    "\n (c) Between groups"
    "\n (d) Randomization \n",
    "If a result is statistically significant, it cannot be due to chance."
    "\n (a) True"
    "\n (b) False \n",
    "Ms. Landry has set a very conservative alpha level of .001 for her analysis. She is likely trying to avoid: "
    "\n (a) a Type I error"
    "\n (b) a Type II error"
    "\n (c) standard error"
    "\n (d) multicollinearity \n",
    "As effect size increases:"
    "\n (a) power increases"
    "\n (b) power decreases"
    "\n (c) alpha increases"
    "\n (d) alpha decreases "
    "\n (e) a & d "
    "\n (f) b & c"

]

regression_questions = [
    "A decrease in X that corresponds with a decrease in Y is an example of ___ covariance "
    "\n (a) Positive \n (b) Negative \n (c) Inverse \n (d) Residual \n",
    "If the sum of squares due to the regression model (SSM) is 60, which of the following must be true? "
    "\n (a) the coefficient of correlation is .6 \n (b) the coefficient of correlation is .36"
    " \n (c) the total sum of squares (SST) is at least 60 \n (d) the y-intercept is positive \n"
    "(e) the slope, b, is positive \n (f) the coefficient of determination is .77",
    "The strength of r is affected by: "
    "\n (a) restricted range of scores \n (b) the presence of outliers \n "
    "(c) differences in the distribution shapes of X and Y  \n (d) all of the above \n",
    "The residual error is to the regression line as the SD is to the _____."
    "\n (a) variance \n (b) mean \n (c) slope \n (d) predicted score \n",
    "The goal of linear regression is to improve prediction of Y with information about X. "
    "\n (a) True \n (b) False \n ",
    "When interpreting a correlation coefficient, it is important to look at: "
    "\n (a) the significance of the correlation coefficient \n (b) the magnitude of the correlation coefficient"
    " \n (c) the +/- sign of the correlation coefficeint \n (d) all of the above \n",
    "The relationship between two variables controlling for the effect that a third variable has on one of those \n"
    "variables can be expressed using a: "
    "\n (a) Semi-partial correlation \n (b) Bivariate correlation \n (c) Point-biserial correlation"
    " \n (d) Partial correlation \n",
    "Which of the following statements about the t-statistic in regression is not true? "
    "\n (a) the t-statistic provides some idea of how well a predictor predicts the outcome variable"
    "\n (b) the t-statistic tests whether the regression coefficient, b, is equal to 0"
    "\n (c) the t-statistic is equal to the regression coefficient divided by its standard deviation"
    "\n (d) the t-statistic can be used to see whether a predictor variable makes a statistically significant \n"
    "contribution to the regression model \n",
    "Which of the following statements about the F-ratio is true? "
    "\n (a) the F-ratio is the ratio of error variance to the total variance "
    "\n (b) the F-ratio is the ratio of variance explained by the model to the total variance in the outcome variable "
    "\n (c) the F-ratio is the ratio of variance explained by the model to the error in the model "
    "\n (d) the F-ratio is the proportion of variance explained by the regression model \n",
    "r is to covariance as standardized regression coefficients are to _____."
    "\n (a) b weights \n (b) t tests \n (c) regression lines \n (d) Pearson correlations \n",
    "Transforming your outcome variable may help improve the normality of the residuals obtained from your \n"
    "regression model"
    "\n (a) True \n (b) False \n",
    "What is the main difference between simple linear regression and multiple regression?"
    "\n (a) the number of predictor variables "
    "\n (b) the number of outcome variables "
    "\n (c) whether a linear model is used"
    "\n (d) the type of predictors that can be used (e.g., continuous, categorical) \n",
    "What is a significance level?"
    "\n (a) the level at which statistics finally become meaningful"
    "\n (b) the impact that reporting statistics incorrectly could have"
    "\n (c) a pre-set level of probability that the results are correct"
    "\n (d) a pre-set level of probability at which it will be accepted that results are due to chance or not \n",
    "R Squared is"
    "\n (a) percentage of variance in predictor accounted for by the outcome variable"
    "\n (b) proportion of variance in the outcome accounted for by the predictor variable(s)"
    "\n (c) proportion of variance in predictor accounted for by the outcome variable"
    "\n (d) parameter of population effect",
]

questions = [
    Question(descriptive_questions[0], "c", "Descriptive"),
    Question(descriptive_questions[1], "b", "Descriptive"),
    Question(descriptive_questions[2], "a", "Descriptive"),
    Question(descriptive_questions[3], "d", "Descriptive"),
    Question(test_theory_questions[0], "b", "Test_Theory"),
    Question(test_theory_questions[1], "b", "Test_Theory"),
    Question(test_theory_questions[2], "a", "Test_Theory"),
    Question(descriptive_questions[4], "c", "Descriptive"),
    Question(test_theory_questions[3], "b", "Test_Theory"),
    Question(descriptive_questions[5], "c", "Descriptive"),
    Question(descriptive_questions[6], "c", "Descriptive"),
    Question(descriptive_questions[7], "b", "Descriptive"),
    Question(test_theory_questions[4], "a", "Test_Theory"),
    Question(test_theory_questions[5], "d", "Test_Theory"),
    Question(descriptive_questions[8], "b", "Descriptive"),
    Question(test_theory_questions[6], "a", "Test_Theory"),
    Question(descriptive_questions[9], "b", "Descriptive"),
    Question(descriptive_questions[10], "b", "Descriptive"),
    Question(descriptive_questions[11], "b", "Descriptive"),
    Question(test_theory_questions[7], "b", "Test_Theory"),
    Question(hypothesis_testing_questions[0], "d", "Hypothesis_Testing"),
    Question(regression_questions[0], "a", "Regression_Correlation"),
    Question(hypothesis_testing_questions[1], "a", "Hypothesis_Testing"),
    Question(regression_questions[1], "c", "Regression_Correlation"),
    Question(regression_questions[2], "d", "Regression_Correlation"),
    Question(regression_questions[3], "b", "Regression_Correlation"),
    Question(regression_questions[4], "a", "Regression_Correlation"),
    Question(regression_questions[5], "d", "Regression_Correlation"),
    Question(regression_questions[6], "a", "Regression_Correlation"),
    Question(hypothesis_testing_questions[2], "b", "Hypothesis_Testing"),
    Question(hypothesis_testing_questions[3], "c", "Hypothesis_Testing"),
    Question(regression_questions[7], "c", "Regression_Correlation"),
    Question(regression_questions[8], "c", "Regression_Correlation"),
    Question(hypothesis_testing_questions[4], "b", "Hypothesis_Testing"),
    Question(hypothesis_testing_questions[5], "b", "Hypothesis_Testing"),
    Question(regression_questions[9], "a", "Regression_Correlation"),
    Question(regression_questions[10], "a", "Regression_Correlation"),
    Question(regression_questions[11], "a", "Regression_Correlation"),
    Question(hypothesis_testing_questions[6], "a", "Hypothesis_Testing"),
    Question(regression_questions[12], "d", "Regression_Correlation"),
    Question(hypothesis_testing_questions[7], "a", "Hypothesis_Testing"),
    Question(regression_questions[13], "b", "Regression_Correlation"),

]

def pick_length():
    ask = input("The exam is 42 questions long. \n"
                "Would you like to take the whole exam or a random sample of questions? \n"
                "Please type 1 for whole exam or 2 for sample: ")
    if ask == "1":
        run_test(questions)
    elif ask == "2":
        sample_length = int(input("How many questions would you like to answer? Enter an integer: "))
        run_test(sample(questions, sample_length))
def run_test(questions):
    score = 0
    questions_asked = 0
    descriptive_questions_asked = 0
    test_theory_questions_asked = 0
    hypothesis_testing_questions_asked = 0
    regression_questions_asked = 0
    descriptive_questions_correct = 0
    test_theory_questions_correct = 0
    hypothesis_testing_questions_correct = 0
    regression_questions_correct = 0
    for question in questions:
        questions_asked += 1
        answer = input(str(questions_asked) + ". " + question.prompt + "Please type the lowercase letter of your respone: ")
        if question.prompt in descriptive_questions:
            descriptive_questions_asked += 1
        if question.prompt in test_theory_questions:
            test_theory_questions_asked += 1
        if question.prompt in hypothesis_testing_questions:
            hypothesis_testing_questions_asked += 1
        if question.prompt in regression_questions:
            regression_questions_asked += 1
        if answer == question.answer:
            score +=1
            print("\n Correct! \n")
            if question.prompt in descriptive_questions:
                descriptive_questions_correct += 1
            if question.prompt in test_theory_questions:
                test_theory_questions_correct += 1
            if question.prompt in hypothesis_testing_questions:
                hypothesis_testing_questions_correct += 1
            if question.prompt in regression_questions:
                regression_questions_correct += 1
        elif answer != question.answer:
            print("\n Incorrect \n")


    percent = round(int(score) / len(questions) * 100, 2)
    if descriptive_questions_asked > 0:
        descriptive_percent = round(descriptive_questions_correct / descriptive_questions_asked * 100, 2)
        print("Descriptive Score: " + str(descriptive_percent) + "%")
    if test_theory_questions_asked > 0:
        test_theory_percent = round(test_theory_questions_correct / test_theory_questions_asked * 100, 2)
        print("Test Theory Score: " + str(test_theory_percent) + "%")
    if hypothesis_testing_questions_asked > 0:
        hypothesis_percent = round(hypothesis_testing_questions_correct / hypothesis_testing_questions_asked * 100, 2)
        print("Hypothesis Testing Score: " + str(hypothesis_percent) + "%")
    if regression_questions_asked > 0:
        regression_percent = round(regression_questions_correct / regression_questions_asked * 100, 2)
        print("Regression Score: " + str(regression_percent) + "%")

    print("Overall you got " + str(score) + "/" + str(len(questions)) + " correct, " + "a " + str(percent) + "%")

    if percent > 90:
        print("Good Job!")
    elif percent > 80:
        print("Not Bad!")
    elif percent < 60:
        print("You Fail!")

pick_length()