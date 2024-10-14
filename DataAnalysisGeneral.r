# Set the working directory to the relevant folder
setwd(dir = "~")

# Read the data files
comments <- read.csv(file = "~/comments.csv", stringsAsFactors = T)
clientsall <- read.csv(file = "~/surveys.csv", stringsAsFactors = T)

# Create a numerical version of comment direction factor for plotting
comments$Numcd <- as.character(comments$CommentDirection)
# Keep the first character (1 (negative) or 2 (positive))
comments$Numcd <- as.numeric(substr(comments$Numcd, 1, 1))

# Create modified factors based on Major Theme
comments$MajThShort <- as.character(comments$MajorTheme)
# Create shortened labels of major themes referencing only the numbers
# Keep the relevant leading characters
comments$MajThShort <- as.factor(substr(comments$MajThShort, 1, 3))

# Create modified factors based on Minor Theme
# Create shortened labels of minor themes referencing only the numbers
# Keep the relevant leading characters in MinThShort
comments$MinThShort <- as.character(comments$MinorTheme)
comments$MinThShort <- as.factor(substr(comments$MinThShort, 1, 5))

# Create a factor with only the ten most frequent Minor themes for illustrative purposes:
comments$TopThemes <- as.character(comments$MinThShort)
comments$TopThemes[which(comments$MinThShort != "1.1.1" & comments$MinThShort != "1.1.3" & comments$MinThShort != "1.1.4" & comments$MinThShort != "1.1.5" & comments$MinThShort != "1.1.7" & comments$MinThShort != "1.3.1" & comments$MinThShort != "1.4.1" & comments$MinThShort != "1.5.5" & comments$MinThShort != "1.5.6" & comments$MinThShort != "1.5.7")] <- NA
comments$TopThemes <- as.factor(comments$TopThemes)

# Create the numerical variable, Age to aid with visualization (relative to the year 2024)
clientsall$Age <- 2024 - clientsall$ClientYOB

# Remove the leading "region number" characters from Location to ease visualization
clientsall$ClientLocation <- as.character(clientsall$ClientLocation)
clientsall$ClientLocation <- as.factor(substr(clientsall$ClientLocation, 13, nchar(clientsall$ClientLocation)))
# Reorder levels:
clientsall$ClientLocation <- factor(clientsall$ClientLocation, levels = c("VANCOUVER ISLAND/COAST","MAINLAND/SOUTHWEST","THOMPSON OKANAGAN","KOOTENAY","CARIBOO","NORTH COAST & NECHAKO","NORTHEAST"))

# Create a new factor that groups regions other than Van Isl, SW & ThOk together as one since the other regions (a) have less negative comments and (b) don't have as much data
clientsall$LocationGroup <- as.character(clientsall$ClientLocation)
clientsall$LocationGroup[which(clientsall$LocationGroup == "KOOTENAY" | clientsall$LocationGroup == "CARIBOO" | clientsall$LocationGroup == "NORTH COAST & NECHAKO" | clientsall$LocationGroup == "NORTHEAST")] <- "OTHER"
clientsall$LocationGroup <- factor(clientsall$LocationGroup, levels = c("VANCOUVER ISLAND/COAST","MAINLAND/SOUTHWEST","THOMPSON OKANAGAN","OTHER"))

# Create a binary factor that groups all regions together except Mainland/southwest
clientsall$LocationBinary <- as.character(clientsall$LocationGroup)
clientsall$LocationBinary[which(clientsall$LocationBinary == "VANCOUVER ISLAND/COAST" | clientsall$LocationGroup == "THOMPSON OKANAGAN")] <- "OTHER"
clientsall$LocationBinary <- factor(clientsall$LocationBinary, levels = c("MAINLAND/SOUTHWEST","OTHER"))

# Set Unspecified gender to NA to ignore those from the analysis
clientsall$ClientGender <- as.character(clientsall$ClientGender)
clientsall$ClientGender[which(clientsall$ClientGender == "Unspecified")] <- NA
# Create a new factor GenderMF to ignore non-binary (lack of data - n = 40)
clientsall$GenderMF <- clientsall$ClientGender
clientsall$GenderMF[which(clientsall$ClientGender == "Non-Binary")] <- NA
clientsall$ClientGender <- as.factor(clientsall$ClientGender)
clientsall$GenderMF <- as.factor(clientsall$GenderMF)

# Remove all entries in "Surveys" for people with 0 responses received
clientspart <- subset(clientsall, clientsall$Nresponses != 0)
# Some clients are listed in "Surveys" more than once with separate entries that are identical except for "Client Location", which indicates these people moved; the database has 2 entries for these clients.
# Find duplicate clients, set Location to NA, then remove duplicates 
dup1 <- unique(clientspart$ClientID[which(duplicated(clientspart$ClientID))])
clientspart$ClientLocation[which(clientspart$ClientID %in% dup1)] <- NA
clientspart <- clientspart[-which(duplicated(clientspart$ClientID)),]

# Merge the database of clients with the comments data file
alldata <- merge(comments,clientspart, by = "ClientID")

# Select only negative comments
negonly <- droplevels(subset(alldata, alldata$CommentDirection == "1 Negative Feedback"))

# Counts of # of comments by location, gender, age, theme:
table(alldata$ClientLocation)
table(alldata$ClientGender) # Non-binary & unspecified -> not much data
table(alldata$ClientYOB)
table(alldata$MajorTheme)

# =================================
# Statistical Analysis of Feedback Direction by Theme, location, gender and age
# Logistic regression with (some) post-hoc analysis using emmeans
library(emmeans)

# Basic model with all three factors as simple effects (no interactions)
lm1 <- glm(CommentDirection ~ ClientLocation + ClientGender + ClientYOB, data = alldata, family = "binomial")

# Add an interaction for gender & age:
lm2 <- glm(CommentDirection ~ ClientLocation + ClientGender * ClientYOB, data = alldata, family = "binomial")
# Compare lm1 & lm2 with a likelihood ratio test
anova(lm1,lm2)
# p < 0.05; Adopt lm2

# Add an interaction for location & gender:
lm3 <- glm(CommentDirection ~ ClientLocation * ClientGender + ClientGender * ClientYOB, data = alldata, family = "binomial")
anova(lm2,lm3)
# p > 0.05; Retain lm2

# Check interaction for location & age:
lm4  <- glm(CommentDirection ~ ClientLocation * ClientYOB + ClientGender * ClientYOB, data = alldata, family = "binomial")
anova(lm2,lm4)
# p < 0.05 (barely though); Adopt lm4

# Try removing gender x age interaction
lm5 <- glm(CommentDirection ~ ClientLocation * ClientYOB + ClientGender, data = alldata, family = "binomial")
anova(lm4,lm5)
# p < 0.05; Retain lm4 (interactions exist between location & age and gender & age)

# Try a 3-way interaction gender x location x age
lm6 <- glm(CommentDirection ~ ClientLocation * ClientYOB * ClientGender, data = alldata, family = "binomial")
anova(lm4,lm6)
# Adopt lm6
anova(lm6)
summary(lm6)
# gender is insignificant on its own but it interacts with age to a high degree (not with location); the 3-way interaction is also significant!
# Location and age are by far the most significant factors; gender effects are more subtle

# Post-hoc analysis of location (this ignores interactions - so take with a grain of salt)
location <- emmeans(lm4, specs = pairwise ~ ClientLocation)
locationP <- emmeans(lm4, ~ ClientLocation, type="response")
# Location: Significantly more negative ratings than positive ratings in regions Van Isl & Mainland/SW with the latter having the most negative comments (more negative than all but Van Isl & NE); Northeast: not many responses so no sig. findings there
# Van Isl, SW and Th. Ok. have more negative comments than other regions, especially the Mainland/SW
#	 	Van Isl < Koot, N Coast;
# 		SW < ThOk, Koot, Car, N Coast;
# 		ThOk < Koot


# ============================
# Create visualizations of the 3-way interaction effects of age x location x gender
library(ggplot2)

# Set a new y-axis so that "Positive" appears at the top and "Negative" at the bottom for reference.
my_labels <- c("Negative","Positive")

# Feedback by Age with separate smooths for location, faceted by gender
# (high y = positive feedback, low y = neg. feedback)
# Group the less plentiful locations together as "Other" for illustrative purposes
# Exclude non-binary since it's under-represented
g1 <- ggplot(data=subset(alldata, !is.na(GenderMF)), aes(x = Age, y = Numcd, color = LocationGroup)) +
	geom_smooth(na.rm = T)+
	coord_cartesian(ylim = c(1,2))+
	scale_y_continuous(breaks=seq(1,2,1), labels=my_labels)+
	ylab("Comment Direction")+
	scale_x_continuous(breaks = c(seq(20,80,10))) +
	labs(color = "Location")+
	facet_wrap(~GenderMF)+
	theme(legend.position = "bottom")
g1
ggsave("GenLoc.png", g1, width = 9, height = 6)

# Women aged 35-45 are most negative in most locations except Mainland/SW where 20-year olds are most negative
# Men around 40 are most negative in Mainland/SW.
# Men around mid 30s are most negative in Van Isl/Coast
# Elsewhere men are generally less negative, with younger mean in 20s most negative

# On the same graph for comparison:
g2 <- ggplot(data = subset(alldata, !is.na(ClientGender)), aes(x = Age, y = Numcd, color = ClientGender)) +
	geom_smooth(method = "loess")+
	coord_cartesian(ylim = c(1,2))+
	scale_y_continuous(breaks=seq(1,2,1), labels=my_labels)+
	ylab("Comment Direction")+
	scale_x_continuous(breaks = c(seq(20,80,10)))+
	labs(color = "Location")
quartz()
g2

# Younger women and non-binary clients have relatively more negative comments than young men.
# Young non-binary people between age 18 to ~32 are very negative -> not much data but striking result that needs addressing

# =============================
# Analysis of Response Rate
# Number of Surveys sent by region
t1 <- tapply(clientsall$Nsurveys, clientsall$ClientLocation, sum)
# Number of Responses by region
t2 <- tapply(clientsall$Nresponses, clientsall$ClientLocation, sum)
# Response rates by region
t2/t1
# Dissatisfied regions tend to give slightly more responses

# ... by gender
gen1 <- tapply(clientsall$Nsurveys, clientsall$ClientGender, sum)
gen2 <- tapply(clientsall$Nresponses, clientsall$ClientGender, sum)
gen2/gen1
# Women, unspecified respond slightly more than men & non-binary

# ... by age
rrbyage <- aggregate(clientsall$Nresponses/clientsall$Nsurveys, list(2024-clientsall$ClientYOB), mean)
rrbyage$Age <- as.numeric(rrbyage$Group.1)
rrbyage$ResponseRate <- as.numeric(rrbyage$x)

# Plot:
rrbyageplot <- ggplot(rrbyage, aes(x = Age, y = ResponseRate))+
	geom_point()+
	geom_smooth()+
	ylab("Response Rate")+
	scale_x_continuous(breaks = c(seq(20,80,10)))
quartz()
rrbyageplot

cor.test(rrbyage$Age,rrbyage$ResponseRate)
# Big effect! Much higher response rate among older people; younger people respond less (and are less happy)


# ===============================
# Analysis of themes from negative comments

# Logistic regression for theme type by location, gender, age
themelm <- glm(MinorTheme ~ ClientLocation + ClientGender + Age, data = negonly, family = "binomial")

# Add interaction between gender & age
themelm2 <- glm(MinorTheme ~ ClientLocation + ClientGender * Age, data = negonly, family = "binomial")
anova(themelm,themelm2)
# Retain themelm

# Add interaction between location & gender
themelm3 <- glm(MinorTheme ~ ClientLocation * ClientGender + Age, data = negonly, family = "binomial")
anova(themelm,themelm3)
# Retain themelm

# Add interaction between location & age
themelm4 <- glm(MinorTheme ~ ClientLocation * Age + ClientGender, data = negonly, family = "binomial")
anova(themelm,themelm4)
# Retain themelm
anova(themelm)
summary(themelm)

# Post-hoc analysis:
locationtheme <- emmeans(themelm, specs = ~ClientLocation, type = "response")
# There is evidence of a difference in themes among Mainland/SW vs. other locations

# Minor theme by location histogram
gloctheme <- ggplot(data = subset(negonly, !is.na(TopThemes)), aes(x = TopThemes)) +
	geom_bar()+
	ylab("No. of Comments")+
	xlab("Theme Index")+
	facet_wrap(~LocationBinary)+
	theme(axis.text.x = element_text(angle = 90))

# Major theme by location histogram
glocmajtheme <- ggplot(negonly, aes(x = MajThShort)) +
	geom_bar()+
	ylab("No. of Comments")+
	xlab("Theme Index")+
	facet_wrap(~LocationBinary)+
	theme(axis.text.x = element_text(angle = 90))

# A table will be easier for presenting:
tapply(negonly$MinorTheme, negonly$LocationBinary, summary)
# Out of curiosity (what themes are brought up by non-binary people? These may be different...)
tapply(negonly$MinorTheme, negonly$ClientGender, summary)
# Of 40 comments, 2 popped out:
# 1.1.7 deficient interpersonal skills is most frequent (9)
# 1.1.4 center was inaccessible, non-inclusive, not diverse (5), with many others at 3 or less

# Standardize the counts against the numbers of surveys sent out by region
# Calculate theme rates w.r.t. # of surveys sent out by location
library(plyr)

# Get No. of surveys by Location first
loc <- ddply(clientsall, c("ClientLocation"), summarise,
	nsur = sum(Nsurveys))
# Get counts by Location & Theme (focus only on top 10 most popular minor themes "TopThemes")
trd <- ddply(subset(negonly, !is.na(TopThemes)), c("ClientLocation","TopThemes"), summarise,
	N = length(TopThemes)
	)
# Merge the two to get No. of surveys by location
trd <- merge(trd, loc, by = "ClientLocation")
# Calculate Theme rates
trd$rate = trd$N/trd$nsur

# Plot theme by location using rates
locthrate <- ggplot(trd, aes(x = TopThemes, y = rate)) +
	geom_bar(stat = "identity")+
	scale_y_continuous(labels = scales::percent_format(accuracy = 0.5))+
	ylab("Comment Rate")+
	xlab("Theme Index")+
	facet_wrap(~ClientLocation)+
	theme(axis.text.x = element_text(angle = 90))
	# Give y axis in %
quartz()
locthrate
ggsave("ThemeLoc.png", locthrate, width = 10, height = 6)