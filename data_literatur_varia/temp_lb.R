
load("data_literatur_varia/Daten_SchulNW_fullneu.RData")

dt$marks.german.num <- ifelse(dt$marks.german == 'keine Angabe', NA, as.character(dt$marks.german))
dt$marks.german.num <- as.numeric(dt$marks.german.num)
dt$marks.math.num <- ifelse(dt$marks.math == 'keine Angabe', NA, as.character(dt$marks.math))
dt$marks.math.num <- as.numeric(dt$marks.math.num)
dt$marks.french.num <- ifelse(dt$marks.french == 'keine Angabe', NA, as.character(dt$marks.math))
dt$marks.french.num <- as.numeric(dt$marks.french.num)
dt$marks.french.math.num <- rowMeans(data.frame(dt$marks.math.num, dt$marks.french.num), na.rm = TRUE)
dt$marks.french.math.num <- dt$marks.french.math.num[is.na(dt$marks.french.math.num)] <- NA

dt$likes.school.num <- ifelse(dt$likes.school.num >= 3, 'yes', 'no')
dt$hr.spent.TV.num <- NA
dt$hr.spent.TV.num[dt$hr.spent.TV == '0-59 Min.'] <- 1
dt$hr.spent.TV.num[dt$hr.spent.TV == '1 Std.'] <- 2
dt$hr.spent.TV.num[dt$hr.spent.TV == '2 Std.'] <- 3
dt$hr.spent.TV.num[dt$hr.spent.TV == '3 Std.'] <- 4
dt$hr.spent.TV.num[dt$hr.spent.TV == '4 Std.'] <- 5
dt$hr.spent.TV.num[dt$hr.spent.TV == '5 Std.'] <- 6
dt$hr.spent.TV.num[dt$hr.spent.TV == '6 Std.'] <- 7
dt$hr.spent.TV.num[dt$hr.spent.TV == '7 Std.'] <- 8
dt$hr.spent.TV.num[dt$hr.spent.TV == '8 Std.'] <- 9
dt$hr.spent.TV.num[dt$hr.spent.TV == '9 oder mehr Std.'] <- 10
dt$hr.spent.TV.num[dt$hr.spent.TV == 'keine Antwort'] <- NA
dt$hr.spent.friends.num <- NA
dt$hr.spent.friends.num[dt$hr.spent.friends == '0-59 Min.'] <- 1
dt$hr.spent.friends.num[dt$hr.spent.friends == '1 Std.'] <- 2
dt$hr.spent.friends.num[dt$hr.spent.friends == '2 Std.'] <- 3
dt$hr.spent.friends.num[dt$hr.spent.friends == '3 Std.'] <- 4
dt$hr.spent.friends.num[dt$hr.spent.friends == '4 Std.'] <- 5
dt$hr.spent.friends.num[dt$hr.spent.friends == '5 Std.'] <- 6
dt$hr.spent.friends.num[dt$hr.spent.friends == '6 Std.'] <- 7
dt$hr.spent.friends.num[dt$hr.spent.friends == '7 Std.'] <- 8
dt$hr.spent.friends.num[dt$hr.spent.friends == '8 Std.'] <- 9
dt$hr.spent.friends.num[dt$hr.spent.friends == '9 oder mehr Std.'] <- 10
dt$hr.spent.friends.num[dt$hr.spent.friends == 'keine Antwort'] <- NA
dt$hr.spent.reading.num <- NA
dt$hr.spent.reading.num[dt$hr.spent.reading == '0-59 Min.'] <- 1
dt$hr.spent.reading.num[dt$hr.spent.reading == '1 Std.'] <- 2
dt$hr.spent.reading.num[dt$hr.spent.reading == '2 Std.'] <- 3
dt$hr.spent.reading.num[dt$hr.spent.reading == '3 Std.'] <- 4
dt$hr.spent.reading.num[dt$hr.spent.reading == '4 Std.'] <- 5
dt$hr.spent.reading.num[dt$hr.spent.reading == '5 Std.'] <- 6
dt$hr.spent.reading.num[dt$hr.spent.reading == '6 Std.'] <- 7
dt$hr.spent.reading.num[dt$hr.spent.reading == '7 Std.'] <- 8
dt$hr.spent.reading.num[dt$hr.spent.reading == '8 Std.'] <- 9
dt$hr.spent.reading.num[dt$hr.spent.reading == '9 oder mehr Std.'] <- 10
dt$hr.spent.reading.num[dt$hr.spent.reading == 'keine Antwort'] <- NA
dt$agree.be.successful.num
dt$aggree.earn.alot.num
dt$true.parents.interested.num
dt$agree.respect.elderly.num
dt$migration.bg
dt$gender <- ifelse(dt$sex == 'männlich', 'male', ifelse(dt$sex == 'weiblich', 'female', NA))
dt$sport.hours
dt <- subset(dt, select = c(studentID, class, age, gender, migration.bg,
                            marks.german.num, marks.math.num, marks.french.num, marks.french.math.num,
                            likes.school.num, sport.hours, hr.spent.TV.num, 
                            hr.spent.friends.num, hr.spent.reading.num, agree.be.successful.num, 
                            aggree.earn.alot.num, true.parents.interested.num, agree.respect.elderly.num))

write.table(el, sep = ";", file = "data_literatur_varia/Edgelist_highSchoolfriends.csv", row.names = FALSE)
write.table(dt, sep = ";", file = "data_literatur_varia/Students_highSchoolAttributes.csv", row.names = FALSE)

#################

## read data 
el  <- read.table('data_literatur_varia/Edgelist_highSchoolfriends.csv', 
                  sep = ";", header = TRUE)
dt <- read.table('data_literatur_varia/Students_highSchoolAttributes.csv', 
                 sep = ";", header = TRUE)

## create adjacency matrix for all sorts of friends (best friends, drinking buddies, school friends etc.)
students <- unique(c(el$studentID, el$friend.ID.code))
mat <- matrix(0, nrow =length(students) , ncol = length(students))
colnames(mat) <- as.character(students)
rownames(mat) <- as.character(students)
mat[cbind(as.character(el$studentID), 
          as.character(el$friend.ID.code))] <- 1

## create adjacency matrix for school friends
matsf <- matrix(0, nrow =length(students) , ncol = length(students))
colnames(matsf) <- as.character(students)
rownames(matsf) <- as.character(students)
matsf[cbind(as.character(el$studentID[el$school.friend == 'Yes']), 
            as.character(el$friend.ID.code[el$school.friend == 'Yes']))] <- 1

## create adjacency matrix for going-out-friends (friends with whom you go out and have drinks with)
matdf <- matrix(0, nrow =length(students) , ncol = length(students))
colnames(matdf) <- as.character(students)
rownames(matdf) <- as.character(students)
matdf[cbind(as.character(el$studentID[el$go.out.friend == 'Yes']), 
            as.character(el$friend.ID.code[el$go.out.friend == 'Yes']))] <- 1

## create attributes file that matches the adjacency matrix
att <- data.frame('studentID' = rownames(mat))

## add attributes
# Grades in German-Class
att$grade.german <- dt$marks.german.num[match(att$studentID, dt$studentID)]
att$grade.math <- dt$marks.math.num[match(att$studentID, dt$studentID)]
# Name of school class
att$class <- dt$class[match(att$studentID, dt$studentID)]
# Gender
att$gender <- dt$gender[match(att$studentID, dt$studentID)]
# Age
att$age <- dt$age[match(att$studentID, dt$studentID)]
# Migration background
att$migrationbg <- dt$migration.bg[match(att$studentID, dt$studentID)]
# Liking school
att$liking.school <- dt$likes.school.num[match(att$studentID, dt$studentID)]
# Leisure time
att$sport.hours <- dt$sport.hours[match(att$studentID, dt$studentID)]
att$tv.hours <- dt$hr.spent.TV.num[match(att$studentID, dt$studentID)]
att$friends.hours <- dt$hr.spent.friends.num[match(att$studentID, dt$studentID)]
att$reading.hours <- dt$hr.spent.reading.num[match(att$studentID, dt$studentID)]
# Values: Being successful & Earning a lot
att$agree.besuccessfull <- dt$agree.be.successful.num[match(att$studentID, dt$studentID)]
att$agree.earnalot <- dt$aggree.earn.alot.num[match(att$studentID, dt$studentID)]
# Values: Repecting elders & Parental monitoring
att$true.parentsinterested <- dt$true.parents.interested.num[match(att$studentID, dt$studentID)]
att$true.respectelderly <- dt$agree.respect.elderly.num[match(att$studentID, dt$studentID)]


##
dt$marks.french
dt$marks.french.num <- ifelse(dt$marks.french == 'keine Angabe', NA, as.character(dt$marks.math))
dt$marks.french.num <- as.numeric(dt$marks.french.num)


att$grade.math <- dt$marks.math.num[match(att$studentID, dt$studentID)]
att$grade.math.french <- rowMeans(data.frame(att$grade.math, att$grade.french), na.rm = TRUE)

att$temp <- dt$marks.overall.num[match(att$studentID, dt$studentID)]
att$grade.french <- dt$marks.french.num[match(att$studentID, dt$studentID)]

att$grade.math.french <- att$grade.math.french[is.na(att$grade.math.french)] <- NA
att$temp <- att$grade.math.french
##
att$avg.grade.ofriends <- netlag(att$temp, mat, normalization = 'row')[,1]
att$avg.grade.ifriends <- netlag(att$temp, mat, normalization = 'column')[,1]

##
fit3 <- lm(temp ~ age
           + gender 
           #+ liking.school 
           #+ tv.hours 
           #+ reading.hours
           #+ agree.earnalot 
           #+ true.parentsinterested
           #+ sport.hours
           + avg.grade.ofriends,
           data = att)
summary(fit3)


fit1 <- lm(grade.math.french ~ age 
           + gender 
           + liking.school 
           + tv.hours 
           + reading.hours
           + agree.earnalot 
           + true.parentsinterested,
           data = att)
summary(fit1)

