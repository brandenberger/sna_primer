
data_helga <- read.csv2("data_literatur_varia/Data_Helga.csv", encoding = "UTF-8")

adj_list_helga_friends <- cbind(as.character(data_helga$ID),as.character(data_helga$Friends.in.the.Team))

vec_list_helga <- unlist(lapply(adj_list_helga_friends[,2], strsplit, split = ","),recursive = F)
vec_list_helga[unlist(lapply(vec_list_helga, function (x) length(x))) == 0] <- NA
max_adjlist <- max(unlist(lapply(vec_list_helga, length)))
adjlist_mat_helga_friends <- do.call(what = "rbind",lapply(vec_list_helga, function(x) {na_vec <- rep(NA,max_adjlist)
na_vec[1:length(x)] <- x
na_vec}))
adjlist_mat_helga_friends <- cbind(adj_list_helga_friends[,1], adjlist_mat_helga_friends)

el_helga_friends <-
  cbind(
    c(apply(adjlist_mat_helga_friends,1,function(x) rep(x[1], length(x)-1))),
    c(apply(adjlist_mat_helga_friends,1,function(x) x[2:length(x)]))  
  )
el_helga_friends <- el_helga_friends[!(is.na(el_helga_friends[,2])),]
el_helga_friends

ggnet2(network(el_helga_friends), label = T)


