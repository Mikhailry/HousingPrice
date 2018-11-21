#load data
load('data_final.Rdata')

#converting data to miles
data_final$min_dist_cta <- data_final$min_dist_cta/1.60934

#save compiled_data
save(data_final, file = "data_final.Rdata")


#load data for modeling
load('data_model.Rdata')

#converting data to miles
data_model_f$min_dist_cta <- data_model_f$min_dist_cta/1.60934

#save compiled_data
save(data_model_f, file = "data_model.Rdata")
