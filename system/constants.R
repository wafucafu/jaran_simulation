x_vars <- c("wd", "holiday")
x_vars_special <- c("category_id", "device_id", "month")

inter_term <- "sqrt"

response_term <- "sales"


curvature_list <- seq(0.5, 0.9, 0.1)


weight_value <- 3
weight_range <- 100


mean_cost_range <- 0.1


cost_range_start <- 500000
cost_range_step <- 500000


cross_validation_kpi <- "sales"