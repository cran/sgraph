
#' Scale weights
#'
#' First try to linearize the weights with the best logarithmic and polynomial,
#' then perform exponential scaling and set upper and lower bounds.
#'
#' @param weights           Either nodes or links weights vector
#' @param exp_scale         Scale for exponential transform
#' @param upper_bound_mult  Constant to multiply weights by after scaling.
#'                          Use to set an upper bound for weights.
#' @param lower_bound_const Constant to set a lower bound for weights.
#'                          All weights below will be set to lower bound.
#'
#' @return Weights vector
#' @export
scale_graph = function(weights, exp_scale = exp(1), upper_bound_mult = 25,
                   lower_bound_const = 5) {

    # determine best log fit
    weights = fit_loop(weights, 'log')

    # determine best poly fit
    weights = fit_loop(weights, 'sqrt')

    # now exp scale
    weights = weights * exp_scale / max(weights)
    #for (i in seq_len(n_exp_transform)) weights = exp(weights) - 1
    weights = exp(weights) - 1

    # upper_bound
    weights = weights * upper_bound_mult / max(weights)

    # lower_bound
    ifelse(weights < lower_bound_const, lower_bound_const, weights)
}

get_poly_fit_r2 = function(degree, df_lm) {
  summary(lm(y ~ I(x ^ degree), df_lm))$r.squared
}


norm_vals = function(vals, up_bound = 1) {
  range_vals = range(vals)
  vals = (vals - range_vals[1]) / (range_vals[2] - range_vals[1]) * up_bound
}

fit_loop = function(weights, fun, up_bound = 1e2) {

    df_lm = data.frame(y = sort(weights), x = seq_along(weights))
    r2 = get_poly_fit_r2(1, df_lm)

    for (i in 1:100) {
       if (fun == 'log') {
          df_lm$y %<>% norm_vals(1e2)
          df_lm$y = log(df_lm$y + 1) 
       } else {
          df_lm$y %<>% norm_vals
          df_lm$y = sqrt(df_lm$y) 
       }
       new_r2 = get_poly_fit_r2(1, df_lm)

       if (new_r2 <= r2) break

       if (fun == 'log') {
         weights %<>% norm_vals(1e2)
         weights = log(weights + 1) 
       } else {
         weights %<>% norm_vals
         weights = sqrt(weights) 
       }
        
       r2 = new_r2
    }

  weights %<>% norm_vals
}

