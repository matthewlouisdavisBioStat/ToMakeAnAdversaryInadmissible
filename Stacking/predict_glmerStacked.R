#' Making predictions on new data with a stacked model, which requires a new Z matrix if random effects are desired.
#' @param fit a fit object returned by running the SuperModel() function with make_glmerStackedModel() as the super learner.
#' @param newdata a dataframe of new data for making predictions on
#' @param newZ a matrix coding the random effects design matrix for the new data
#' @param use_randint T or F, whether or not to include random effects for predicting on future data
#' @param type the MachineShop response-type to output for predictions
#'
#' @return out-of-sample predictions made on new data
#'
#' @examples
#' \donttest{
#'
#'  predict_glmerStacked(fit,newdata = newdata, newZ = diag(1,10))
#' }
#' @export
predict_glmerStacked <- function(fit,newdata,newZ = NULL, use_randint = F,type){
  if(!use_randint | is.null(newZ)){
    fit$super_fit$data$Zmat <- (fit$super_fit$data$Zmat * 0)[1:nrow(newdata),]
    predict(fit,newdata = newdata,type = type)
  } else{
    if(!is.null(newZ)){
      fit$super_fit$data$Zmat <- newZ
    }
    predict(fit,newdata = newdata,type = type)
  }
}


