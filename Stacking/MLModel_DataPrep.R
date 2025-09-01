#' Pair a data pre-processing step and MLModel for insertion into a stacked model
#' @param fit hardcode the data preparation steps associated with this MLModelFit into a new MLModel object. Only works if all predictors are numeric.
#' @param name an optional, new custom name for the new MLModel returned
#' @param y the name of the outcome of interest modelled by the fit object.
#'
#' @details Take in previous model fit paired with a certain data prep process, return a new MLModel paired with the same data preparation process aka, this fits the same data prep + model to new data, while this is already the case with single as.MLModel() function, it does not work with stacking thus,this function allows us to stack base learners trained on different datasets into one model, rather than have all of them trained on the same set of normalized  data. Remember to clusterExport() what you need!
#'
#' @return A MachineShop custom MLModel that can be used standalone or in a stacked model.
#'
#' @importFrom MachineShop MLModel
#'
#' @seealso \code{\link[MLModel]{MLModel}}
#'
#' @examples
#' \donttest{
#'
#'  MLModel_DataPrep(fit,name = "new_fit",y = "outcome")
#' }
#' @export
MLModel_DataPrep <- function (fit, name = "Default", y) {
  MLModel(name = name, response_types = c("BinomialVariate", 
                                          "factor", "matrix", "NegBinomialVariate", 
                                          "numeric", "PoissonVariate", "Surv"), 
          weights = F, fit = function(formula, data, weights, ...) {
            tr <- try(fit$mlmodel@x, silent = T)
            if (class(tr) == "try-error") {
              tr <- try(fit$mlmodel@input, silent = T)
            }
            if (class(tr) == "try-error") {
              stop("Cannot find recipe from fit object: try updating MachineShop to latest version, or using a recipe object to fit instead of a raw data set and formula separately")
            }
            dat <- tr %>% prep(training = data) %>% bake(new_data = data)
            rm(tr)
            mod <- fit
            temp_dat <- dat
            if (nrow(dat) == 1) {
              dat <- rbind(dat, dat)
            }
            temp_dat <- temp_dat[,c(y,
                                    colnames(temp_dat)[!(colnames(temp_dat) %in% 
                                                               c(y, "(names)"))])]
            fo <- as.formula(paste0(y,"~ .",collapse = ""))
            mlmodel <- as.MLModel(mod)
            fitNew <- fit(fo, data = temp_dat, model = mlmodel)
            fit <- lm(as.numeric(dat[[y]])[1:nrow(dat)] ~ 0 + 
                        predict(fitNew, newdata = dat, type = "prob"))
            fit$coefficients <- 1
            fit$true_fit <- fitNew
            fit$fit_mod <- mod
            return(fit)
          }, predict = function(object, newdata, ...) {
            mod <- object$fit_mod
            tr <- try(mod$mlmodel@x, silent = T)
            if (class(tr) == "try-error") {
              tr <- try(mod$mlmodel@input, silent = T)
            }
            if (class(tr) == "try-error") {
              stop("Cannot find recipe from fit object: try updating MachineShop to latest version, or using a recipe object to fit instead of a raw data set and formula separately")
            }
            dat <- tr %>% prep %>% bake(new_data = newdata)
            if (nrow(dat) == 1) {
              predict(object$true_fit, newdata = rbind(dat, 
                                                       dat), type = "prob")[1]
            }
            else {
              predict(object$true_fit, newdata = dat, type = "prob")
            }
          }, varimp = function(object, ...) {
            try(MachineShop::varimp(fit$true_fit, "model"), 
                silent = T)
          })
}

