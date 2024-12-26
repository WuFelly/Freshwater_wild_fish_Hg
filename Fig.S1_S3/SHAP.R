library(shapr)       # v0.2.2
library(kernelshap)  # v0.4.1
library(shapviz)     # v0.9.3

# Should load ML_PDP_SHAP.RData before running this script

set.seed(123)
## Random Forest
kernel_rf <- kernelshap(rf_final_fit, rd_train[, -5], bg_X = rd_train)
sv_rf <- shapviz(kernel_rf)
rf_shap <- sv_importance(sv_rf, fill = '#01868B', kind = 'bee')
## Cubist
kernel_cubist <- kernelshap(cubist_final_fit, rd_train[, -5], bg_X = rd_train)
sv_cubist <- shapviz(kernel_cubist)
cubist_shap <- sv_importance(sv_cubist, fill = '#01868B', kind = 'bee')
## SVM_RBF
kernel_svmrbf <- kernelshap(svmrbf_final_fit, rd_train_transformed[, -11], bg_X = rd_train_transformed)
sv_svmrbf <- shapviz(kernel_svmrbf)
svmrbf_shap <- sv_importance(sv_svmrbf, fill = '#01868B', kind = 'bee')
## Glmnet
kernel_glm <- kernelshap(glm_final_fit, rd_train_transformed[, -11], bg_X = rd_train_transformed)
sv_glm <- shapviz(kernel_glm)
glm_shap <- sv_importance(sv_glm, fill = '#01868B', kind = 'bee')

merged_shap <- (rf_shap|cubist_shap)/(svmrbf_shap|glm_shap)

save.image('ML_PDP_SHAP.RData')
