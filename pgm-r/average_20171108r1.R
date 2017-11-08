library(dplyr)

pred_rf <- read.csv("../submit/harada_submit_1029_5_rf.csv", header=FALSE)
pred_xgboost <- read.csv("../submit/harada_submit_1103_4_xgboost.csv", header=FALSE)
pred_stacking <- read.csv("../submit/kawasoe_submit_20171108_ens_tree_logi_1.csv", header=FALSE)

str(pred_rf)
str(pred_xgboost)
str(pred_stacking)

pred_stacking <- pred_stacking %>%
    dplyr::arrange(V1)

str(pred_stacking)

out <- data.frame(pred_rf[1],(pred_rf[2]+pred_xgboost[2]+pred_stacking[2])/3)

str(out)

write.table(out, #出力データ
            "../submit/submit_20171108_1_ensemble_average.csv", #出力先
            quote=FALSE, #文字列を「"」で囲む有無
            col.names=FALSE, #変数名(列名)の有無
            row.names=FALSE, #行番号の有無
            sep="," #区切り文字の指定
)

