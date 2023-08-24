#' Example data set: Comparing the age between COVID-19 survivors and nonsurvivors (raw version)
#'
#' A data set from the meta-analysis of Katzenschlager et al. (2021). Specifically, this data set corresponds to the meta-analysis comparing the age of COVID-19 infected patients who died and those who survived. The unit of measurement for the age values is years. The rows in the data set correspond to the primary studies.
#'
#' @docType data
#'
#' @format A data frame with 52 rows and 13 columns:
#' \tabular{ll}{
#'   \code{author} \tab first author of the primary study and the journal name. \cr
#'   \code{n.g1} \tab number of subjects (nonsurvivor group). \cr
#'   \code{q1.g1} \tab first quartile of age (nonsurvivor group). \cr
#'   \code{med.g1} \tab median age (nonsurvivor group). \cr
#'   \code{q3.g1} \tab third quartile of age (nonsurvivor group).\cr
#'   \code{mean.g1} \tab mean age (nonsurvivor group). \cr
#'   \code{sd.g1} \tab standard deviation of age (nonsurvivor group).\cr
#'   \code{n.g2} \tab number of subjects (survivor group). \cr
#'   \code{q1.g2} \tab first quartile of age (survivor group). \cr
#'   \code{med.g2} \tab median age (survivor group). \cr
#'   \code{q3.g2} \tab third quartile of age (survivor group).\cr
#'   \code{mean.g2} \tab mean age (survivor group). \cr
#'   \code{sd.g2} \tab standard deviation of age (survivor group).\cr}
#'
#' @references Katzenschlager S., Zimmer A.J., Gottschalk C., Grafeneder J., Seitel A., Maier-Hein L., Benedetti A., Larmann J., Weigand M.A., McGrath S., and Denkinger C.M. (2021). Can we predict the severe course of COVID-19 - A systematic review and meta-analysis of indicators of clinical outcome? \emph{PLOS One}, \strong{16}, e0255154.
#'
"dat.age_raw"


#' Example data set: Comparing the age between COVID-19 survivors and nonsurvivors (cleaned version)
#'
#' A data set from the meta-analysis of Katzenschlager et al. (2021). Specifically, this data set corresponds to the meta-analysis comparing the age of COVID-19 infected patients who died and those who survived. The unit of measurement for the age values is years. The rows in the data set correspond to the primary studies. Compared to the "raw" version (i.e., \code{\link{dat.age_raw}}), this data set excludes the primary study of Qi et al. (2021) due to its small sample size in the nonsurvivor group.
#'
#' @docType data
#'
#' @format A data frame with 51 rows and 13 columns:
#' \tabular{ll}{
#'   \code{author} \tab first author of the primary study and the journal name. \cr
#'   \code{n.g1} \tab number of subjects (nonsurvivor group). \cr
#'   \code{q1.g1} \tab first quartile of age (nonsurvivor group). \cr
#'   \code{med.g1} \tab median age (nonsurvivor group). \cr
#'   \code{q3.g1} \tab third quartile of age (nonsurvivor group).\cr
#'   \code{mean.g1} \tab mean age (nonsurvivor group). \cr
#'   \code{sd.g1} \tab standard deviation of age (nonsurvivor group).\cr
#'   \code{n.g2} \tab number of subjects (survivor group). \cr
#'   \code{q1.g2} \tab first quartile of age (survivor group). \cr
#'   \code{med.g2} \tab median age (survivor group). \cr
#'   \code{q3.g2} \tab third quartile of age (survivor group).\cr
#'   \code{mean.g2} \tab mean age (survivor group). \cr
#'   \code{sd.g2} \tab standard deviation of age (survivor group).\cr}
#'
#' @references Katzenschlager S., Zimmer A.J., Gottschalk C., Grafeneder J., Seitel A., Maier-Hein L., Benedetti A., Larmann J., Weigand M.A., McGrath S., and Denkinger C.M. (2021). Can we predict the severe course of COVID-19 - A systematic review and meta-analysis of indicators of clinical outcome? \emph{PLOS One}, \strong{16}, e0255154.
#' @references Qi X., Liu Y., Wang J., Fallowfield J.A., Wang J., Li X., Shi J., Pan H., Zou S., Zhang H., and others. (2021). Clinical course and risk factors for mortality of COVID-19 patients with pre-existing cirrhosis: A multicentre cohort study, \emph{Gut}, \strong{70}, 433–436.
#'
"dat.age"


#' Example data set: Comparing aspartate transaminase levels between COVID-19 survivors and nonsurvivors (raw version)
#'
#' A data set from the meta-analysis of Katzenschlager et al. (2021). Specifically, this data set corresponds to the meta-analysis comparing aspartate transaminase (ASAT) levels of COVID-19 infected patients who died and those who survived. The unit of measurement for the ASAT levels is U/L. The rows in the data set correspond to the primary studies.
#'
#' @docType data
#'
#' @format A data frame with 27 rows and 13 columns:
#' \tabular{ll}{
#'   \code{author} \tab first author of the primary study and the journal name. \cr
#'   \code{n.g1} \tab number of subjects (nonsurvivor group). \cr
#'   \code{q1.g1} \tab first quartile of the ASAT levels (nonsurvivor group). \cr
#'   \code{med.g1} \tab median ASAT level (nonsurvivor group). \cr
#'   \code{q3.g1} \tab third quartile of the ASAT levels (nonsurvivor group).\cr
#'   \code{mean.g1} \tab mean ASAT level (nonsurvivor group). \cr
#'   \code{sd.g1} \tab standard deviation of the ASAT levels (nonsurvivor group).\cr
#'   \code{n.g2} \tab number of subjects (survivor group). \cr
#'   \code{q1.g2} \tab first quartile of the ASAT levels (survivor group). \cr
#'   \code{med.g2} \tab median ASAT level (survivor group). \cr
#'   \code{q3.g2} \tab third quartile of the ASAT levels (survivor group).\cr
#'   \code{mean.g2} \tab mean ASAT level (survivor group). \cr
#'   \code{sd.g2} \tab standard deviation of the ASAT levels (survivor group).\cr}
#'
#' @references Katzenschlager S., Zimmer A.J., Gottschalk C., Grafeneder J., Seitel A., Maier-Hein L., Benedetti A., Larmann J., Weigand M.A., McGrath S., and Denkinger C.M. (2021). Can we predict the severe course of COVID-19 - A systematic review and meta-analysis of indicators of clinical outcome? \emph{PLOS One}, \strong{16}, e0255154.
#'
"dat.asat_raw"

#' Example data set: Comparing aspartate transaminase levels between COVID-19 survivors and nonsurvivors (cleaned version)
#'
#' A data set from the meta-analysis of Katzenschlager et al. (2021). Specifically, this data set corresponds to the meta-analysis comparing aspartate transaminase (ASAT) levels of COVID-19 infected patients who died and those who survived. The unit of measurement for the ASAT levels is U/L. The rows in the data set correspond to the primary studies. Compared to the "raw" version (i.e., \code{\link{dat.asat_raw}}), this data set excludes the primary study of Qi et al. (2021) due to its small sample size in the nonsurvivor group.
#'
#' @docType data
#'
#' @format A data frame with 26 rows and 13 columns:
#' \tabular{ll}{
#'   \code{author} \tab first author of the primary study and the journal name. \cr
#'   \code{n.g1} \tab number of subjects (nonsurvivor group). \cr
#'   \code{q1.g1} \tab first quartile of the ASAT levels (nonsurvivor group). \cr
#'   \code{med.g1} \tab median ASAT level (nonsurvivor group). \cr
#'   \code{q3.g1} \tab third quartile of the ASAT levels (nonsurvivor group).\cr
#'   \code{mean.g1} \tab mean ASAT level (nonsurvivor group). \cr
#'   \code{sd.g1} \tab standard deviation of the ASAT levels (nonsurvivor group).\cr
#'   \code{n.g2} \tab number of subjects (survivor group). \cr
#'   \code{q1.g2} \tab first quartile of the ASAT levels (survivor group). \cr
#'   \code{med.g2} \tab median ASAT level (survivor group). \cr
#'   \code{q3.g2} \tab third quartile of the ASAT levels (survivor group).\cr
#'   \code{mean.g2} \tab mean ASAT level (survivor group). \cr
#'   \code{sd.g2} \tab standard deviation of the ASAT levels (survivor group).\cr}
#'
#' @references Katzenschlager S., Zimmer A.J., Gottschalk C., Grafeneder J., Seitel A., Maier-Hein L., Benedetti A., Larmann J., Weigand M.A., McGrath S., and Denkinger C.M. (2021). Can we predict the severe course of COVID-19 - A systematic review and meta-analysis of indicators of clinical outcome? \emph{PLOS One}, \strong{16}, e0255154.
#' @references Qi X., Liu Y., Wang J., Fallowfield J.A., Wang J., Li X., Shi J., Pan H., Zou S., Zhang H., and others. (2021). Clinical course and risk factors for mortality of COVID-19 patients with pre-existing cirrhosis: A multicentre cohort study, \emph{Gut}, \strong{70}, 433–436.
#'
"dat.asat"


#' Example data set: Comparing creatine kinase transaminase levels between COVID-19 survivors and nonsurvivors (raw version)
#'
#' A data set from the meta-analysis of Katzenschlager et al. (2021). Specifically, this data set corresponds to the meta-analysis comparing creatine kinase (CK) levels of COVID-19 infected patients who died and those who survived. The unit of measurement for the CK levels is U/L. The rows in the data set correspond to the primary studies.
#'
#' @docType data
#'
#' @format A data frame with 18 rows and 13 columns:
#' \tabular{ll}{
#'   \code{author} \tab first author of the primary study and the journal name. \cr
#'   \code{n.g1} \tab number of subjects (nonsurvivor group). \cr
#'   \code{q1.g1} \tab first quartile of the CK levels (nonsurvivor group). \cr
#'   \code{med.g1} \tab median CK level (nonsurvivor group). \cr
#'   \code{q3.g1} \tab third quartile of the CK levels (nonsurvivor group).\cr
#'   \code{mean.g1} \tab mean CK level (nonsurvivor group). \cr
#'   \code{sd.g1} \tab standard deviation of the CK levels (nonsurvivor group).\cr
#'   \code{n.g2} \tab number of subjects (survivor group). \cr
#'   \code{q1.g2} \tab first quartile of the CK levels (survivor group). \cr
#'   \code{med.g2} \tab median CK level (survivor group). \cr
#'   \code{q3.g2} \tab third quartile of the CK levels (survivor group).\cr
#'   \code{mean.g2} \tab mean CK level (survivor group). \cr
#'   \code{sd.g2} \tab standard deviation of the CK levels (survivor group).\cr}
#'
#' @references Katzenschlager S., Zimmer A.J., Gottschalk C., Grafeneder J., Seitel A., Maier-Hein L., Benedetti A., Larmann J., Weigand M.A., McGrath S., and Denkinger C.M. (2021). Can we predict the severe course of COVID-19 - A systematic review and meta-analysis of indicators of clinical outcome? \emph{PLOS One}, \strong{16}, e0255154.
#'
"dat.ck_raw"

#' Example data set: Comparing creatine kinase levels between COVID-19 survivors and nonsurvivors (cleaned version)
#'
#' A data set from the meta-analysis of Katzenschlager et al. (2021). Specifically, this data set corresponds to the meta-analysis comparing creatine kinase (CK) levels of COVID-19 infected patients who died and those who survived. The unit of measurement for the CK levels is U/L. The rows in the data set correspond to the primary studies. Compared to the "raw" version (i.e., \code{\link{dat.asat_raw}}), this data set excludes the primary study of Qi et al. (2021) due to its small sample size in the nonsurvivor group.
#'
#' @docType data
#'
#' @format A data frame with 17 rows and 13 columns:
#' \tabular{ll}{
#'   \code{author} \tab first author of the primary study and the journal name. \cr
#'   \code{n.g1} \tab number of subjects (nonsurvivor group). \cr
#'   \code{q1.g1} \tab first quartile of the CK levels (nonsurvivor group). \cr
#'   \code{med.g1} \tab median CK level (nonsurvivor group). \cr
#'   \code{q3.g1} \tab third quartile of the CK levels (nonsurvivor group).\cr
#'   \code{mean.g1} \tab mean CK level (nonsurvivor group). \cr
#'   \code{sd.g1} \tab standard deviation of the CK levels (nonsurvivor group).\cr
#'   \code{n.g2} \tab number of subjects (survivor group). \cr
#'   \code{q1.g2} \tab first quartile of the CK levels (survivor group). \cr
#'   \code{med.g2} \tab median CK level (survivor group). \cr
#'   \code{q3.g2} \tab third quartile of the CK levels (survivor group).\cr
#'   \code{mean.g2} \tab mean CK level (survivor group). \cr
#'   \code{sd.g2} \tab standard deviation of the CK levels (survivor group).\cr}
#'
#' @references Katzenschlager S., Zimmer A.J., Gottschalk C., Grafeneder J., Seitel A., Maier-Hein L., Benedetti A., Larmann J., Weigand M.A., McGrath S., and Denkinger C.M. (2021). Can we predict the severe course of COVID-19 - A systematic review and meta-analysis of indicators of clinical outcome? \emph{PLOS One}, \strong{16}, e0255154.
#' @references Qi X., Liu Y., Wang J., Fallowfield J.A., Wang J., Li X., Shi J., Pan H., Zou S., Zhang H., and others. (2021). Clinical course and risk factors for mortality of COVID-19 patients with pre-existing cirrhosis: A multicentre cohort study, \emph{Gut}, \strong{70}, 433–436.
#'
"dat.ck"

#' Example data set: Patient Health Questionnaire-9 (PHQ-9) scores (raw version)
#'
#' A data set based on a meta-analysis of Patient Health Questionnaire-9 (PHQ-9) scores (Thombs et al. 2014, Levis et al. 2019). This particular data set is from Table S1 and Figure 3 in McGrath et al. (2020); See McGrath et al. (2020) for additional details. The rows in the data set correspond to the primary studies.
#'
#' @docType data
#'
#' @format A data frame with 58 rows and 9 columns:
#' \tabular{ll}{
#'   \code{author} \tab first author of the primary study and the year of publication. \cr
#'   \code{n.g1} \tab number of subjects. \cr
#'   \code{min.g1} \tab minimum PHQ-9 score. \cr
#'   \code{q1.g1} \tab first quartile of the PHQ-9 scores. \cr
#'   \code{med.g1} \tab median PHQ-9 score. \cr
#'   \code{q3.g1} \tab third quartile of the PHQ-9 scores.\cr
#'   \code{max.g1} \tab maximum PHQ-9 score. \cr
#'   \code{mean.g1} \tab mean PHQ-9 score. \cr
#'   \code{sd.g1} \tab standard deviation of the PHQ-9 scores.\cr}
#'
#' @references Thombs B.D., Benedetti A., Kloda L.A., et al. (2014). The diagnostic accuracy of the Patient Health Questionnaire-2 (PHQ-2), Patient Health Questionnaire-8 (PHQ-8), and Patient Health Questionnaire-9 (PHQ-9) for detecting major depression: protocol for a systematic review and individual patient data meta-analyses. \emph{Systematic Reviews}. \strong{3}(1):1-16.
#' @references Levis B., Benedetti A., Thombs B.D., and the DEPRESsion Screening Data (DEPRESSD) Collaboration. (2019). The diagnostic accuracy of the Patient Health Questionnaire-9 (PHQ-9) for detecting major depression. \emph{BMJ}. \strong{365}:l1476.
#' @references McGrath S., Zhao X., Steele R., Thombs B.D., Benedetti A., and the DEPRESsion Screening Data (DEPRESSD) Collaboration. (2020). Estimating the sample mean and standard deviation from commonly reported quantiles in meta-analysis. \emph{Statistical Methods in Medical Research}. \strong{29}(9):2520-2537.
#'
"dat.phq9_raw"


#' Example data set: Patient Health Questionnaire-9 (PHQ-9) scores (processed version)
#'
#' A data set based on a meta-analysis of Patient Health Questionnaire-9 (PHQ-9) scores (Thombs et al. 2014, Levis et al. 2019). This data set was obtained by performing the following data processing items to \code{\link{dat.phq9_raw}}. We randomly selected 14 of the primary studies to report S1 summary statistics, 14 to report S2 summary statistics, 15 to report S3 summary statistics, and 15 to report S4 summary statistics. Since some of the mean-based methods require the sample quantiles to be strictly positive and PHQ-9 scores of 0 were observed in some of the primary studies, we added a value of 0.01 to sample quantiles with a value of 0. Additionally, since some of the mean-based methods do not allow for "ties" in the quantiles (e.g., when the minimum value equals the first quartile value), we added a value of 0.25 to the larger quantile in the event of ties.
#'
#' @docType data
#'
#' @format A data frame with 58 rows and 9 columns:
#' \tabular{ll}{
#'   \code{author} \tab first author of the primary study and the year of publication. \cr
#'   \code{n.g1} \tab number of subjects. \cr
#'   \code{min.g1} \tab minimum PHQ-9 score. \cr
#'   \code{q1.g1} \tab first quartile of the PHQ-9 scores. \cr
#'   \code{med.g1} \tab median PHQ-9 score. \cr
#'   \code{q3.g1} \tab third quartile of the PHQ-9 scores.\cr
#'   \code{max.g1} \tab maximum PHQ-9 score. \cr
#'   \code{mean.g1} \tab mean PHQ-9 score. \cr
#'   \code{sd.g1} \tab standard deviation of the PHQ-9 scores.\cr}
#'
#' @references Thombs B.D., Benedetti A., Kloda L.A., et al. (2014). The diagnostic accuracy of the Patient Health Questionnaire-2 (PHQ-2), Patient Health Questionnaire-8 (PHQ-8), and Patient Health Questionnaire-9 (PHQ-9) for detecting major depression: protocol for a systematic review and individual patient data meta-analyses. \emph{Systematic Reviews}. \strong{3}(1):1-16.
#' @references Levis B., Benedetti A., Thombs B.D., and the DEPRESsion Screening Data (DEPRESSD) Collaboration. (2019). The diagnostic accuracy of the Patient Health Questionnaire-9 (PHQ-9) for detecting major depression. \emph{BMJ}. \strong{365}:l1476.
#' @references McGrath S., Zhao X., Steele R., Thombs B.D., Benedetti A., and the DEPRESsion Screening Data (DEPRESSD) Collaboration. (2020). Estimating the sample mean and standard deviation from commonly reported quantiles in meta-analysis. \emph{Statistical Methods in Medical Research}. \strong{29}(9):2520-2537.
#'
"dat.phq9"
