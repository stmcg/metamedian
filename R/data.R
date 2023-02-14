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
"dat.ck"
