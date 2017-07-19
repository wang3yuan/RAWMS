#' Read raw mass spectrometry data
#'
#' @param filename Path name of the netCDF, mzData, mzXML or mzML file to read.
#'
#' @return  A list includes mz and intensity, "Spec" and information of each epectrum, "DataInfo".
#'
#' @author San-Yuan Wang

openMSfileAsMatrix <- function(filename)
{
	ms = mzR::openMSfile(filename)
	DataInfo = mzR::header(ms)
	Spec = data.table::data.table(cbind(rep(1:nrow(DataInfo), DataInfo[,"peaksCount"]), (do.call(rbind, peaks(ms)))))
	colnames(Spec) = c("scan", "mz", "intensity")
	mzR::close(ms)
	return(mzProfile = list(Spec = Spec, DataInfo = DataInfo))
}
