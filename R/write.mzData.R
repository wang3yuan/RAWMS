#' Write mass spectrometry data
#'
#' @param Spec A dataframe include "mz" and "intensity"
#' @param DataInfo A dataframe gererated by openMSfileAsMatrix include "msLevel", "lowMZ", "highMZ", "retentionTime", "peaksCount"
#' @param filename Path name of the mzData file to write.
#'
#' @author San-Yuan Wang

write.mzData <- function(Spec, DataInfo, filename)
{
  data.table::setDT(Spec)

  sink(file = filename)
  cat('<?xml version="1.0" encoding="utf-8"?>\n')
  cat('<mzData version="1.05" accessionNumber="psi-ms:100" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">\n')
  cat('   <cvLookup cvLabel="psi" fullName="The PSI Ontology" version="1.00" address="http://psidev.sourceforge.net/ontology" />\n')
  cat ('  <spectrumList count="', nrow(DataInfo),'">\n', sep = "")

  for(cScan in 1:nrow(DataInfo))
  {
    msLevel = DataInfo[cScan, "msLevel"]
    RT = DataInfo[cScan, "retentionTime"]/60

    lowMZ = DataInfo[cScan, "lowMZ"]
    highMZ = DataInfo[cScan, "highMZ"]
    peaksCount = DataInfo[cScan, "peaksCount"]
    mzVector = caTools::base64encode(Spec[scan == cScan, mz], size = 8, endian = "little")
    intVector = caTools::base64encode(Spec[scan == cScan, intensity], size = 4, endian = "little")
    cat('    <spectrum id=', '"', cScan, '">\n', sep = "")
      cat('      <spectrumDesc>\n')
        cat('        <spectrumSettings>\n')
          cat('          <acqSpecification spectrumType="discrete" methodOfCombination="average" count="1">\n')
            cat('            <acquisition acqNumber="', cScan, '" />\n', sep = "")
          cat('          </acqSpecification>\n')
          cat('          <spectrumInstrument msLevel="2" mzRangeStart="', lowMZ, '" mzRangeStop="', highMZ, '">\n', sep = "")
            cat('            <cvParam cvLabel="psi" accession="PSI:1000036" name="ScanMode" value="PrecursorIon" />\n')
            cat('            <cvParam cvLabel="psi" accession="PSI:1000037" name="Polarity" value="Positive" />\n')
            cat('            <cvParam cvLabel="psi" accession="PSI:1000038" name="TimeInMinutes" value="', RT, '" />\n', sep = "")
            cat('          </spectrumInstrument>\n')
        cat('        </spectrumSettings>\n')
      cat('      </spectrumDesc>\n')
      cat('      <mzArrayBinary>\n')
        cat('        <data precision="64" endian="little" length="', peaksCount, '">', mzVector, '</data>\n', sep = "")
      cat('      </mzArrayBinary>\n')
      cat('      <intenArrayBinary>\n')
        cat('        <data precision="32" endian="little" length="', peaksCount, '">', intVector, '</data>\n', sep = "")
      cat('      </intenArrayBinary>\n')
    cat('    </spectrum>\n')
  }
  cat('    </spectrumList>\n')
  cat('</mzData>')
  sink(NULL)
}
