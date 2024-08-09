**Overview**: This spreadsheet provides complete compositional and NIR data for liquid samples taken throughout the xylo-oligomer purification process. Each observation represents a sample taken from one location at a given time. Compositional data and NIRS spectra were collected in duplicate and averaged. 

* **sample**: Unique sample identifier. Format is YYMMDD### using the date that the sample was initially analyzed on at Prenexus and then a 3 digit number denoting which number sample that sample was on that day.
* **sampleType**: Describes sample location, as defined in Figure 1 of manuscript
* **Lot #**: Identifier for the batch of material processed, LCX codes corresponding to the cook date of crude liquid material, and the FIL codes corresponding to the Friday of the week, and are a composite of all LCX cooks completed in the week prior. Lot numbers are formatted as MMDDYY. Suffixes describe the type of material.
  * C - Composite, usually for when several cooks are completed in one day and combined.
  * MF-P – Microfiltration permeate
  * IE1 – First pass over ion exchange column
  * IE2 – Second pass over ion exchange column
  * NF-R – Nanofiltration retentate. May be NF2 or NF3, number indicating a repetition of the step.
  * NF-P – Nanofiltration permeate. May be NF2 or NF3, number indicating a repetition of the step.
  * UF-R – Ultrafiltration retentate
  * UF-P – Ultrafiltration permeate
* **grouping**: model grouping group, as described in manuscript
* **groupingType**: subtype from modeling grouping group, as described in manuscript
* **xos**: concentration (g/L) of xylo-oligomers in liquor sample, as measured by analytical hydrolysis and HPLC
* **solids**: concentration (g/L) of insoluble solids in sample, as measured gravimetrically (10 mL of material dried overnight in duplicate at 105°C and weighed)
* **xylose_total**: concentration (g/L) of monomeric and oligomeric xylose in liquor sample, as measured by analytical hydrolysis and HPLC
* **xylose_free**: concentration (g/L) of monomeric xylose in liquor sample, as measured by HPLC
* **glucose_total**:concentration (g/L) of monomeric and oligomeric glucose in liquor sample, as measured by analytical hydrolysis and HPLC
* **glucose_free**: concentration (g/L) of monomeric glucose in liquor sample, as measured by HPLC
* **galactose_total**:concentration (g/L) of monomeric and oligomeric galactose in liquor sample, as measured by analytical hydrolysis and HPLC
* **galactose_free**: concentration (g/L) of monomeric galactose in liquor sample, as measured by HPLC
* **arabinose_total**:concentration (g/L) of monomeric and oligomeric arabinose in liquor sample, as measured by analytical hydrolysis and HPLC
* **arabinose_free**: concentration (g/L) of monomeric arabinose in liquor sample, as measured by HPLC
* **acetic_total**: bound and unbound acetic acid concentration measured after acid hydrolysis with Megazyme acetic acid kit
* **acetic_free**: concentration (g/L) unbound acetic acid concentration measured with Megazyme acetic acid kit
* **pH**: pH of sample, measured by Sartorius pH probe on day of analysis
* **scanInstrument**: The Instrument used to collect the spectra. `Metrohm NIRS XDS NIR Multivial Analyzer` refers to the monochromator (Serial #: 3010-2412) which is a dispersive NIR, while Multivial Module refers to the reflectance module (Serial #: 3013-1678). 
* **samplePresentation**: Describes the set up used to present the sample to the spectrometer. The quartz ring cup is made of quartz optical glass and has a diameter of ~ 36mm (FOSS Part #IH-0836), which is larger than the sampling window. A 0.1mm gold transflectance head (FOSS Part #99-157) was used to produce transflectance spectra.
* **dataCollectionMethod**: Documents the data collection method used in scanning. `Iris` refers to the aperture used to center the sample onto the collection window in reflectance mode to collect stationary spectra. The spot size refers to the width of the beam used to collect the spectra, which is chosen based on the surface area that the sample takes up within the window, in accordance with the Metrohm XDS manual. 
* **measurementUnit**: denotes the unit of the variables held within columns AA to FFN. Log(1/R) is the default absorbance measurement for a reflectance mode instrument. R refers to the reflectance measurement, which ranges from 0 to 1.0, and is the ratio of the sample and the internal reference reflectance measurements. 
* **variableUnit**: denotes the unit of the column names held within columns AA to FFN. ‘nm’ refers to the wavelength measurement of nanometers. Another possible unit is ‘cm^-1’, which refers to wavenumbers. 
* **scanDate**: The day in which the spectra was collected (m/d/y)
* **labTemp_C**: Ambient temperature (degrees Celsius) within the laboratory (NREL IBRF 218) collected immediately before the beginning of a spectra collection campaign. Collected using a HOBO Temperature/Relative Humidity Data Logger (H14-001) (accuracy is +/- 7 °C  at 20 °C) 
* **relativeHumidity_percent**: Relative humidity (%) of the laboratory (NREL IBRF 218) collected immediately before the beginning of a spectra collection campaign. Collected using a HOBO Temperature/Relative Humidity Data Logger (H14-001) (accuracy is +/- 3% at 20-