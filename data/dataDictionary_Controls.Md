**Overview:** This spreadsheet includes control sample data taken throughout the scanning campaign. Each observation is a unique NIR scan of a given control. 

* **sample**: Same as ‘Lab sample code’ in primary analytical. Unique sample identifier assigned by Prenexus. Format is YYMMDD### using the date that the sample was initially analyzed on at Prenexus and then a 3 digit number denoting which number sample that sample was on that day.
    *** _Note_**: Reference Samples are labeled differently in this column:
   * _Water_ reference is labeled ‘water’
   * Prenexus Liquid Reference is labeled by its Lot Number (_FIL030422-MF-P_)
   * _Acetal B_ polymer reference is labeled ‘Acetal B’
*** sampleIdType**: Describes whether a spectra is a unique sample (labeled ‘external ID’) or is a reference sample used to evaluate instrument variability (labeled ‘check cell’)
* **scandate**: The day in which the spectra was collected (m/d/y)
* **scanTime**: The time of day in which the spectra was collected (MST)
* **sampleCell**: The unique optical glass used to hold the sample and collect the spectra. For reference, NREL has ~30 quartz optical glass ring cups, each labeled with a unique lower-case letter. 
* **relativeHumidity_percent**: Relative humidity (%) of the laboratory (NREL IBRF 218) collected immediately before the beginning of a spectra collection campaign. Collected using a HOBO Temperature/Relative Humidity Data Logger (H14-001) (accuracy is +/- 3% at 20-80%) 
* **labTemp_C**: Ambient temperature (degrees Celsius) within the laboratory (NREL IBRF 218) collected immediately before the beginning of a spectra collection campaign. Collected using a HOBO Temperature/Relative Humidity Data Logger (H14-001) (accuracy is +/- 7 °C  at 20 °C) 
* **standardizationType**: The column verifies that the Metrohm XDS data collection method used reference standardization to calculate the reference spectra. At NREL, references are standardized in accordance with Metrohm Diagnostics guides for non-pharmacology laboratories, and the physical references are recertified yearly. If reference standardization was included in data collection, the value should read, “Reference standardized (Vision and NSAS)”. Vision is the name of the software used to collect the spectra, and NSAS is file structure used within the vision database to store the spectra.  
* **referenceTime**: The time at which the reference spectra was collected on the day of scanning (MST)
* **spectralID**; GUID for the spectra
* **projectPath**: path to project in which spectra is located within NREL IBRF 218 vision database. Together with product_name, allows for original spectra file to be relocated within vision. 
* **product_name**:  name of folder within project in which spectra is located within NREL IBRF 219 vision database. Together with projectPath, allows for original spectra file to be relocated within vision. 
* **Instrument**: The Instrument used to collect the spectra. `Metrohm NIRS XDS NIR Multivial Analyzer` refers to the monochromator (Serial #: 3010-2412) which is a dispersive NIR, while Multivial Module refers to the reflectance module (Serial #: 3013-1678). 
* **dataCollectionMethod**: Documents the data collection method used in scanning. `Iris` refers to the aperture used to center the sample onto the collection window in reflectance mode to collect stationary spectra. The spot size refers to the width of the beam used to collect the spectra, which is chosen based on the surface area that the sample takes up within the window, in accordance with the Metrohm XDS manual. 
* **samplePresentation**: Describes the set up used to present the sample to the spectrometer. The quartz ring cup is made of quartz optical glass and has a diameter of ~ 36mm (FOSS Part #IH-0836), which is larger than the sampling window. A 0.1mm gold transflectance head (FOSS Part #99-157) was used to produce transflectance spectra.
* **measurementUnit**: denotes the unit of the variables held within columns R to FFE. Log(1/R) is the default absorbance measurement for a reflectance mode instrument. R refers to the reflectance measurement, which ranges from 0 to 1.0, and is the ratio of the sample and the internal reference reflectance measurements. 
* **variableUnit**: denotes the unit of the column names held within columns R to FFE. ‘nm’ refers to the wavelength measurement of nanometers. Another possible unit is ‘cm^-1’, which refers to wavenumbers. 
