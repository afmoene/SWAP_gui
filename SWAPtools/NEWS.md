# SWAPtools 0.1.70 (2022-09-15)

* plotting modelperformance also based on range observed values

# SWAPtools 0.1.69 (2022-09-07)

* bug in plotting modelperformance resolved

# SWAPtools 0.1.68 (2022-09-01)

* update SWAP-variables (basic drainage)

# SWAPtools 0.1.67 (2022-09-01)

* check if swap-variable is available as a vector or as a array (get_value_SWAP)
* update SWAP-variables (CROPMODULE)

# SWAPtools 0.1.66 (2022-09-01)

* update SWAP-variables (MISCELLANEOUS)

# SWAPtools 0.1.65 (2022-08-30)

* kleine wijzigingen in plot_profile

# SWAPtools 0.1.64 (2022-08-21)

* update SWAP-variables (SFU)

# SWAPtools 0.1.63 (2022-07-22)

* meteogegevens AgERA5 en neerslaggegevens CHIRPS (alleen indien toegang tot WUR netwerk)

# SWAPtools 0.1.62 (2022-07-22)

* minor bug in label actual evapotranspiration (plot_etact)

# SWAPtools 0.1.61 (2022-06-22)

* update templates

# SWAPtools 0.1.60 (2022-06-10)

* minor bug in get_modelperformance in case of usage index

# SWAPtools 0.1.59 (2022-06-09)

* minor bug in label actual evapotranspiration (swap_output.rds)

# SWAPtools 0.1.58 (2022-06-08)

* str_trim in case of spaces (inlist)

# SWAPtools 0.1.57 (2022-05-11)

* add plots for drainage (plot_drainage)

# SWAPtools 0.1.56 (2022-05-09)

* add plots for reduction root water uptake and development grass (plot_rrwu; plot_grass)
* update controlR and KNMItools

# SWAPtools 0.1.55 (2022-05-08)

* general plotting of SWAP results

# SWAPtools 0.1.54 (2022-04-25)

* plot soil-profiles
* hide internal functions in Help Pages
* revision of get_gxg and create_label_graph

# SWAPtools 0.1.53 (2022-04-17)

* update of SWAP variables (DPL_A0 and DPL_A1)

# SWAPtools 0.1.52 (2022-04-12)

* update of SWAP variables (SWDROUGHT = 3)
* update of KNMItools (version: 0.1.15)

# SWAPtools 0.1.51 (2022-01-20)

* update of SWAP variables (swcsv_tz)

# SWAPtools 0.1.50 (2022-01-04)

* set template with customized swap-variables (create_SWAP)

# SWAPtools 0.1.49 (2021-12-30)

* automatically extract column depth if available (read_csv_SWAP) 

# SWAPtools 0.1.48 (2021-12-27)

* extract date with format DD MM from template (get_value_SWAP)
* check date with format DD MM from template (check_value_SWAP)
* optional specification of dir_atm (create_SWAP) 

# SWAPtools 0.1.47 (2021-11-17)

* extract single column array-variable (get_value_SWAP)

# SWAPtools 0.1.46 (20210-11-08)

* update of SWAP variables (LINGRA)
* extract vector-variable without index (get_value_SQL)
* filter table-variables (set_variable_SWAP)
* only load columns with SWAP data from Excel (create_database_SWAP)
* extract SWAP version from csv-output

# SWAPtools 0.1.45 (2021-10-11)

* string-variabelen naar kleine letters
* opvragen variabelen in template zonder header
* controle op naam worksheet

# SWAPtools 0.1.44 (2021-09-02)

* bug in wegschrijven van meta-informatie meteo-bestanden
* set progress to FALSE in case of reading file

# SWAPtools 0.1.43 (2021-08-23)

* sorteren run_id (get_run_id_SQL)

# SWAPtools 0.1.42 (2021-08-16)

* update van readr en controlR
* toevoegen van meta-informatie aan header meteo-bestanden

# SWAPtools 0.1.41 (2021-07-22)

* decreasing order in case variable is function of depth (get_value_SQL)

# SWAPtools 0.1.40 (2021-07-01)

* update of SWAP variables (lingra)

# SWAPtools 0.1.39 (2021-06-08)

* optionele argumenten create_swap

# SWAPtools 0.1.38 (2021-05-25)

* unzip modelresultaten

# SWAPtools 0.1.37 (2021-05-18)

* bepaling van watervolume op basis van drukhoogte of pF waarde

# SWAPtools 0.1.36 (2021-05-05)

* filter run_id op basis van specificatie argumenten
* new output from SWAP version 4.1.81

# SWAPtools 0.1.35 (2021-04-30)

* update of SWAP variables (aeratecrit)

# SWAPtools 0.1.34 (2021-03-16)

* opvragen van meerdere SWAP variabelen tegelijk (get_settings_SWAP)

# SWAPtools 0.1.33 (2021-03-11)

* update of SWAP variables (swcompensate)

# SWAPtools 0.1.32 (2021-02-02)

* uitbreiding SWAP uitvoer variabelen
* wegschrijven gemiddelde berekende waarde (get_performance)

# SWAPtools 0.1.31 (2020-10-29)

* melding naar het scherm bij aanmaken sql-database
* verbeterde afhandeling foutmeldingen bij aanmaken sql-database
* inlezen van vap-file uitvoer

# SWAPtools 0.1.30 (2020-10-22)

* update R-paketten

# SWAPtools 0.1.29 (2020-09-29)

* update of SWAP variables (swgerm)

# SWAPtools 0.1.28 (2020-09-24)

* update of SWAP variables (vernrtb)

# SWAPtools 0.1.27 (2020-07-29)

* update of SWAP variables (swtsum)

# SWAPtools 0.1.26 (2020-07-22)

* add observations to sqlite-database

# SWAPtools 0.1.25 (2020-07-21)

* flags specified when approaching sqlite-database (dbConnect)

# SWAPtools 0.1.24 (2020-06-30)

* read date with timestamp (read_csv_SWAP)

# SWAPtools 0.1.23 (2020-05-29)

* get non-unique index value (get_value_SQL)
* add space between columns (set_tabel_SWAP)
* update of SWAP variables (tillage)

# SWAPtools 0.1.22 (2020-05-07)

* update of SWAP variables (tillage)

# SWAPtools 0.1.21 (2020-04-30)

* update of SWAP variables (CN-method)

# SWAPtools 0.1.20 (2020-03-17)

* update of SWAP variables
* handling NA values in table (Bi-modal Mualem - Van Genuchten)

# SWAPtools 0.1.19 (2020-03-13)

* ggplot functionality moved to controlR
* option to replace missing values in meteo download

# SWAPtools 0.1.18 (2020-03-10)

* Bi-modal Mualem - Van Genuchten added to SWAP variables
* Delete table-column containing missing values

# SWAPtools 0.1.17 (2020-03-02)

* calculation of actual vapour pressure moved to KNMItools

# SWAPtools 0.1.16 (2020-03-02)

* Bi-modal Mualem - Van Genuchten added to SWAP variables

# SWAPtools 0.1.15 (2020-03-02)

* lowercase (TTUTILL on LINUX)
* export of function get_run_info_SQL

# SWAPtools 0.1.14 (2020-02-27)

* added Mualem van Genuchten functionalities
* tools for downloading KNMI data moved to package KNMItools

# SWAPtools 0.1.13 (2020-01-23)

* initialisation of SWAP (INIFIL)
* links to website of SWAP, KNMI and FAO added in documentation

# SWAPtools 0.1.12 (2020-01-17)

* check case-sensitive (set_template and get_value_SWAP)

# SWAPtools 0.1.11 (2020-01-14)

* check if SWAP variable exists (set_template)
* upper boundary for HBOT5 changed

# SWAPtools 0.1.10 (2020-01-10)

* allow multiple date formats (get_value_SWAP)
* set_template and get_value_SWAP case-insensitive 

# SWAPtools 0.1.9 (2020-01-08)

* unit of groundwaterlevel characteristics changed in get_gt (now based on get_gxg)

# SWAPtools 0.1.8 (2020-01-03)

* calculation of model performance
* bug in calculation of GHG

# SWAPtools 0.1.7 (2019-12-23)

* calculation of GxG
* zip SWAP results based on OUTFIL by default

# SWAPtools 0.1.6 (2019-12-10)

* check SQL-query run_info

# SWAPtools 0.1.5 (2019-12-07)

* extract SWAP variable from sql-database based on index, stop in case of failure 

# SWAPtools 0.1.4 (2019-12-02)

* possibility to continue with R-script after SWAP crash

# SWAPtools 0.1.3 (2019-11-27)

* extract SWAP variable from sql-database based on index 

# SWAPtools 0.1.2 (2019-10-24)

* bug in date check on table-variables solved (create_database_SWAP)
* meterological data in a single file
* function created for reading output with specified variables (read_csv_SWAP)

# SWAPtools 0.1.1 (2019-10-24)

* get_value_SQL: convert date-variables to date-class

# SWAPtools 0.1.0 (2019-10-20)

* first version of SWAPtools available at: https://waterwijzerlandbouw.wur.nl/index.html
