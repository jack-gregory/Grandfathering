***************************************************************
*****************************************************************
*****************************************************************
** Optimal grandfathering project ***
** Master file for cleaning the data and running simulations
** a s well as data description
** previous steps done in R - see file XXX 
** (dataset of power plants and the regulatory dataset)
** as well as file YY (IV and CEMS) 
*****************************************************************
*****************************************************************
*****************************************************************


set matsize  900, permanently
global path_orig_data "Here_your_path\data\orig_data"
global path_use_data "Here_your_path\data\use_data"
global path_code "Here_your_path\code\3_Final_dataset_creation" 
global path_data "Here_your_path\data"
global path_output "Here_your_path\out"


cap program drop states_def

 
program define states_def, rclass
replace state="Alaska" if state=="AK"
replace state="Alabama" if state=="AL"
replace state="Arkansas" if state=="AR" 	
replace state="Arizona" if state=="AZ" 	
replace state="California" if state=="CA"
replace state="Colorado" if state=="CO"
replace state="Connecticut" if state=="CT"
replace state="Delaware" if state=="DE"
replace state="Florida" if state=="FL"
replace state="Georgia" if state=="GA"
replace state="Hawaii" if state=="HI"
replace state="Idaho" if state=="ID"
replace state="Illinois" if state=="IL"
replace state="Indiana" if state=="IN"
replace state="Iowa" if state=="IA"
replace state="Kansas" if state=="KS"
replace state="Kentucky" if state=="KY"
replace state="Louisiana" if state=="LA"
replace state="Maine" if state=="ME"
replace state="Maryland" if state=="MD"
replace state="Massachusetts" if state=="MA"
replace state="Michigan" if state=="MI"
replace state="Minnesota" if state=="MN"
replace state="Mississippi" if state=="MS"
replace state="Missouri" if state=="MO"
replace state="Montana" if state=="MT"
replace state="Nebraska" if state=="NE"
replace state="Nevada" if state=="NV"
replace state="New Hampshire" if state=="NH"
replace state="New Jersey" if state=="NJ"
replace state="New Mexico" if state=="NM"
replace state="New York" if state=="NY"
replace state="North Carolina" if state=="NC"
replace state="North Dakota" if state=="ND"
replace state="Ohio" if state=="OH"
replace state="Oklahoma" if state=="OK"
replace state="Oregon" if state=="OR"
replace state="Pennsylvania" if state=="PA"
replace state="Rhode Island" if state=="RI"
replace state="South Carolina" if state=="SC"
replace state="South Dakota" if state=="SD"
replace state="Tennessee" if state=="TN"
replace state="Texas" if state=="TX"
replace state="Utah" if state=="UT"
replace state="Vermont" if state=="VT"
replace state="Virginia" if state=="VA"
replace state="Washington" if state=="WA"
replace state="West Virginia" if state=="WV"
replace state="Wisconsin" if state=="WI"
replace state="Wyoming" if state=="WY"
 end


do "$path_code\Step1_processing_data.do"
do "$path_code\Step2_Descriptives.do"
*do "$path_code\Step3_Simulations.do"