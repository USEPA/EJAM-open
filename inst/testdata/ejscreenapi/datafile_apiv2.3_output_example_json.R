##     x = ejscreenRESTbroker(lat = testpoints_5$lat[1], lon = testpoints_5$lon[1], radius = 1,  f = "pjson")
# > names(x)
# [1] "url"         "status_code" "headers"     "all_headers" "cookies"     "content"     "date"        "times"       "request"     "handle"   


# > print(x, max.lines = 100)

# Response [https://ejscreen.epa.gov/mapper/ejscreenRESTbroker1.aspx?namestr=&geometry={"spatialReference":{"wkid":4326},"x":-111.9040233,"y":33.5604162}&distance=1&unit=9035&areatype=&areaid=&f=pjson]
# Date: 2024-09-05 23:03
# Status: 200
# Content-Type: application/json; charset=utf-8
# Size: 9.73 kB
# {
#   "data": {
#     "demographics": {
#       "P_ENGLISH": "91",
#       "P_SPANISH": "5",
#       "P_FRENCH": "0",
#       "P_RUS_POL_SLAV": "1",
#       "P_OTHER_IE": "2",
#       "P_VIETNAMESE": "0",
#       "P_OTHER_ASIAN": "0",
#       "P_ARABIC": "0",
#       "P_OTHER": "0",
#       "P_NON_ENGLISH": "9",
#       "P_NHWHITE": "84",
#       "P_NHBLACK": "5",
#       "P_NHASIAN": "1",
#       "P_HISP": "6",
#       "P_NHAMERIND": "0",
#       "P_NHHAWPAC": "0",
#       "P_NHOTHER_RACE": "0",
#       "P_NHTWOMORE": "3",
#       "P_AGE_LT5": "3",
#       "P_AGE_LT18": "17",
#       "P_AGE_GT17": "83",
#       "P_AGE_GT64": "31",
#       "P_HLI_SPANISH_LI": "24",
#       "P_HLI_IE_LI": "76",
#       "P_HLI_API_LI": "0",
#       "P_HLI_OTHER_LI": "0",
#       "P_LOWINC": "19",
#       "PCT_MINORITY": "16",
#       "P_EDU_LTHS": "1",
#       "P_LIMITED_ENG_HH": "1",
#       "P_EMP_STAT_UNEMPLOYED": "4",
#       "P_DISABILITY": "8",
#       "P_MALES": "51",
#       "P_FEMALES": "49",
#       "LIFEEXP": "80",
#       "PER_CAP_INC": "71569",
#       "HSHOLDS": "4785",
#       "P_OWN_OCCUPIED": "70",
#       "inputAreaMiles": "3.14",
#       "TOTALPOP": "10157"
#     },
#     "main": {
#       "RAW_D_PEOPCOLOR": "16%",
#       "RAW_D_INCOME": "19%",
#       "RAW_D_LESSHS": "1%",
#
#
#
