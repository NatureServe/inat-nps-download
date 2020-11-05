cd "C:\Users\Michael_Lee\Documents\NPS_bioblitz\exotics\rerun"

:startRun
"C:\Program Files\R\R-4.0.2\bin\x64\Rscript.exe" "C:\Users\Michael_Lee\Documents\NPS_bioblitz\exotics\rerun\altRApproach_v18a_install.R"

"C:\Program Files\R\R-4.0.2\bin\x64\Rscript.exe" "C:\Users\Michael_Lee\Documents\NPS_bioblitz\exotics\rerun\altRApproach_v18b_getSppID.R"

"C:\Program Files\R\R-4.0.2\bin\x64\Rscript.exe" "C:\Users\Michael_Lee\Documents\NPS_bioblitz\exotics\rerun\altRApproach_v18c_downloadObsSspVar.R"

GOTO startRun

