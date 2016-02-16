# will make all archives for coursera on-demand

all: archives/week_01.zip archives/week_02.zip archives/week_03.zip archives/week_04.zip archives/week_05.zip archives/week_06.zip archives/week_07.zip archives/week_08.zip archives/week_09.zip archives/week_10.zip

archives/week_01.zip: lab_01/script_01_b_after.R lab_01/script_01_a_after.R lec_01/lec_01.pdf lec_01/lec_01_addon.pdf
	zip archives/week_01.zip lab_01/script_01_b_after.R lab_01/script_01_a_after.R lec_01/lec_01.pdf lec_01/lec_01_addon.pdf

archives/week_02.zip: lab_02/lab_02_before.R lab_02/lab_02_after.R datasets/flats_moscow.txt datasets/flats_moscow_description.txt lec_02/lec_02.pdf
	zip archives/week_02.zip lab_02/lab_02_before.R lab_02/lab_02_after.R datasets/flats_moscow.txt datasets/flats_moscow_description.txt lec_02/lec_02.pdf

archives/week_03.zip: lab_03/lab_03_before.R lab_03/lab_03_after.R datasets/flats_moscow.txt datasets/flats_moscow_description.txt lab_03/nano_research.Rmd lec_03/lec_03.pdf
	zip archives/week_03.zip lab_03/lab_03_before.R lab_03/lab_03_after.R datasets/flats_moscow.txt datasets/flats_moscow_description.txt lab_03/nano_research.Rmd lec_03/lec_03.pdf

archives/week_04.zip: lab_04/lab_04_before.R lab_04/lab_04_after.R lec_04/lec_04.pdf
	zip archives/week_04.zip lab_04/lab_04_before.R lab_04/lab_04_after.R lec_04/lec_04.pdf

archives/week_05.zip: lab_05/lab_05_before.R lab_05/lab_05_after.R datasets/flats_moscow.txt datasets/flats_moscow_description.txt lec_05/lec_05.pdf lab_05/file01.csv lab_05/file02.csv
	zip archives/week_05.zip lab_05/lab_05_before.R lab_05/lab_05_after.R datasets/flats_moscow.txt datasets/flats_moscow_description.txt lec_05/lec_05.pdf lab_05/file01.csv lab_05/file02.csv

archives/week_06.zip: lab_06/lab_06_before.R lab_06/lab_06_after.R lec_06/lec_06.pdf
	zip archives/week_06.zip lab_06/lab_06_before.R lab_06/lab_06_after.R lec_06/lec_06.pdf

archives/week_07.zip: lab_07/lab_07_before.R lab_07/lab_07_after.R lec_07/lec_07.pdf lab_07/titanic3.csv
	zip archives/week_07.zip lab_07/lab_07_before.R lab_07/lab_07_after.R lec_07/lec_07.pdf lab_07/titanic3.csv

archives/week_08.zip: lab_08/lab_08_before.R lab_08/lab_08_after.R lec_08/lec_08.pdf
	zip archives/week_08.zip lab_08/lab_08_before.R lab_08/lab_08_after.R lec_08/lec_08.pdf

archives/week_09.zip: lab_09/lab_09_before.R lab_09/lab_09_after.R datasets/flats_moscow.txt datasets/flats_moscow_description.txt lec_09/lec_09.pdf
	zip archives/week_09.zip lab_09/lab_09_before.R lab_09/lab_09_after.R datasets/flats_moscow.txt datasets/flats_moscow_description.txt lec_09/lec_09.pdf

archives/week_10.zip: lab_10/lab_10_before.R lab_10/lab_10_after.R datasets/flats_moscow.txt datasets/flats_moscow_description.txt lec_10/lec_10.pdf
	zip archives/week_10.zip lab_10/lab_10_before.R lab_10/lab_10_after.R datasets/flats_moscow.txt datasets/flats_moscow_description.txt lec_10/lec_10.pdf
