################ 
#Homework Number 5
#Fixed Issue where code 
#would not run by replacing 
#Json_file with Json_file_Adios
################ 

################ Step 2
#changes#
#Added in the tidyverse library
library("stringr")
library("jsonlite")
library("tidyverse")

################ Part 1
#changes#
#No change necessary, this step is just assigning a variable 
Json_file_Adios <- "The Front Bottoms-Talon Of The Hawk-Au Revoir (Adios).json"

################ Part 2
#changes#
#Took out Pastes since they were unnecessary
#Since this is using Stringr already, which is part of tidyverse, this step is already being done with tidyverse
split_Json_file_Adios <- str_split(Json_file_Adios, "-", simplify = TRUE) 
song_name_with.json_Adios = (split_Json_file_Adios[3])
track_Adios <- str_sub(song_name_with.json_Adios, 1, str_length(song_name_with.json_Adios) - 5)
artist_Adios = (split_Json_file_Adios[1])
album_Adios = (split_Json_file_Adios[2])



################ Part 3
#changes#
#No change necessary, this step is just assigning a variable 
JSON_Data <- fromJSON(paste("EssentiaOutput","/", Json_file_Adios, sep =""))

################ Part 4
#changes#
#Used the pluck() function and introduced pipes
overall_loudness  <- JSON_Data |> 
  pluck("lowlevel", "loudness_ebu128", "integrated")
spectral_energy   <- JSON_Data |>
  pluck("lowlevel", "spectral_energy")
dissonance        <- JSON_Data |>
  pluck("lowlevel", "dissonance")
danceability      <- JSON_Data |>
  pluck("rhythm", "danceability")
pitch_salience    <- JSON_Data |>
  pluck("lowlevel", "pitch_salience")
bpm               <- JSON_Data |>
  pluck("rhythm", "bpm")
beats_loudness    <- JSON_Data |>
  pluck("rhythm", "beats_loudness")
tuning_frequency  <- JSON_Data |>
  pluck("tonal", "tuning_frequency")

################ Step 2
# artist album track
#changes#
#Changed my data frame to a tibble
df_notplot <- tibble(
  artist = character(), album = character(), track = character(),
  overall_loudness = numeric(), spectral_energy = numeric(),
  dissonance = numeric(), danceability = numeric(),
  pitch_salience = numeric(), bpm = numeric(),
  beats_loudness = numeric(), tuning_frequency = numeric()
)
# Create a data frame to save the results
essentia_files <- list.files(path = "EssentiaOutput/")
json_files_new <- essentia_files[str_count(essentia_files, ".json")==1]
#changes#
#Took out pastes that were not necessary
#This step is already using tidyverse since it is using stringr which is a part of tidyverse
for (i in 1:length(json_files_new)) {
  current_filename = json_files_new[i] 
  split_Json_file <- str_split(current_filename, "-", simplify = TRUE) 
  song_name_with.json = (split_Json_file[3])
  track <- str_sub(song_name_with.json, 1, str_length(song_name_with.json) - 5)
  artist = (split_Json_file[1])
  album = (split_Json_file[2])
  # remove .json from the song
  # save artist/album/track
  
  

  # Read in the file as JSON_DataVie  JSON_Data_New <- fromJSON(paste("json_files_new","/", i, sep =""))
  JSON_Data_New <- fromJSON(paste0("EssentiaOutput/",current_filename))
#changes#
#Used the pluck() function and introduced pipes
  overall_loudness  <- JSON_Data_New |> 
    pluck("lowlevel", "loudness_ebu128", "integrated")
  spectral_energy   <- JSON_Data_New |>
    pluck("lowlevel", "spectral_energy")
  dissonance        <- JSON_Data_New |>
    pluck("lowlevel", "dissonance")
  danceability      <- JSON_Data_New |>
    pluck("rhythm", "danceability")
  pitch_salience    <- JSON_Data_New |>
    pluck("lowlevel", "pitch_salience")
  bpm               <- JSON_Data_New |>
    pluck("rhythm", "bpm")
  beats_loudness    <- JSON_Data_New |>
    pluck("rhythm", "beats_loudness")
  tuning_frequency  <- JSON_Data_New |>
    pluck("tonal", "tuning_frequency")
  
# Save the results to the ith row of the data frame
#changes#
#Changed data frame to a tibble and used bind_rows() instead of rbind()
#Removed column renaming since doing the dataframe this way solved the 
#issue which caused me to have to rename some columns in the first place
  df_notplot <- df_notplot %>%
    bind_rows(tibble(
      artist = artist, 
      album = album, 
      track = track, 
      overall_loudness = as.numeric(overall_loudness), 
      spectral_energy = as.numeric(spectral_energy),
      dissonance = as.numeric(dissonance), 
      danceability = as.numeric(danceability),
      pitch_salience = as.numeric(pitch_salience), 
      bpm = as.numeric(bpm),
      beats_loudness = as.numeric(beats_loudness), 
      tuning_frequency = as.numeric(tuning_frequency)
    ))
}


################ Step 3 Part 1
#changes#
#changed read.csv() to read_csv()
Essentia_Output_Original <- read_csv("EssentiaOutput/EssentiaModelOutput.csv")

################ Step 3 Part 2
Essentia_Output_Original$arousal = (Essentia_Output_Original$emo_arousal+Essentia_Output_Original$deam_arousal+Essentia_Output_Original$muse_arousal)/3
Essentia_Output_Original$valence = (Essentia_Output_Original$deam_valence+Essentia_Output_Original$emo_valence+Essentia_Output_Original$muse_valence)/3

################ Step 3 Part 3
Essentia_Output_Original$aggressive = (Essentia_Output_Original$nn_aggressive+Essentia_Output_Original$eff_aggressive)/2
Essentia_Output_Original$happy = (Essentia_Output_Original$nn_happy+Essentia_Output_Original$eff_happy)/2
Essentia_Output_Original$party = (Essentia_Output_Original$nn_party+Essentia_Output_Original$eff_party)/2
Essentia_Output_Original$relaxed = (Essentia_Output_Original$nn_relax+Essentia_Output_Original$eff_relax)/2
Essentia_Output_Original$sad = (Essentia_Output_Original$nn_sad+ Essentia_Output_Original$eff_sad)/2

################ Step 3 Part 4
Essentia_Output_Original$acoustic = (Essentia_Output_Original$nn_acoustic+Essentia_Output_Original$eff_acoustic)/2
Essentia_Output_Original$electric = (Essentia_Output_Original$nn_electronic+Essentia_Output_Original$eff_electronic)/2

################ Step 3 Part 5
Essentia_Output_Original$instrumental = (Essentia_Output_Original$nn_instrumental+Essentia_Output_Original$eff_instrumental)/2

################ Step 3 Part 6
colnames(Essentia_Output_Original)[colnames(Essentia_Output_Original) == "eff_timbre_bright"] = "timbreBright"

################ Step 3 Part 7
Essentia_Output_New <- Essentia_Output_Original[c("artist", "album", "track", "arousal", 
                                                  "valence", "aggressive", "happy","party","relaxed",
                                                  "sad", "acoustic","electric","instrumental",
                                                  "timbreBright")]

################ Step 4 Part 1
#changes#
#changed read.csv() to read_csv()
LIWC_Output <- read_csv("LIWCOutput/LIWCOutput.csv")

################ Step 4 Part 2
#changes#
#Used full_join() instead of merge() and introduced pipes
Merged_df_1 <- df_notplot |>
  full_join(Essentia_Output_New, by = c("artist", "album", "track"))
Final_Merged_df <- Merged_df_1 |>
  full_join(LIWC_Output, by = c("artist", "album", "track")) |>
################ Step 4 Part 3
#changes#
#Used rename() function and ran it through a pipe
    rename(funct = `function`)


