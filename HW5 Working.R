################ 
#Homework Number 5
#Fixed Issue where code 
#would not run by replacing 
#Json_file with Json_file_Adios
################ 

################ Step 2
library("stringr")
library("jsonlite")

################ Part 1
Json_file_Adios <- "The Front Bottoms-Talon Of The Hawk-Au Revoir (Adios).json"

################ Part 2
split_Json_file_Adios <- str_split(Json_file_Adios, "-", simplify = TRUE)
song_name_with.json_Adios = (paste(split_Json_file_Adios[3]))
track_Adios <- str_sub(song_name_with.json_Adios, 1, str_length(song_name_with.json_Adios) - 5)
artist_Adios = (paste(split_Json_file_Adios[1]))
album_Adios = (paste(split_Json_file_Adios[2]))



################ Part 3
JSON_Data <- fromJSON(paste("EssentiaOutput","/", Json_file_Adios, sep =""))

################ Part 4
overall_loudness = JSON_Data$lowlevel$loudness_ebu128$integrated
spectral_energy = JSON_Data$lowlevel$spectral_energy
dissonance = JSON_Data$lowlevel$dissonance
danceability = JSON_Data$rhythm$danceability
pitch_salience = JSON_Data$lowlevel$pitch_salience
bpm = JSON_Data$rhythm$bpm
beats_loudness = JSON_Data$rhythm$beats_loudness
tuning_frequency = JSON_Data$tonal$tuning_frequency

################ Step 2
# artist album track
df_notplot = data.frame(artist = character(), album = character(), track = character(), 
                overall_loudness = numeric(), spectral_energy = numeric(),
                dissonance = numeric(), danceability = numeric(),
                pitch_salience = numeric(), bpm = numeric(),
                beats_loudness = numeric(), tuning_frequency = numeric(),
                stringsAsFactors = FALSE)
# Create a data frame to save the results
essentia_files <- list.files(path = "EssentiaOutput/")
json_files_new <- essentia_files[str_count(essentia_files, ".json")==1]

for (i in 1:length(json_files_new)) {
  current_filename = json_files_new[i]
  split_Json_file <- str_split(current_filename, "-", simplify = TRUE)
  song_name_with.json = (paste(split_Json_file[3]))
  track <- str_sub(song_name_with.json, 1, str_length(song_name_with.json) - 5)
  artist = (paste(split_Json_file[1]))
  album = (paste(split_Json_file[2]))
  # remove .json from the song
  # save artist/album/track
  
  

  # Read in the file as JSON_DataVie  JSON_Data_New <- fromJSON(paste("json_files_new","/", i, sep =""))
  JSON_Data_New <- fromJSON(paste0("EssentiaOutput/",current_filename))
  
  overall_loudness = JSON_Data_New$lowlevel$loudness_ebu128$integrated
  spectral_energy = JSON_Data_New$lowlevel$spectral_energy
  dissonance = JSON_Data_New$lowlevel$dissonance
  danceability = JSON_Data_New$rhythm$danceability
  pitch_salience = JSON_Data_New$lowlevel$pitch_salience
  bpm = JSON_Data_New$rhythm$bpm
  beats_loudness = JSON_Data_New$rhythm$beats_loudness
  tuning_frequency = JSON_Data_New$tonal$tuning_frequency
  
  # Save the results to the ith row of the data frame
  df_notplot <- rbind(df_notplot, data.frame(artist = artist, album = album, track = track, 
                             overall_loudness = overall_loudness, spectral_energy = spectral_energy,
                             dissonance = dissonance, danceability = danceability,
                             pitch_salience = pitch_salience, bpm = bpm,
                             beats_loudness = beats_loudness, tuning_frequency = tuning_frequency,
                             stringsAsFactors = FALSE))
}
colnames(df_notplot)[colnames(df_notplot) == "mean"] = "spectral_energy"
colnames(df_notplot)[colnames(df_notplot) == "mean.1"] = "dissonance"
colnames(df_notplot)[colnames(df_notplot) == "mean.2"] = "pitch_salience"
colnames(df_notplot)[colnames(df_notplot) == "mean.3"] = "beats_loudness"

################ Step 3 Part 1
Essentia_Output_Original <- read.csv("EssentiaOutput/EssentiaModelOutput.csv")

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
LIWC_Output <- read.csv("LIWCOutput/LIWCOutput.csv")

################ Step 4 Part 2
Merged_df_1 <- merge(df_notplot,Essentia_Output_New, by = c("artist", "album", "track"), all = TRUE)
Final_Merged_df <- merge(Merged_df_1,LIWC_Output, by = c("artist", "album", "track"), all = TRUE)

################ Step 4 Part 3
colnames(Final_Merged_df)[colnames(Final_Merged_df) == "function."] = "funct"

################ Step 5 Part 1
write.csv(json_files_new[-120], "trainingdata.csv")
trainingdata.csv = read.csv("trainingdata.csv")

################ Step 5 Part 2
write.csv(json_files_new[120], "testingdata.csv")
testingdata.csv = read.csv("testingdata.csv")


################ Task 3
write.csv(Final_Merged_df, "Final_Merged_df.csv")


################################################################################
#PLOTS
################################################################################


################ Plot 1
library(tidyverse)
####################################
# Load Data
####################################
dat <- read_csv("Final_Merged_df.csv")
####################################
# Select data for plot
####################################
df <- dat %>%
  dplyr::select("bpm", "artist") %>%
  filter(!is.na(!!sym("artist"))) %>%
  mutate(denoted.group = paste("artist", " = ", !!sym("artist"), sep = ""))
####################################
# Create Plot
####################################
p <- ggplot(df, aes(x = !!sym("bpm"), y = after_stat(!!sym("density")))) +
  geom_histogram(breaks = seq(67, 185, length.out = 17), color = "grey30", fill = "lightblue") +
  geom_density(alpha = 0.2, trim = FALSE) +
  get("theme_classic")() +
  xlab("BPM") +
  ylab("Density") +
  ggtitle("Danceability For Each Artist", "") +
  geom_hline(yintercept = 0) +
  facet_wrap(~denoted.group, ncol =1)
####################################
# Print Plot
####################################
p
####################################
# Summarize Data
####################################
dat.summary <- dat %>%
  select(!!sym("bpm"), !!sym("artist")) %>%
  group_by(!!sym("artist")) %>%
  summarize(Observations = sum(!is.na(!!sym("bpm"))), Mean = mean(!!sym("bpm"), na.rm = T), `Standard Deviation` = sd(!!sym("bpm"), na.rm = T), Min = min(!!sym("bpm"), na.rm = T), Q1 = quantile(!!sym("bpm"), probs = 0.25, na.rm = T), Median = median(!!sym("bpm"), na.rm = T), Q3 = quantile(!!sym("bpm"), probs = 0.75, na.rm = T), Max = max(!!sym("bpm"), na.rm = T), IQR = IQR(!!sym("bpm"), na.rm = T)) %>%
  filter(!is.na(!!sym("artist"))) %>%
  tidyr::complete(!!sym("artist")) %>%
  mutate_if(is.numeric, round, 4)
missing.obs <- dat %>%
  summarize(missing = sum(is.na(!!sym("bpm")) | is.na(!!sym("artist")))) %>%
  pull(missing)
dat.summary <- dat.summary %>%
  ungroup() %>%
  add_row(`:=`(!!sym("artist"), "Rows with Missing Data"), Observations = missing.obs, Mean = NA, `Standard Deviation` = NA, Min = NA, Q1 = NA, Median = NA, Q3 = NA, Max = NA, IQR = NA)
####################################
# Print Data Summary
####################################
dat.summary






################ Plot 2
# Load Data
####################################
dat <- read_csv("Final_Merged_df.csv")
####################################
# Select data for plot
####################################
df <- dat %>%
  dplyr::select("acoustic", "artist") %>%
  filter(!is.na(!!sym("artist"))) %>%
  mutate(denoted.group = paste("artist", " = ", !!sym("artist"), sep = ""))
####################################
# Create Plot
####################################
p <- ggplot(df, aes(x = !!sym("acoustic"), y = after_stat(!!sym("density")))) +
  geom_histogram(breaks = seq(0, 1, length.out = 17), color = "grey30", fill = "lightblue") +
  geom_density(alpha = 0.2, trim = FALSE) +
  get("theme_classic")() +
  xlab("Acoustic") +
  ylab("Density") +
  ggtitle("Acousticness per Artist", "") +
  geom_hline(yintercept = 0) +
  facet_wrap(~denoted.group, ncol=1)
####################################
# Print Plot
####################################
p
####################################
# Summarize Data
####################################
dat.summary <- dat %>%
  select(!!sym("acoustic"), !!sym("artist")) %>%
  group_by(!!sym("artist")) %>%
  summarize(Observations = sum(!is.na(!!sym("acoustic"))), Mean = mean(!!sym("acoustic"), na.rm = T), `Standard Deviation` = sd(!!sym("acoustic"), na.rm = T), Min = min(!!sym("acoustic"), na.rm = T), Q1 = quantile(!!sym("acoustic"), probs = 0.25, na.rm = T), Median = median(!!sym("acoustic"), na.rm = T), Q3 = quantile(!!sym("acoustic"), probs = 0.75, na.rm = T), Max = max(!!sym("acoustic"), na.rm = T), IQR = IQR(!!sym("acoustic"), na.rm = T)) %>%
  filter(!is.na(!!sym("artist"))) %>%
  tidyr::complete(!!sym("artist")) %>%
  mutate_if(is.numeric, round, 4)
missing.obs <- dat %>%
  summarize(missing = sum(is.na(!!sym("acoustic")) | is.na(!!sym("artist")))) %>%
  pull(missing)
dat.summary <- dat.summary %>%
  ungroup() %>%
  add_row(`:=`(!!sym("artist"), "Rows with Missing Data"), Observations = missing.obs, Mean = NA, `Standard Deviation` = NA, Min = NA, Q1 = NA, Median = NA, Q3 = NA, Max = NA, IQR = NA)
####################################
# Print Data Summary
####################################
dat.summary






################ Plot 3
# Load Data
####################################
dat <- read_csv("Final_Merged_df.csv")
####################################
# Select data for plot
####################################
df <- dat %>%
  dplyr::select("aggressive", "artist") %>%
  filter(!is.na(!!sym("artist")))
####################################
# Create Plot
####################################
p <- ggplot(df, aes(x = !!sym("artist"), y = !!sym("aggressive"))) +
  geom_boxplot(fill = "lightblue", width = 0.5) +
  get("theme_classic")() +
  xlab("Artist") +
  ylab("Aggressiveness") +
  ggtitle("Agressiveness per Artist", "")
####################################
# Print Plot
####################################
p
####################################
# Summarize Data
####################################
dat.summary <- dat %>%
  select(!!sym("aggressive"), !!sym("artist")) %>%
  group_by(!!sym("artist")) %>%
  summarize(Observations = sum(!is.na(!!sym("aggressive"))), Mean = mean(!!sym("aggressive"), na.rm = T), `Standard Deviation` = sd(!!sym("aggressive"), na.rm = T), Min = min(!!sym("aggressive"), na.rm = T), Q1 = quantile(!!sym("aggressive"), probs = 0.25, na.rm = T), Median = median(!!sym("aggressive"), na.rm = T), Q3 = quantile(!!sym("aggressive"), probs = 0.75, na.rm = T), Max = max(!!sym("aggressive"), na.rm = T), IQR = IQR(!!sym("aggressive"), na.rm = T)) %>%
  filter(!is.na(!!sym("artist"))) %>%
  tidyr::complete(!!sym("artist")) %>%
  mutate_if(is.numeric, round, 4)
missing.obs <- dat %>%
  summarize(missing = sum(is.na(!!sym("aggressive")) | is.na(!!sym("artist")))) %>%
  pull(missing)
dat.summary <- dat.summary %>%
  ungroup() %>%
  add_row(`:=`(!!sym("artist"), "Rows with Missing Data"), Observations = missing.obs, Mean = NA, `Standard Deviation` = NA, Min = NA, Q1 = NA, Median = NA, Q3 = NA, Max = NA, IQR = NA)
####################################
# Print Data Summary
####################################
dat.summary






################ Plot 4
# Load Data
####################################
dat <- read_csv("Final_Merged_df.csv")
####################################
# Select data for plot
####################################
df <- dat %>%
  dplyr::select("happy", "artist") %>%
  filter(!is.na(!!sym("artist")))
####################################
# Create Plot
####################################
plottt <- ggplot(df, aes(x = fct_rev(!!sym("artist")), y = !!sym("happy"))) +
  geom_violin(fill = "darkorchid", trim = FALSE) +
  geom_boxplot(fill = "white", width = 0.1) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9, width = 0.125) +
  get("theme_classic")() +
  xlab("Happy") +
  ylab("Artist") +
  ggtitle("Happiness For Each Artist", "") +
  coord_flip()
####################################
# Print Plot
####################################
plottt
####################################
# Summarize Data
####################################
dat.summary <- dat %>%
  select(!!sym("happy"), !!sym("artist")) %>%
  group_by(!!sym("artist")) %>%
  summarize(Observations = sum(!is.na(!!sym("happy"))), Mean = mean(!!sym("happy"), na.rm = T), `Standard Deviation` = sd(!!sym("happy"), na.rm = T), Min = min(!!sym("happy"), na.rm = T), Q1 = quantile(!!sym("happy"), probs = 0.25, na.rm = T), Median = median(!!sym("happy"), na.rm = T), Q3 = quantile(!!sym("happy"), probs = 0.75, na.rm = T), Max = max(!!sym("happy"), na.rm = T), IQR = IQR(!!sym("happy"), na.rm = T)) %>%
  filter(!is.na(!!sym("artist"))) %>%
  tidyr::complete(!!sym("artist")) %>%
  mutate_if(is.numeric, round, 4)
missing.obs <- dat %>%
  summarize(missing = sum(is.na(!!sym("happy")) | is.na(!!sym("artist")))) %>%
  pull(missing)
dat.summary <- dat.summary %>%
  ungroup() %>%
  add_row(`:=`(!!sym("artist"), "Rows with Missing Data"), Observations = missing.obs, Mean = NA, `Standard Deviation` = NA, Min = NA, Q1 = NA, Median = NA, Q3 = NA, Max = NA, IQR = NA)
####################################
# Print Data Summary
####################################
dat.summary

# Save the violin plot as a PNG
ggsave("violin_plot.png", plot = plottt, width = 8, height = 6, dpi = 300)