source(here::here("scripts", "r", "eng_mono", "0_libs.R"))
source(here::here("scripts", "r", "eng_mono", "03_load_data.R"))


compare_vowels(eng_mono_df, eng_bil_df, vowel = "i")

compare_vowels(eng_mono_df, eng_bil_df, vowel = "o")

compare_vowels(eng_mono_df, eng_bil_df, vowel = "u")

compare_vowels(eng_mono_df, eng_bil_df, vowel = "schwa")

compare_vowels_all(eng_mono_df, eng_bil_df)

compare_vowels_all(span_bil_df, eng_bil_df)

compare_vowels_all_3(span_bil_df, eng_bil_df, eng_mono_df)