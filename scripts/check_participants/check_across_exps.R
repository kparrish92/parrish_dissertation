# A script to check which prolific ids can be added to new phases
# of the dissertation given the "demographic data" from the prolific
# experiment page 

# libs

library(here)
library(tidyverse)

# add current allowlist 

allowed_list = c("5dc483cf74939e34e55c3619", 
"5ef9df1812148a78af8e76dc", 
"5fb0a3d6a8e4224b973e750b", 
"5e27868c5cbbcc0b3599574b", 
"601e1253bbecc3279e4fb472", 
"60e1d5f866e681d7e33fd01c", 
"6125b040ffa3c0b9e67b92c9", 
"60c24c8ec01a8596ba5c8b85", 
"6124d122d91be3f612508804", 
"5bebd4111296920001d55c5a", 
"5e169361afb8b0c69c239371", 
"6108b317855b3eb7cd588352", 
"5898b6e256357700011c2c6b", 
"60f83b167c76803d88b92d53", 
"61094c7cb8c79b6a734c2d0c", 
"5ff5f7ad932d56101bf7c90d", 
"5eeabb4143299b1e176fb83e",
"5f9057735c69b00e72665f84", 
"5fed41e3e14422f5e308c6e5", 
"5a5479b4ac56240001537ffa", 
"6108678bdc10bef3ef27f7f0", 
"6100c28ed43a1dea00f08633", 
"6112c911a83cc494df38b468", 
"5ddc07391e42a9000b2622be", 
"60bc18bb8b971280ef13060c", 
"565a4547c51d43000587507d", 
"62029a4cef1cb18a1337c607", 
"60121fca666552254559110d", 
"600dd1ea271cf06201979123", 
"5ffe6b8b773ec452c1d28adc", 
"60b7f09a4462d1d1aa6bbcf6", 
"60b9af795939f976cb74232a", 
"604fc38a4de8e6e1e0e0ea7b", 
"554856d0fdf99b34b6cc7276", 
"60d79f82b917a3cfd6643432", 
"5c2fae74d76cde0001c11590", 
"5bcc60b95c2b810001dc68e2", 
"558a9729fdf99b6567d005ab", 
"5ed1cd25eb6dc1122dbd700f", 
"59349d79f1b3f00001964d04", 
"60fdf26bd58032c8ab446722", 
"60fe4c46e72a8b28dc55d376", 
"60dcfda0c88e0fd28fa697ea",
"61741ba984f258d5c6a1303c",
"5ef59d011c4119208ae8e40d",
"6100496a0ef6706b8895cbf7",
"60fcdd8599d18f3caf737f38",
"6109ab59c73e95d7a1f004a5",
"5aef97b09516fe00010959f4",
"6112f31f34628c8bc50e3aa7",
"6105ac9411551a0880809277",
"60fdf50aedd35312d90f94fd",
"5c6b56058a12bb00013dd2aa",
"5a08c9a7f2e3460001edb063")

lextale_done = read.csv(here("scripts", "check_participants", 
                             "data", "lextale_done.csv")) %>% 
  filter(status == "AWAITING REVIEW")


df <- lextale_done %>% 
  filter(!participant_id %in% allowed_list)

to_add = as.tibble(df$participant_id)

