library(tidyverse)
library(lubridate)
library(stringr)

check.annotations <- function(annfile, nameannfile) {
  
# ---------- set up ----------
  
  ## ------ alert table builder ------
  
  alert.table <- tibble(
    filename = character(),
    alert = character(),
    severity = character(),
    onset = integer(),
    offset = integer(),
    tier = character(),
    value = character()
  )
  
  ## ------ msec conversion ------
  
  convert_ms_to_hhmmssms <- function(msectime) {
    
    ifelse(
      is.na(msectime),
      "none",
      {
        ms_p_hr <- 3600000
        ms_p_mn <- 60000
        ms_p_sc <- 1000
        
        hh <- floor(msectime / ms_p_hr)
        msectime2 <- msectime %% ms_p_hr
        mm <- floor(msectime2 / ms_p_mn)
        msectime3 <- msectime2 %% ms_p_mn
        ss <- floor(msectime3 / ms_p_sc)
        msec <- msectime3 %% ms_p_sc
        
        sprintf("%02d:%02d:%02d.%03d", hh, mm, ss, msec)
      }
    )
  }
  
  ## ------- add alerts function ------
  
  add_alert <- function(filename, alert, severity, onset, offset, tier, value) {
    tibble(
      filename = filename,
      alert = alert,
      severity = severity,
      onset = onset,
      offset = offset,
      tier = tier,
      value = value
    )
  }
  
  ## ------ load txt file ------
  
  annots <- read_tsv(
    annfile,
    col_names = FALSE,
    locale = locale(encoding = "UTF-8")
  ) %>%
    rename(
      tier = X1,
      speaker = X2,
      onset = X3,
      offset = X4,
      duration = X5,
      value = X6
    )
  
  filename <- str_remove(nameannfile, "\\.txt$")
  
  
# ---------- check functions ---------
  
  ## ------ check parent tier names ------
  
  validate_parent_tiers <- function(df) {
    
    parent_tier_pattern <- "^[FM][AC][1-9][0-9]*$|^UC[1-9][0-9]*$|^CHI$"
    
    bad_parent_tiers <- df %>%
      filter(!grepl("^xds@[FMU][AC][1-9][0-9]*$", tier)) %>% 
      filter(!grepl(parent_tier_pattern, tier)) %>%
      distinct(tier)
    
    if (nrow(bad_parent_tiers) == 0) return(tibble())
    
    output <- bad_parent_tiers %>%
      mutate(
        filename = filename,
        alert = "illegal tier format",
        severity = "error",
        onset = NA,
        offset = NA,
        tier = tier,
        value = ""
      ) %>%
      select(filename, alert, severity, onset, offset, tier, value)
    
    return(output)
  }
  
  ## ------ check xds tiers and annotations ------
  
  validate_xds <- function(df) {
    
    out <- tibble()
    
    xds_df <- df %>% filter(grepl("^xds@", tier))
    
    # 1. check xds tier format
    bad_xds_tiers <- xds_df %>%
      filter(!grepl("^xds@[FMU][AC][1-9][0-9]*$", tier)) %>%
      distinct(tier)
    
    if (nrow(bad_xds_tiers) > 0) {
      out <- bind_rows(
        out,
        bad_xds_tiers %>%
          mutate(
            filename = filename,
            alert = "illegal xds tier format",
            severity = "error",
            onset = NA,
            offset = NA,
            value = ""
          ) %>%
          select(filename, alert, severity, onset, offset, tier, value)
      )
    }
    
    # 2. check xds annotation values
    bad_xds_vals <- xds_df %>%
      filter(!value %in% c("T", "O"))
    
    if (nrow(bad_xds_vals) > 0) {
      out <- bind_rows(
        out,
        bad_xds_vals %>%
          mutate(
            filename = filename,
            alert = "illegal xds annotation value",
            severity = "error"
          ) %>%
          select(filename, alert, severity, onset, offset, tier, value)
      )
    }
    
    return(out)
  }
  
  ## ------ check modality markers ------
  
  validate_modality <- function(df) {
    
    out <- tibble()
    
    utts <- df %>% filter(!is.na(value))
    
    for (i in seq_len(nrow(utts))) {
      
      utt <- utts$value[i]
      
      # 1. angle bracket mismatch
      if (str_count(utt, "<") != str_count(utt, ">")) {
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "unmatched angle brackets", "error",
            utts$onset[i], utts$offset[i],
            utts$tier[i], utt
          )
        )
      }
      
      # 2. square bracket mismatch
      if (str_count(utt, "\\[") != str_count(utt, "\\]")) {
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "unmatched square brackets", "error",
            utts$onset[i], utts$offset[i],
            utts$tier[i], utt
          )
        )
      }
      
      # 3. illegal sequence checks
      if (str_detect(utt, ">(?! \\[=! )")) {
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "illegal modality sequence", "error",
            utts$onset[i], utts$offset[i],
            utts$tier[i], utt
          )
        )
      }
      
      # 4. terminal mark before >
      if (str_detect(utt, "[.!?]\\s*>")) {
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "terminal mark before '>'", "error",
            utts$onset[i], utts$offset[i],
            utts$tier[i], utt
          )
        )
      }
      
      # extract contents inside modality markers
      modality_contents <- str_extract_all(
        utt,
        "(?<=\\[=! )[^\\]]+(?=\\])"
      )[[1]]
      
      # 5. modality verb must end in lowercase s
      if (length(modality_contents) > 0 &&
          any(!str_detect(modality_contents, "s$"))) {
        
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "modality marker must end in 's]'", "error",
            utts$onset[i], utts$offset[i],
            utts$tier[i], utt
          )
        )
      }
      
      # 6. missing space/comma/terminal mark after ]
      if (str_detect(utt, "\\](?!($| |,|[.!?]))")) {
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "missing space/comma/terminal mark after ']'", "error",
            utts$onset[i], utts$offset[i],
            utts$tier[i], utt
          )
        )
      }
      
      # 7. punctuation inside modality marker
      if (length(modality_contents) > 0 &&
          any(str_detect(modality_contents, "[.!?]"))) {
        
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "illegal punctuation inside modality marker", "error",
            utts$onset[i], utts$offset[i],
            utts$tier[i], utt
          )
        )
      }
    }
    
    return(out)
  }
  
  ## ------ check punctuation ------
  
  validate_punctuation <- function(df) {
    
    out <- tibble()
    
    illegal_punct <- "[;/`~#$%^&*()+]"
    
    utts <- df %>% filter(!is.na(value), !str_detect(tier, "^xds"))
    
    for (i in seq_len(nrow(utts))) {
      
      utt <- utts$value[i]
      
      # ignore modality exclamation mark in terminal counting
      utt_no_modality_markers <- str_replace_all(
        utt,
        "\\[=!",
        "[="
      )
      
      # 1. illegal punctuations
      if (str_detect(utt, illegal_punct)) {
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "illegal punctuation",
            "error",
            utts$onset[i], utts$offset[i],
            utts$tier[i], utt
          )
        )
      }
      
      # 2. double punctuation
      if (str_detect(utt, "([,.!?])\\1")) {
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "double punctuation",
            "error",
            utts$onset[i], utts$offset[i],
            utts$tier[i], utt
          )
        )
      }
      
      # 3. must end in terminal mark
      if (!str_detect(utt, "[.!?]$")) {
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "missing terminal punctuation at utterance end",
            "error",
            utts$onset[i], utts$offset[i],
            utts$tier[i], utt
          )
        )
      }
      
      # 4. more than one terminal mark in utterance
      terminal_count <- str_count(utt_no_modality_markers, "[.!?]")
      
      if (terminal_count > 1) {
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "multiple terminal marks in utterance",
            "error",
            utts$onset[i], utts$offset[i],
            utts$tier[i], utt
          )
        )
      }
      
      # 5. underscore must be between letters
      if (str_detect(utt, "(?<![A-Za-z])_|_(?![A-Za-z])")) {
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "illegal underscore use",
            "error",
            utts$onset[i], utts$offset[i],
            utts$tier[i], utt
          )
        )
      }
      
      # 6. use of &= is illegal
      if (str_detect(utt, "&=")) {
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "illegal &= usage",
            "error",
            utts$onset[i], utts$offset[i],
            utts$tier[i], utt
          )
        )
      }
      
      # 7. hyphens warnings
      if (str_detect(utt, "-")) {
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "hyphen usage",
            "warning",
            utts$onset[i], utts$offset[i],
            utts$tier[i], utt
          )
        )
      }
      
      # 8. illegal tag sign usage
      if (str_detect(utt, "@(?!l\\b|c\\b|s:)")) {
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "illegal tag sign syntax",
            "error",
            utts$onset[i], utts$offset[i],
            utts$tier[i], utt
          )
        )
      }
    }
    
    return(out)
  }
  
  ## ------ check spacing ------
  
  validate_spacing <- function(df) {
    
    out <- tibble()
    
    # 1. double spaces
    bad_double <- df %>% filter(str_detect(value, "  "))
    
    if (nrow(bad_double) > 0) {
      out <- bind_rows(
        out,
        add_alert(
          filename,
          "double spacing", 
          "error",
          bad_double$onset, bad_double$offset,
          bad_double$tier, bad_double$value
        )
      )
    }
    
    # 2. space before terminal mark
    letter_space_terminal <- df %>% 
      filter(str_detect(value, "[A-Za-z]\\s[.!?]"))
    
    if (nrow(letter_space_terminal) > 0) {
      out <- bind_rows(
        out,
        add_alert(
          filename,
          "space before terminal mark", 
          "warning",
          letter_space_terminal$onset, letter_space_terminal$offset,
          letter_space_terminal$tier, letter_space_terminal$value
        )
      )
    }
    
    return(out)
  }
  
  ## ------ check capitalization ------
  
  validate_caps <- function(df) {
    
    out <- tibble()
    
    # 1. contiguous capitalized letters
    caps_multi <- df %>% filter(!str_detect(tier, "^xds"),
                                str_detect(value, "[A-Z]{2,}"))
    
    if (nrow(caps_multi) > 0) {
      out <- bind_rows(
        out,
        add_alert(
          filename,
          "contiguous capitalized letters", 
          "warning",
          caps_multi$onset, caps_multi$offset, 
          caps_multi$tier, caps_multi$value
        )
      )
    }
    
    # 2. flag capitalized letter
    caps_any <- df %>% filter(!str_detect(tier, "^xds"),
                              str_detect(value, "[A-Z]"))
    
    if (nrow(caps_any) > 0) {
      out <- bind_rows(
        out,
        add_alert(
          filename,
          "capitalized letter", "warning",
          caps_any$onset, caps_any$offset, 
          caps_any$tier, caps_any$value
        )
      )
    }
    
    return(out)
  }
  
  ## ------ check empty segments and digits ------
  
  validate_empty_and_numbers <- function(df) {
    
    out <- tibble()
    
    for (i in seq_len(nrow(df))) {
      
      utt <- df$value[i]
      
      # 1. empty utterances
      if (is.na(utt) || str_trim(utt) == "") {
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "empty utterance", "warning",
            df$onset[i], df$offset[i],
            df$tier[i], utt
          )
        )
      }
      
      # 2. use of numerical digits
      if (!is.na(utt) && utt != "0." && str_detect(utt, "\\d")) {
        out <- bind_rows(
          out,
          add_alert(
            filename,
            "numerical digits present", "error",
            df$onset[i], df$offset[i],
            df$tier[i], utt
          )
        )
      }
    }
    
    return(out)
  }
  
  
  
# ---------- the end! ---------
  
  ## ------ run all checks ------ 
  
  alert.table <- bind_rows(
    validate_parent_tiers(annots),
    validate_xds(annots),
    validate_modality(annots),
    validate_punctuation(annots),
    validate_spacing(annots),
    validate_caps(annots),
    validate_empty_and_numbers(annots)
  )
  
  ## ------ all errors output ------
  
  if (nrow(alert.table) > 0) {
    
    alert.table <- alert.table %>%
      mutate(
        start = convert_ms_to_hhmmssms(onset),
        stop = convert_ms_to_hhmmssms(offset)
      ) %>%
      select(-onset, -offset)
    
    return(list(
      alert.table = alert.table,
      n.a.alerts = nrow(alert.table)
    ))
    
  } else {
    
    return(list(
      alert.table = tibble(
        filename = filename,
        alert = "No errors detected! :D",
        severity = "none"
      )))
  }
}