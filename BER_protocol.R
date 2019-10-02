#!/usr/bin/env Rscript

# libraries
library("argparse")
library("readxl")
library("dplyr")
library("ggplot2")

# Input data from command line arguments
parser <- ArgumentParser(description='Analysis of StepOne fluorescence measurement results')
parser$add_argument("-f", "--fluorescence", required=T, help="Path to StepOne measurement results in xls format")
parser$add_argument("-s", "--samplesetup", required=T,  help="Path to StepOne sample setup file in xls format")
parser$add_argument("-d", "--dye", default="FAM", choices=c("FAM", "SYBR", "JOE", "VIC", "TAMRA", "NEX", "ROX"))
parser$add_argument("-o", "--output", default="out", help="Output dir name")
args <- parser$parse_args()

raw_data_path <- args$fluorescence
samples_data_path <- args$samplesetup
dye <- args$dye
output_path <- args$output

# Import sheets as data.frames
raw_data <- as.data.frame(read_excel(raw_data_path, col_names = T, skip = 7))
samples_data <- as.data.frame(read_excel(samples_data_path, col_names = T, skip = 7))

# Select target channel of detection based on dye specified
if (dye %in% c("FAM", "SYBR")){
  channel <- "BLUE"
} else if (dye %in% c("JOE", "VIC")) {
  channel <- "GREEN"
} else if (dye %in% c("TAMRA", "NEX")) {
  channel <- "YELLOW"
} else {
  channel <- "RED"
}

# Prepare input data
# merge measurement results with sample data
annotated_data <- merge(raw_data, samples_data)
# remove empty wells
annotated_data <- annotated_data %>% filter(!is.na(annotated_data$`Sample Name`))
# select only columns of interest
annotated_data <- subset(annotated_data, select=c("Well", "Cycle", channel, "Sample Name"))
# rename columns
names(annotated_data)[names(annotated_data) == "Sample Name"] <- "Name"
names(annotated_data)[names(annotated_data) == channel] <- "Value"
# correct data types
annotated_data$Well <- as.factor(annotated_data$Well)
annotated_data$Name <- as.factor(annotated_data$Name)

# Plot fluorescence
raw_channel_plot <- ggplot(data = annotated_data) +
  geom_line(aes(x = Cycle, y = Value, color = Well)) +
  theme_bw()

samples_channel_plot <- ggplot(data = annotated_data) +
  geom_boxplot(aes(x = as.factor(Cycle), y = Value, color = Name)) +
  theme_bw()

# Create output files
write.table(annotated_data, 
            file = file.path(output_path, "fluorescence.txt"), 
            row.names = F, quote = F, sep = "\t")
ggsave(filename = file.path(output_path, "raw_data.png"),
       plot = raw_channel_plot,
       width = 250, height = 170, units = "mm")
ggsave(filename = file.path(output_path, "samples_data.png"),
       plot = samples_channel_plot,
       width = 250, height = 170, units = "mm")

print("Ok")