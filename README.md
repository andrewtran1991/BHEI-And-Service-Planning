
# Service Planning Tool

This project provides a comprehensive tool for generating detailed reports on social, economic, housing, and demographic profiles for various regions. The tool is designed to assist in service planning, ensuring that provisions are informed by data and tailored to the cultural and regional specifics of the target communities. 

## Overview

The Service Planning Tool leverages data from the U.S. Census Bureau's American Community Survey (ACS) for the 5-year period from 2017 to 2021. It generates reports that summarize key differences in selected areas compared to San Diego County, helping identify regions with the greatest need for behavioral health services.

## Features

- **Data-Driven Reports**: Summarizes social, economic, housing, and demographic data for selected regions.
- **Key Differences Identification**: Highlights areas with significant differences compared to the county average.
- **Customizable**: Users can select specific regions and indicators for the analysis.
- **Exportable Outputs**: Generates reports in various formats including PDF and Excel.

## Methodology

The methodology used to identify "Key Differences" in the reports includes:

1. **Ranking Indicators**: Each of the 50+ indicators is ranked by geography.
2. **Criteria for Key Differences**:
   - The area is in the top or bottom 25% (quartile) in terms of percentage.
   - The difference between the area estimate and the county estimate is at least 5 percentage points.
   - There are at least four geographies available to rank.

## Installation

To install and run the Service Planning Tool, follow these steps:

1. **Clone the repository**:
    ```bash
    git clone https://github.com/yourusername/service-planning-tool.git
    ```
2. **Install required dependencies**:
    Ensure you have R and RStudio installed. Then, install the required R packages:
    ```R
    install.packages(c("rmarkdown", "dplyr", "ggplot2", "DT"))
    ```
3. **Run the tool**:
    Open the R Markdown files (`service_planning_report.Rmd` and `service_planning_summary.Rmd`) in RStudio and knit the documents to generate the reports.

## Usage

1. **Select Regions**: Define the regions of interest in the R Markdown files.
2. **Generate Reports**: Knit the R Markdown files to produce detailed reports.
3. **Review Outputs**: The generated reports will include key differences and comprehensive data summaries.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Acknowledgements

This tool was developed by UC San Diego in collaboration with the County of San Diego Behavioral Health Services.

---

Feel free to customize this README file further to better fit the specifics of your project. Let me know if you need any additional details or modifications!
