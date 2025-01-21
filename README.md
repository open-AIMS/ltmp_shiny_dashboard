# Shiny front end preview dashboard for AIMS long term benthic monitoring

## Purpose

   The purpose of this shiny app is to act as a front end to the on-prem
   instance of the LTMP monitoring dashboard.
   
   ```mermaid
  flowchart TD 
    subgraph "~/dashboard"
      Shiny@{shape: docs, label: "Shiny dashboard"}
      Sql@{shape: card, label: sql files}
    end
    subgraph "~/dev"
      subgraph "~/dev/R"
      Batch@{shape: card, label: batch.R}
      dbExport@{shape: doc, label: dbExport.jar}
      PP@{shape: doc, label: post_db_extract.R}
      Run_model@{shape: card, label: run_model.R}
      end
      subgraph Docker[Docker container]
       R
      end
    end
    subgraph "~/data"
      DB@{shape: cyl, label: "dashboard.sqlite"}
      Data@{shape: lin-cyl, label: csv files}
    end
    subgraph AIMS[AIMS network]
      Oracle@{shape: cyl, label: "Oracle\ndatabase"}
    end
    
    Shiny ---->|4.Fit model| Batch -->|4.Fitmodel| Run_model
    Shiny -->|3.Prepare folders| Batch -->|3.Prepare folders| PP
    Shiny -->|2.Process data| Batch -->|2.Process data| PP
    Shiny -->|1.Run sql| Batch --> |1.Run sql| dbExport
    Shiny --> Run_model
    Shiny <--> DB
    Batch <--> DB
    PP --> Data
    dbExport --> Oracle --> Data
    Sql --> dbExport 
    Run_model -->|docker run| R
    R ----> |mount|Data 
   ```

## Accessing the shiny app
   
   The shiny app is available at
   http://tsv-ltmp-proc.aims.gov.au:3838/dashboard/. **Note** only
   AIMS employees who are either on-site or on the VPN can access this
   site
