This step performs the following:

1.  extracts data from the Oracle database via an sql (which can be
  reviewed from the "Manual" tab
2. run a post-export processing script to standardise the format of the
  extracted data to ensure it is consistent with that produced on the
  public-facing dashboard
3. prepare buckets of data for each of:

  - sectors: individual folders (containing data) for each individual
    sector
  - nrm regions: individual folders (containing data) for each
    individual NRM region
  - reefs: individual folders (containing data) for each individual
    reef
  
<div class="callout call-info"><h4>Note</h4> 
Since this third step is specific to individual spatial scales,
it is also invokable from the individual "Prepare Sectors", "Prepare
NRMs" and "Prepare Reefs" tabs. Thus, once the above auto steps have
been completed, there is no need to repeat the "Prepare sectors for
modelling" steps in the associated "Prepare ..." tabs.
</div>
