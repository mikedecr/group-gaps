# download ANES cumulative file
echo "downloading ANES data"
mkdir tmp
cd tmp
curl -LO https://electionstudies.org/anes_timeseries_cdf_stata_20220916

# move it to data folder
echo "relocating to /data"
cd ..
mv tmp/anes_timeseries_cdf_stata_20220916 data/anes_2020.zip

# unzip
echo "unzipping ANES data in data/anes_2020"
unzip data/anes_2020.zip -d data/anes_2020

echo "you may delete tmp/ directory if you wish"


