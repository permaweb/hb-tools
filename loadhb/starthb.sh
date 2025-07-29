
TOOLS_PATH=$(pwd)
kill $(lsof -t -i :6363)
cd $HYPERBEAM_PATH
rm -rf cache-mainnet cache-HTTP
mkdir -p cache-mainnet
cp -r $TOOLS_PATH/data/dbsnap/* cache-mainnet/
rebar3 as genesis_wasm shell