loadhb
=====

HyperBEAM load and integration tests

Build
-----

```sh
chmod 755 build.sh
chmod 755 run.sh
./build.sh
```


Database snapshot flow
-----

```sh
# In one shell
export HYPERBEAM_PATH=/path/to/your/HyperBEAM
chmod 755 starthb.sh
./starthb.sh

# In another shell
./run.sh local_from_db
```


Run
-----

```sh
./run.sh local_basic
```