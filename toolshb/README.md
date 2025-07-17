# Tools for configuring HyperBEAM nodes

## Installation, Ubuntu

### Install hype
```sh
# Run these in another directory
git clone https://github.com/twilson63/hype
go build -o hype .
# Add the path where your hype binary is to your PATH variable
nano ~/.bashrc
export PATH=$PATH:/your/path/to/hype
```

### Run the cli
```sh
hype build main.lua -t linux -o tools
```