## SAT: Box Wrapping

For detailed information about the variables and constraints read the [report](./report.pdf).

### Compiling the project

> Beware, the first compilation may take 15-20' because it needs to compile the whole Haskell's compiler - GHC and all its dependencies.

In order to run this project, you will need to install [stack](https://docs.haskellstack.org/en/stable/README/):

```bash
curl -sSL https://get.haskellstack.org/ | sh

# Alternatively:
wget -qO- https://get.haskellstack.org/ | sh
```

Once `stack` is installed, you can compile it.
On the root of the project, execute the following:

```bash
stack install
```

This should have installed the executable `sat` in the folder `/home/arnau/.local/bin` (otherwise check the output of the command to see where it was instaled). You can add this to your `$PATH` to call the programm from your terminal by `./sat`.

### Running the examples

I prepared a bash script `run.sh` that runs all instances with a timeout of 60 seconds and checks them with the checker:

```bash
# First copy the executable to the root of your project
cp /path/to/sat .
# Compile the checker.cc
make checker
# Run the checker
./run.sh
```
