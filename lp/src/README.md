## Linear Programming: Box Wrapping

### Compiling

You will need to change the routes in the Makefile in order to compile the project (I added a comment about the lines that need to be changed). Afterwards, `make` should work out of the box.

```bash
$ vim Makefile
$ make
```

### Running the examples

I prepared a bash script `run.sh` that runs all instances with a timeout of 60seconds and checks them with the checker:

```bash
$ ./run.sh
```
