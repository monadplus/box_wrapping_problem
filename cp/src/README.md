## Combinatorial Problem: Box Wrapping

### Compiling

I use [Nix](https://nixos.org/nix/about.html) as an immutable dependecy manager, so here are the instructions:

```bash
$ curl -L https://nixos.org/nix/install | sh
$ nix-shell
nix-shell> $ make
```

### Running the examples

I prepared a bash script `run.sh` that runs all instances with a timeout of 60seconds and checks them with the checker:

```bash
$ ./run.sh
```
