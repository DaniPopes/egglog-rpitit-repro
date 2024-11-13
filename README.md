Massive compile time difference between `RPITIT` "map" closure and a manually defined `Map` struct that implements the traits.

Extracted from [`egglog`](https://github.com/egraphs-good/egglog) @ [`ca52ac13cb3c0bbacc8e7cc540789521d1019bd2`](https://github.com/egraphs-good/egglog/commit/ca52ac13cb3c0bbacc8e7cc540789521d1019bd2). Issue: <https://github.com/egraphs-good/egglog/issues/468>.

Note that unboxed closures etc is not required, as seen in https://github.com/egraphs-good/egglog/pull/470 this can be fixed on stable by changing the definition but I opted to use the nightly feature to make the diff smaller.
It can also be fixed by moving `map` outside of the trait.

Reproduce:

```bash
# Takes 48s
time cargo clean && cargo build

# Takes 0.2s
git apply fix.patch
time cargo clean && cargo build
```

On nightly:

```bash
rustc -Vv
rustc 1.84.0-nightly (fbab78289 2024-11-04)
binary: rustc
commit-hash: fbab78289dd8c6e8860034e0048cfb538f217700
commit-date: 2024-11-04
host: x86_64-unknown-linux-gnu
release: 1.84.0-nightly
LLVM version: 19.1.3
```

On stable (with `RUSTC_BOOTSTRAP=1`):

```bash
rustc +stable -Vv
rustc 1.82.0 (f6e511eec 2024-10-15)
binary: rustc
commit-hash: f6e511eec7342f59a25f7c0534f1dbea00d01b14
commit-date: 2024-10-15
host: x86_64-unknown-linux-gnu
release: 1.82.0
LLVM version: 19.1.1
```
