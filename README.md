Massive compile time difference between `RPITIT` "map" closure and a manually defined `Map` struct that implements the traits.

Extracted from [`egglog`](https://github.com/egraphs-good/egglog) @ [`ca52ac13cb3c0bbacc8e7cc540789521d1019bd2`](https://github.com/egraphs-good/egglog/commit/ca52ac13cb3c0bbacc8e7cc540789521d1019bd2). Issue: <https://github.com/egraphs-good/egglog/issues/468>.

Note that unboxed closures etc is not required, as seen in https://github.com/egraphs-good/egglog/pull/470 this can be fixed on stable by changing the definition but I opted to use the nightly feature to make the diff smaller.
It can also be fixed by moving `map` outside of the trait.

Reproduce:

```bash
# Takes 48s
time cargo clean && cargo build

# Takes 0.2s
git restore . && git apply fix.patch
time cargo clean && cargo build

# Takes 3m30s+ for a single extra no-op map
git restore . && git apply worse.patch
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

---

Output of `cargo rustc -- -Ztime-passes`:

```
time:   0.000; rss:   83MB ->   87MB (   +4MB)  setup_global_ctxt
time:   0.006; rss:   87MB ->  117MB (  +31MB)  expand_crate
time:   0.006; rss:   87MB ->  117MB (  +31MB)  macro_expand_crate
time:   0.003; rss:  117MB ->  125MB (   +7MB)  late_resolve_crate
time:   0.003; rss:  117MB ->  125MB (   +7MB)  resolve_crate
time:   0.005; rss:  125MB ->  131MB (   +6MB)  looking_for_entry_point
time:   0.005; rss:  125MB ->  131MB (   +6MB)  unused_lib_feature_checking
time:   0.006; rss:  125MB ->  131MB (   +6MB)  misc_checking_1
time:   0.020; rss:  131MB ->  169MB (  +38MB)  coherence_checking
time:   0.064; rss:  131MB ->  192MB (  +61MB)  type_check_crate
time:   0.026; rss:  192MB ->  199MB (   +7MB)  MIR_borrow_checking
time:  43.618; rss:  199MB ->  208MB (   +9MB)  MIR_effect_checking
time:   0.004; rss:  208MB ->  208MB (   +0MB)  privacy_checking_modules
time:   0.003; rss:  208MB ->  208MB (   +0MB)  lint_checking
time:   0.000; rss:  208MB ->  208MB (   +0MB)  check_lint_expectations
time:   0.005; rss:  208MB ->  208MB (   +1MB)  misc_checking_3
time:   0.002; rss:  208MB ->  210MB (   +1MB)  monomorphization_collector_graph_walk
time:   0.000; rss:  212MB ->  219MB (   +8MB)  write_allocator_module
time:   0.003; rss:  219MB ->  227MB (   +8MB)  compile_first_CGU_batch
time:   0.006; rss:  219MB ->  247MB (  +28MB)  codegen_to_LLVM_IR
time:   0.011; rss:  208MB ->  247MB (  +39MB)  codegen_crate
time:   0.000; rss:  247MB ->  246MB (   -1MB)  check_dirty_clean
time:   0.000; rss:  246MB ->  246MB (   +0MB)  incr_comp_persist_dep_graph
time:   0.005; rss:  227MB ->  246MB (  +19MB)  LLVM_passes
time:   0.002; rss:  246MB ->  243MB (   -3MB)  encode_query_results
time:   0.002; rss:  246MB ->  243MB (   -3MB)  incr_comp_serialize_result_cache
time:   0.002; rss:  246MB ->  243MB (   -3MB)  incr_comp_persist_result_cache
time:   0.002; rss:  247MB ->  243MB (   -4MB)  serialize_dep_graph
time:   0.003; rss:  243MB ->  202MB (  -41MB)  free_global_ctxt
time:   0.031; rss:  202MB ->  202MB (   +0MB)  run_linker
time:   0.031; rss:  202MB ->  202MB (   +0MB)  link_binary
time:   0.031; rss:  202MB ->  202MB (   +0MB)  link_crate
time:   0.032; rss:  202MB ->  202MB (   +0MB)  link
time:  43.784; rss:   31MB ->  147MB ( +116MB)  total
```

---

[`samply`](https://github.com/mstange/samply) profile: <https://share.firefox.dev/3O7AzuO>
