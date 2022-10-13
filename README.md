# latuc

A LAtex-To-UniCode converter.

v1 of latuc is a one-to-one Haskell port of the fabulous [latex2unicode](https://github.com/tomtung/latex2unicode) project [1]. By the time you read this, latex2unicode and latuc may or may not have diverged.

[1]: commit `1c8b7c4af7c0f2aee3937c5c24305fd01063bc1b`, for what it's worth

## Usage

```bash
$ nix-build
$ echo '\mathbf{AB}_{a-1} \cdot \Delta \not\ni \Upsilon' | ./result/bin/latuc
𝐀𝐁ₐ₋₁ ⋅ Δ ∌ Υ
```
