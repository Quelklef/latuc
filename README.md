# latuc

A LAtex-To-UniCode converter

Forked from [latex2unicode](https://github.com/tomtung/latex2unicode)<sup>[1]</sup>, ported from Scala to Haskell, Nixified, and with some small tweaks.

<sup>[1]: at `1c8b7c4af7c0f2aee3937c5c24305fd01063bc1b`</sup>

## Usage

```bash
$ nix-build
$ echo '\mathbf{AB}_{a-1} \cdot \Delta \not\ni \Upsilon' | ./result/bin/latuc
𝐀𝐁ₐ₋₁ ⋅ Δ ∌ Υ
```
