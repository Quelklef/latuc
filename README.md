# latuc

A **La**TeX-**t**o-**u**ni**c**ode converter.

v1 of latuc is a one-to-one Haskell port of the fabulous [latex2unicode](https://github.com/tomtung/latex2unicode) project (commit `1c8b7c4af7c0f2aee3937c5c24305fd01063bc1b`, for what it's worth). By the time you read this, latex2unicode and latuc may or may not have diverged.

## Usage

```bash
$ nix-build
$ echo '\mathbf{AB}^x_{\Delta-1}' | ./result/bin/latuc
𝐀𝐁ˣ_(Δ-1)
```
