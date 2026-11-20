# chez-analysis

Source analysis for Chez Scheme tools: a lossless reader, text positions, diagnostics, `.cz` configuration and project layout.

```sh
make SCHEME=/path/to/chez-10.4.1/scheme build
make SCHEME=/path/to/chez-10.4.1/scheme test
```

Programs import `(analysis ...)` with `--libdirs /path/to/chez-analysis/build/obj/src`.
