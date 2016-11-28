# Compilation

```
sbt -J"-Dcom.github.fommil.netlib.BLAS=com.github.fommil.netlib.NativeRefBLAS"  assembly
```

#Running

```
java -jar -Dcom.github.fommil.netlib.BLAS=com.github.fommil.netlib.NativeRefBLAS  path/to/cpd-assembly-1.0.jar
```
