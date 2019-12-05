# Lift-Spatial tutorial #
This page suggests some practical exercises to get familiar with Lift development.

## Basic examples ##
Play around with examples in `src/test/tutorial` to familiarise yourself with the Lift language. These examples use the 
OpenCL backend with a separate compiler chain. It is a bit more complicated than the Spatial backend since it also includes
the Executor that runs the compiled code. But the user-facing side is the same.
 
Find the code lines where the expressions are compiled. 
  
`src/test/tutorial/Views.stencil1D()` generates a graphical PDF representation of a simple AST; 
as a dependency, it requires `dot` that can be installed with `sudo apt-get install graphviz`. 
The generated PDF will be placed in the `/tmp` directory.

## Add a new primitive ##
To familiarise yourself with the Lift-Spatial compiler chain, introduce a new primitive `MapAccumRightSeq` 
that maps its function on elements from right to left (`MapAccumSeq` maps from left to right). Lift's `MapAccumSeq` 
behaves exactly like Haskell's `mapAccumL`: 
[https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:mapAccumL].
Another description of `mapAccumL` and examples is here: [http://zvon.org/other/haskell/Outputlist/mapAccumL_f.html].
The primitive you wil limplement behaves like Haskell's `mapAccumR`: [http://zvon.org/other/haskell/Outputlist/mapAccumR_f.html].
 
You can use `backends.spatial.accel.ir.pattern.MapAccumSeq` as an example and add your changes as suggested below. 
The instructions refer to default keyboard shortcuts in the IntelliJ IDEA.

1. Find `MapAccumSeq` by pressing Shift twice and typing its name.
2. In `MapAccumSeq` source file (`src/main/backends/spatial/accel/ir/pattern/MapAccumSeq.scala`), find `case class MapAccumSeq`.
3. Add the new file `MapAccumRightSeq` besides `MapAccumSeq`. Implement it.
4. Take a look at `src/main/backends/spatial/accel/AccelCompiler` (`Shift-Shift -> "AccelCompiler"`). 
These are all compiler passes (components) that will require your attention.
5. Right-click on `MapAccumSeq` class name, and click `Find Usages`. This will show you all files where the class is used.
 (If you do the same of the singleton object `MapAccumSeq` in the same file, you'll find which tests apply `MapAccumSeq`).
6. Make sure all compiler components that operate on `MapAccumSeq`, also handle your new `MapAccumSeq`.
7. **Hint:** Once you get to the code generator `AccelGenerator`, you might realise that you need the loop that you generate 
for `MapAccumRightSeq` to have its counter decrease on each step. For that to happen, go back to your new `MapAccumRightSeq` 
and change the way the singleton object `MapAccumRightSeq` initialises `loopVar`.

At this point, you should have implemented everything the backend needs. 
Now, add a test example to make sure the generated code is correct. 

1. Take a look at `src/test/backends/spatial/generator/ReduceSeqTest.scala` (`Shift-Shift -> "ReduceSeqTest"`).
This is a minimum example of code generation using the Spatial backend in Lift.
2. Using `ReduceSeqTest` as an example, implement examples from the Haskell documentation of `mapAccumR`: 
[http://zvon.org/other/haskell/Outputlist/mapAccumR_f.html].
3. To run the compiled code, copy-paste it to the Spatial repository and feed it some test data. 