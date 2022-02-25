echo "*** Layer config ***"
echo "inputChannels -> 512, inputWidth -> 14, inputHeight -> 14,"
echo "padFuncX -> 1, padFuncY -> 1,"
echo "kernelWidth -> 3, kernelHeight -> 3,"
echo "kernelStrideX -> 1, kernelStrideY -> 1,"
echo "numKernels -> 512"
echo "*** Rewrite point params *** "
echo "tiling_hook_ABTiling.cacheAaddressSpace -> None, tiling_hook_ABTiling.cacheBaddressSpace -> None, tiling_hook_ABTiling.orderOfCacheADims -> None, tiling_hook_ABTiling.orderOfCacheBDims -> Some(List(1, 0))"
echo "tiling_hook_ABTiling.vectorizeTileALoad_Vectorize.vectorLen -> None"
echo "tiling_hook_ABTiling.vectorizeTileBLoad_Vectorize.vectorLen -> None"
echo "DivideAndConquer.conquerFun_0_Privatize.resultAddressSpace -> Some(global)"
echo "caching_hook_ABTiling.cacheAaddressSpace -> Some(private), caching_hook_ABTiling.cacheBaddressSpace -> Some(private), caching_hook_ABTiling.orderOfCacheADims -> None, caching_hook_ABTiling.orderOfCacheBDims -> Some(List(0, 1))"
echo "caching_hook_ABTiling.vectorizeTileALoad_Vectorize.vectorLen -> None"
echo "caching_hook_ABTiling.vectorizeTileBLoad_Vectorize.vectorLen -> None"
echo "Privatize.resultAddressSpace -> Some(global)"
echo "partReduceVectorize_hook_Vectorize.vectorLen -> None"
echo "*** Rewrite pass params *** "
echo "mapTransform.24.p4242 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.26.p9218 -> Some(Map(f) => MapGlb(f))"
echo "mapTransform.27.p6358 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.31.p5182 -> Some(Map(f) => MapGlb(f))"
echo "mapTransform.32.p26808 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.33.p53101 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.34.p34159 -> Some(AbstractMap1(AbstractMap2(f)) => Split(I) o AbstractMap1(f) o Join())"
echo "mapTransform.35.p29147 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.85.p45979 -> Some(Map(f) => MapGlb(f))"
echo "mapTransform.104.p46627 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.105.p41025 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.106.p3187 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.109.p57588 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.110.p44274 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.111.p4301 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.112.p64983 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.139.p7884 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.140.p35565 -> Some(AbstractMap1(AbstractMap2(f)) => Split(I) o AbstractMap1(f) o Join())"
echo "mapTransform.141.p28491 -> Some(AbstractMap1(AbstractMap2(f)) => Split(I) o AbstractMap1(f) o Join())"
echo "mapTransform.142.p1300 -> Some(AbstractMap1(AbstractMap2(f)) => Split(I) o AbstractMap1(f) o Join())"
echo "mapTransform.143.p21936 -> Some(AbstractMap1(AbstractMap2(f)) => Split(I) o AbstractMap1(f) o Join())"
echo "mapTransform.153.p43264 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.154.p56297 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.155.p51478 -> Some(Map(f) => MapSeq(f))"
echo "mapTransform.156.p25387 -> Some(AbstractMap1(AbstractMap2(f)) => Split(I) o AbstractMap1(f) o Join())"
echo "*** Tuning params ***"
echo "tileWidth -> 1, tileHeight -> 1, tileDepth -> 1,"
echo "kernelGroupSize -> 1,"
echo "inputCacheSizeX -> 1, inputCacheSizeY -> 1,"
echo "kernelCacheSize -> 1"
echo "localSize0 -> 1"
echo "localSize1 -> 12"
echo "localSize2 -> 1"
echo "nWrgs0 -> 512"
echo "nWrgs1 -> 1"
echo "nWrgs2 -> 1"

echo "Generated on 2020-07-16-16-25-04"
g++ ./test_harness.cpp -I. -o ./a.out -std=c++11 -lOpenCL -I/home/shunya/armnn-onnx/ComputeLibrary/include -L/usr/lib/aarch64-linux/gnu/ -lmali -D INPUT_CHANNELS=512 -D INPUT_WIDTH=14 -D INPUT_HEIGHT=14 -D PAD_FUNC_X=1 -D PAD_FUNC_Y=1 -D KERNEL_WIDTH=3 -D KERNEL_HEIGHT=3 -D KERNEL_STRIDE_X=1 -D KERNEL_STRIDE_Y=1 -D NUM_KERNELS=512$TRIALS -Wno-ignored-attributes
./a.out .