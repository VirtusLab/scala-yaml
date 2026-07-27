package org.virtuslab.yaml.benchmark

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(
  value = 5,
  jvmArgs = Array(
    "-server",
    "-Xnoclassgc",
    "-Xms4g",
    "-Xmx4g",
    "-Xss1m",
    "-XX:NewSize=3g",
    "-XX:MaxNewSize=3g",
    "-XX:InitialCodeCacheSize=512m",
    "-XX:ReservedCodeCacheSize=512m",
    "-XX:NonNMethodCodeHeapSize=32m",
    "-XX:NonProfiledCodeHeapSize=240m",
    "-XX:ProfiledCodeHeapSize=240m",
    "-XX:TLABSize=4m",
    "-XX:-ResizeTLAB",
    "-XX:+UseParallelGC",
    "-XX:-UseAdaptiveSizePolicy",
    "-XX:MaxInlineLevel=20",
    "-XX:InlineSmallCode=2500", // Use defaults from Open JDK 17+
    "-XX:+AlwaysPreTouch",
    // "-XX:+UseTransparentHugePages", Linux only
    "-XX:-UseDynamicNumberOfGCThreads",
    "-XX:+UseNUMA",
    "-XX:-UseAdaptiveNUMAChunkSizing",
    "-XX:+PerfDisableSharedMem", // See https://github.com/Simonis/mmap-pause#readme
    "-XX:-UseDynamicNumberOfCompilerThreads",
    "-XX:-UsePerfData",
    "-XX:+UnlockExperimentalVMOptions",
    "-XX:+TrustFinalNonStaticFields"
  )
)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
abstract class CommonParams
