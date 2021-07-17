# Image Processing Benchmarks

The paclet aims providing automatic tools for benchmarking and analyzing the performance of image processing functions.

## How to contribute

* Open a PR to contribute to the functional part.
* Open a PR to updated any function result.

## How to install

Clone the repo:

```
git clone ssh://git@stash.wolfram.com:7999/~mikayele/ipbenchmarks.git *directory*
```

Add the repo to the path:

```
PacletDirectoryLoad[*directory*]
```

Load the repo:

```
Needs["IPBenchmarks`"]
```

## Paclet structure
![](Kernel/Extras/DocumentationImages/Structure.png)

### Some elaborations

##### Benchmark types

There are 4 type of benchmarks that measure function behaviour dependency on:
 - input image size and memory usage
 - input image color space
 - input image type
 - input image channel interleaving property

##### ResultStorage

Each function has it's own file `functionName.wl` that contains a serialized byte array, which is an association of the following form:

```
<|
	"Time" -> <|
		2 -> {
			10 reals corresponding to image size benchmark,
			10 reals corresponding to color space benchmark,
			5 reals corresponding to image type benchmark,
			4 reals corresponding to image channel interleaving benchmark
		}
		,
		3 -> {
			10 reals corresponding to image size benchmark,
			4 reals corresponding to color space benchmark,
			5 reals corresponding to image type benchmark,
			4 reals corresponding to image channel interleaving benchmark
		}
	|>
	,
	"Memory" -> <|
		2 -> {
			10 integers corresponding to image size benchmark,
			10 integers corresponding to color space benchmark,
			5 integers corresponding to image type benchmark,
			4 integers corresponding to image channel interleaving benchmark
		}
		,
		3 -> {
			10 integers corresponding to image size benchmark,
			4 integers corresponding to color space benchmark,
			5 integers corresponding to image type benchmark,
			4 integers corresponding to image channel interleaving benchmark
		}
	|>
	,
	"TotalTimeSpent" -> quantity
	,
	"Thread" -> <|
		2 -> <|
			1 -> real, 4 -> real
		|>
		,
		3 -> <|
			1 -> real, 4 -> real
		|>
	|>
	,
	"MemoryLeaks" -> <|
		2 -> <|
			quantity
		|>
		,
		3 -> <|
			quantity
		|>
	|>
|>
```

Clearly the `dimension` key is not present if the function doesn't support `dimension`-al image (i.e. `Image.wl` doesn't have `3` as an internal key).
Some files may have only 10 integers for `Memory` block, since the benchmarks have been run differently before; re-running the benchmark will re-generate the 'proper' file structure. For each of the benchmarks current time constrained in set to 200 seconds.

## Definitions

There are a few assumptions and definitions:

* The original image is an RGB image of size `{1024, 768}` 
* For size benchmark the following 10 image sizes are used:

| MP  |        2D      |        3D        |
|:---:|:---------------|:-----------------|
| 0.1 | `{100, 100}`   | `{25, 25, 20}`   |
| 1   | `{1632, 1200}` | `{128, 128, 64}` |
| 2   | `{1632, 1200}` | `{204, 200, 48}` |
| 3   | `{2048, 1536}` | `{256, 192, 64}` |
| 4   | `{2048, 1536}` | `{204, 176, 112}`|
| 6   | `{3008, 2000}` | `{256, 200, 94}` |
| 8   | `{3264, 2448}` | `{256, 289, 108}`|
| 10  | `{3872, 2592}` | `{256, 363, 108}`|
| 12  | `{4290, 2800}` | `{400, 390, 77}` |
| 14  | `{4920, 3264}` | `{510, 192, 164}`|

* Following color spaces are used:

| Color space           |   2D		| 3D	  |
|:--------------------: |:------:	|:-------:|
| GrayLevel          	| &check;	| &check; |
| GrayLevel + &alpha; 	| &check;	| &cross; |
| RGB           		| &check;	| &check; |
| RGB + &alpha;			| &check;	| &cross; |
| CMYK           		| &check;	| &cross; |
| HSB           		| &check;	| &cross; |
| LCH           		| &check;	| &cross; |
| LUV           		| &check;	| &cross; |
| XYZ          			| &check;	| &cross; |
| Binary           		| &cross;	| &check; |
| 6 channel automatic	| &cross;	| &check; |

* Following image types are used:

| Image type   	    |   2D		| 3D	  |
|:----------------:	|:-------:  |:-------:|
| Bit				| &check;	| &check; |
| Byte				| &check;	| &check; |
| Bit16			    | &check;	| &check; |
| Real32			| &check;	| &check; |
| Real64			| &check;	| &check; |

* Following interleaving combinations are used:

| Color space					|2D		  | 3D      |
|:--------------------------:	|:------: |:-------:|
| RGB planar					| &check; | &check; |
| RGB interleaved				| &check; | &check; |
| RGB + &alpha; planar			| &check; | &check; |
| RGB + &alpha; interleaved	    | &check; | &check; |

* Copy button: ![Copy](Kernel/Extras/DocumentationImages/copy.png)

Clicking the button on any construct will copy the data used to create the latter to the clipboard.

* Refresh button: ![Refresh](Kernel/Extras/DocumentationImages/refresh.png)

Clicking the button on any construct will re-run the benchmarks of the data at the  narrowest scope.

An example operation looks like:

![](Kernel/Extras/DocumentationImages/RefreshProcess.png) 

## Exposed functions

The main and only namespace to use is `IPBenchmarks`. 

#### IPBenchmarks palette	

Simply call `InstallPalette[]`. This will require a Mathematica restart to have an effect, after which `IPBenchmarks` can be found in the palettes menu.

![](Kernel/Extras/DocumentationImages/Palette.png)

The palette itsels is a simple mechanism to generate a summary notebook for a function, which will be stored on the desktop.

*$HomeDirectory is default IP Palette directory.*

To uninstall the palette simply call `UnInstallPalette[]`. This will require a Mathematica restart to have an effect. 

#### Notebook created by IPPalette

After installing the IPPalette and entering the `functionName`, a notebook is created with basic information about the `functionName`.

![](Kernel/Extras/DocumentationImages/PaletteNB.png)

Simply call `Update IPBenchmarks`:
 - Clone the repo: `git clone; ssh://git@stash.wolfram.com:7999/~mikayele/ipbenchmarks.git`;
 - Add the repo to the path: `PacletDirectoryLoad["$HomeDirectory/IPBenchmarks"]`;
 - Fetch/Pull repo;
 - Load the repo: `Needs["IPBenchmarks"]`.

Get Results cell contains `FunctionSummary[functionName, numberOfDimensions]` storing the main information about `functionName`.

Notebook allows to compare Master and Branch timings for 2D/3D image benchmakrs by using three comparison functions:

 - `FunctionBenchmarkTimingsDifference` gives execution timing and used memory based on benchmark types;
 - `FunctionSummaryDifference` gives following results:
 	- Threads timings (1 vs 4);
 	- Memory leaks;
 	- ImageType preserving after `functionName` apply ;
 	- ColorSpace preserving after `functionName` apply;
 - `FunctionAutomaticIssuesSummaryDifference` gives median and issue timings for benchmark types.

 *Master and Branch results can be copied from FunctionSummary GUI.*

#### DatasetAllFunctionMeanTiming

```DatasetAllFunctionMeanTiming[numberOfDimensions]``` 

![](Kernel/Extras/DocumentationImages/Dataset.png)

The function returns a mean time for all functions for given `numberOfDimensions` as an Dataset object.

#### FunctionPropertyPreserveDataset

```FunctionPropertyPreserveDataset[imageProperty: "ImageType" | "ColorSpace", functionName, numberOfDimensions]```   

![](Kernel/Extras/DocumentationImages/FunctionPropertyPreserveDataset.png)

The function returns a grid with ImageType/ColorSpace preserving after `functionName` apply.

#### FunctionPropertyPreserveCluster

```FunctionPropertyPreserveCluster[imageProperty: "ImageType" | "ColorSpace", functionName, numberOfDimensions]```

![](Kernel/Extras/DocumentationImages/FunctionPropertyPreserveCluster.png)

The function returns a grid with ImageType/ColorSpace preserve cluster after `functionName` apply.

#### FunctionSummary 

```FunctionSummary[functionName, numberOfDimensions]```

![](Kernel/Extras/DocumentationImages/FunctionSummary.png)

*Refresh on each of the charts will run only the specific benchmark for the function, and will rebase the byte array stored in the original file in ResultStorage.*

*Edit allows to update the function signature. The original signature will be overwritten*

The function returns a dynamic grid with a general information about `functionName` based on 4 benchmarks (image size, image type, color space, channel interleaving)..

#### FunctionSummaryDifference

```FunctionSummaryDifference[master, branch]``` or ```FunctionSummaryDifference[<|"Master" -> master, "Branch" -> branch|>]```

![](Kernel/Extras/DocumentationImages/FunctionSummaryDifference.png)

The function returns a grid contains comparison for function thread timings, memory leaks and image type, color space preserves.

*Master and Branch results can be copied from FunctionSummary GUI.*

#### FunctionBenchmarkTimingsDifference

```FunctionBenchmarkTimingsDifference[master, branch]```
```FunctionBenchmarkTimingsDifference[<|"Master" -> master, "Branch" -> branch|>]```

![](Kernel/Extras/DocumentationImages/FunctionBenchmarkTimingsDifference.png)

The function returns a grid contains comparison for function execution timings and used memories based on 4 benchmarks (image size, image type, color space, channel interleaving)..

*Master and Branch results can be copied from FunctionSummary GUI.*

#### FunctionAutomaticIssuesSummaryDifference

```FunctionAutomaticIssuesSummaryDifference[master, branch]```   
```FunctionAutomaticIssuesSummaryDifference[<|"Master" -> master, "Branch" -> branch|>]```

![](Kernel/Extras/DocumentationImages/FunctionAutomaticIssuesSummaryDifference.png)   

The function returns a grid contains comparison for function median and issue values based on benchmark types;, where:
- `Median time` is the median execution time for a one of the benchmarks;
- `Median input` is the function call corresponding to the `Median time`;
- `Issue slowness` is the percentage deviation from the `Median time` (starting at 100 percent);
- `Issue input` is the function call corresponding to the `Issue slowness`;

*Master and Branch results can be copied from FunctionSummary GUI.*

#### FunctionTimeCharts

```FunctionTimeCharts[functionName, numberOfDimensions]```   

![](Kernel/Extras/DocumentationImages/FunctionTimeCharts.png)

*Refresh on each of the charts will run only the specific benchmark for the function, and will rebase the byte array stored in the original file in ResultStorage.*

The function returns a dynamic grid of 4 plots that visualize the `functionName` behaviour dependency on:

 - input image size and memory usage
 - input image color space
 - input image type
 - input image channel interleaving property

Each of charts above can be generated individually using the following functions:

- `PlotSizeAndMemoryDependency`
- `PlotColorSpaceDependency`
- `PlotImageTypeDependency`
- `PlotChannelInterleavingDependency`