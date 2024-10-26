# F# Image Processing Library

This F# library provides functions for basic image processing, including filters, transformations, and edge detection, all achieved through RGB adjustments and pixel comparisons.

## Features

- **Sepia Filter**: Applies a sepia tone to an image by adjusting RGB values.
- **Increase Intensity**: Increases or decreases intensity of a specified RGB channel.
- **Flip Horizontal**: Flips the image horizontally, mirroring the content.
- **Rotate 180°**: Rotates the image by 180 degrees.
- **Edge Detection**: Detects edges based on color differences between adjacent pixels.

## Usage

To use this library, import the `ImageLibrary.Operations` module and call the functions with your image data formatted as a list of lists of RGB tuples.

### Example

Here’s a quick example of applying the Sepia filter:

```fsharp
open ImageLibrary.Operations

// Sample image data (3x3 image)
let image = [
    [(100, 150, 200); (120, 170, 220); (130, 180, 210)]
    [(110, 140, 190); (130, 160, 210); (140, 170, 200)]
    [(120, 150, 180); (130, 140, 190); (150, 160, 180)]
]

// Apply Sepia filter
let sepiaImage = Sepia 3 3 255 image
