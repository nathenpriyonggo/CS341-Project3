//
// F# image processing functions.
//
// This F# code defines functions for applying filters, transformations, and 
// edge detection to images through RGB adjustments and pixel comparisons.
//
// Nathen Priyonggo
// U. of Illinois, Chicago
// CS 341, Spring 2024
//

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //

  //
  // Sepia:
  //
  // Applies a sepia filter onto the image and returns the 
  // resulting image as a list of lists. 
  // The sepia filter adjusts the RGB values of each pixel
  // according to the following formulas:
  //    newRed = 0.393*origRed + 0.769*origGreen + 0.189*origBlue
  //    newGreen = 0.349*origRed + 0.686*origGreen + 0.168*origBlue
  //    newBlue = 0.272*origRed + 0.534*origGreen + 0.131*origBlue
  // We will use truncation to cast from the floating point result 
  // to the integer value.
  // 
  // If any of these values exceed 255, then 255 should be used
  // instead for that value.
  //
  // Returns: updated image.
  //


  // Helper function to cap values at 255
  let fixVal value =
    if value > 255 then
      255
    else
      value


  // Recursive sepia function
  let rec Sepia (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    match image with
    | [] -> image // Return final list of sepia-ed RGB values
    | row::tl ->
      let newRow = 
        List.map (fun (r, g, b) ->
            let newR = fixVal(int(0.393* float r + 0.769* float g + 0.189* float b)) // Adjust values
            let newG = fixVal(int(0.349* float r + 0.686* float g + 0.168* float b))
            let newB = fixVal(int(0.272* float r + 0.534* float g + 0.131* float b))
            (newR, newG, newB) // Return new RGB values
          ) row
      
      newRow :: Sepia width height depth tl // Prepend new RGB values, recursive call rest of list
    

  //
  // Increase Intensity
  //
  // Increase the intensity of a particular RGB channel
  // according to the values of the parameters.
  // The intensity is the scaling factor by which the
  // channel selected should be increased (or decreased 
  // if the value is less than 1).
  // The channel is one of 'r', 'g', or 'b' which 
  // correspond to red, green, and blue respectively.
  // If the channel is not one of those three values,
  // do not modify the image.
  // Remember that the maximum value for any pixel 
  // channel is 255, so be careful of overflow!
  //
  // Returns: updated image.
  //


  // Helper function to determine channel
  let internsifyValues r g b intensity channel =
    if channel = 'r' then 
      let newR = fixVal(int(double r * intensity))
      (newR, g, b)
    else if channel = 'g' then  
      let newG = fixVal(int(double g * intensity))
      (r, newG, b)
    else if channel = 'b' then
      let newB = fixVal(int(double b * intensity))
      (r, g, newB)
    else 
      (r, g, b)


  // Recursive intensifying function
  let rec IncreaseIntensity (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (intensity:double)
                    (channel:char) = 
    match image with
    | [] -> image // Return final list of intensified RGB values
    | row::tl ->
      let newRow = 
        List.map (fun (r, g, b) -> 
          internsifyValues r g b intensity channel // Return new RGB values
          ) row
      
      newRow :: IncreaseIntensity width height depth tl intensity channel // Prepend new RGB values, recursive call rest of list


  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    match image with
    | [] -> [] // Return final list of flipped RGB values
    | row::tl ->
      (List.rev row)::FlipHorizontal width height depth tl // Prepend reversed RGB values, recursive call rest of list


  //
  // Rotate180:
  //
  // Rotates the image 180 degrees.
  //
  // Returns: updated image.
  //
  let rec Rotate180 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    match image with
    | [] -> [] // Return final list of flipped RGB values
    | row::tl ->
      Rotate180 width height depth tl @ [List.rev row]// Append reversed RGB values, recursive call rest of list


  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "significantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compare each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //


  // Helper function to calculate color difference
  let distance (r1, g1, b1) (r2, g2, b2) = 
    let diffR = float(r1 - r2)
    let diffG = float(g1 - g2)
    let diffB = float(b1 - b2)
    sqrt (diffR * diffR + diffG * diffG + diffB * diffB)


  // Helper function to process each pixel
  let rec processRow row nextRow threshold = 
    match row, nextRow with
    | [], _ -> [] // End of row
    | _, [] -> [] // End of next row
    | [(r, g, b)], _ -> [] // Last pixel in row
    | (r, g, b)::(rr, rg, rb)::restRow, (br, bg, bb)::restNextRow ->
      // Calculate color differences
      let rightDiff = distance (r, g, b) (rr, rg, rb)
      let belowDiff = distance (r, g, b) (br, bg, bb)

      // Determine if edge occurs
      let newPixel = 
        if (rightDiff > float threshold || belowDiff > float threshold) then
          (0, 0, 0)
        else 
          (255, 255, 255)

      // Recursive call to process next pixel
      newPixel :: processRow ((rr, rg, rb)::restRow) restNextRow threshold


  // Recursive edge detecting function
  let rec EdgeDetect (width:int)
                     (height:int)
                     (depth:int)
                     (image:(int*int*int) list list)
                     (threshold:int) = 
    match image with 
    | [] -> [] // End of image
    | [_] -> [] // Last row
    | row::nextRow::rest ->
      // Process current row and recursively prepend to new list
      processRow row nextRow threshold :: EdgeDetect width height depth (nextRow::rest) threshold