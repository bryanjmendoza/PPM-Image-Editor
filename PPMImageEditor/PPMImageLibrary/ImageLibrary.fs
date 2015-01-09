//
//F #-based PPM image library
//
// Bryan Mendoza
//U. of Illinois, Chicago
//CS 341, Fall 2014
//Homework 4
//

module PPMImageLibrary

#light
open System.IO

//
// DebugOutput:
//
// Outputs to console, which appears in the "Output" window pane of
// Visual Studio when you run with debugging (F5).
//
let rec private OutputImage(image:int list list) = 
  match image with
  | [ ] -> printfn "**END**"
  |  _  -> printfn "%A" image.Head
           OutputImage(image.Tail)
           
let DebugOutput(width:int, height:int, depth:int, image:int list list) =
  printfn "**HEADER**"
  printfn "W=%A, H=%A, D=%A" width height depth
  printfn "**IMAGE**"
  OutputImage(image)


//
// TransformFirstRowWhite:
//
// An example transformation: replaces the first row of the given image
// with a row of all white pixels.
//
let rec BuildRowOfWhite cols white = 
  match cols with
  | 0 -> []
  | _ -> // 1 pixel, i.e. RGB value, followed by remaining pixels:
         white :: white :: white :: BuildRowOfWhite (cols-1) white

let TransformFirstRowWhite(width:int, height:int, depth:int, image:int list list) = 
  // first row all white :: followed by rest of original image
  BuildRowOfWhite width depth :: image.Tail



//
// WriteP3Image:
//
// Writes the given image out to a text file, in "P3" format.  Returns true if successful,
// false if not.
//
let rec parsePixels(pixel:int list, wr:StreamWriter, multiple:int) = 
    if pixel = [] then
        wr.Write("")
    else
        wr.Write(pixel.Head.ToString()+ " ")
        parsePixels(pixel.Tail,wr,(multiple+1))


let rec parseImage(image:int list list, wr:StreamWriter) =  
    if image = [] then
        wr.WriteLine()
    else
        parsePixels(image.Head,wr,1)
        wr.WriteLine()
        parseImage(image.Tail,wr)


let WriteP3Image(filepath:string, width:int, height:int, depth:int, image:int list list) = 
  
  if image = [] then
    false
  else  
    use wr = new StreamWriter(filepath)
    wr.WriteLine("P3")
    wr.WriteLine("{0} {1}",width,height)
    wr.WriteLine(depth)
    parseImage(image, wr)  
    true  // success


//GrayScale Functions

//Returns average of pixel
let calculateAve(pixel:int list) = 
    let ave = List.average ([(double)pixel.[0]]@[(double)pixel.[1]]@[(double)pixel.[2]])
    (int)ave

let rec _TransformGrayscale(ave:int, index:int, pixel:int list, result) =
    match pixel with
    | [] -> List.rev result
    | _ -> if index % 3 = 0 then //Every three integers compute the pixel average
            let newAve = calculateAve(pixel)
            _TransformGrayscale(newAve,(index+1),pixel.Tail,newAve::result)
           else
            _TransformGrayscale(ave,(index+1),pixel.Tail,ave::result)

//TransformGrayScale:
//Converts image into Grayscale

let TransformGrayscale(width:int, height:int, depth:int, image:int list list) =
    let GrayScaleImage = List.map (fun pixel -> _TransformGrayscale(0,0,pixel,[])) image
    GrayScaleImage


//Invert image 
let rec _TransformInvert(depth:int, pixel:int list, result) = 
    match pixel with
    | [] -> List.rev result
    | _ -> _TransformInvert(depth,pixel.Tail,(depth-pixel.Head)::result)

let TransformInvert(width:int, height:int, depth:int, image:int list list) = 
    let InvertedImage =  List.map (fun pixel -> _TransformInvert(depth, pixel,[])) image 
    InvertedImage //Return modified image


//FlipHorizontal
let rec _TransformFlipHorizonal(pixel:int list, index:int, result:int list) = 
    match pixel with
    | [] -> result
    | _ -> if index % 3 = 0 then //Every index that is multiple of 3, we append the next three ints  
                _TransformFlipHorizonal(pixel.Tail, (index+1), pixel.[0]::pixel.[1]::pixel.[2]::result)
           else //Otherwise just call withouth appending any ints
                _TransformFlipHorizonal(pixel.Tail, (index+1), result)


let TransformFlipHorizontal(width:int, height:int, depth:int, image:int list list) =
    let horizontalImage = List.map(fun pixel -> _TransformFlipHorizonal(pixel,0,[])) image
    horizontalImage

//Flip Vertical

let TransformFlipVertical(width:int, height:int, depth:int, image:int list list) =
    let verticalImage = List.rev image
    verticalImage


// Rotate 90

//Creates list of three ints that form new pixel
let createSet(pixel:int list) = 
    pixel.[0]::pixel.[1]::[pixel.[2]]


let rec _RotateRight90(cols:int, index:int, image:int list list, result) =
    //Columns is the width of the image
    match cols with
    | 0 ->  List.rev result 
    | _ ->  if index % 3 = 0 then //Every multiple of 3 represents new pixel start index
                //Create an int list list by calling createSet function. Then reverse this list and
                //concatenate the new int list list into an int list 
                let newL = List.concat (List.rev ((List.map (fun L -> createSet L) image)))
                //Every recursive call we remove the first element of every row in the image list using List.map List.tail image
                _RotateRight90(cols-1,index+1,(List.map List.tail image),newL::result) 
            else
                //Do not decrement columns here, we need a multiple of 3 to work with next pixel
                _RotateRight90(cols,index+1,(List.map List.tail image),result)
             

let RotateRight90(width:int, height:int, depth:int, image:int list list) =
    let newImage = _RotateRight90(width,0,image,[])
    newImage
