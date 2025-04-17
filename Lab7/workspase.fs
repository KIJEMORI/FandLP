// Задание 1

[<AbstractClass>]

type Shape() =
    abstract member Area: float
 

type IPrint =
    abstract member Print: unit -> unit
 

type Rectangle(width: int, height: int) =
    inherit Shape()
    member this.Width = width
    member this.Height = height
    override this.Area = float this.Width * float this.Height
    override this.ToString() =
        sprintf "ширина = %d, высота = %d, площадь = %.2f" this.Width this.Height this.Area
    interface IPrint with
        member this.Print() = System.Console.WriteLine(this.ToString())
 
type Square(side: int) =
    inherit Rectangle(side, side)

let PI = 3.14

type Circle(radius: int) =
    inherit Shape()
    member this.Radius = radius
    override this.Area = PI * float this.Radius * float this.Radius
    override this.ToString() =
        sprintf "Круг: радиус = %d, площадь = %.2f" radius this.Area
    interface IPrint with
        member this.Print() = System.Console.WriteLine(this.ToString())

let rect = Rectangle(4, 5)
let square = Square(3)
let circle = Circle(2)

(rect :> IPrint).Print()
(square :> IPrint).Print()
(circle :> IPrint).Print()
