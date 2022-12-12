# When we have a radius field and a radius method we get the less than helpful error message
#
#Error in R6::R6Class("Circle", inherit = Shape, private = list(radius = 1),  : 
#  All items in public, private, and active must have unique names.


# https://stackoverflow.com/questions/50793617/object-oriented-design-shapes

Shape = R6::R6Class("Shape",
                    private = list(x = 0, y = 0),
                    public = list(
                        moveTo = function(nx, ny) {
                            self$x = nx
                            self$y = ny
                        },
                        move = function(dx, dy) {
                            self$x = self$x + dx
                            self$y = self$y + dy
                        },                        
                        
                        area = function() NA
                    ))

Polygon = R6::R6Class("Polygon", inherit = Polygon)

Quadrilateral = R6::R6Class("Quadrilateral", inherit = Polygon,
                            private = list(.lengths = c(1, 1)))

Triangle = R6::R6Class("Triangle", inherit = Polygon)

Hexagon = R6::R6Class("Hexagon", inherit = Polygon)



Ellipse = R6::R6Class("Ellipse", inherit = Shape,
                      private = list(.radii = 1),
                      public = list(area = function() pi*.radius[1]*.radius[2])
                      )

Circle = R6::R6Class("Circle", inherit = Ellipse,
                    
                     public = list(
                         initialize = function(x, y, r) {
                             super$initialize(x, y)
                             
                         },
                        radius = function(value) if(missing(value)) self$private$.radius else  self$private$.radius = value
                    )
                    )

Rectangle = R6::R6Class("Rectangle", inherit = Quadrilateral,
                        public = list(
                            area = function()
                                prod(.lengths),
                            initialize = function(x, y, sides) {
                                super$initialize(x, y)
                                self$private$.lengths = sides
                            }
                        ))
             
                        
Square = R6::R6Class("Square", inherit = Rectangle,
                     public = list(initialize = function(x, y, side) {
                            super$initialize(x, y, c(side, side))
                     })
                     )


