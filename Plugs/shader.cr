struct Vec2
    property x, y
    def initialize(@x : Int64, @y : Int64)
    end
end
#Vec2 End
struct Vec3
    property x, y, z
    def initialize(@x : Int64, @y : Int64, @z : Int64)
    end
end
#Vec3 End
struct Vec4
    property w, x, y, z
    def initialize(@w : Int64, @x : Int64, @y : Int64, @z : Int64)
    end
end
#Vec4 End

class Shader
    Vec2.new(1,1)
    Vec3.new(1,1,1)
    Vec4.new(1,1,1,1)
end

# Note OpenGL interop will begin here, embracing composition with language, material, math and graphics